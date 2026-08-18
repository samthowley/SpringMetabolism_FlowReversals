# Breakpoint (segmented) K600~depth: power-law decline up to a breakpoint
# depth, then held flat (constant) beyond it. Two variants:
#   M6_breakpoint_stat   -- breakpoint chosen by grid search minimizing total
#                           SSE (declining power fit below, flat above).
#   M7_breakpoint_domain -- breakpoint fixed at the max depth actually sampled
#                           by the gas dome (no data beyond it, so hold flat
#                           rather than extrapolate a decay that was never
#                           validated out there).
# Both use a power fit (log-log OLS) on the declining segment, using the
# judgment-trimmed points below (hand-vetted per-point exclusions -- reasoning
# per point: AM row 24 & row 5 = replicate/neighbor disagreement, GB row 16 =
# 2.6x its own same-day same-depth replicates, ID row 6 = 2x spike vs
# immediate depth neighbors, LF row 5 = low outlier in a tight depth cluster).

library(tidyverse)

sites <- c("AM", "GB", "ID", "LF")
outdir <- "04_Outputs/Power Function RC"

valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE)

judgment_drop <- tribble(
  ~ID, ~row,
  "AM", 24,
  "AM", 5,
  "GB", 16,
  "ID", 6,
  "LF", 5
)

trimmed <- valid %>% anti_join(judgment_drop, by = c("ID", "row"))

# ---- statistical breakpoint search per site --------------------------------
find_breakpoint <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < max(df$depth)]  # need >=4 pts left, >=1 right
  results <- map_dfr(cand, function(bp) {
    left <- df %>% filter(depth <= bp)
    right <- df %>% filter(depth > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600_1.day) ~ log(depth), data = left)
    flatval <- exp(predict(m, newdata = data.frame(depth = bp)))
    sse_left <- sum((left$k600_1.day - exp(predict(m)))^2)
    sse_right <- sum((right$k600_1.day - flatval)^2)
    tibble(bp = bp, sse = sse_left + sse_right, n_left = nrow(left), n_right = nrow(right))
  })
  results
}

bp_search <- map_dfr(sites, function(s) find_breakpoint(trimmed %>% filter(ID == s)) %>% mutate(ID = s))
write_csv(bp_search, file.path(outdir, "breakpoint_search_by_site.csv"))

best_bp <- bp_search %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()
cat("=== Statistically best breakpoint per site ===\n")
print(best_bp)

# How flat is the SSE curve near the minimum? (= how confidently is the
# breakpoint actually identified, given so little data)
flatness <- bp_search %>% group_by(ID) %>%
  summarise(sse_min = min(sse), sse_range = max(sse) - min(sse),
            pct_candidates_within_10pct_of_min = round(100 * mean(sse <= 1.1 * min(sse)), 0),
            n_candidates = n(), .groups = "drop")
cat("\n=== How well-identified is the breakpoint? (many candidates near-tied = poorly identified) ===\n")
print(flatness)

# ---- fit the declining-segment power model at the chosen breakpoint -------
fit_segment <- function(df, bp) {
  left <- df %>% filter(depth <= bp)
  m <- lm(log(k600_1.day) ~ log(depth), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  flatval <- a * bp^b
  list(a = a, b = b, bp = bp, flatval = flatval)
}

m6_fits <- map(sites, function(s) {
  bp <- best_bp %>% filter(ID == s) %>% pull(bp)
  fit_segment(trimmed %>% filter(ID == s), bp)
})
names(m6_fits) <- sites

m7_fits <- map(sites, function(s) {
  bp <- max(trimmed %>% filter(ID == s) %>% pull(depth))  # max sampled depth
  fit_segment(trimmed %>% filter(ID == s), bp)
})
names(m7_fits) <- sites

cat("\n=== M6 (statistical breakpoint) fits ===\n")
print(map_dfr(sites, function(s) with(m6_fits[[s]], tibble(ID = s, a, b, bp, flatval))))
cat("\n=== M7 (domain breakpoint = max sampled depth) fits ===\n")
print(map_dfr(sites, function(s) with(m7_fits[[s]], tibble(ID = s, a, b, bp, flatval))))

# ---- apply to full depth series, same daily-max aggregation as before -----
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

apply_breakpoint <- function(fits, label) {
  applied <- depth %>%
    rowwise() %>%
    mutate(
      a = fits[[ID]]$a, b = fits[[ID]]$b, bp = fits[[ID]]$bp, flatval = fits[[ID]]$flatval,
      k600_1d = if_else(depth <= bp, a * depth^b, flatval)
    ) %>%
    ungroup()
  daily <- applied %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop") %>%
    mutate(K600_1.d_daily = na_if(K600_1.d_daily, -Inf),
           Date = ymd_hms(paste(Date, "00:00:00")))
  out_path <- file.path(outdir, paste0("K600_", label, ".csv"))
  write_csv(daily, out_path)
  cat("wrote", out_path, "\n")
}

apply_breakpoint(m6_fits, "M6_breakpoint_stat")
apply_breakpoint(m7_fits, "M7_breakpoint_domain")

# ---- plot: raw data + breakpoint curves, log y -----------------------------
full_depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>% group_by(ID) %>%
  summarise(depth_min_hist = min(depth, na.rm = TRUE), depth_max_hist = max(depth, na.rm = TRUE), .groups = "drop")

curve_grid <- map_dfr(sites, function(s) {
  rng <- full_depth %>% filter(ID == s)
  tibble(ID = s, depth = exp(seq(log(rng$depth_min_hist), log(rng$depth_max_hist), length.out = 300)))
})

pred_all <- curve_grid %>%
  rowwise() %>%
  mutate(
    M6 = with(m6_fits[[ID]], if_else(depth <= bp, a * depth^b, flatval)),
    M7 = with(m7_fits[[ID]], if_else(depth <= bp, a * depth^b, flatval))
  ) %>%
  ungroup() %>%
  select(ID, depth, M6, M7) %>%
  pivot_longer(c(M6, M7), names_to = "method", values_to = "k600")

bp_lines <- bind_rows(
  best_bp %>% transmute(ID, bp, method = "M6"),
  tibble(ID = sites, bp = map_dbl(sites, ~max(trimmed %>% filter(ID == .x) %>% pull(depth))), method = "M7")
)

for (s in sites) {
  p <- ggplot(pred_all %>% filter(ID == s), aes(x = depth, y = k600, color = method)) +
    geom_line(linewidth = 1) +
    geom_vline(data = bp_lines %>% filter(ID == s), aes(xintercept = bp, color = method),
               linetype = "dashed", linewidth = 0.5) +
    geom_point(data = trimmed %>% filter(ID == s), aes(x = depth, y = k600_1.day),
               inherit.aes = FALSE, size = 1.6, alpha = 0.6) +
    scale_y_log10() +
    scale_color_manual(values = c(M6 = "#1b7837", M7 = "#e08214")) +
    labs(title = paste0(s, ": breakpoint K600 curves, M6 vs M7"),
         subtitle = "Dashed = breakpoint depth. M6 = statistically fit, M7 = fixed at max sampled depth",
         x = "depth (m)", y = "K600 (1/day, log scale)", color = NULL) +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")
  ggsave(file.path(outdir, paste0("figures/07_breakpoint_", s, ".png")), p, width = 9, height = 6, dpi = 150)
}

cat("\nDone -> K600_M6_breakpoint_stat.csv, K600_M7_breakpoint_domain.csv, figures/07_breakpoint_<ID>.png\n")
