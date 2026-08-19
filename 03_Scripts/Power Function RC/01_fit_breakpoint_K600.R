library(tidyverse)

sites <- c("AM", "GB", "ID", "LF")
outdir <- "04_Outputs/Power Function RC"

valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE)

judgment_drop <- tribble(
  ~ID, ~row,
  "AM", 24, "AM", 5, "GB", 16, "ID", 6, "LF", 5
)
trimmed <- valid %>% anti_join(judgment_drop, by = c("ID", "row"))

#1. M6: grid search for the breakpoint depth that minimizes combined SSE####
find_breakpoint <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < max(df$depth)]  # need >=4 pts left, >=1 right
  map_dfr(cand, function(bp) {
    left <- df %>% filter(depth <= bp)
    right <- df %>% filter(depth > bp)
    fit <- lm(log(k600_1.day) ~ log(depth), data = left)
    flatval <- exp(predict(fit, newdata = data.frame(depth = bp)))
    sse <- sum((left$k600_1.day - exp(predict(fit)))^2) + sum((right$k600_1.day - flatval)^2)
    tibble(bp = bp, sse = sse)
  })
}

bp_search <- map_dfr(sites, function(s) find_breakpoint(trimmed %>% filter(ID == s)) %>% mutate(ID = s))
write_csv(bp_search, file.path(outdir, "breakpoint_search_by_site.csv"))

best_bp <- bp_search %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()
best_bp

# how confidently is that breakpoint actually identified? (many candidates
# near-tied with the minimum = the location isn't sharply pinned down)
bp_search %>% group_by(ID) %>%
  summarise(pct_within_10pct_of_min = round(100 * mean(sse <= 1.1 * min(sse)), 0), n = n(), .groups = "drop")

#2. fit the power-law decline below the breakpoint, flat above it####
fit_segment <- function(df, bp) {
  left <- df %>% filter(depth <= bp)
  fit <- lm(log(k600_1.day) ~ log(depth), data = left)
  a <- exp(coef(fit)[1]); b <- unname(coef(fit)[2])
  list(a = a, b = b, bp = bp, flatval = a * bp^b)
}

m6_fits <- map(sites, ~fit_segment(trimmed %>% filter(ID == .x), best_bp$bp[best_bp$ID == .x])) %>%
  set_names(sites)
m7_fits <- map(sites, ~fit_segment(trimmed %>% filter(ID == .x), max(trimmed$depth[trimmed$ID == .x]))) %>%
  set_names(sites)

#3. apply to the full depth series, same daily-max aggregation as before####
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

apply_breakpoint <- function(fits) {
  depth %>%
    rowwise() %>%
    mutate(k600_1d = with(fits[[ID]], if_else(depth <= bp, a * depth^b, flatval))) %>%
    ungroup() %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop") %>%
    mutate(K600_1.d_daily = na_if(K600_1.d_daily, -Inf),
           Date = ymd_hms(paste(Date, "00:00:00")))
}

write_csv(apply_breakpoint(m6_fits), file.path(outdir, "K600_M6_breakpoint_stat.csv"))
write_csv(apply_breakpoint(m7_fits), file.path(outdir, "K600_M7_breakpoint_domain.csv"))

#3b. M8: plain power decay, no breakpoint at all -- same a,b as M7 (M7's
# breakpoint sits at max(depth), so its fit already used every point), just
# never flattened; K600 keeps decaying toward 0 as depth increases####
apply_power <- function(fits) {
  depth %>%
    rowwise() %>%
    mutate(k600_1d = with(fits[[ID]], a * depth^b)) %>%
    ungroup() %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop") %>%
    mutate(K600_1.d_daily = na_if(K600_1.d_daily, -Inf),
           Date = ymd_hms(paste(Date, "00:00:00")))
}

write_csv(apply_power(m7_fits), file.path(outdir, "K600_M8_power.csv"))

#4. plot: raw points + both breakpoint curves, log y, one figure per site####
pred_all <- map_dfr(sites, function(s) {
  rng <- range(depth$depth[depth$ID == s], na.rm = TRUE)
  d <- exp(seq(log(rng[1]), log(rng[2]), length.out = 300))
  tibble(ID = s, depth = d,
         M6 = with(m6_fits[[s]], if_else(d <= bp, a * d^b, flatval)),
         M7 = with(m7_fits[[s]], if_else(d <= bp, a * d^b, flatval)),
         M8 = with(m7_fits[[s]], a * d^b))
}) %>%
  pivot_longer(c(M6, M7, M8), names_to = "method", values_to = "k600")

bp_lines <- bind_rows(
  best_bp %>% transmute(ID, bp, method = "M6"),
  tibble(ID = sites, bp = map_dbl(sites, ~max(trimmed$depth[trimmed$ID == .x])), method = "M7")
)

for (s in sites) {
  pred_all %>% filter(ID == s) %>%
    ggplot(aes(x = depth, y = k600, color = method)) +
    geom_line(linewidth = 1) +
    geom_vline(data = bp_lines %>% filter(ID == s), aes(xintercept = bp, color = method), linetype = "dashed") +
    geom_point(data = trimmed %>% filter(ID == s), aes(x = depth, y = k600_1.day), inherit.aes = FALSE, alpha = 0.6) +
    scale_y_log10() +
    theme_minimal()
  ggsave(file.path(outdir, paste0("figures/07_breakpoint_", s, ".png")), width = 9, height = 6, dpi = 150)
}

