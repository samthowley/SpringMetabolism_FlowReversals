rm(list=ls())
library(tidyverse)
library(readxl)

# Breakpoint-anchored velocity~depth: a declining power-law fit (log-log OLS)
# using points up to a breakpoint depth chosen by SSE grid search, instead of
# velocity.R's single global linear fit (velocity ~ depth, no breakpoint),
# which extrapolates straight through zero into implausible negative
# velocities once applied to today's full depth.csv range (e.g. AM min
# velocity -0.42 m/s) -- see [[velocity_rating_curve_extrapolation_bug]].
#
# Unlike K600's M6/M7 (01_fit_breakpoint_K600.R), the fitted curve is NOT
# flattened past the breakpoint. Samantha's call: velocity should only level
# off at zero, not at an arbitrary positive floor pinned to the deepest
# measured point. The power law (b<0) already satisfies that on its own --
# strictly positive, monotonically decaying toward zero as depth increases,
# never needs a hard ceiling/floor at the high end. The breakpoint still
# matters for FITTING (which points anchor the regression so a few sparse
# deep points don't drag the whole curve), just not for what the curve does
# afterward -- it keeps extrapolating a*depth^b past bp instead of flattening.
#
# Source: 04_Outputs/velocity.xlsx (the UNEDITED file velocity.R writes
# straight from 01_Raw_data/u.csv joined to depth by Date), NOT
# velocity_edit.xlsx -- diffing edit against unedited showed edit's Date
# column is scrambled at every site (rows resorted, e.g. by u, without Date
# staying attached), and for AM/GB specifically the (depth,u) pairs
# themselves aren't even the same set as the unedited join (real values
# changed, not just reordered) with no way yet to confirm those changes are
# deliberate corrections vs. an accidental overwrite -- see
# [[velocity_edit_xlsx_alignment_issue]]. Samantha trusts velocity.xlsx more
# pending that decision.
#
# Extends the same "don't extrapolate past validated data" logic to the LOW
# end too: the hand-measured depth range starts well above depth.csv's
# minimum at every site (AM sampled from 0.47 m but depth.csv goes down to
# 0.17 m; similarly GB/ID/OS), and a declining power law diverges as depth ->
# 0, so a floor breakpoint holds velocity flat below the minimum sampled
# depth too. K600's script only handled the high end because K600's sampled
# range already covered its low-depth extreme.
#
# Two variants, same as K600:
#   M6_breakpoint_stat   -- high breakpoint chosen by grid search minimizing
#                           total SSE (declining power fit below, flat above).
#   M7_breakpoint_domain -- high breakpoint fixed at the max depth actually
#                           sampled (no data beyond it, so hold flat rather
#                           than extrapolate a decay that was never
#                           validated out there).
# M6 is the variant that ended up used downstream for K600
# (19_am_lf_onestation_calibrated_K600.R), so it's the primary candidate here
# too; M7 is produced alongside for comparison.

sites <- c("AM", "GB", "ID", "LF", "OS")
outdir <- "04_Outputs/Power Function RC"

sheet_names <- excel_sheets("04_Outputs/velocity.xlsx")
raw <- map_dfr(sheet_names, function(s) read_excel("04_Outputs/velocity.xlsx", sheet = s))

valid <- raw %>%
  filter(!is.na(depth), !is.na(u), u > 0, depth > 0) %>%
  distinct()

cat("=== n valid (depth & u both present, u>0) per site ===\n")
print(valid %>% count(ID))

# ---- statistical (high) breakpoint search per site --------------------------
find_breakpoint <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < max(df$depth)]  # need >=4 pts left, >=1 right
  results <- map_dfr(cand, function(bp) {
    left <- df %>% filter(depth <= bp)
    right <- df %>% filter(depth > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(u) ~ log(depth), data = left)
    flatval <- exp(predict(m, newdata = data.frame(depth = bp)))
    sse_left <- sum((left$u - exp(predict(m)))^2)
    sse_right <- sum((right$u - flatval)^2)
    tibble(bp = bp, sse = sse_left + sse_right, n_left = nrow(left), n_right = nrow(right))
  })
  results
}

bp_search <- map_dfr(sites, function(s) find_breakpoint(valid %>% filter(ID == s)) %>% mutate(ID = s))
write_csv(bp_search, file.path(outdir, "25_velocity_breakpoint_search_by_site.csv"))

best_bp <- bp_search %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()
cat("\n=== Statistically best (high) breakpoint per site ===\n")
print(best_bp)

flatness <- bp_search %>% group_by(ID) %>%
  summarise(sse_min = min(sse), sse_range = max(sse) - min(sse),
            pct_candidates_within_10pct_of_min = round(100 * mean(sse <= 1.1 * min(sse)), 0),
            n_candidates = n(), .groups = "drop")
cat("\n=== How well-identified is the breakpoint? (many candidates near-tied = poorly identified) ===\n")
print(flatness)

# ---- fit the declining-segment power model at the chosen high breakpoint,
#      and the low floor (min sampled depth) ---------------------------------
fit_segment <- function(df, bp) {
  df <- df %>% arrange(depth)
  lo <- min(df$depth)
  left <- df %>% filter(depth <= bp)
  m <- lm(log(u) ~ log(depth), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  flatval_hi <- a * bp^b
  flatval_lo <- a * lo^b
  list(a = a, b = b, bp = bp, lo = lo, flatval_hi = flatval_hi, flatval_lo = flatval_lo)
}

m6_fits <- map(sites, function(s) {
  bp <- best_bp %>% filter(ID == s) %>% pull(bp)
  fit_segment(valid %>% filter(ID == s), bp)
})
names(m6_fits) <- sites

m7_fits <- map(sites, function(s) {
  bp <- max(valid %>% filter(ID == s) %>% pull(depth))  # max sampled depth
  fit_segment(valid %>% filter(ID == s), bp)
})
names(m7_fits) <- sites

cat("\n=== M6 (statistical high breakpoint) fits ===\n")
print(map_dfr(sites, function(s) with(m6_fits[[s]], tibble(ID = s, a, b, lo, bp, flatval_lo, flatval_hi))))
cat("\n=== M7 (domain high breakpoint = max sampled depth) fits ===\n")
print(map_dfr(sites, function(s) with(m7_fits[[s]], tibble(ID = s, a, b, lo, bp, flatval_lo, flatval_hi))))

# ---- apply to full depth series, same daily-mean structure as velocity.R --
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

apply_breakpoint <- function(fits) {
  # High end: no flat plateau at bp/max-sampled-depth anymore -- Samantha's
  # call is that velocity should only level off at zero, not at some
  # arbitrary positive floor pinned to the deepest measured point. The
  # power law (b<0) already does this on its own: it decays toward zero as
  # depth increases but is strictly positive and monotonic, so it never
  # needs a hard floor -- just keep applying a*depth^b past bp instead of
  # flattening there. bp still matters for FITTING (which points anchor the
  # regression), just not for what happens to the curve afterward.
  # Low end unchanged: still flat below the minimum sampled depth, since
  # that's a genuine extrapolation problem (power law diverges as
  # depth -> 0) that a "level at zero" rule doesn't address.
  depth %>%
    rowwise() %>%
    mutate(
      a = fits[[ID]]$a, b = fits[[ID]]$b, lo = fits[[ID]]$lo,
      flatval_lo = fits[[ID]]$flatval_lo,
      velocity = if_else(depth < lo, flatval_lo, a * depth^b)
    ) %>%
    ungroup() %>%
    select(Date, ID, depth, velocity)
}

velocity_m6 <- apply_breakpoint(m6_fits)
velocity_m7 <- apply_breakpoint(m7_fits)

cat("\n=== Candidate velocity range per site (M6, should have no negatives) ===\n")
print(velocity_m6 %>% group_by(ID) %>% summarise(min=min(velocity,na.rm=T), max=max(velocity,na.rm=T), n_neg=sum(velocity<0,na.rm=T)))

write_csv(velocity_m6 %>% select(Date, ID, velocity), file.path(outdir, "25_velocity_M6_breakpoint_stat.csv"))
write_csv(velocity_m7 %>% select(Date, ID, velocity), file.path(outdir, "25_velocity_M7_breakpoint_domain.csv"))
cat("\nWrote 25_velocity_M6_breakpoint_stat.csv and 25_velocity_M7_breakpoint_domain.csv to", outdir, "\n")

# ---- plot: raw data + breakpoint curves, log y -----------------------------
full_depth <- depth %>% group_by(ID) %>%
  summarise(depth_min_hist = min(depth, na.rm = TRUE), depth_max_hist = max(depth, na.rm = TRUE), .groups = "drop")

curve_grid <- map_dfr(sites, function(s) {
  rng <- full_depth %>% filter(ID == s)
  tibble(ID = s, depth = exp(seq(log(rng$depth_min_hist), log(rng$depth_max_hist), length.out = 300)))
})

pred_all <- curve_grid %>%
  rowwise() %>%
  mutate(
    M6 = with(m6_fits[[ID]], if_else(depth < lo, flatval_lo, a * depth^b)),
    M7 = with(m7_fits[[ID]], if_else(depth < lo, flatval_lo, a * depth^b))
  ) %>%
  ungroup() %>%
  select(ID, depth, M6, M7) %>%
  pivot_longer(c(M6, M7), names_to = "method", values_to = "velocity")

bp_lines <- bind_rows(
  best_bp %>% transmute(ID, bp, method = "M6"),
  tibble(ID = sites, bp = map_dbl(sites, ~max(valid %>% filter(ID == .x) %>% pull(depth))), method = "M7")
)

for (s in sites) {
  p <- ggplot(pred_all %>% filter(ID == s), aes(x = depth, y = velocity, color = method)) +
    geom_line(linewidth = 1) +
    geom_vline(data = bp_lines %>% filter(ID == s), aes(xintercept = bp, color = method),
               linetype = "dashed", linewidth = 0.5) +
    geom_point(data = valid %>% filter(ID == s), aes(x = depth, y = u),
               inherit.aes = FALSE, size = 1.6, alpha = 0.6) +
    scale_y_log10() +
    scale_color_manual(values = c(M6 = "#1b7837", M7 = "#e08214")) +
    labs(title = paste0(s, ": breakpoint velocity curves, M6 vs M7"),
         subtitle = "Dashed = fitting cutoff depth (M6 statistically fit, M7 = max sampled). Curve keeps declining past it -- levels only as it approaches zero, not at a flat floor. Flat below min sampled depth still.",
         x = "depth (m)", y = "velocity (m/s, log scale)", color = NULL) +
    theme_bw(base_size = 12) + theme(legend.position = "bottom")
  ggsave(file.path(outdir, "figures", paste0("25_velocity_breakpoint_", s, ".png")), p, width = 9, height = 6, dpi = 150)
}
cat("\nDone -> 25_velocity_M6_breakpoint_stat.csv, 25_velocity_M7_breakpoint_domain.csv, figures/25_velocity_breakpoint_<ID>.png\n")
