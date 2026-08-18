# Are the two<->one-station flips near the stage boundary short, isolated
# blips (arguably classification noise, safe to drop) or sustained multi-day
# excursions (a real hydrologic signal that shouldn't be discarded)? Run-length
# analysis of the daily two-station-included/excluded sequence, per site.

library(tidyverse)

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "ID", "LF")

two <- read_csv(file.path(outdir, "two.station.results_M7_breakpoint_domain.csv"), show_col_types = FALSE) %>%
  distinct(ID, date) %>% mutate(included = TRUE)

depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

daily <- depth_daily %>%
  left_join(two, by = c("ID", "date")) %>%
  mutate(included = replace_na(included, FALSE)) %>%
  arrange(ID, date)

# ---- run-length encoding of excluded streaks, per site --------------------
run_summary <- map_dfr(sites, function(s) {
  d <- daily %>% filter(ID == s)
  r <- rle(d$included)
  runs <- tibble(run_included = r$values, run_length = r$lengths)
  excluded_runs <- runs %>% filter(!run_included)
  tibble(
    ID = s,
    n_excluded_days_total = sum(excluded_runs$run_length),
    n_excluded_streaks = nrow(excluded_runs),
    pct_excluded_streaks_1to2_days = round(100 * mean(excluded_runs$run_length <= 2), 1),
    pct_excluded_DAYS_in_streaks_le2 = round(100 * sum(excluded_runs$run_length[excluded_runs$run_length <= 2]) / sum(excluded_runs$run_length), 1),
    median_excluded_streak_days = median(excluded_runs$run_length),
    max_excluded_streak_days = max(excluded_runs$run_length),
    n_days_total = nrow(d)
  )
})
cat("=== Excluded (one-station-only) streak lengths, per site ===\n")
print(run_summary)

# ---- same, but restricted to the near-boundary zone (depth in the top 20%
# of that site's observed range) -- this is specifically where the flip-flop
# matters for coalescence -----------------------------------------------
near_boundary_summary <- map_dfr(sites, function(s) {
  d <- daily %>% filter(ID == s)
  thresh <- quantile(d$depth, 0.80, na.rm = TRUE)
  d_top <- d %>% filter(depth >= thresh) %>% arrange(date)
  r <- rle(d_top$included)
  runs <- tibble(run_included = r$values, run_length = r$lengths)
  excluded_runs <- runs %>% filter(!run_included)
  tibble(
    ID = s, depth_p80 = round(thresh, 2),
    n_days_in_zone = nrow(d_top),
    pct_of_zone_excluded = round(100 * mean(!d_top$included), 1),
    n_excluded_streaks = nrow(excluded_runs),
    median_excluded_streak_days = if (nrow(excluded_runs) > 0) median(excluded_runs$run_length) else NA,
    pct_excluded_days_in_streaks_le2 = if (sum(excluded_runs$run_length) > 0)
      round(100 * sum(excluded_runs$run_length[excluded_runs$run_length <= 2]) / sum(excluded_runs$run_length), 1) else NA
  )
})
cat("\n=== Same, restricted to top-20%-depth zone (the coalescence-relevant zone) ===\n")
print(near_boundary_summary)

write_csv(run_summary, file.path(outdir, "transition_runlength_summary.csv"))
write_csv(near_boundary_summary, file.path(outdir, "transition_runlength_near_boundary.csv"))

# ---- visual: same time-series-by-depth view, but now showing INCLUDED vs
# EXCLUDED as day-level status (post all filters, not just the raw reach test)
p <- daily %>%
  ggplot(aes(x = date, y = depth, color = included)) +
  geom_point(size = 0.7) +
  facet_wrap(~ID, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(`TRUE` = "#1b7837", `FALSE` = "grey70"),
                      labels = c(`TRUE` = "two-station included", `FALSE` = "one-station only")) +
  labs(title = "Final day-level classification (M7): where do isolated one-station-only days occur?",
       subtitle = "Grey specks scattered inside a green run = short isolated exclusions near the boundary",
       x = NULL, y = "depth (m)", color = NULL) +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/09_daily_classification_timeseries.png"), p, width = 13, height = 11, dpi = 150)

cat("\nDone -> transition_runlength_summary.csv, transition_runlength_near_boundary.csv, figures/09_daily_classification_timeseries.png\n")
