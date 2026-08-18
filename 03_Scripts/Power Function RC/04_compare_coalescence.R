# Compares each RC methodology's two-station (mass-balance) ER/GPP against
# the existing, unchanged one-station Bayesian results, per site, to see
# which methodology gives the smoothest ER transition at the stage boundary
# (Samantha's stated primary criterion). One-station results are NOT re-run
# per methodology -- see memory/power_rc_methodology.md for why: the
# one-station Bayesian model (file "two station.R") is informed by the raw
# gas-dome floats directly via its K600 priors, not by the depth-applied RC,
# so it's a fixed reference point across both methodologies here.

library(tidyverse)

outdir <- "04_Outputs/Power Function RC"
methods <- c("M6_breakpoint_stat", "M7_breakpoint_domain")
method_labels <- c(M6_breakpoint_stat = "M6 breakpoint (statistical)",
                    M7_breakpoint_domain = "M7 breakpoint (max sampled depth)")
sites <- c("AM", "GB", "ID", "LF")

depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  rename(ER1 = ER, GPP1 = GPP) %>%
  left_join(depth_daily, by = c("ID", "date")) %>%
  filter(!is.na(depth)) %>%
  select(ID, date, depth, GPP1, ER1)

two_station_all <- map_dfr(methods, function(m) {
  read_csv(file.path(outdir, paste0("two.station.results_", m, ".csv")), show_col_types = FALSE) %>%
    distinct(ID, date, GPP, ER) %>%
    left_join(depth_daily, by = c("ID", "date")) %>%
    filter(!is.na(depth)) %>%
    mutate(method = m) %>%
    select(method, ID, date, depth, GPP2 = GPP, ER2 = ER)
})

# ---- boundary depth per site/method: max depth the two-station calc actually
# used (i.e. passed the reach-length + non-reversal test) -----------------
boundary <- two_station_all %>%
  group_by(method, ID) %>%
  summarise(boundary_depth = max(depth, na.rm = TRUE), .groups = "drop")

# ---- discontinuity score: mean ER/GPP in a window just below the boundary
# (two-station) vs just above it (one-station), using the nearest 15 days on
# each side of the boundary (small-n friendly; avoids picking an arbitrary
# depth window width per site given how different each site's depth range is)
discontinuity <- map_dfr(methods, function(m) {
  map_dfr(sites, function(s) {
    b <- boundary %>% filter(method == m, ID == s) %>% pull(boundary_depth)
    two_near <- two_station_all %>% filter(method == m, ID == s, depth <= b) %>%
      arrange(desc(depth)) %>% slice_head(n = 15)
    one_near <- one_station %>% filter(ID == s, depth >= b) %>%
      arrange(depth) %>% slice_head(n = 15)
    tibble(
      method = m, ID = s, boundary_depth = b,
      n_two_near = nrow(two_near), n_one_near = nrow(one_near),
      ER_two_near = mean(two_near$ER2, na.rm = TRUE),
      ER_one_near = mean(one_near$ER1, na.rm = TRUE),
      GPP_two_near = mean(two_near$GPP2, na.rm = TRUE),
      GPP_one_near = mean(one_near$GPP1, na.rm = TRUE)
    )
  })
}) %>%
  mutate(
    ER_jump = abs(ER_two_near - ER_one_near),
    GPP_jump = abs(GPP_two_near - GPP_one_near)
  ) %>%
  arrange(ID, ER_jump)

write_csv(discontinuity, file.path(outdir, "coalescence_discontinuity_by_method.csv"))
cat("=== ER/GPP jump at the stage boundary, by site and methodology (smaller = smoother) ===\n")
print(discontinuity %>% select(ID, method, boundary_depth, ER_two_near, ER_one_near, ER_jump, GPP_jump),
      n = 100)

cat("\n=== Best (smoothest ER transition) methodology per site ===\n")
discontinuity %>% group_by(ID) %>% slice_min(ER_jump, n = 1) %>%
  select(ID, method, ER_jump, GPP_jump) %>% print()

# ---- comparison plots: GPP/ER vs depth, colored by source, one panel per
# site, M6 vs M7 side by side -------------------------------------------
plot_data <- map_dfr(methods, function(m) {
  two <- two_station_all %>% filter(method == m) %>%
    transmute(ID, depth, GPP = GPP2, ER = ER2, source = "two-station", method = m)
  one <- one_station %>%
    transmute(ID, depth, GPP = GPP1, ER = ER1, source = "one-station", method = m)
  bind_rows(two, one)
}) %>%
  mutate(method_label = factor(method_labels[method], levels = unname(method_labels))) %>%
  pivot_longer(c(GPP, ER), names_to = "flux", values_to = "value")

boundary_lines <- boundary %>%
  mutate(method_label = factor(method_labels[method], levels = unname(method_labels)))

for (s in sites) {
  p_site <- plot_data %>% filter(ID == s) %>%
    ggplot(aes(x = depth, y = value, color = source)) +
    geom_point(size = 1.3, alpha = 0.55) +
    geom_vline(data = boundary_lines %>% filter(ID == s), aes(xintercept = boundary_depth),
               linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = 0, color = "grey70") +
    facet_grid(flux ~ method_label, scales = "free_y") +
    scale_color_manual(values = c("two-station" = "#1b7837", "one-station" = "#762a83")) +
    labs(title = paste0(s, ": one-station vs two-station, M6 vs M7"),
         subtitle = "Dashed line = stage boundary (max depth that methodology's two-station reach test allowed)",
         x = "depth (m)", y = NULL, color = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(outdir, paste0("figures/06_comparison_", s, ".png")), p_site,
         width = 8, height = 6.5, dpi = 150)
}

cat("\nDone. See:\n",
    "-", file.path(outdir, "coalescence_discontinuity_by_method.csv"), "\n",
    "- per-site comparison figures: figures/06_comparison_<ID>.png\n")
