# Does the two-station RC's K600 (M6/M7, a function of depth only) agree in
# MAGNITUDE with what the one-station Bayesian model independently infers
# (estimated from discharge-binned priors on the raw gas-dome floats, not
# from the RC at all)? A persistent offset in overall level -- not just curve
# shape -- would explain a roughly depth-independent ER gap like GB's, since
# K.flux = K600 * depth * DO.deficit directly scales the reaeration
# correction in the two-station mass-balance calc.

library(tidyverse)

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "ID", "LF")

one_station_k600 <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  select(ID, date, K600_one = K600)

depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

compare <- map_dfr(c("M6_breakpoint_stat", "M7_breakpoint_domain"), function(m) {
  rc_k600 <- read_csv(file.path(outdir, paste0("K600_", m, ".csv")), show_col_types = FALSE) %>%
    mutate(date = as.Date(Date)) %>%
    select(ID, date, K600_rc = K600_1.d_daily)

  one_station_k600 %>%
    inner_join(rc_k600, by = c("ID", "date")) %>%
    left_join(depth_daily, by = c("ID", "date")) %>%
    filter(!is.na(K600_one), !is.na(K600_rc), !is.na(depth)) %>%
    mutate(method = m, ratio = K600_one / K600_rc)
})

cat("=== K600 magnitude comparison: one-station (independent) vs RC-predicted, by site ===\n")
compare %>% group_by(method, ID) %>%
  summarise(n = n(),
            median_K600_one = round(median(K600_one), 2),
            median_K600_rc = round(median(K600_rc), 2),
            median_ratio_one_over_rc = round(median(ratio), 2),
            .groups = "drop") %>%
  print(n = 20)

write_csv(compare, file.path(outdir, "K600_magnitude_comparison.csv"))

p <- ggplot(compare, aes(x = depth, y = K600_rc), ) +
  geom_point(aes(y = K600_rc, color = "RC-predicted (two-station input)"), size = 1, alpha = 0.4) +
  geom_point(aes(y = K600_one, color = "one-station (independent estimate)"), size = 1, alpha = 0.4) +
  facet_grid(method ~ ID, scales = "free") +
  scale_color_manual(values = c("RC-predicted (two-station input)" = "#1b7837",
                                 "one-station (independent estimate)" = "#762a83")) +
  labs(title = "K600 magnitude: RC curve (drives two-station) vs one-station's own independent estimate",
       x = "depth (m)", y = "K600 (1/day)", color = NULL) +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/11_K600_magnitude_comparison.png"), p, width = 13, height = 6, dpi = 150)

cat("\nDone -> K600_magnitude_comparison.csv, figures/11_K600_magnitude_comparison.png\n")
