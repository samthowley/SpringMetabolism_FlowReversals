## =============================================================================
rm(list = ls())
source("03_Scripts/Methodology Comparison/_engine_two_station.R")

EXCL <- "base"        # "base" | "strict" -- which trim_k600 rule to show as "would trim"

##---- helper: the depth range each site actually experiences -----------------
depth_range <- depth %>%
  group_by(ID) %>%
  summarise(dmin = min(depth, na.rm = TRUE),
            dmax = max(depth, na.rm = TRUE), .groups = "drop")

## label each raw point kept/trimmed by trim_k600()'s own logic, so this can
## never drift out of sync with what trim_k600() actually does
label_trim_status <- function(df, excl) {
  df %>%
    mutate(
      trim_status = case_when(
        ID == 'GB' & k600_1.day > 20                    ~ "trimmed (GB > 20)",
        excl == "strict" & ID == 'ID' & k600_1.day < 3   ~ "trimmed (ID < 3, strict)",
        excl == "strict" & ID == 'AM' & k600_1.day < 1   ~ "trimmed (AM < 1, strict)",
        k600_1.day >= 15                                 ~ "trimmed (>=15, global)",
        TRUE                                             ~ "kept"
      )
    )
}

##============================================================================
## 1. K600 ~ DEPTH, untrimmed
##============================================================================
k600_cal_depth_all <- map_dfr(sites, function(s) {
  k600_clean %>%
    filter(ID == s) %>%
    left_join(depth_daily %>% filter(ID == s), by = c("ID", "Date" = "day")) %>%
    rename(x = depth) %>%
    filter(!is.na(x))
}) %>%
  label_trim_status(EXCL)

k600_depth_fits_untrimmed <- map(sites, function(s) {
  cal <- k600_cal_depth_all %>% filter(ID == s)   # no trim_k600() call
  list(ID = s,
       linear = fit_k600(cal, "linear"),
       power  = fit_k600(cal, "power"),
       M6     = fit_k600(cal, "M6"))
}) %>% set_names(sites)

k600_depth_curves_untrimmed <- map_dfr(sites, function(s) {
  r  <- depth_range %>% filter(ID == s)
  xs <- seq(r$dmin, r$dmax, length.out = 400)
  f  <- k600_depth_fits_untrimmed[[s]]
  bind_rows(
    tibble(ID = s, x = xs, form = "linear",        K600 = predict_k600(f$linear, xs)),
    tibble(ID = s, x = xs, form = "power (M8)",    K600 = predict_k600(f$power,  xs)),
    tibble(ID = s, x = xs, form = "M6 breakpoint", K600 = predict_k600(f$M6,    xs))
  )
})

ggplot() +
  geom_line(data = k600_depth_curves_untrimmed %>% mutate(ID = factor(ID, levels = sites)),
            aes(x, K600, color = form), linewidth = 0.9) +
  geom_point(data = k600_cal_depth_all %>% mutate(ID = factor(ID, levels = sites)),
             aes(x, k600_1.day, shape = trim_status), size = 2.4, alpha = 0.85) +
  scale_color_manual(values = c(`power (M8)` = "#1b9e77", linear = "#7570b3",
                                `M6 breakpoint` = "#d95f02")) +
  scale_shape_manual(values = c(kept = 16, `trimmed (GB > 20)` = 4,
                                `trimmed (ID < 3, strict)` = 4, `trimmed (AM < 1, strict)` = 4,
                                `trimmed (>=15, global)` = 4)) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = "K600 rating curves on depth -- UNTRIMMED calibration data",
       subtitle = paste0("curves fit to ALL points; shape marks what trim_k600(EXCL='", EXCL, "') would drop"),
       x = "depth (m)", y = "K600 (1/day)", color = "form", shape = "point status") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

##============================================================================
## 2. K600 ~ VELOCITY, untrimmed
##============================================================================
vel_RC_power <- fit_velocity_RC(uniform_recipe("power", "RC", "power", "depth", EXCL))

vel_daily_power <- vel_RC_power %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(x = mean(velocity, na.rm = TRUE), .groups = "drop")

k600_cal_vel_all <- map_dfr(sites, function(s) {
  k600_clean %>%
    filter(ID == s) %>%
    left_join(vel_daily_power %>% filter(ID == s), by = c("ID", "Date" = "day")) %>%
    filter(!is.na(x))
}) %>%
  label_trim_status(EXCL)

k600_vel_fits_untrimmed <- map(sites, function(s) {
  cal <- k600_cal_vel_all %>% filter(ID == s)   # no trim_k600() call
  list(ID = s,
       linear = fit_k600(cal, "linear"),
       power  = fit_k600(cal, "power"),
       M6     = fit_k600(cal, "M6"))
}) %>% set_names(sites)

vel_pred_range <- vel_RC_power %>%
  group_by(ID) %>%
  summarise(vmin = min(velocity, na.rm = TRUE),
            vmax = max(velocity, na.rm = TRUE), .groups = "drop")

k600_vel_curves_untrimmed <- map_dfr(sites, function(s) {
  r  <- vel_pred_range %>% filter(ID == s)
  xs <- seq(r$vmin, r$vmax, length.out = 400)
  f  <- k600_vel_fits_untrimmed[[s]]
  bind_rows(
    tibble(ID = s, x = xs, form = "linear",        K600 = predict_k600(f$linear, xs)),
    tibble(ID = s, x = xs, form = "power (M8)",    K600 = predict_k600(f$power,  xs)),
    tibble(ID = s, x = xs, form = "M6 breakpoint", K600 = predict_k600(f$M6,    xs))
  )
})

ggplot() +
  geom_line(data = k600_vel_curves_untrimmed %>% mutate(ID = factor(ID, levels = sites)),
            aes(x, K600, color = form), linewidth = 0.9) +
  geom_point(data = k600_cal_vel_all %>% mutate(ID = factor(ID, levels = sites)),
             aes(x, k600_1.day, shape = trim_status), size = 2.4, alpha = 0.85) +
  scale_color_manual(values = c(`power (M8)` = "#1b9e77", linear = "#7570b3",
                                `M6 breakpoint` = "#d95f02")) +
  scale_shape_manual(values = c(kept = 16, `trimmed (GB > 20)` = 4,
                                `trimmed (ID < 3, strict)` = 4, `trimmed (AM < 1, strict)` = 4,
                                `trimmed (>=15, global)` = 4)) +
  facet_wrap(~ID, scales = "free", ncol = 2) +
  labs(title = "K600 rating curves on velocity -- UNTRIMMED calibration data",
       subtitle = paste0("curves fit to ALL points; shape marks what trim_k600(EXCL='", EXCL, "') would drop"),
       x = "velocity (m/s)", y = "K600 (1/day)", color = "form", shape = "point status") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

##============================================================================
## 3. how many points does each site actually lose to trimming?
##============================================================================
cat("\n==== points dropped by trim_k600(EXCL = '", EXCL, "'), K600~depth calibration ====\n", sep = "")
k600_cal_depth_all %>%
  count(ID, trim_status) %>%
  pivot_wider(names_from = trim_status, values_from = n, values_fill = 0) %>%
  print(n = Inf)
