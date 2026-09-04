## =============================================================================
## 01 -- RATING CURVE REFERENCE FIGURES
## =============================================================================
rm(list = ls())
source("03_Scripts/Methodology Comparison/_engine_two_station.R")

EXCL <- "base"        # "base" | "strict" -- which point exclusions to display

##---- helper: the depth range each site actually experiences -----------------
depth_range <- depth %>%
  group_by(ID) %>%
  summarise(dmin = min(depth, na.rm = TRUE),
            dmax = max(depth, na.rm = TRUE), .groups = "drop")

##============================================================================
## 1. VELOCITY ~ DEPTH
##============================================================================
vel_cal <- map_dfr(sites, function(s) {
  make_vel_cal(s, units = "county", excl = EXCL) %>%
    mutate(point_type = if_else(is.na(Date), "flow-reversal anchor", "hand measured"))
})

vel_fits <- map_dfr(sites, function(s) {
  cal <- vel_cal %>% filter(ID == s)
  lin <- lm(velocity ~ depth, data = cal)
  pw  <- lm(log(velocity) ~ log(depth), data = cal %>% filter(velocity > 0))
  tibble(
    ID    = s,
    n_lin = nrow(cal),
    n_pw  = sum(cal$velocity > 0),
    lin_int = coef(lin)[[1]], lin_slope = coef(lin)[[2]], lin_r2 = summary(lin)$r.squared,
    pw_a    = exp(coef(pw)[[1]]), pw_b = unname(coef(pw)[[2]]), pw_r2 = summary(pw)$r.squared)
})

vel_curves <- map_dfr(sites, function(s) {
  r  <- depth_range %>% filter(ID == s)
  f  <- vel_fits %>% filter(ID == s)
  xs <- seq(r$dmin, r$dmax, length.out = 400)
  bind_rows(
    tibble(ID = s, depth = xs, form = "linear",
           velocity = pmax(f$lin_int + f$lin_slope * xs, 0)),   # floored at 0
    tibble(ID = s, depth = xs, form = "power",
           velocity = f$pw_a * xs^f$pw_b)
  )
})

ggplot() +
  geom_line(data = vel_curves %>% mutate(ID = factor(ID, levels = sites)),
            aes(depth, velocity, color = form), linewidth = 0.9) +
  geom_point(data = vel_cal %>% mutate(ID = factor(ID, levels = sites)),
             aes(depth, velocity, shape = point_type), size = 2.4, alpha = 0.85) +
  scale_shape_manual(values = c("hand measured" = 16, "flow-reversal anchor" = 4)) +
  scale_color_manual(values = c(power = "#1b9e77", linear = "#7570b3")) +
  facet_wrap(~ID, scales = "free", ncol = 2) +
  labs(title = "Velocity rating curves: power vs linear",
       x = "depth (m)", y = "velocity (m/s)", color = "form", shape = NULL) +
  theme_bw(base_size = 11)

##============================================================================
## 2. K600 ~ DEPTH   (the predictor used by M1-M6 and by AM/LF/OS in the final)
##============================================================================
k600_cal_depth <- map_dfr(sites, function(s) {
  trim_k600(k600_clean, EXCL) %>%
    filter(ID == s) %>%
    left_join(depth_daily %>% filter(ID == s), by = c("ID", "Date" = "day")) %>%
    rename(x = depth) %>%
    filter(!is.na(x))
})

k600_depth_fits <- map(sites, function(s) {
  cal <- k600_cal_depth %>% filter(ID == s)
  list(ID = s,
       linear = fit_k600(cal, "linear"),
       power  = fit_k600(cal, "power"),
       M6     = fit_k600(cal, "M6"))
}) %>% set_names(sites)

k600_depth_curves <- map_dfr(sites, function(s) {
  r  <- depth_range %>% filter(ID == s)
  xs <- seq(r$dmin, r$dmax, length.out = 400)
  f  <- k600_depth_fits[[s]]
  bind_rows(
    tibble(ID = s, x = xs, form = "linear",        K600 = predict_k600(f$linear, xs)),
    tibble(ID = s, x = xs, form = "power (M8)",    K600 = predict_k600(f$power,  xs)),
    tibble(ID = s, x = xs, form = "M6 breakpoint", K600 = predict_k600(f$M6,    xs))
  )
})

## calibration range -- outside it the curve is clamped flat (no extrapolation)
k600_depth_range <- map_dfr(sites, function(s) {
  f <- k600_depth_fits[[s]]
  tibble(ID = s, xmin = f$power$xmin, xmax = f$power$xmax)
})

ggplot() +
  geom_vline(data = k600_depth_range %>% mutate(ID = factor(ID, levels = sites)),
             aes(xintercept = xmin), linetype = "dashed", color = "grey55") +
  geom_vline(data = k600_depth_range %>% mutate(ID = factor(ID, levels = sites)),
             aes(xintercept = xmax), linetype = "dashed", color = "grey55") +
  geom_line(data = k600_depth_curves %>% mutate(ID = factor(ID, levels = sites)),
            aes(x, K600, color = form), linewidth = 0.9) +
  geom_point(data = k600_cal_depth %>% mutate(ID = factor(ID, levels = sites)),
             aes(x, k600_1.day), size = 2.4, alpha = 0.85) +
  scale_color_manual(values = c(`power (M8)` = "#1b9e77", linear = "#7570b3",
                                `M6 breakpoint` = "#d95f02")) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = "K600 rating curves on depth: linear vs power (M8) vs M6 breakpoint",
       x = "depth (m)", y = "K600 (1/day)", color = "form") +
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom")

##============================================================================
## 3. K600 ~ VELOCITY  (GB's final recipe uses this predictor with the M6 form)
##============================================================================

vel_RC_power <- fit_velocity_RC(uniform_recipe("power", "RC", "power", "depth", EXCL))

vel_daily_power <- vel_RC_power %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(x = mean(velocity, na.rm = TRUE), .groups = "drop")

k600_cal_vel <- map_dfr(sites, function(s) {
  trim_k600(k600_clean, EXCL) %>%
    filter(ID == s) %>%
    left_join(vel_daily_power %>% filter(ID == s), by = c("ID", "Date" = "day")) %>%
    filter(!is.na(x))
})

k600_vel_fits <- map(sites, function(s) {
  cal <- k600_cal_vel %>% filter(ID == s)
  list(ID = s,
       linear = fit_k600(cal, "linear"),
       power  = fit_k600(cal, "power"),
       M6     = fit_k600(cal, "M6"))
}) %>% set_names(sites)

vel_pred_range <- vel_RC_power %>%
  group_by(ID) %>%
  summarise(vmin = min(velocity, na.rm = TRUE),
            vmax = max(velocity, na.rm = TRUE), .groups = "drop")

k600_vel_curves <- map_dfr(sites, function(s) {
  r  <- vel_pred_range %>% filter(ID == s)
  xs <- seq(r$vmin, r$vmax, length.out = 400)
  f  <- k600_vel_fits[[s]]
  bind_rows(
    tibble(ID = s, x = xs, form = "linear",        K600 = predict_k600(f$linear, xs)),
    tibble(ID = s, x = xs, form = "power (M8)",    K600 = predict_k600(f$power,  xs)),
    tibble(ID = s, x = xs, form = "M6 breakpoint", K600 = predict_k600(f$M6,    xs))
  )
})

ggplot() +
  geom_line(data = k600_vel_curves %>% mutate(ID = factor(ID, levels = sites)),
            aes(x, K600, color = form), linewidth = 0.9) +
  geom_point(data = k600_cal_vel %>% mutate(ID = factor(ID, levels = sites)),
             aes(x, k600_1.day), size = 2.4, alpha = 0.85) +
  scale_color_manual(values = c(`power (M8)` = "#1b9e77", linear = "#7570b3",
                                `M6 breakpoint` = "#d95f02")) +
  facet_wrap(~ID, scales = "free", ncol = 2) +
  labs(title = "K600 rating curves on velocity: linear vs power (M8) vs M6 breakpoint",
       x = "velocity (m/s)", y = "K600 (1/day)", color = "form") +
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom")


##============================================================================
## 5. FREE-WATER K600 vs DEPTH
##============================================================================
K600_freewater_all %>%
  filter(ID %in% sites) %>%
  left_join(depth_daily, by = c("ID", "day")) %>%
  filter(!is.na(depth)) %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  ggplot(aes(depth, K600)) +
  geom_point(size = 1, alpha = 0.4, color = "#386cb0") +
  geom_smooth(method = "loess", se = FALSE, color = "#d95f02", linewidth = 0.9) +
  facet_wrap(~ID, scales = "free", ncol = 2) +
  labs(title = "Free-water (streamMetabolizer) K600 vs depth",
       x = "depth (m)", y = "K600 (1/day)") +
  theme_bw(base_size = 11)

