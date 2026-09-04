rm(list=ls())
library(tidyverse)
library(readxl)

## K600 rating curve against discharge (Q): linear and plain power law, same
## two methods as the first two K600-velocity fits in velocity_discharge_K600_RC.R,
## just swapping the predictor. Self-contained -- doesn't touch that script.

sites <- c("AM", "GB", "ID", "LF")
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% select(ID, w)

## ==================== discharge: from the current velocity power RC ====================
## Reuses velocity_RC_power.csv (depth, velocity already resolved per site,
## including the smooth taper to 0 at each site's reversal depth) rather than
## refitting velocity from scratch -- Q = depth * width * velocity.
velocity_RC_power <- read_csv("04_Outputs/velocity_RC_power.csv", show_col_types = FALSE)

discharge_RC <- velocity_RC_power %>%
  left_join(width, by = "ID") %>%
  mutate(discharge = depth * w * velocity * 86400) %>%  # m^3/day
  select(Date, ID, depth, velocity, discharge)

## ==================== clean rC_k600.xlsx ====================
## Same per-site trims as the live K600 pipeline (velocity_discharge_K600_RC.R),
## kept identical so the two RCs are comparable against the same calibration set.
k600_sheet_names <- excel_sheets("04_Outputs/rC_k600.xlsx")
k600_raw_dirty <- map_dfr(k600_sheet_names, function(s) read_excel("04_Outputs/rC_k600.xlsx", sheet = s))

k600_clean <- k600_raw_dirty %>%
  rename(Date = day) %>%  # rC_k600.xlsx's date column is now "day", not "Date"
  select(Date, ID, rep, CO2, CO2_enviro, depth, k600_1.day) %>%
  filter(!is.na(ID)) %>%
  filter(!is.na(k600_1.day))

## same many-to-many join quirk as the live velocity/K600 script (calendar-date
## k600_clean vs. sub-daily discharge_RC) -- accepted there, left as-is here too.
k600s.raw <- k600_clean %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(discharge_RC, by = c('ID', 'Date')) %>%
  mutate(
    k600_1.day = ifelse(ID == 'GB' & k600_1.day > 20, NA, k600_1.day),
    k600_1.day = ifelse(ID == 'ID' & k600_1.day < 3, NA, k600_1.day),
    k600_1.day = ifelse(ID == 'AM' & k600_1.day < 1, NA, k600_1.day),
  ) %>%
  filter(!is.na(k600_1.day), !is.na(discharge))

ggplot() +
  geom_point(data = k600s.raw, aes(x = discharge, y = k600_1.day), alpha = 0.7) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  theme_bw(base_size = 12)

## ==================== method 1: linear ====================
k600_fits_linear <- map(sites, function(s) lm(k600_1.day ~ discharge, data = k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ==================== method 2: M8 -- plain power law, no breakpoint ====================
## Same method (and name) as K600_M8 in velocity_discharge_K600_RC.R: log-log
## OLS over the whole calibration sample, no breakpoint, no high-end ceiling
## or reflection-to-zero -- it just decays asymptotically like every other M8
## fit in this project. Floored only at the low end (below the shallowest
## sampled discharge), same as M8 everywhere else -- nothing velocity-style
## (no forced zero, no taper) applied to K600 here.
fit_M8 <- function(df) {
  df <- df %>% filter(discharge > 0)
  m <- lm(log(k600_1.day) ~ log(discharge), data = df)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  lo <- min(df$discharge)
  list(a = a, b = b, lo = lo, flatval_lo = a * lo^b)
}
k600_fits_M8 <- map(sites, function(s) fit_M8(k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ==================== apply both to the full discharge series ====================
K600_RC <- discharge_RC %>%
  rowwise() %>%
  mutate(
    K600_linear = predict(k600_fits_linear[[ID]], newdata = data.frame(discharge = discharge)),
    K600_M8     = with(k600_fits_M8[[ID]], if_else(discharge < lo, flatval_lo, a * discharge^b))
  ) %>%
  ungroup()

K600_RC_long <- K600_RC %>%
  pivot_longer(c(K600_linear, K600_M8), names_to = "method", values_to = "K600_1.d_daily")

write_csv(K600_RC_long, "04_Outputs/K600_RC_discharge.csv")

## ==================== fit-quality plot, over the calibration data's own range ====================
curve_grid <- map_dfr(sites, function(s) {
  rng <- range(k600s.raw$discharge[k600s.raw$ID == s & k600s.raw$discharge > 0], na.rm = TRUE)
  tibble(ID = s, discharge = exp(seq(log(rng[1]), log(rng[2]), length.out = 100)))
})

pred <- curve_grid %>%
  rowwise() %>%
  mutate(
    K600_linear = predict(k600_fits_linear[[ID]], newdata = data.frame(discharge = discharge)),
    K600_M8     = with(k600_fits_M8[[ID]], a * discharge^b)
  ) %>%
  ungroup() %>%
  pivot_longer(c(K600_linear, K600_M8), names_to = "method", values_to = "K600_1.d_daily")

ggplot() +
  geom_point(data = k600s.raw, aes(x = discharge, y = k600_1.day), alpha = 0.7) +
  geom_line(data = pred, aes(x = discharge, y = K600_1.d_daily, color = method), linewidth = 0.8) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(title = "K600-discharge RC: linear vs. M8 power law, fit over calibration range",
       x = "discharge (m^3/day, log)", y = "K600 (1/day, log)") +
  theme_bw(base_size = 12)
