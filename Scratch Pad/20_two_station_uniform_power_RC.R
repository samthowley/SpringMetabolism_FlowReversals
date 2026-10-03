rm(list=ls())
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')      # Cs()
library(streamMetabolizer)       # calc_light()
library(readxl)
library(measurements)

## ============================================================================

sites <- c("AM", "GB", "ID", "LF", "OS")

## ---- CONFIG: change these two lines to switch RC forms ----------------------
VEL_FORM  <- "power"   # "power" | "linear"        -- velocity ~ depth
K600_FORM <- "M8"      # "M8" (plain power) | "linear" | "M6" (power+breakpoint)
## M6 -> M8 2026-10-01: once the gasdome1.R depth bug was fixed, K600 is nearly
## flat against depth (ID R2 .73 -> .20), so the breakpoint has nothing left to
## model and its flat-above segment just holds K600 too high on deep days.
K600_PRED <- "depth"   # "depth" | "velocity"       -- K600 predictor
EXCL      <- "strict"  # "base" | "strict"          -- point-exclusion severity, both forms
DUSK_TRIM_HR <- 4      # night hours right after dusk left out of ER (flux still decaying off the day signal)
## Best uniform method (swept 80 velocity x K600-power combos, same recipe at every site):
## vel power / strict, K600 depth-M6 / log-log fit / clamped, k600_judgment_drop below.

recipe <- tibble(ID = sites) %>%
  mutate(
    units     = "county",     # AM/LF/OS converted ft/s->m/s, GB/ID raw (county-gauge evidence)
    vel_form  = VEL_FORM,
    vel_excl  = EXCL,
    k600_src  = "RC",
    k600_pred = K600_PRED,
    k600_form = K600_FORM,
    k600_excl = EXCL
  )

## ---- inputs -----------------------------------------------------------------
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>%
  select(ID, w)
reach_gps <- read_csv("04_Outputs/Power Function RC/reach_length_from_coords.csv", show_col_types = FALSE) %>%
  select(ID, km = length_km, m = length_m)
area <- left_join(width, reach_gps, by = "ID") %>% mutate(area = w * m)

depth <- read_csv("02_Clean_data/Chem/depth.csv", col_types = cols(ID = col_character())) %>%
  filter(ID %in% sites)
DO <- read_csv("02_Clean_data/Chem/DO.csv", col_types = cols(ID = col_character())) %>%
  select(Date, ID, DO, Temp)

depth_daily <- depth %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

u_raw <- read_csv("01_Raw_data/u.csv", show_col_types = FALSE) %>%
  mutate(Date = mdy(Date)) %>%
  select(Date, ID, velocity = u) %>%
  filter(ID %in% sites, !is.na(velocity), velocity > 0)

unit_lists <- list(
  all_conv = c('AM', 'GB', 'ID', 'LF', 'OS'),
  county   = c('AM', 'LF', 'OS')          # GB/ID stay raw
)

## LF flow-reversal anchors: velocity goes to 0 at high depth (backflooding).
## Dropped automatically under a power fit (can't log velocity = 0).
lf_flow_reversals <- tribble(
  ~ID,  ~depth, ~velocity,
  "LF",  1.75,   0,
  "LF",  1.90,   0,
  "LF",  2.10,   0
) %>%
  left_join(width, by = "ID") %>%
  mutate(Date = as.Date(NA)) %>%
  select(Date, ID, w, depth, velocity)

VentDO <- read_csv("02_Clean_data/Chem/VentDO.csv", show_col_types = FALSE) %>%
  filter(VentDO >= 0) %>%
  mutate(
    VentDO   = ifelse(ID == 'GB' & VentDO < 2,   NA, VentDO),
    VentDO   = ifelse(ID == 'AM' & VentDO < 0.9, NA, VentDO),
    ## VentTemp is MIXED units: own gas-dome rows F (~72), county/NWIS rows C (~22).
    ## Cs() assumes C. >40 can only be F for a FL spring vent.
    VentTemp = if_else(VentTemp > 40, fahrenheit.to.celsius(VentTemp), VentTemp)
  )

## GB 2023-10-19 VentDO = 3.03 is an outlier: below the IQR fence of GB's 2021-2024
## samples (4.09-5.06) AND of the full 1990-2026 record (3.33). fill() carried it
## flat for months -> positive-ER stretch. Replaced with the GB 2021-2024 mean.
gb_vent_mean <- VentDO %>%
  filter(ID == 'GB', year(Date) %in% 2021:2024, as_date(Date) != as.Date('2023-10-19')) %>%
  summarise(m = mean(VentDO, na.rm = TRUE)) %>%
  pull(m)

VentDO <- VentDO %>%
  mutate(VentDO = if_else(ID == 'GB' & as_date(Date) == as.Date('2023-10-19'), gb_vent_mean, VentDO))

lat.lon <- tibble(
  ID  = c('AM', 'LF', 'GB', 'ID', 'OS'),
  lat = c(30.155, 29.585, 29.83, 29.93, 29.6448),
  lon = c(-83.238, -82.93, -82.68, -82.8, -82.9428))

## ==================== 1. velocity rating curve, per site =====================
make_vel_cal <- function(site, units, excl) {
  base <- u_raw %>%
    filter(ID == site) %>%
    mutate(velocity = if_else(ID %in% unit_lists[[units]],
                              conv_unit(velocity, 'ft_per_sec', 'm_per_sec'), velocity)) %>%
    left_join(width, by = "ID") %>%
    left_join(depth_daily, by = c("ID", "Date" = "day")) %>%
    filter(!is.na(depth), depth > 0) %>%
    select(Date, ID, w, depth, velocity) %>%
    bind_rows(lf_flow_reversals %>% filter(ID == site)) %>%
    mutate(
      velocity = if_else(ID == 'LF' & velocity > 0.06, NA, velocity),   # base trims
      velocity = if_else(ID == 'OS' & velocity > 0.1,  NA, velocity)
    )
  if (excl == "strict") {
    base <- base %>%
      mutate(
        velocity = if_else(ID == 'AM' & velocity > 0.15, NA, velocity),
        velocity = if_else(ID == 'GB' & velocity > 0.3,  NA, velocity)
      )
  }
  base %>% filter(!is.na(velocity))
}

velocity_RC <- map_dfr(seq_len(nrow(recipe)), function(i) {
  r   <- recipe[i, ]
  cal <- make_vel_cal(r$ID, r$units, r$vel_excl)
  d   <- depth %>% filter(ID == r$ID)
  if (r$vel_form == "linear") {
    m <- lm(velocity ~ depth, data = cal)
    d <- d %>% mutate(velocity = pmax(coef(m)[[1]] + coef(m)[[2]] * depth, 0))   # floored at 0
  } else {
    m <- lm(log(velocity) ~ log(depth), data = cal %>% filter(velocity > 0))
    d <- d %>% mutate(velocity = exp(coef(m)[[1]]) * depth^unname(coef(m)[[2]]))
  }
  d %>% select(Date, ID, depth, velocity)
})

## ==================== 2. K600, per site ======================================
k600_clean <- map_dfr(excel_sheets("04_Outputs/rC_k600.xlsx"),
                      function(s) read_excel("04_Outputs/rC_k600.xlsx", sheet = s)) %>%
  rename(Date = day) %>%
  select(Date, ID, rep, k600_1.day) %>%
  filter(!is.na(ID), !is.na(k600_1.day), ID %in% sites) %>%
  mutate(Date = as.Date(Date))

trim_k600 <- function(df, excl) {
  df <- df %>% mutate(k600_1.day = ifelse(ID == 'GB' & k600_1.day > 20, NA, k600_1.day))
  if (excl == "strict") {
    df <- df %>% mutate(
      k600_1.day = ifelse(ID == 'ID' & k600_1.day < 3, NA, k600_1.day),
      k600_1.day = ifelse(ID == 'AM' & k600_1.day < 1, NA, k600_1.day)
    )
  }
  df %>% filter(k600_1.day < 15, !is.na(k600_1.day))
}

fit_k600 <- function(df, form) {
  df <- df %>% filter(!is.na(x), !is.na(k600_1.day))
  if (form == "linear") {
    m <- lm(k600_1.day ~ x, data = df)
    return(list(form = "linear", int = coef(m)[[1]], slope = coef(m)[[2]],
                xmin = min(df$x), xmax = max(df$x)))
  }
  dfp <- df %>% filter(x > 0, k600_1.day > 0)   # both get logged
  m8  <- lm(log(k600_1.day) ~ log(x), data = dfp)
  a8  <- exp(coef(m8)[[1]]); b8 <- unname(coef(m8)[[2]])
  if (form == "M8") {
    return(list(form = "M8", a = a8, b = b8, xmin = min(dfp$x), xmax = max(dfp$x)))
  }
  ## M6: grid-search breakpoint -- power below, flat above, flat below lowest sample
  dfp  <- dfp %>% arrange(x)
  cand <- unique(dfp$x)
  cand <- if (nrow(dfp) >= 5) cand[cand > sort(dfp$x)[4] & cand < max(dfp$x)] else numeric(0)
  bps  <- map_dfr(cand, function(bp) {
    left <- dfp %>% filter(x <= bp); right <- dfp %>% filter(x > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600_1.day) ~ log(x), data = left)
    flatv <- exp(predict(m, newdata = data.frame(x = bp)))
    tibble(bp = bp, sse = sum((left$k600_1.day - exp(predict(m)))^2) +
                           sum((right$k600_1.day - flatv)^2))
  })
  if (nrow(bps) == 0) return(list(form = "M8", a = a8, b = b8, xmin = min(dfp$x), xmax = max(dfp$x)))
  bp   <- bps$bp[which.min(bps$sse)]
  m    <- lm(log(k600_1.day) ~ log(x), data = dfp %>% filter(x <= bp))
  a    <- exp(coef(m)[[1]]); b <- unname(coef(m)[[2]]); lo <- min(dfp$x)
  list(form = "M6", a = a, b = b, bp = bp,
       flatval = a * bp^b, flatval_lo = a * lo^b, xmin = lo, xmax = max(dfp$x))
}

## never extrapolate past the calibration range; floor at 0.05
predict_k600 <- function(fit, x) {
  out <- switch(fit$form,
    linear = fit$int + fit$slope * pmin(pmax(x, fit$xmin), fit$xmax),
    M8     = fit$a * pmin(pmax(x, fit$xmin), fit$xmax)^fit$b,
    M6     = case_when(x < fit$xmin ~ fit$flatval_lo,
                       x <= fit$bp  ~ fit$a * x^fit$b,
                       TRUE         ~ fit$flatval))
  pmax(out, 0.05)
}

## whole-day drops of physically implausible gas-dome K600, same rule at every site
k600_judgment_drop <- tribble(
  ~ID,  ~Date,                 # reason
  "GB", as.Date("2022-10-24"), # K600 negative (-0.001)
  "GB", as.Date("2022-11-07"), # 25.3 -- >20, ~2.5x every other GB day
  "GB", as.Date("2022-11-21"), # 22.2-22.3 -- >20, ~2.5x every other GB day
  "OS", as.Date("2022-10-31"), # all 3 reps 8.4-8.8, 4-15x the rest of OS
  "LF", as.Date("2023-02-06")  # both reps 0.81, ~3x below LF days at similar depth (2.1-2.5);
                               # alone it pins the M6 plateau at 1.17 for all depth > 0.4 m,
                               # below the 1.7-4.4 the LF night DO budget needs for ER -2 to -7
)

k600_fits <- map(seq_len(nrow(recipe)), function(i) {
  r <- recipe[i, ]
  x_daily <- if (r$k600_pred == "velocity") {
    velocity_RC %>% filter(ID == r$ID) %>% mutate(day = as.Date(Date)) %>%
      group_by(ID, day) %>% summarise(x = mean(velocity, na.rm = TRUE), .groups = "drop")
  } else {
    depth_daily %>% filter(ID == r$ID) %>% rename(x = depth)
  }
  cal <- k600_clean %>%                        ## <-- ADD YOUR OWN K600 TRIMMING HERE
    filter(ID == r$ID) %>%                     ##     (trim_k600() above is defined
    anti_join(k600_judgment_drop, by = c("ID", "Date")) %>%
    left_join(x_daily, by = c("ID", "Date" = "day")) %>%  ##     but no longer called)
    filter(!is.na(x))
  fit <- fit_k600(cal, r$k600_form)

  pred_x <- if (r$k600_pred == "velocity") {
    velocity_RC %>% filter(ID == r$ID) %>% select(Date, ID, x = velocity)
  } else {
    depth %>% filter(ID == r$ID) %>% select(Date, ID, x = depth)
  }
  list(cal  = cal %>% select(Date, ID, x, k600_1.day),
       pred = pred_x %>% mutate(K600 = predict_k600(fit, x)) %>% select(Date, ID, x, K600))
})

K600_RC <- map_dfr(k600_fits, ~ .x$pred %>% select(Date, ID, K600))

K600 <- K600_RC

## ==================== 2b. rating-curve scatter plots (calibration points + curve used) ====
site_levels <- c("AM", "GB", "LF", "ID", "OS")

## velocity ~ depth: points = calibration set actually fit (LF velocity = 0 anchors are
## plotted but not fit under a power form); line = curve applied to the whole depth record
vel_cal_pts <- map_dfr(seq_len(nrow(recipe)), function(i) {
  r <- recipe[i, ]
  make_vel_cal(r$ID, r$units, r$vel_excl)
})

vel_cal_pts %>%
  mutate(ID = factor(ID, levels = site_levels)) %>%
  ggplot(aes(x = depth, y = velocity)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_line(data = velocity_RC %>% filter(!is.na(velocity)) %>%
              mutate(ID = factor(ID, levels = site_levels)) %>% arrange(ID, depth),
            color = "#d95f02", linewidth = 0.8) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = paste0("Velocity rating curves used (", VEL_FORM, ", ", EXCL, " exclusions)"),
       x = "depth (m)", y = "velocity (m/s)") +
  theme_bw(base_size = 10)

## K600 ~ predictor: points = gas-dome K600 kept for the fit; line = curve used
k600_cal_pts <- map_dfr(k600_fits, "cal")
k600_curve   <- map_dfr(k600_fits, "pred")

k600_cal_pts %>%
  mutate(ID = factor(ID, levels = site_levels)) %>%
  ggplot(aes(x = x, y = k600_1.day)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_line(data = k600_curve %>% filter(!is.na(x)) %>%
              mutate(ID = factor(ID, levels = site_levels)) %>% arrange(ID, x),
            aes(y = K600), color = "#1b9e77", linewidth = 0.8) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = paste0("K600 rating curves used (", K600_PRED, "-", K600_FORM, ", ", EXCL, " exclusions)"),
       x = paste0(K600_PRED, if (K600_PRED == "depth") " (m)" else " (m/s)"),
       y = "K600 (1/day)") +
  theme_bw(base_size = 10)

## ==================== 3. two-station DO mass balance =========================
master <- reduce(list(depth, DO, K600, velocity_RC %>% select(-depth)),
                 full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID") %>%
  mutate(discharge = w * depth * velocity * 86400) %>%
  full_join(VentDO, by = c("ID", "Date"), relationship = "many-to-many") %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  fill(VentDO, VentTemp, .direction = "downup") %>%
  ungroup() %>%
  distinct(ID, Date, .keep_all = TRUE) %>%
  filter(ID %in% sites, !is.na(Date), !is.na(K600)) %>%
  left_join(lat.lon, by = "ID") %>%
  mutate(
    ## 3a. change in total DO flux across the reach
    change.DO.flux = ((DO - VentDO) * discharge) / area,
    ## 3b. DO deficit from saturation, mean of the two stations
    Vent.DO.sat         = Cs(VentTemp),
    stat2.DO.sat        = Cs(fahrenheit.to.celsius(Temp)),
    DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2,
    ## 3c. reaeration flux
    K.flux = K600 * depth * DO.deficit.from.sat,
    ## 3d. everything that isn't air-water exchange = metabolism
    not.air.water.xchange = change.DO.flux - K.flux,
    ## 3e. solar time, travel-time corrected, for day/night classification
    solar.time.raw       = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    travel.time.hr       = if_else(velocity > 0, (m / velocity) / 3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr / 2) * 3600,
    ## day/night on the logger clock, NOT the travel-time-shifted clock: half of AM's
    ## travel time is median ~10 h (max 69 h), which put "day" at midnight at AM.
    ## The lag is handled by DUSK_TRIM_HR below instead.
    light.corrected      = calc_light(solar.time.raw, lat, lon),
    ## 3f. carbon-turnover / footprint diagnostic (not filtered on, kept for review)
    reach.km   = ((velocity * 86400) / K600) / 10^3,
    reach.test = case_when(velocity < 0        ~ 'below',
                           reach.km > 3   * km ~ 'too fast',
                           reach.km < 0.4 * km ~ 'too slow',
                           TRUE                ~ 'passes'),
    date = as_date(Date)          # observation day -- NOT the shifted clock
  ) %>%
  group_by(ID, date) %>% filter(n() >= 20) %>% ungroup() %>%              # near-complete days
  mutate(time = if_else(light.corrected > 0, 'day', 'night')) %>%
  filter(!is.na(time)) %>%
  group_by(ID, date) %>% filter(sum(light.corrected > 0, na.rm = TRUE) >= 5) %>% ungroup() %>%
  ## first DUSK_TRIM_HR night hours -> 'dusk': excluded from ER, still in the GPP average
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(last_day = if_else(time == 'day', Date, as.POSIXct(NA))) %>%
  fill(last_day, .direction = "down") %>%
  ungroup() %>%
  mutate(hrs.since.dusk = as.numeric(difftime(Date, last_day, units = "hours")),
         time = if_else(time == 'night' & !is.na(hrs.since.dusk) & hrs.since.dusk <= DUSK_TRIM_HR,
                        'dusk', time))

## ==================== 4. isolate GPP / ER ====================================
ER_std <- master %>%
  group_by(ID, date, time) %>%
  summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = time, values_from = avg, values_fn = mean) %>%
  rename(ER = night) %>%
  filter(!is.na(ER))%>%
  select(ID, date, ER)

## ---- narrower night window for weak/positive-ER days, site-specific ----------
## Only on days where the standard ER > -2: recompute ER from night hours more than
## dusk_hr after lights-off and more than dawn_hr before sunrise (>= 3 h kept).
## One fixed window per site (best of a 0-5 h grid), not chosen day by day.
## Most positive-ER nights are positive in EVERY hour (DO - VentDO offset), so this
## only rescues a minority of them. ID/OS not listed -> standard window.
flag_night_window <- tribble(
  ~ID,  ~dusk_hr, ~dawn_hr,
  "AM",  2,        4,
  "GB",  1,        5,
  "LF",  3,        5
)

ER_alt <- master %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(next_day = if_else(time == 'day', Date, as.POSIXct(NA))) %>%
  fill(next_day, .direction = "up") %>%
  ungroup() %>%
  inner_join(flag_night_window, by = "ID") %>%
  mutate(hrs.to.dawn = as.numeric(difftime(next_day, Date, units = "hours"))) %>%
  filter(time %in% c('night', 'dusk'),
         !is.na(hrs.since.dusk), hrs.since.dusk > dusk_hr,
         is.na(hrs.to.dawn) | hrs.to.dawn > dawn_hr) %>%
  group_by(ID, date) %>%
  summarise(ER_alt = mean(not.air.water.xchange, na.rm = TRUE),
            n_alt  = sum(!is.na(not.air.water.xchange)), .groups = "drop")

ER <- ER_std %>%
  left_join(ER_alt, by = c("ID", "date")) %>%
  mutate(ER.window = if_else(ER > -2 & !is.na(ER_alt) & n_alt >= 3, 'narrow', 'standard'),
         ER        = if_else(ER.window == 'narrow', ER_alt, ER)) %>%
  select(ID, date, ER, ER.window)

GPP<-master%>%
  mutate(date=as.Date(Date))%>%left_join(ER)%>%
  mutate(GPP.hourly=not.air.water.xchange-ER)%>%
  group_by(ID, date)%>%
  summarise(GPP=mean(GPP.hourly, na.rm=TRUE), .groups = "drop")

gpp_er <-left_join(ER, GPP)
  

## ---- daily summary of EVERY other driver, not just GPP/ER -------------------
drivers <- c("depth", "velocity", "discharge", "K600", "DO", "Temp",
             "VentDO", "VentTemp", "Vent.DO.sat", "stat2.DO.sat",
             "DO.deficit.from.sat", "change.DO.flux", "K.flux",
             "not.air.water.xchange", "travel.time.hr", "reach.km",
             "light.corrected", "w", "km", "m", "area")

daily_drivers <- master %>%
  group_by(ID, date) %>%
  summarise(
    n_obs       = n(),
    n_day       = sum(time == "day",   na.rm = TRUE),
    n_night     = sum(time == "night", na.rm = TRUE),
    across(all_of(drivers), ~mean(.x, na.rm = TRUE)),
    pct_reach_pass = mean(reach.test == "passes", na.rm = TRUE),
    reach_test_mode = names(sort(table(reach.test), decreasing = TRUE))[1],
    .groups = "drop")

## day/night split of the two fluxes that drive GPP vs ER, kept separately
daily_daynight <- master %>%
  filter(!is.na(time)) %>%
  group_by(ID, date, time) %>%
  summarise(change.DO.flux = mean(change.DO.flux, na.rm = TRUE),
            K.flux         = mean(K.flux, na.rm = TRUE),
            depth          = mean(depth, na.rm = TRUE),
            K600           = mean(K600, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = time,
              values_from = c(change.DO.flux, K.flux, depth, K600),
              names_glue = "{.value}_{time}")

daily <- gpp_er %>%
  left_join(daily_drivers,  by = c("ID", "date")) %>%
  left_join(daily_daynight, by = c("ID", "date")) %>%
  mutate(NEP = GPP + ER, .after = ER) %>%
  arrange(ID, date)

results <- left_join(master, gpp_er %>% select(ID, date, GPP, ER), by = c("ID", "date"))

## ==================== 5. plots ================================================
methodology_label <- paste0("vel ", VEL_FORM, ", K600 ", K600_PRED, "-", K600_FORM)

daily %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")),
         panel = paste0(ID, "\n", methodology_label)) %>%
  ggplot(aes(x = date)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin =   2, ymax =  25, fill = "#1b9e77", alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -25, ymax =  -2, fill = "#d95f02", alpha = 0.08) +
  geom_point(aes(y = GPP, color = "GPP"),
             size = 1) +
  geom_point(aes(y = ER,  color = "ER"),  size = 1) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(GPP = "#1b9e77", ER = "#d95f02")) +
  facet_wrap(~panel, scales = "free", ncol = 3) +
  labs(title = "Two-station metabolism -- uniform RC across all sites",
       subtitle = "shaded = plausible range (GPP 2-25, ER -2 to -25)",
       x = NULL, y = "g O2 m-2 d-1") +
  theme_bw(base_size = 10)


library(cowplot)
plot_grid(
  
daily %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP, color=reach_test_mode), size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol=3) +
  ggtitle("GPP")+
  theme_bw(base_size = 10)+
  theme(legend.position = "none")
,

daily %>%filter(velocity>0)%>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = ER, color=reach_test_mode), size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol=3) +
  ggtitle("ER")+
  theme_bw(base_size = 10)+
  theme(legend.position = "none"),
ncol=1)

#COMBINE ONE STATION AND TWO STATION###################

file.names <- list.files(path="04_Outputs/one station results", pattern=".csv", full.names=TRUE)
onestation.df <- data.frame()
for(fil in file.names){
  df <- read_csv(fil)
  onestation.df <- rbind(onestation.df, df)}


onestation<-onestation.df%>%
  rename(GPP1=GPP_daily_mean,
         ER1=ER_daily_mean,
         K6001=K600_daily_mean,
         Date=date)%>%
  separate(ID,into = c('ID', 'stage'),sep='_')%>%
  mutate(GPP1=if_else(GPP1<0, 0, GPP1),
         model="1")%>%
  select(-ER_Rhat, -K600_daily_Rhat, -stage)%>%
  arrange(ID, Date)

two<-daily%>% select(date, ID, depth, GPP, ER, K600_day)%>%
  rename(GPP2=GPP, ER2=ER, K6002=K600_day, Date=date)%>%
  mutate(model="2",
         GPP2=if_else(GPP2<3, NA, GPP2),
         ER2=if_else(ER2> -3, NA, ER2),
         )%>%
  arrange(ID, Date)


two%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP2, shape='GPP'),) +
  geom_point(aes(y = ER2, shape='ER'), shape=1) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()


all.met <- onestation %>%
  full_join(two, by = c("Date", "ID"), suffix = c("_onestation", "_two")) %>%
  mutate(
    # Prioritize "two" over "onestation"
    GPP = coalesce(GPP2, GPP1),
    ER = coalesce(ER2, ER1),

    # Track which dataset was used
    source = case_when(
      !is.na(GPP2) ~ "two",
      !is.na(GPP1) ~ "one",
      TRUE ~ "neither"
    )
  ) %>%
  arrange(ID, Date)

all.met%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP1, color='1'),alpha=0.5) +
  geom_point(aes(y = ER1, color='1'), alpha=0.5) +

  geom_point(aes(y = ER2, color='2'), alpha=0.5) +
  geom_point(aes(y = GPP2, color='2'),alpha=0.5) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()
