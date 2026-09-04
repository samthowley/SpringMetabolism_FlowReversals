## =============================================================================
## SHARED ENGINE for the methodology comparison
##
## Sourced by each M1..M6 runner script and by 00_compare_methodologies.R.
## Nothing runs on source() -- it only defines functions + loads the inputs once.
##
## Faithful to Scratch Pad/19_two_station_FINAL.R, including its two bug fixes:
##   1. date = as_date(Date)  (observation day, NOT the travel-time-shifted clock)
##   2. NEVER fill(K600)      (that backfills unconverged free-water days)
##
## The ONLY thing that changes between methodologies is the recipe passed in.
## =============================================================================

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')      # Cs()
library(streamMetabolizer)       # calc_light()
library(readxl)
library(measurements)

sites <- c("AM", "GB", "ID", "LF", "OS")

## plausible-range definition used by every success metric ---------------------
GPP_RANGE <- c(  3,  25)         # g O2 m-2 d-1
ER_RANGE  <- c(-25,  -3)         # note: script 19 used 2 / -2, this uses 3 / -3

## velocity unit convention: county-gauge evidence says AM/LF/OS were logged in
## ft/s and must be converted; GB and ID were already m/s and stay RAW.
unit_lists <- list(
  all_conv = c('AM', 'GB', 'ID', 'LF', 'OS'),
  county   = c('AM', 'LF', 'OS')
)

## ---- inputs (loaded once per session) ---------------------------------------
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

## LF flow-reversal anchors: velocity goes to 0 at high depth (backflooding)
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

lat.lon <- tibble(
  ID  = c('AM', 'LF', 'GB', 'ID', 'OS'),
  lat = c(30.155, 29.585, 29.83, 29.93, 29.6448),
  lon = c(-83.238, -82.93, -82.68, -82.8, -82.9428))

k600_clean <- map_dfr(excel_sheets("04_Outputs/rC_k600.xlsx"),
                      function(s) read_excel("04_Outputs/rC_k600.xlsx", sheet = s)) %>%
  rename(Date = day) %>%
  select(Date, ID, rep, k600_1.day) %>%
  filter(!is.na(ID), !is.na(k600_1.day), ID %in% sites) %>%
  mutate(Date = as.Date(Date))

## free-water (Bayesian one-station) K600, converged days only.
## AM/GB/ID/LF live in met_results_two.csv as _hi / _lo prior variants; OS is separate.
K600_freewater_all <- bind_rows(
  read_csv("04_Outputs/one station results/met_results_two.csv", show_col_types = FALSE) %>%
    separate(ID, into = c("ID", "prior"), sep = "_") %>%
    filter(prior == "hi"),
  read_csv("04_Outputs/one station results/OS.csv", show_col_types = FALSE) %>%
    mutate(prior = "hi")
) %>%
  filter(ID %in% sites, !is.na(K600_daily_mean), K600_daily_Rhat < 1.05) %>%
  transmute(ID, day = as_date(date), K600 = K600_daily_mean)

## ==================== recipe helper ==========================================
## Builds a per-site recipe where every site gets the SAME methodology.
uniform_recipe <- function(vel_form, k600_src, k600_form = NA,
                           k600_pred = "depth", excl = "base", units = "county") {
  ## resolved OUTSIDE tibble() -- inside, the bare names would resolve to the
  ## length-5 columns being built rather than these scalar arguments.
  pred <- if (k600_src == "RC") k600_pred else NA_character_
  form <- if (k600_src == "RC") k600_form else NA_character_
  tibble(ID        = sites,
         units     = units,
         vel_form  = vel_form,
         vel_excl  = excl,
         k600_src  = k600_src,
         k600_pred = pred,
         k600_form = form,
         k600_excl = excl)
}

## ==================== 1. velocity rating curve ===============================
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

fit_velocity_RC <- function(recipe) {
  map_dfr(seq_len(nrow(recipe)), function(i) {
    r   <- recipe[i, ]
    cal <- make_vel_cal(r$ID, r$units, r$vel_excl)
    d   <- depth %>% filter(ID == r$ID)
    if (r$vel_form == "linear") {
      m <- lm(velocity ~ depth, data = cal)
      d <- d %>% mutate(velocity = pmax(coef(m)[[1]] + coef(m)[[2]] * depth, 0))  # floored at 0
    } else {
      ## power: log-log. LF's velocity = 0 flow-reversal anchors cannot be logged.
      m <- lm(log(velocity) ~ log(depth), data = cal %>% filter(velocity > 0))
      d <- d %>% mutate(velocity = exp(coef(m)[[1]]) * depth^unname(coef(m)[[2]]))
    }
    d %>% select(Date, ID, depth, velocity)
  })
}

## ==================== 2. K600 ================================================
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
                xmin = min(df$x), xmax = max(df$x), n = nrow(df),
                r2 = summary(m)$r.squared))
  }
  dfp <- df %>% filter(x > 0, k600_1.day > 0)   # both get logged
  m8  <- lm(log(k600_1.day) ~ log(x), data = dfp)
  a8  <- exp(coef(m8)[[1]]); b8 <- unname(coef(m8)[[2]])
  if (form == "power" || form == "M8") {
    return(list(form = "power", a = a8, b = b8,
                xmin = min(dfp$x), xmax = max(dfp$x), n = nrow(dfp),
                r2 = summary(m8)$r.squared))
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
  if (nrow(bps) == 0) return(list(form = "power", a = a8, b = b8,
                                  xmin = min(dfp$x), xmax = max(dfp$x), n = nrow(dfp),
                                  r2 = summary(m8)$r.squared))
  bp   <- bps$bp[which.min(bps$sse)]
  m    <- lm(log(k600_1.day) ~ log(x), data = dfp %>% filter(x <= bp))
  a    <- exp(coef(m)[[1]]); b <- unname(coef(m)[[2]]); lo <- min(dfp$x)
  list(form = "M6", a = a, b = b, bp = bp,
       flatval = a * bp^b, flatval_lo = a * lo^b,
       xmin = lo, xmax = max(dfp$x), n = nrow(dfp), r2 = summary(m)$r.squared)
}

## never extrapolate past the calibration range; floor at 0.05
predict_k600 <- function(fit, x) {
  out <- switch(fit$form,
    linear = fit$int + fit$slope * pmin(pmax(x, fit$xmin), fit$xmax),
    power  = fit$a * pmin(pmax(x, fit$xmin), fit$xmax)^fit$b,
    M6     = case_when(x < fit$xmin ~ fit$flatval_lo,
                       x <= fit$bp  ~ fit$a * x^fit$b,
                       TRUE         ~ fit$flatval))
  pmax(out, 0.05)
}

build_K600 <- function(recipe, velocity_RC) {
  ## ---- 2a. gas-dome rating curves ----
  rc_sites <- recipe %>% filter(k600_src == "RC")
  K600_RC <- if (nrow(rc_sites) == 0) NULL else
    map_dfr(seq_len(nrow(rc_sites)), function(i) {
      r <- rc_sites[i, ]
      x_daily <- if (r$k600_pred == "velocity") {
        velocity_RC %>% filter(ID == r$ID) %>% mutate(day = as.Date(Date)) %>%
          group_by(ID, day) %>% summarise(x = mean(velocity, na.rm = TRUE), .groups = "drop")
      } else {
        depth_daily %>% filter(ID == r$ID) %>% rename(x = depth)
      }
      cal <- trim_k600(k600_clean, r$k600_excl) %>%
        filter(ID == r$ID) %>%
        left_join(x_daily, by = c("ID", "Date" = "day")) %>%
        filter(!is.na(x))
      fit <- fit_k600(cal, r$k600_form)

      pred_x <- if (r$k600_pred == "velocity") {
        velocity_RC %>% filter(ID == r$ID) %>% select(Date, ID, x = velocity)
      } else {
        depth %>% filter(ID == r$ID) %>% select(Date, ID, x = depth)
      }
      pred_x %>% mutate(K600 = predict_k600(fit, x)) %>% select(Date, ID, K600)
    })

  ## ---- 2b. free-water Bayesian K600 ----
  fw_sites <- recipe %>% filter(k600_src == "freewater") %>% pull(ID)
  K600_fw <- if (length(fw_sites) == 0) NULL else
    depth %>%
      filter(ID %in% fw_sites) %>%
      mutate(day = as_date(Date)) %>%
      inner_join(K600_freewater_all, by = c("ID", "day")) %>%  # inner_join drops unconverged days
      select(Date, ID, K600)

  bind_rows(K600_RC, K600_fw)
}

## ==================== 3. two-station DO mass balance =========================
run_two_station <- function(recipe, label = "unnamed") {

  velocity_RC <- fit_velocity_RC(recipe)
  K600        <- build_K600(recipe, velocity_RC)

  master <- reduce(list(depth, DO, K600, velocity_RC %>% select(-depth)),
                   full_join, by = c("ID", "Date")) %>%
    left_join(area, by = "ID") %>%
    mutate(discharge = w * depth * velocity * 86400) %>%
    full_join(VentDO, by = c("ID", "Date"), relationship = "many-to-many") %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    fill(VentDO, VentTemp, .direction = "downup") %>%    # NEVER fill K600
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
      light.corrected      = calc_light(solar.time.corrected, lat, lon),
      ## 3f. carbon-turnover / footprint diagnostic
      reach.km   = ((velocity * 86400) / K600) / 10^3,
      reach.test = case_when(velocity < 0        ~ 'below',
                             reach.km > 3   * km ~ 'too fast',
                             reach.km < 0.4 * km ~ 'too slow',
                             TRUE                ~ 'passes'),
      date = as_date(Date)          # observation day -- NOT the shifted clock
    ) %>%
    group_by(ID, date) %>% filter(n() >= 20) %>% ungroup() %>%          # near-complete days
    mutate(time = if_else(light.corrected > 0, 'day', 'night')) %>%
    filter(!is.na(time)) %>%
    group_by(ID, date) %>% filter(sum(light.corrected > 0, na.rm = TRUE) >= 5) %>% ungroup()

  ## ==================== 4. isolate GPP / ER ==================================
  gpp_er <- master %>%
    group_by(ID, date, time) %>%
    summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = time, values_from = avg, values_fn = mean) %>%
    rename(GPP = day, ER = night) %>%
    filter(!is.na(GPP) | !is.na(ER))

  drivers <- c("depth", "velocity", "discharge", "K600", "DO", "Temp",
               "VentDO", "VentTemp", "DO.deficit.from.sat", "change.DO.flux",
               "K.flux", "travel.time.hr", "reach.km", "w", "km", "m", "area")

  daily_drivers <- master %>%
    group_by(ID, date) %>%
    summarise(
      n_obs           = n(),
      across(all_of(drivers), ~mean(.x, na.rm = TRUE)),
      pct_reach_pass  = mean(reach.test == "passes", na.rm = TRUE),
      reach_test_mode = names(sort(table(reach.test), decreasing = TRUE))[1],
      .groups = "drop")

  daily <- gpp_er %>%
    left_join(daily_drivers, by = c("ID", "date")) %>%
    mutate(NEP = GPP + ER, .after = ER) %>%
    mutate(methodology = label, .before = 1) %>%
    arrange(ID, date)

  list(label = label, recipe = recipe, daily = daily, master = master,
       velocity_RC = velocity_RC, K600 = K600)
}

## ==================== 5. success metrics =====================================
## A day "passes reach" when the majority of that day's observations pass.
score_methodology <- function(daily) {
  daily %>%
    mutate(
      in_range   = !is.na(GPP) & !is.na(ER) &
                   GPP >= GPP_RANGE[1] & GPP <= GPP_RANGE[2] &
                   ER  >= ER_RANGE[1]  & ER  <= ER_RANGE[2],
      reach_pass = reach_test_mode == "passes",
      both       = in_range & reach_pass
    ) %>%
    group_by(methodology, ID) %>%
    summarise(
      n_days       = n(),
      pct_in_range = 100 * mean(in_range,   na.rm = TRUE),
      pct_reach    = 100 * mean(reach_pass, na.rm = TRUE),
      pct_both     = 100 * mean(both,       na.rm = TRUE),
      K600_mean    = mean(K600, na.rm = TRUE),
      GPP_mean     = mean(GPP,  na.rm = TRUE),
      ER_mean      = mean(ER,   na.rm = TRUE),
      K600_med     = median(K600, na.rm = TRUE),
      GPP_med      = median(GPP,  na.rm = TRUE),
      ER_med       = median(ER,   na.rm = TRUE),
      .groups = "drop") %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
}
