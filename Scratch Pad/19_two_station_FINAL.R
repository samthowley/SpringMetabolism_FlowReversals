rm(list=ls())
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')      # Cs()
library(streamMetabolizer)       # calc_light()
library(readxl)
library(measurements)

## =============================================================================

outdir <- "Scratch Pad"
sites  <- c("AM", "GB", "ID", "LF", "OS")

recipe <- tribble(
  ~ID,  ~status,                            ~units,   ~vel_form, ~vel_excl, ~k600_src,    ~k600_pred, ~k600_form, ~k600_excl,
  "AM", "reliable",                         "county", "linear",  "strict",  "RC",         "depth",    "linear",   "strict",
  "GB", "reliable",                         "county", "power",   "strict",  "RC",         "velocity", "M6",       "base",
  "LF", "reliable",                         "county", "power",   "base",    "RC",         "depth",    "linear",   "base",
  "ID", "plausible (K600 prior-dependent)", "county", "power",   "base",    "freewater",  NA,         NA,         NA,
  "OS", "UNRELIABLE",                       "county", "linear",  "base",    "RC",         "depth",    "M8",       "base"
)

unit_lists <- list(
  all_conv = c('AM', 'GB', 'ID', 'LF', 'OS'),
  county   = c('AM', 'LF', 'OS')          # GB/ID stay raw
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
## ---- 2a. gas-dome rating curves (AM, GB, LF, OS) ----
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
  dfp <- df %>% filter(x > 0)
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

K600_RC <- recipe %>% filter(k600_src == "RC") %>%
  { map_dfr(seq_len(nrow(.)), function(i) {
      r <- .[i, ]
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
    }) }

## ---- 2b. free-water Bayesian K600 (ID only) ----
## Daily K600 from her one-station Stan runs; converged days only.
K600_fw <- read_csv("04_Outputs/one station results/met_results_two.csv", show_col_types = FALSE) %>%
  separate(ID, into = c("ID", "prior"), sep = "_") %>%
  filter(ID == "ID", prior == "hi",
         !is.na(K600_daily_mean), K600_daily_Rhat < 1.05) %>%
  transmute(ID, day = as_date(date), K600 = K600_daily_mean)

K600_fw_expanded <- depth %>%
  filter(ID == "ID") %>%
  mutate(day = as_date(Date)) %>%
  inner_join(K600_fw, by = c("ID", "day")) %>%     # inner_join: drops unconverged days
  select(Date, ID, K600)

K600 <- bind_rows(K600_RC, K600_fw_expanded)

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
  left_join(recipe %>% select(ID, status), by = "ID") %>%
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
  group_by(ID, date) %>% filter(sum(light.corrected > 0, na.rm = TRUE) >= 5) %>% ungroup()

## ==================== 4. isolate GPP / ER ====================================
gpp_er <- master %>%
  group_by(ID, status, date, time) %>%
  summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = time, values_from = avg, values_fn = mean) %>%
  rename(GPP = day, ER = night) %>%
  filter(!is.na(GPP) | !is.na(ER))

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

## ==================== 6. plots ===============================================
methodology <- recipe %>%
  mutate(methodology = if_else(
    k600_src == "freewater",
    paste0("vel ", vel_form, ", K600 free-water"),
    paste0("vel ", vel_form, ", K600 ", k600_pred, "-", k600_form)
  )) %>%
  select(ID, methodology)

daily %>%
  left_join(methodology, by = "ID") %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")),
         panel = paste0(ID, ifelse(status == "reliable", "", paste0("  [", status, "]")),
                         "\n", methodology)) %>%
  ggplot(aes(x = date)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin =   2, ymax =  25, fill = "#1b9e77", alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -25, ymax =  -2, fill = "#d95f02", alpha = 0.08) +
  geom_point(aes(y = GPP, color = "GPP"), 
             size = 1) +
  geom_point(aes(y = ER,  color = "ER"),  size = 1) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(GPP = "#1b9e77", ER = "#d95f02")) +
  facet_wrap(~panel, scales = "free", ncol = 3) +
  labs(title = "Two-station metabolism -- preferred methodology per site",
       subtitle = "shaded = plausible range (GPP 2-25, ER -2 to -25)",
       x = NULL, y = "g O2 m-2 d-1") +
  theme_bw(base_size = 10)


daily %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")))%>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = GPP, color=reach_test_mode), size = 0.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free") +
  theme_bw(base_size = 10)

