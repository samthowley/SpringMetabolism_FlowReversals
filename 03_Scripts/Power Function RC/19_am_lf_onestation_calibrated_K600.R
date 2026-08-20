rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# Recalibrates AM and LF's K600(depth) curve to one-station's own independent
# K600 (Bayesian, fit from discharge-binned gas-dome priors, no depth RC
# involved) instead of the gas-dome-float breakpoint fit (M6) -- per the
# magnitude check in K600_magnitude_comparison.csv:
#   AM: RC is too HIGH at every depth band (6-11x at the shallow/deep
#       extremes) -- refit the whole curve to one-station's own K600 vs depth.
#   LF: RC roughly matches one-station in the shallow/mid bands (ratio ~1) but
#       is ~3x too high in the deep tail -- keep M6's gas-dome decline for the
#       sampled range, recalibrate only the flat tail (same hybrid logic as
#       GB's M8, see gb-investigation memory).
#   GB: unchanged, flat site-average K600 = 7.77 d^-1 (already established,
#       see gb-investigation memory).
#   ID: unchanged, M6 (already coalesces well).
# Then reruns the two-station mass balance with the solar-day/night +
# travel-time-shift correction from 16_travel_time_correction.R on top of
# this recalibrated K600, and compares GPP/ER against one-station.

outdir <- "04_Outputs/Power Function RC"

## ---- breakpoint fit helper (same methodology as 02_fit_breakpoint_K600.R) ----
find_breakpoint <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < max(df$depth)]
  map_dfr(cand, function(bp) {
    left <- df %>% filter(depth <= bp)
    right <- df %>% filter(depth > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600) ~ log(depth), data = left)
    flatval <- exp(predict(m, newdata = data.frame(depth = bp)))
    sse_left <- sum((left$k600 - exp(predict(m)))^2)
    sse_right <- sum((right$k600 - flatval)^2)
    tibble(bp = bp, sse = sse_left + sse_right)
  })
}
fit_segment <- function(df, bp) {
  left <- df %>% filter(depth <= bp)
  m <- lm(log(k600) ~ log(depth), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  list(a = a, b = b, bp = bp, flatval = a * bp^b)
}

## ---- AM: refit the whole curve to one-station's own K600 ----
magnitude <- read_csv(file.path(outdir, "K600_magnitude_comparison.csv"), show_col_types = FALSE)

am_onestation <- magnitude %>% filter(ID == "AM", method == "M6_breakpoint_stat") %>%
  distinct(date, depth, K600_one) %>% rename(k600 = K600_one)

am_bp_search <- find_breakpoint(am_onestation)
am_bp <- am_bp_search %>% slice_min(sse, n = 1, with_ties = FALSE) %>% pull(bp)
am_fit <- fit_segment(am_onestation, am_bp)
cat("AM refit to one-station K600: a=", am_fit$a, " b=", am_fit$b, " breakpoint=", am_fit$bp,
    " flatval=", am_fit$flatval, "\n")

## ---- LF: keep M6's decline, recalibrate only the flat tail ----
raw_valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE) %>%
  rename(k600 = k600_1.day)
judgment_drop <- tribble(~ID, ~row, "LF", 5)
lf_gasdome <- raw_valid %>% filter(ID == "LF") %>% anti_join(judgment_drop, by = c("ID","row"))
lf_bp_search <- find_breakpoint(lf_gasdome)
lf_bp <- lf_bp_search %>% slice_min(sse, n = 1, with_ties = FALSE) %>% pull(bp)
lf_gasdome_fit <- fit_segment(lf_gasdome, lf_bp)

lf_onestation_tail <- magnitude %>% filter(ID == "LF", method == "M6_breakpoint_stat", depth > lf_bp) %>%
  pull(K600_one) %>% median(na.rm = TRUE)
cat("LF: keeping gas-dome decline (a=", lf_gasdome_fit$a, " b=", lf_gasdome_fit$b,
    ") to breakpoint=", lf_bp, "; flat tail recalibrated from", lf_gasdome_fit$flatval,
    "to one-station's median beyond breakpoint =", lf_onestation_tail, "\n")

## ---- apply both curves to the full hourly depth series, daily-max aggregation ----
depth_hourly <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE)

am_daily <- depth_hourly %>% filter(ID == "AM") %>%
  mutate(k600_1d = if_else(depth <= am_fit$bp, am_fit$a * depth^am_fit$b, am_fit$flatval),
         Date = as.Date(Date)) %>%
  group_by(ID, Date) %>% summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop")

lf_daily <- depth_hourly %>% filter(ID == "LF") %>%
  mutate(k600_1d = if_else(depth <= lf_bp, lf_gasdome_fit$a * depth^lf_gasdome_fit$b, lf_onestation_tail),
         Date = as.Date(Date)) %>%
  group_by(ID, Date) %>% summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop")

gb_daily <- depth_hourly %>% filter(ID == "GB") %>% distinct(ID, Date = as.Date(Date)) %>%
  mutate(K600_1.d_daily = 7.77)  # flat site-average, see gb-investigation memory

id_daily <- read_csv(file.path(outdir, "K600_M6_breakpoint_stat.csv"), show_col_types = FALSE) %>%
  filter(ID == "ID") %>% mutate(Date = as.Date(Date))

K600_combined <- bind_rows(am_daily, gb_daily, id_daily, lf_daily) %>%
  mutate(Date = ymd_hms(paste(Date, "00:00:00")))

write_csv(K600_combined, file.path(outdir, "19_K600_combined_onestation_calibrated.csv"))

## ---- rerun two-station mass balance with this K600, + solar day/night + travel-time shift ----
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area<-left_join(width, length_tbl)%>% mutate(area=w*m)%>%
  mutate(m=if_else(ID=='AM', 800, m))  # unchanged for now -- pending GPS reach length (script 17/18)

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names <- file.names[c(2,4,6,12)]  # depth, DO, K600 (will be overridden), velocity
data <- lapply(file.names, function(x) read_csv(x, col_types = cols(ID = col_character())))
master <- reduce(data, full_join, by = c("ID", "Date")) %>%
  select(-K600_1.d_daily) %>%  # drop the original K600, replace with the recalibrated series
  left_join(K600_combined, by = c("ID","Date")) %>%
  left_join(area)

VentDO <- read_csv("04_Outputs/VentDO.csv")

master <- full_join(master, VentDO, by=c('ID','Date')) %>%
  arrange(ID, Date) %>% group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction="downup") %>%
  filter(!ID %in% c('OS','IU')) %>%
  distinct(ID, Date, .keep_all = TRUE)

discharge <- master %>% mutate(discharge = w*depth*velocity*86400)
change.DO.flux <- discharge %>% mutate(change.DO.flux = ((DO-VentDO)*discharge)/area)
DO.deficit <- change.DO.flux %>% mutate(
  Vent.DO.sat = Cs(VentTemp),
  stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
  DO.deficit.from.sat = ((Vent.DO.sat-VentDO)+(stat2.DO.sat-DO))/2
)
K.rearation <- DO.deficit %>% mutate(K.flux = K600_1.d_daily*depth*DO.deficit.from.sat)
air.water.xchange <- K.rearation %>% mutate(not.air.water.xchange = change.DO.flux - K.flux)

lat.lon <- data.frame(ID = c('AM','LF','GB','ID'),
                       lat = c(30.155, 29.585, 29.83, 29.93),
                       lon = c(-83.238, -82.93, -82.68, -82.8))

travel <- air.water.xchange %>%
  left_join(lat.lon, by='ID') %>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    travel.time.hr = if_else(velocity>0, (m/velocity)/3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr/2)*3600,
    light.corrected = calc_light(solar.time.corrected, lat, lon)
  )

active.reach <- travel %>%
  mutate(reach.km = ((velocity*86400)/K600_1.d_daily)/10^3,
         reach.test = if_else(reach.km>3*km, 'above', 'passes'),
         reach.test = if_else(reach.km<0.4*km, 'below', reach.test),
         reach.test = if_else(velocity<0, 'below', reach.test)) %>%
  filter(reach.test %in% c('passes','above')) %>%
  mutate(date = as_date(Date)) %>% group_by(date) %>% filter(n() >= 20) %>% ungroup() %>% select(-date)

day.parse <- active.reach %>%
  mutate(time = if_else(light.corrected>0, 'day', 'night')) %>%
  filter(!is.na(time)) %>%
  mutate(date = as_date(solar.time.corrected)) %>%
  group_by(date) %>% filter(sum(light.corrected>0, na.rm=TRUE) >= 5) %>% ungroup()

isolate <- day.parse %>% group_by(date, ID, time) %>%
  summarise(avg = mean(not.air.water.xchange, na.rm=TRUE), .groups='drop')

ER <- isolate %>% filter(time=='night') %>% rename(ER=avg) %>% select(-time)
GPP <- isolate %>% filter(time=='day') %>% rename(GPP=avg) %>% select(-time)
NEP <- left_join(GPP, ER, by=c('date','ID')) %>% filter(GPP<=34, ER>=-34)

one.station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE)

summary.tbl <- bind_rows(
  NEP %>% group_by(ID) %>% summarise(GPP=median(GPP,na.rm=TRUE), ER=median(ER,na.rm=TRUE), n=n()) %>%
    mutate(method='two-station, one-station-calibrated K600 + solar/shift'),
  one.station %>% group_by(ID) %>% summarise(GPP=median(GPP,na.rm=TRUE), ER=median(ER,na.rm=TRUE), n=n()) %>%
    mutate(method='one-station')
) %>% select(ID, method, GPP, ER, n) %>% arrange(ID, method)

cat("\nMedian GPP/ER by site and method:\n")
print(as.data.frame(summary.tbl))

write_csv(summary.tbl, file.path(outdir, "19_am_lf_onestation_calibrated_summary.csv"))
write_csv(NEP, file.path(outdir, "19_two_station_onestation_calibrated.csv"))
