rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

## ---- Tests whether ID's reaeration term (K.flux) swamping the transport
## term (change.DO.flux) is explained by ID's unusually long travel time
## (median 3.72 hr vs 0.7-1.0 hr at GB/AM/LF). K.flux is currently computed
## from the downstream sample's INSTANTANEOUS depth/DO.deficit.from.sat, while
## change.DO.flux is implicitly averaged over the whole transit (it reduces to
## (DO-VentDO)*depth/travel_time_days). This script shifts depth and
## DO.deficit.from.sat to the travel-time MIDPOINT (same shift already used
## for day/night classification in the live script) before computing K.flux,
## and compares the K.flux/change.DO.flux ratio before vs after.
## Standalone -- does not touch the live pipeline.

outdir_pf <- "04_Outputs/"

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
reach_gps <- read_csv(file.path(outdir_pf, "Power Function RC/reach_length_from_coords.csv"), show_col_types = FALSE) %>%
  select(ID, km = length_km, m = length_m)
area <- left_join(width, reach_gps, by = "ID") %>% mutate(area = w * m)

depth <- read_csv("02_Clean_data/Chem/depth.csv", col_types = cols(ID = col_character()))
DO    <- read_csv("02_Clean_data/Chem/DO.csv", col_types = cols(ID = col_character()))

K600  <- read_csv(file.path(outdir_pf, "K600_RC_depth.csv"), col_types = cols(ID = col_character())) %>%
  filter(method=='K600_M6')%>%
  select(Date, ID, K600_1.d_daily)

velocity_RC <- read_csv("04_Outputs/velocity_RC_power.csv")%>%
  select(-depth)

master <- reduce(list(depth, DO, K600,velocity_RC), full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID")%>%
  mutate(discharge=w*depth*velocity*86400)

VentDO <- read_csv("02_Clean_data/Chem/VentDO.csv", show_col_types = FALSE) %>%
  filter(VentDO >= 0)%>%
  mutate(
    VentDO=ifelse(ID=='GB' & VentDO<2, NA, VentDO),
    VentDO=ifelse(ID=='AM' & VentDO<0.9, NA, VentDO)
  )

master <- full_join(master, VentDO, by = c('ID', 'Date'), relationship = "many-to-many") %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
  filter(!ID %in% c('OS', 'IU')) %>%
  distinct(ID, Date, .keep_all = TRUE) %>%
  ungroup()

change.DO.flux <- master %>%
  mutate(change.DO.flux = ((DO - VentDO) * discharge) / area)

DO.deficit <- change.DO.flux %>% mutate(
  Vent.DO.sat = Cs(VentTemp),
  stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
  DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2,
)

## ---- travel-time midpoint shift ----
shifted <- DO.deficit %>%
  mutate(
    travel.time.hr = if_else(velocity > 0, (m / velocity) / 3600, NA_real_),
    Date.num = as.numeric(Date),
    shift.time.num = Date.num - (travel.time.hr / 2) * 3600
  )

## interpolate depth and DO.deficit.from.sat to the shifted (midpoint) time, per site
interp_shift <- function(df) {
  ok <- !is.na(df$Date.num) & !is.na(df$depth)
  depth.fun <- approxfun(df$Date.num[ok], df$depth[ok], rule = 2)
  ok2 <- !is.na(df$Date.num) & !is.na(df$DO.deficit.from.sat)
  deficit.fun <- approxfun(df$Date.num[ok2], df$DO.deficit.from.sat[ok2], rule = 2)
  df$depth.shifted <- if_else(!is.na(df$shift.time.num), depth.fun(df$shift.time.num), NA_real_)
  df$deficit.shifted <- if_else(!is.na(df$shift.time.num), deficit.fun(df$shift.time.num), NA_real_)
  df
}

shifted <- shifted %>% group_by(ID) %>% group_modify(~interp_shift(.x)) %>% ungroup()

K.rearation <- shifted %>%
  mutate(
    K.flux = K600_1.d_daily * depth * DO.deficit.from.sat,
    K.flux.shifted = K600_1.d_daily * depth.shifted * deficit.shifted
  )

air.water.xchange <- K.rearation %>%
  mutate(
    not.air.water.xchange = change.DO.flux - K.flux,
    not.air.water.xchange.shifted = change.DO.flux - K.flux.shifted,
    ratio = abs(K.flux) / abs(change.DO.flux),
    ratio.shifted = abs(K.flux.shifted) / abs(change.DO.flux)
  )

cat("=== before vs after travel-time-midpoint shift of reaeration inputs ===\n")
air.water.xchange %>%
  group_by(ID) %>%
  summarize(
    reach_m = first(m),
    median_travel_time_hr = median(travel.time.hr, na.rm = T),
    n = n(),
    median_ratio_before = median(ratio, na.rm = T),
    median_ratio_after = median(ratio.shifted, na.rm = T),
    pct_negative_before = mean(not.air.water.xchange < 0, na.rm = T),
    pct_negative_after = mean(not.air.water.xchange.shifted < 0, na.rm = T)
  ) %>%
  print(width = 200)

#write_csv(air.water.xchange, file.path(outdir_pf, "Power Function RC/39_travel_time_shifted_reaeration.csv"))
