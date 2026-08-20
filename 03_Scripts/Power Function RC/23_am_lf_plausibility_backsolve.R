rm(list=ls())
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# Four plausibility checks for AM/LF, requested directly:
#  1. What would VentDO need to be, holding K600 fixed, to hit one-station's
#     target ER?
#  2. What would K600 need to be, holding VentDO fixed, to hit target ER?
#  3. Does raw DO actually fall overnight the way a normal stream's does
#     (independent of the mass-balance formula entirely)?
#  4. Does the discharge estimate (w*depth*velocity) match the independent
#     "discharge.csv" already in the pipeline?

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM","GB","ID","LF")
one_station_target <- c(AM=-11.696010, GB=-19.364467, ID=-17.143626, LF=-17.028680)  # one-station median ER

## ---- rebuild the corrected pipeline through air.water.xchange (KMZ reach length, solar+shift class.) ----
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
reach_gps <- read_csv(file.path(outdir, "reach_length_from_coords.csv"), show_col_types = FALSE) %>%
  select(ID, km = length_km, m = length_m)
area <- left_join(width, reach_gps, by = "ID") %>% mutate(area = w*m)

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names <- file.names[c(2,4,6,12)]
data <- lapply(file.names, function(x) read_csv(x, col_types = cols(ID = col_character())))
master <- reduce(data, full_join, by = c("ID","Date")) %>% left_join(area, by = "ID")

VentDO <- read_csv("04_Outputs/VentDO.csv")
master <- full_join(master, VentDO, by=c('ID','Date')) %>%
  arrange(ID, Date) %>% group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction="downup") %>%
  filter(!ID %in% c('OS','IU')) %>%
  distinct(ID, Date, .keep_all = TRUE)

discharge_computed <- master %>% mutate(discharge = w*depth*velocity*86400)

change.DO.flux <- discharge_computed %>% mutate(change.DO.flux = ((DO-VentDO)*discharge)/area)
DO.deficit <- change.DO.flux %>% mutate(
  Vent.DO.sat = Cs(VentTemp),
  stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
  DO.deficit.from.sat = ((Vent.DO.sat-VentDO)+(stat2.DO.sat-DO))/2
)
K.rearation <- DO.deficit %>% mutate(K.flux = K600_1.d_daily*depth*DO.deficit.from.sat)
air.water.xchange <- K.rearation %>% mutate(not.air.water.xchange = change.DO.flux - K.flux)

lat.lon <- data.frame(ID=c('AM','LF','GB','ID'), lat=c(30.155,29.585,29.83,29.93), lon=c(-83.238,-82.93,-82.68,-82.8))
travel <- air.water.xchange %>% left_join(lat.lon, by='ID') %>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    travel.time.hr = if_else(velocity>0, (m/velocity)/3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr/2)*3600,
    light.corrected = calc_light(solar.time.corrected, lat, lon),
    time = if_else(light.corrected>0, 'day', 'night')
  ) %>% filter(!is.na(time), velocity>0)

night <- travel %>% filter(time=='night', ID %in% sites)

## ---- Q1: VentDO needed to hit target ER, holding K600 fixed ----
# not.air.water.xchange = C - VentDO*D   (linear in VentDO; derived from the mass-balance algebra)
#   C = DO*discharge/area - K600*depth*(Vent.DO.sat+stat2.DO.sat-DO)/2
#   D = discharge/area - K600*depth/2
q1 <- night %>%
  mutate(
    C = DO*discharge/area - K600_1.d_daily*depth*(Vent.DO.sat+stat2.DO.sat-DO)/2,
    D = discharge/area - K600_1.d_daily*depth/2,
    target = one_station_target[ID],
    VentDO_needed = (C - target)/D
  ) %>% filter(is.finite(VentDO_needed))

cat("=== Q1: VentDO needed (mg/L) to hit target ER, vs. actual VentDO used ===\n")
q1 %>% group_by(ID) %>%
  summarise(actual_VentDO_median = round(median(VentDO, na.rm=TRUE), 2),
            actual_VentDO_range = paste0(round(min(VentDO,na.rm=TRUE),2), "-", round(max(VentDO,na.rm=TRUE),2)),
            VentDO_needed_median = round(median(VentDO_needed), 2),
            VentDO_needed_q25 = round(quantile(VentDO_needed, 0.25), 2),
            VentDO_needed_q75 = round(quantile(VentDO_needed, 0.75), 2),
            .groups='drop') %>% print(width=Inf)

## ---- Q2: K600 needed to hit target ER, holding VentDO fixed ----
q2 <- night %>%
  mutate(target = one_station_target[ID],
         K600_needed = (change.DO.flux - target)/(depth*DO.deficit.from.sat)) %>%
  filter(is.finite(K600_needed))

cat("\n=== Q2: K600 needed (1/day) to hit target ER, vs. actual RC K600 used ===\n")
q2 %>% group_by(ID) %>%
  summarise(actual_K600_median = round(median(K600_1.d_daily, na.rm=TRUE), 2),
            K600_needed_median = round(median(K600_needed), 2),
            K600_needed_q25 = round(quantile(K600_needed, 0.25), 2),
            K600_needed_q75 = round(quantile(K600_needed, 0.75), 2),
            pct_negative_deficit = round(100*mean(DO.deficit.from.sat<0, na.rm=TRUE),1),
            .groups='drop') %>% print(width=Inf)

## ---- Q3: does raw DO actually fall overnight, independent of the formula? ----
diel <- air.water.xchange %>%
  left_join(lat.lon, by='ID') %>%
  mutate(solar.time.raw = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
         hour = hour(solar.time.raw)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, hour) %>%
  summarise(mean_DO = mean(DO, na.rm=TRUE), .groups='drop')

cat("\n=== Q3: mean DO by hour-of-day (UTC) -- does it peak midday and fall overnight? ===\n")
diel %>% pivot_wider(names_from = ID, values_from = mean_DO) %>% print(n=24)

night_rate <- travel %>% filter(time=='night', ID %in% sites) %>%
  arrange(ID, solar.time.raw) %>% group_by(ID) %>%
  mutate(dDO_dt = (DO - lag(DO)) / as.numeric(difftime(solar.time.raw, lag(solar.time.raw), units='hours'))) %>%
  filter(!is.na(dDO_dt), is.finite(dDO_dt), abs(as.numeric(difftime(solar.time.raw, lag(solar.time.raw), units='hours'))-1) < 0.01)

cat("\n=== Q3b: hour-to-hour raw DO change at night (mg/L per hr) -- negative = falling as expected ===\n")
night_rate %>% group_by(ID) %>%
  summarise(n=n(), median_dDO_dt = round(median(dDO_dt),3),
            pct_falling = round(100*mean(dDO_dt<0),1), .groups='drop') %>% print()

## ---- Q4: discharge estimate vs. the independent discharge.csv already in the pipeline ----
discharge_indep <- read_csv("02_Clean_data/Chem/discharge.csv", show_col_types = FALSE) %>%
  rename(discharge_indep = discharge)

q4 <- discharge_computed %>% select(ID, Date, discharge_computed = discharge) %>%
  inner_join(discharge_indep, by = c("ID","Date")) %>%
  filter(ID %in% sites, is.finite(discharge_computed), is.finite(discharge_indep))

cat("\n=== Q4: computed (w*depth*velocity) vs. independent discharge.csv ===\n")
q4 %>% group_by(ID) %>%
  summarise(n=n(),
            median_computed = round(median(discharge_computed),0),
            median_indep = round(median(discharge_indep),0),
            median_ratio = round(median(discharge_computed/discharge_indep, na.rm=TRUE),3),
            correlation = round(cor(discharge_computed, discharge_indep, use='complete.obs'),3),
            .groups='drop') %>% print(width=Inf)

write_csv(q1, file.path(outdir, "23_q1_ventdo_needed.csv"))
write_csv(q2, file.path(outdir, "23_q2_k600_needed.csv"))
write_csv(q4, file.path(outdir, "23_q4_discharge_compare.csv"))
