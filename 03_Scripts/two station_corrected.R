rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

outdir_pf <- "04_Outputs/Power Function RC"

## ---- reach length (GPS/KMZ), replaces length width.xlsx + AM override ----
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
reach_gps <- read_csv(file.path(outdir_pf, "reach_length_from_coords.csv"), show_col_types = FALSE) %>%
  select(ID, km = length_km, m = length_m)
area <- left_join(width, reach_gps, by = "ID") %>% mutate(area = w * m)


## ---- explicit named reads, replaces list.files()[c(2,4,6,12)] ----
depth <- read_csv("02_Clean_data/Chem/depth.csv", col_types = cols(ID = col_character()))
DO    <- read_csv("02_Clean_data/Chem/DO.csv", col_types = cols(ID = col_character()))

K600_static<- read_csv("04_Outputs/Power Function RC/K600_static_all_sites.csv", col_types = cols(ID = col_character()))
#K600_static%>%filter(ID=="GB")

# K600 and velocity now come from 38_velocity_discharge_K600_RC.R (depth-velocity linear RC,
# 2-segment depth-discharge RC, K600-depth RC w/ linear/M6/M8) -- replaces
# K600_M6_breakpoint_stat.csv and 25_velocity_M6_breakpoint_stat.csv. K600_M6 is the method
# column picked here to match what this pipeline used before (K600_M8 or K600_linear are the
# other options sitting in the same file if you want to compare).
K600  <- read_csv(file.path(outdir_pf, "38_K600_RC.csv"), col_types = cols(ID = col_character())) %>%
  select(Date, ID, K600_1.d_daily = K600_M6) #%>%
  #mutate(K600_1.d_daily=if_else(ID=="GB",8.27,K600_1.d_daily))

discharge_RC <- read_csv("04_Outputs/Power Function RC/38_discharge_RC.csv")

master <- reduce(list(depth, DO, K600, discharge_RC), full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID")

## ---- VentDO: current source, negative readings dropped ----
VentDO <- read_csv("02_Clean_data/Chem/VentDO.csv", show_col_types = FALSE) %>%
  filter(VentDO >= 0)%>%
  mutate(
    VentDO=ifelse(ID=='GB' & VentDO<2, NA, VentDO),
    VentDO=ifelse(ID=='AM' & VentDO<0.9, NA, VentDO)
    
         )

master <- full_join(master, VentDO, by = c('ID', 'Date')) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
  filter(!ID %in% c('OS', 'IU')) %>%
  distinct(ID, Date, .keep_all = TRUE)

## ---- 2. change in total DO flux ----
change.DO.flux <- master %>%
  mutate(change.DO.flux = ((DO - VentDO) * discharge) / area)

## ---- 3. DO deficit from saturation ----
library(StreamMetabolism)
library(weathermetrics)

DO.deficit <- change.DO.flux %>% mutate(
  Vent.DO.sat = Cs(VentTemp),
  stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
  DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2,
)

## ---- 4. K reaeration ----
K.rearation <- DO.deficit %>%
  mutate(K.flux = (K600_1.d_daily) * depth * DO.deficit.from.sat)

## ---- 5. air-water gas exchange ----
air.water.xchange <- K.rearation %>%
  mutate(not.air.water.xchange = change.DO.flux - K.flux)

## ---- 6. solar time + travel-time correction ----
lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

travel <- air.water.xchange %>%
  left_join(lat.lon, by = 'ID') %>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    travel.time.hr = if_else(velocity > 0, (m / velocity) / 3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr / 2) * 3600,
    light.corrected = calc_light(solar.time.corrected, lat, lon)
  )

cat("\nMedian travel time by site (hr):\n")
print(travel %>% group_by(ID) %>% summarize(median.travel.hr = median(travel.time.hr, na.rm = T)))

## ---- 7. carbon-turnover (reach length vs. gas-exchange distance) filter ----
active.reach <- travel %>%
  mutate(reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
         reach.test = if_else(reach.km > 3 * km, 'too fast', 'passes'),
         reach.test = if_else(reach.km < 0.4 * km, 'too slow', reach.test),
         reach.test = if_else(velocity < 0, 'below', reach.test)
  ) %>%
  #filter(reach.test == 'passes') %>%
  mutate(date = as_date(Date)) %>%
  group_by(date) %>%
  filter(n() >= 20) %>%
  ungroup() %>% select(-date)

## ---- 8. day/night from real solar time, not signal sign ----
day.parse <- active.reach %>%
  mutate(time = if_else(light.corrected > 0, 'day', 'night')) %>%
  filter(!is.na(time)) %>%
  mutate(date = as_date(solar.time.corrected)) %>%
  group_by(date) %>%
  filter(sum(light.corrected > 0, na.rm = TRUE) >= 5) %>%
  ungroup()

## ---- 9. isolate ER/GPP ----
isolate <- day.parse %>%
  group_by(date, ID, time) %>%
  summarize(avg = mean(not.air.water.xchange, na.rm = T), .groups = 'drop')

ER <- isolate %>% filter(time == 'night') %>% rename(ER = avg) %>% select(-time)
GPP <- isolate %>% filter(time == 'day') %>% rename(GPP = avg) %>% select(-time)
NEP <- left_join(GPP, ER, by = c('date', 'ID'))

## ---- 10. write results ----
results <- left_join(day.parse, NEP, by = c('date' = 'date', 'ID' = 'ID')) 

#write_csv(results, "04_Outputs/two.station.results_corrected.csv")
## ---- diagnostic plot, same as original ----

results %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = velocity, color = reach.test)) +
  geom_hline(yintercept = 0, color = 'black') +
  #scale_color_viridis_c(name = "K600") +
  facet_wrap(~ID, scales = 'free')


results %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = GPP, color = reach.test)) +
  geom_point(aes(y = ER, color = reach.test)) +
  geom_hline(yintercept = 0) +
  #scale_color_viridis_c(name = "K600") +
  facet_wrap(~ID, scales = 'free')

