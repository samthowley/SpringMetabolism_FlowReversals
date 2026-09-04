rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)


outdir_pf <- "04_Outputs/"

## ---- call in data---#########
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
reach_gps <- read_csv(file.path(outdir_pf, "Power Function RC/reach_length_from_coords.csv"), show_col_types = FALSE) %>%
  select(ID, km = length_km, m = length_m)
area <- left_join(width, reach_gps, by = "ID") %>% mutate(area = w * m)

depth <- read_csv("02_Clean_data/Chem/depth.csv", col_types = cols(ID = col_character()))
DO    <- read_csv("02_Clean_data/Chem/DO.csv", col_types = cols(ID = col_character()))

K600  <- read_csv(file.path(outdir_pf, "K600_RC_velocity.csv"), col_types = cols(ID = col_character())) %>%
  filter(method=='K600_M8')%>%
  select(Date, ID, K600_1.d_daily) 

velocity_RC <- read_csv("04_Outputs/velocity_RC.csv")%>%
  select(-depth)

master <- reduce(list(depth, DO, K600,velocity_RC), full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID")%>%
  mutate(discharge=w*depth*velocity*86400)


## ---- VentDO: current source, negative readings dropped ----
VentDO <- read_csv("02_Clean_data/Chem/VentDO.csv", show_col_types = FALSE) %>%
  filter(VentDO >= 0)%>%
  mutate(
    VentDO=ifelse(ID=='GB' & VentDO<2, NA, VentDO),
    VentDO=ifelse(ID=='AM' & VentDO<0.9, NA, VentDO),
    ## VentTemp is MIXED units in this file: own gas-dome rows are F (~72),
    ## county/NWIS-merged rows are C (~22). Cs() below assumes C, so F rows
    ## (all of AM's and most of OS's) produced meaningless Vent.DO.sat.
    ## Values >40 can only be F for a FL spring vent -> converted.
    VentTemp=if_else(VentTemp>40, fahrenheit.to.celsius(VentTemp), VentTemp)
         )


master <- full_join(master, VentDO, by = c('ID', 'Date')) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
  filter(ID != 'IU') %>%   # OS now included (reach length from OS.kmz, RCs regenerated with OS)
  distinct(ID, Date, .keep_all = TRUE)

## ---- 2. change in total DO flux ----
change.DO.flux <- master %>%
  mutate(change.DO.flux = ((DO - VentDO) * discharge) / area)

## ---- 3. DO deficit from saturation ----


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
  ID = c('AM', 'LF', 'GB', 'ID', 'OS'),
  lat = c(30.155, 29.585, 29.83, 29.93, 29.6448),
  lon = c(-83.238, -82.93, -82.68, -82.8, -82.9428))  # OS = headspring, first point of OS.kmz trace

travel <- air.water.xchange %>%
  left_join(lat.lon, by = 'ID') %>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    travel.time.hr = if_else(velocity > 0, (m / velocity) / 3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr / 2) * 3600,
    light.corrected = calc_light(solar.time.corrected, lat, lon)
  )

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
  ggplot(aes(x = depth)) +
  geom_point(aes(y = K.flux, color = reach.test)) +
  #scale_color_viridis_c(name = "K600") +
  facet_wrap(~ID, scales = 'free')

results %>%
  #filter(reach.test != 'too slow')%>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = discharge, color = reach.test)) +
  #geom_point(aes(y = ER, color = reach.test)) +
  geom_hline(yintercept = 0) +
  #scale_color_viridis_c(name = "K600") +
  facet_wrap(~ID, scales = 'free')

results %>%
  #filter(reach.test != 'too slow')%>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = GPP, color = "GPP")) +
  geom_point(aes(y = ER, color = "ER")) +
  geom_hline(yintercept = 0) +
  #scale_color_viridis_c(name = "K600") +
  facet_wrap(~ID, scales = 'free')
