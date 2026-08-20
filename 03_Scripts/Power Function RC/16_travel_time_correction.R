rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# Self-contained: reproduces "two station.R"'s read/join pipeline, then adds a
# time-of-travel correction (Marzolf et al. 1994; Young & Huryn 1998) that the
# original script doesn't apply -- upstream (vent) and downstream readings are
# currently paired on the same clock timestamp, which ignores the time it takes
# a water parcel to actually travel the reach.

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area<-left_join(width, length)%>% mutate(area=w*m)%>%
  mutate(m=if_else(ID=='AM', 800, m))

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,4,6,12)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  left_join(area)

VentDO <- read_csv("04_Outputs/VentDO.csv")

master<-
  full_join(master, VentDO, by=c('ID', 'Date'))%>%
  arrange(ID, Date)%>%
  group_by(ID)%>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction= "downup")%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  distinct(ID, Date, .keep_all = T)

discharge<-master%>%
  mutate(discharge=w*depth*velocity*86400)

change.DO.flux<-discharge%>%
  mutate(change.DO.flux=((DO-VentDO)*discharge)/area)

DO.deficit<-change.DO.flux%>%  mutate(
  Vent.DO.sat=Cs(VentTemp),
  stat2.DO.sat=Cs(fahrenheit.to.celsius(Temp)),
  DO.deficit.from.sat=((Vent.DO.sat-VentDO)+(stat2.DO.sat-DO))/2,
)

K.rearation<-DO.deficit%>%
  mutate(K.flux=(K600_1.d_daily)*depth*DO.deficit.from.sat)

air.water.xchange<-K.rearation%>%
  mutate(not.air.water.xchange=change.DO.flux-K.flux)

## ---- time-of-travel correction ----------------------------------------
# travel time (hr) = reach length (m) / velocity (m/s), converted to hours.
# Each hour's flux estimate represents the metabolism/reaeration that happened
# to a parcel of water as it moved from the vent to the downstream sensor, so
# it's assigned to solar time at the midpoint of that transit, not the raw
# downstream sample clock time (standard simplification: Young & Huryn 1998).

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

travel<-air.water.xchange%>%
  left_join(lat.lon, by='ID')%>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    travel.time.hr = if_else(velocity>0, (m/velocity)/3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr/2)*3600,
    light.raw = calc_light(solar.time.raw, lat, lon),
    light.corrected = calc_light(solar.time.corrected, lat, lon)
  )

cat("Median travel time by site (hr):\n")
print(travel%>%group_by(ID)%>%summarize(median.travel.hr=median(travel.time.hr, na.rm=T)))

#5.5 filter: estimating reach (unchanged from two station.R)#####
active.reach <-
  travel %>%
  mutate(reach.km=( (velocity*86400) /K600_1.d_daily)/10^3,
         reach.test=if_else(reach.km>3*km, 'above', 'passes'),
         reach.test=if_else(reach.km<0.4*km, 'below', reach.test),
         reach.test=if_else(velocity<0, 'below', reach.test)
  )%>%
  filter(reach.test %in% c('passes', 'above'))%>%
  mutate(date = as_date(Date)) %>%
  group_by(date) %>%
  filter(n() >= 20) %>%
  ungroup()%>%select(-date)

## ---- (A) baseline: original sign-based day/night, no lag correction ----
day.parse.baseline <- active.reach %>%
  mutate(time=case_when(not.air.water.xchange>0~ 'day',
                         not.air.water.xchange<0~ 'night'))%>%
  filter(!is.na(time)) %>%
  mutate(date = as_date(solar.time.raw)) %>%
  group_by(date) %>%
  filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>%
  ungroup()

isolate.baseline<-day.parse.baseline%>%
  group_by(date,ID,time) %>%
  summarize(avg = mean(not.air.water.xchange, na.rm=T), .groups='drop')

ER.baseline<-isolate.baseline%>%filter(time=='night')%>%rename(ER=avg)%>%select(-time)
GPP.baseline<-isolate.baseline%>%filter(time=='day')%>%rename(GPP=avg)%>%select(-time)
NEP.baseline<-left_join(GPP.baseline, ER.baseline, by=c('date','ID')) %>%
  filter(GPP<=34, ER>=-34)

## ---- (B) travel-time corrected: real solar light + shifted process time ----
day.parse.corrected <- active.reach %>%
  mutate(time=if_else(light.corrected>0, 'day', 'night')) %>%
  filter(!is.na(time)) %>%
  mutate(date = as_date(solar.time.corrected)) %>%
  group_by(date) %>%
  filter(sum(light.corrected > 0, na.rm = TRUE) >= 5) %>%
  ungroup()

isolate.corrected<-day.parse.corrected%>%
  group_by(date,ID,time) %>%
  summarize(avg = mean(not.air.water.xchange, na.rm=T), .groups='drop')

ER.corrected<-isolate.corrected%>%filter(time=='night')%>%rename(ER=avg)%>%select(-time)
GPP.corrected<-isolate.corrected%>%filter(time=='day')%>%rename(GPP=avg)%>%select(-time)
NEP.corrected<-left_join(GPP.corrected, ER.corrected, by=c('date','ID')) %>%
  filter(GPP<=34, ER>=-34)

## ---- (C) control: real solar light, but at raw clock time (no travel-time shift) ----
day.parse.solaronly <- active.reach %>%
  mutate(time=if_else(light.raw>0, 'day', 'night')) %>%
  filter(!is.na(time)) %>%
  mutate(date = as_date(solar.time.raw)) %>%
  group_by(date) %>%
  filter(sum(light.raw > 0, na.rm = TRUE) >= 5) %>%
  ungroup()

isolate.solaronly<-day.parse.solaronly%>%
  group_by(date,ID,time) %>%
  summarize(avg = mean(not.air.water.xchange, na.rm=T), .groups='drop')

ER.solaronly<-isolate.solaronly%>%filter(time=='night')%>%rename(ER=avg)%>%select(-time)
GPP.solaronly<-isolate.solaronly%>%filter(time=='day')%>%rename(GPP=avg)%>%select(-time)
NEP.solaronly<-left_join(GPP.solaronly, ER.solaronly, by=c('date','ID')) %>%
  filter(GPP<=34, ER>=-34)

## ---- compare ----
one.station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE)

summary.tbl <- bind_rows(
  NEP.baseline %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='two-station, baseline (sign-based, no lag)'),
  NEP.solaronly %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='two-station, solar day/night, no lag'),
  NEP.corrected %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='two-station, solar day/night + travel-time shift'),
  one.station %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='one-station')
) %>% select(ID, method, GPP, ER, n) %>% arrange(ID, method)

cat("\nMedian GPP/ER by site and method:\n")
print(as.data.frame(summary.tbl))

write_csv(summary.tbl, "04_Outputs/Power Function RC/16_travel_time_correction_summary.csv")
write_csv(NEP.corrected, "04_Outputs/Power Function RC/16_two_station_travel_time_corrected.csv")
