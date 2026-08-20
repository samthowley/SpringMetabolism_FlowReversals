rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# Same as 16_travel_time_correction.R, except the reach length ("m"/"km",
# used both for the travel-time shift and the reach.test filter) comes from
# GPS waypoints (run 17_reach_length_from_coords.R first) instead of
# "length width.xlsx" -- which for AM specifically held a manual override
# (900 -> 800) Samantha flagged as a past deliberate adjustment made while
# trying to fix GPP/ER, not a surveyed value. That override is NOT applied
# here; AM's reach length instead comes straight from its GPS coordinates,
# same as every other site.

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length_old <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")

gps_path <- "04_Outputs/Power Function RC/reach_length_from_coords.csv"
if (!file.exists(gps_path)) {
  stop("Run 17_reach_length_from_coords.R first (after filling in reach_waypoints_template.csv).")
}
reach_gps <- read_csv(gps_path, show_col_types = FALSE) %>% select(ID, km = length_km, m = length_m)

missing_sites <- setdiff(c("AM","GB","ID","LF"), reach_gps$ID)
if (length(missing_sites) > 0) {
  cat("No GPS-derived reach length for:", paste(missing_sites, collapse=", "),
      "-- falling back to length width.xlsx for those sites.\n")
  reach_gps <- bind_rows(reach_gps, length_old %>% filter(ID %in% missing_sites))
}

area<-left_join(width, reach_gps, by = "ID") %>% mutate(area=w*m)

cat("Reach length actually used (m):\n")
print(area %>% select(ID, m, km))

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,4,6,12)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  left_join(area, by = "ID")

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

## ---- time-of-travel correction, reach length from GPS ----
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
    light.corrected = calc_light(solar.time.corrected, lat, lon)
  )

cat("\nMedian travel time by site (hr), GPS-derived reach length:\n")
print(travel%>%group_by(ID)%>%summarize(median.travel.hr=median(travel.time.hr, na.rm=T)))

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

## ---- compare against one-station and against the old (excel-based) reach length ----
one.station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE)
old.summary <- read_csv("04_Outputs/Power Function RC/16_travel_time_correction_summary.csv", show_col_types = FALSE) %>%
  filter(method == 'two-station, solar day/night + travel-time shift') %>%
  mutate(method = 'two-station, solar+shift, OLD reach length (excel/AM override)')

summary.tbl <- bind_rows(
  old.summary,
  NEP.corrected %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='two-station, solar+shift, GPS reach length'),
  one.station %>% group_by(ID) %>% summarize(GPP=median(GPP,na.rm=T), ER=median(ER,na.rm=T), n=n()) %>% mutate(method='one-station')
) %>% select(ID, method, GPP, ER, n) %>% arrange(ID, method)

cat("\nMedian GPP/ER by site and method:\n")
print(as.data.frame(summary.tbl))

write_csv(summary.tbl, "04_Outputs/Power Function RC/18_travel_time_correction_gps_reach_summary.csv")
write_csv(NEP.corrected, "04_Outputs/Power Function RC/18_two_station_gps_reach_corrected.csv")
