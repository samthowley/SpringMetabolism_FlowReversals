rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# Is AM/LF's weak nighttime ER a K600 problem at all, or is the raw
# pre-K600 signal (change.DO.flux -- doesn't involve K600 or depth) already
# too weak/positive at night, independent of any K600 choice? If the raw
# signal itself is the problem, no K600 value can fix it within this
# deterministic linear-subtraction formula.

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area<-left_join(width, length_tbl)%>% mutate(area=w*m)%>%
  mutate(m=if_else(ID=='AM', 800, m))

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,4,6,12)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  left_join(area)

VentDO <- read_csv("02_Clean_data/Chem/VentDO_all.csv")

master<-
  full_join(master, VentDO, by=c('ID', 'Date'))%>%
  arrange(ID, Date)%>%
  group_by(ID)%>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction= "downup")%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  distinct(ID, Date, .keep_all = T)

discharge<-master%>%
  mutate(discharge=w*depth*velocity*86400)

# raw pre-K600 signal -- does NOT involve K600 or depth at all
change.DO.flux<-discharge%>%
  mutate(change.DO.flux=((DO-VentDO)*discharge)/area)

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

classified <- change.DO.flux %>%
  left_join(lat.lon, by='ID') %>%
  mutate(
    solar.time.raw = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    travel.time.hr = if_else(velocity>0, (m/velocity)/3600, NA_real_),
    solar.time.corrected = solar.time.raw - (travel.time.hr/2)*3600,
    light.corrected = calc_light(solar.time.corrected, lat, lon),
    time = if_else(light.corrected>0, 'day', 'night')
  ) %>%
  filter(!is.na(time), !is.na(change.DO.flux), velocity>0)

cat("=== Raw change.DO.flux at night (K600-independent), by site ===\n")
classified %>% filter(time=='night') %>%
  group_by(ID) %>%
  summarise(
    n = n(),
    pct_positive_or_zero = round(100*mean(change.DO.flux >= 0), 1),
    median = round(median(change.DO.flux), 3),
    q25 = round(quantile(change.DO.flux, 0.25), 3),
    q75 = round(quantile(change.DO.flux, 0.75), 3),
    .groups='drop'
  ) %>% print()

cat("\n=== For comparison, raw change.DO.flux during the DAY, by site ===\n")
classified %>% filter(time=='day') %>%
  group_by(ID) %>%
  summarise(
    n = n(),
    median = round(median(change.DO.flux), 3),
    q25 = round(quantile(change.DO.flux, 0.25), 3),
    q75 = round(quantile(change.DO.flux, 0.75), 3),
    .groups='drop'
  ) %>% print()

write_csv(classified %>% select(ID, Date, solar.time.corrected, time, change.DO.flux, depth, velocity),
          "04_Outputs/Power Function RC/20_raw_nighttime_signal.csv")
