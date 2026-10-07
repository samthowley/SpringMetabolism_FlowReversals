rm(list=ls())

library(tidyverse)
library(writexl)
library(grid)
library(weathermetrics)
library('StreamMetabolism')
library("hydroTSM")
library(imputeTS)
library(streamMetabolizer)
library(dataRetrieval)

#call in data for two station sites####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(4, 2, 3, 6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%select(-min)
data <- master[!duplicated(master[c('Date','ID')]),]

df_tail <- data %>%
  filter(ID != "OS")%>%
  group_by(ID)%>%
  mutate(
    discharge=if_else(discharge<=0, 0.01, discharge)   # keep: log(Q) needs > 0
  )%>%
  ungroup()


#Prepare data for two station sites#######

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID', 'OS'),
  lat = c(30.155, 29.585, 29.83, 29.93, 29.6448),
  lon = c(-83.238, -82.93, -82.68, -82.8, -82.9428))

input <- df_tail %>%
  left_join(lat.lon, by = "ID") %>%   # join coords by ID [web:93]
  rename(DO.obs = DO) %>%
  mutate(
    temp.water = fahrenheit.to.celsius(Temp),
    DO.sat     = Cs(temp.water),
    solar.time = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    light      = calc_light(solar.time, lat, lon)
  )

split_list <- input %>%
  group_by(ID) %>%
  group_split()

names(split_list) <- input %>%
  group_by(ID) %>%
  group_keys() %>%
  pull(ID)

rdy_for_sm <- lapply(split_list, function(df) {
  samplingperiod <- data.frame(solar.time = seq(from = as.POSIXct(min(df$solar.time)),
                                                to = as.POSIXct(max(df$solar.time)),
                                                by = "hour"))
  
  df <- left_join(samplingperiod, df, by = "solar.time") %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(as.numeric(solar.time)) > 0)) %>%
    select(solar.time, light, depth, discharge, DO.sat, DO.obs, temp.water) %>%
    distinct(solar.time, .keep_all = TRUE)
  
  return(df)
})

#OS########
(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
## 2026-10-06: was file.names[c(2,4,6,11)], which resolved to SpC.csv, not
## velocity.csv -- index 11 shifted when raw.depth.csv was added to the folder,
## so the select(-velocity) below errored. Named explicitly now.
file.names <- file.path("02_Clean_data/Chem", c("depth.csv","DO.csv","K600.csv","velocity.csv"))
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
OS <- reduce(data, full_join, by = c("ID", 'Date'))%>%filter(ID=='OS')

input.OS <- OS %>%
  rename(DO.obs = DO) %>%
  arrange(Date)%>%
  mutate(
    lat=29.6448,   # was 29.585 -- that is LF. OS coords from the KMZ.
    lon=-82.9428,
    temp.water = fahrenheit.to.celsius(Temp),
    DO.sat     = Cs(temp.water),
    solar.time = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    light      = calc_light(solar.time, lat, lon)
  )%>%
  ## 2026-10-06: positive select, not a negative one. Dropping named columns
  ## let DO.csv's `source_file` and `remove` through, and metab() rejects any
  ## column it does not expect ("data should omit these extra columns"). Listing
  ## what the model needs is also immune to new columns appearing upstream.
  ## pool_K600='normal' needs no discharge. Same column set as the IU block below.
  select(solar.time, DO.obs, DO.sat, depth, temp.water, light)

k600.OS <- k600s %>%
  filter(ID == "OS", !is.na(k600_1.day)) %>%
  pull(k600_1.day) %>%
  mean(na.rm = TRUE)

bayes_name.OS <- mm_name(type='bayes',
                         pool_K600='normal',
                         err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs.OS <- specs(bayes_name.OS,
                        K600_daily_meanlog_meanlog= log(k600.OS),
                        K600_daily_meanlog_sdlog=log(2),
                        GPP_daily_lower=0,
                        burnin_steps=1000,
                        saved_steps=1000)

mm<-metab(bayes_specs.OS, data = input.OS)
prediction2.OS <- mm@fit$daily

OS.edit<-prediction2.OS%>%
  mutate(
    ID='OS'
  )%>%
  select(date, ID, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ER_Rhat, K600_daily_Rhat)

write_csv(OS.edit, "04_Outputs/one station results/OS.csv")