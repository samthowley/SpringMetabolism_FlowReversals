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

ggplot(df_tail, aes(discharge))+geom_histogram()+facet_wrap(~ID, scales='free')

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

#K600: LF, ID, GB#############

discharge <- read_csv("02_Clean_data/Chem/discharge.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID)%>%
  summarise(discharge=mean(discharge, na.rm = T))

library(readxl)
## 2026-10-06: was rC_K600_edited.xlsx, which no longer exists. Now reads the
## rating-curve K600 that 07_gasdome_K600.R produces. Three differences from the
## old file: its date column is "day" (already a Date, so no mdy()), it already
## carries an ID column (so no bind_rows(.id=)), and it has no "Vent DO" sheet.
sheet_names <- excel_sheets("04_Outputs/rC_k600.xlsx")

list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/rC_k600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}
## whole-day drops of physically implausible gas-dome K600 -- same tribble as
## 10_two_station.R, so both metabolism scripts clean the field data identically
k600_judgment_drop <- tribble(
  ~ID,   ~Date,
  "GB", as.Date("2022-10-24"), # K600 = 0
  "GB", as.Date("2022-11-07"), # 25.3, >20
  "GB", as.Date("2022-11-21"), # 22.3 / 22.2, >20
  "OS", as.Date("2022-10-31")  # all reps 8.4-8.9, 4-15x above the rest of OS
)

k600s <- bind_rows(list_of_ks)%>%
  rename(Date=day)%>%
  distinct(ID, k600_1.day, .keep_all = T)%>%
  select(Date, ID, k600_1.day)%>%
  mutate(Date=as.Date(Date))%>%
  ## cleaning (2026-10-06): these feed the binned-K600 PRIOR, so drop the values
  ## that cannot be real before taking the median. Unusable: NA, and <=0 (K600 is
  ## a rate; 0 also breaks log()). Then the judgment drops above.
  filter(!is.na(k600_1.day), k600_1.day > 0)%>%
  anti_join(k600_judgment_drop, by = c("ID", "Date"))

cat("\n== cleaned gas-dome K600 feeding the binned priors ==\n")
print(as.data.frame(k600s %>%
  summarise(n = n(), min = round(min(k600_1.day),2),
            median = round(median(k600_1.day),2),
            max = round(max(k600_1.day),2), .by = ID) %>% arrange(ID)))

prepped.k600s<-left_join(k600s, discharge)%>%
  mutate(discharge=if_else(discharge<0, NA, discharge))%>%
  filter(!is.na(discharge))

## one K600 calibration set per site now. Previously this was duplicated into
## ID_hi / ID_lo, which gave both halves IDENTICAL ln(Q) node centres and priors
## -- the only thing that differed was which half of the record was fitted.
Ks <- prepped.k600s

k_list <- Ks %>%
  group_by(ID) %>%
  group_split()

names(k_list) <- Ks %>%
  group_by(ID) %>%            # group_keys(ID) was removed in dplyr 1.0 -- group first
  group_keys() %>%
  pull(ID)

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- function(site) {
  
  # Define ln(Q) node centers spanning the discharge range (5 quantiles)
  lnQ_breaks <- quantile(log(site$discharge), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  K600_lnQ_nodes_centers <- lnQ_breaks  # ln(Q) centers for piecewise linear model [web:2]
  
  # Initial K600 values at nodes (log-normal priors): use median K600 across all data
  median_K600 <- median(site$k600_1.day, na.rm = TRUE)
  
  bayes_specs <- specs(
    bayes_name,
    K600_lnQ_nodes_centers = K600_lnQ_nodes_centers,
    K600_lnQ_nodes_meanlog = rep(log(median_K600), length(K600_lnQ_nodes_centers)),
    K600_lnQ_nodes_sdlog = rep(1.32, length(K600_lnQ_nodes_centers)),  # typical prior [web:2]
    K600_lnQ_nodediffs_sdlog = 0.5,  # smoothness between nodes [web:2]
    K600_daily_sigma_sigma = 0.24,
    burnin_steps = 1000, 
    saved_steps = 1000
  )
  
  return(bayes_specs)
}

k600.specs <- lapply(k_list, function(k600_df) {
  k600 <- k600_df %>%
    group_by(ID) %>%
    bayes_specs()

  return(k600)
})

#run the model####
valid_ids <- names(k600.specs)[!sapply(k600.specs, is.null)]
valid_streams <- rdy_for_sm[valid_ids]
valid_specs <- k600.specs[valid_ids]

metab_results_base <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)

met_list_base <- lapply(metab_results_base, function(metab_results) {
  prediction2 <- metab_results@fit$daily #%>%
  return(prediction2)
})

met_results_two <- bind_rows(met_list_base, .id = "ID")%>%select(date, ID, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ER_Rhat, K600_daily_Rhat)

write_csv(met_results_two, "04_Outputs/one station results/met_results_two.csv")

ggplot(met_results_two, aes(x = date)) +
  #geom_line(aes(y = K600))+
  geom_line(aes(y = GPP_daily_mean), color='green')+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')


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



#IU##########
library(dataRetrieval)


startDate <- "2021-04-03"
endDate <- "2024-02-06"
parameterCd <- c('00010','00300','00065')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)%>% 
  rename('Date'='dateTime', 'temp.water'='X_00010_00000', 'DO.obs'='X_00300_00000')%>%
  mutate(depth=X_00065_00000-13.72,
         min=minute(Date),
         DO.sat= Cs(temp.water), 
         solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         light=calc_light(solar.time,  29.8, -82.6) )%>% 
  filter(min==0) 

IU<-IU %>% select(DO.obs,depth,temp.water,DO.sat,solar.time,light)

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)
mm<- metab(bayes_specs, IU)
prediction2.IU <- mm@fit$daily 

IU.edit<-prediction2.IU%>%
  mutate(
    ID='IU'
  )%>%
  select(date, ID, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ER_Rhat, K600_daily_Rhat)

write_csv(IU.edit, "04_Outputs/one station results/IU.csv")

#organize#####





names(one.station)
one.station<-rbind(
  read_csv("04_Outputs/one station results/met_results_two.csv")%>%
  filter(ID!='OS'),
  read_csv("04_Outputs/one station results/IU.csv"),
  read_csv("04_Outputs/one station results/OS.csv")
)%>%
  filter(
    GPP_daily_mean>0, ER_daily_mean<0, ER_Rhat > 0.9 & ER_Rhat < 1.2,K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2)%>%
  rename(Date=date, GPP.1=GPP_daily_mean, ER.1=ER_daily_mean, K600.1=K600_daily_mean)%>%
  select(ID, Date, GPP.1, ER.1, K600.1)%>%
  arrange(ID, Date)

glimpse(one.station)


ggplot(one.station, aes(x = Date)) +
  #geom_line(aes(y = K600))+
  geom_line(aes(y = GPP.1), color='green')+
  geom_line(aes(y = ER.1), color='red')+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')

write_csv(one.station, "04_Outputs/one.station.metabolism.csv")





