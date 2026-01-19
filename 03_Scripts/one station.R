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

#split high and low periods#####

df_tail <- data %>%
  group_by(ID)%>%
  mutate(
    discharge=if_else(discharge<=0, 0.01, discharge),
    threshold= case_when(
      discharge >= mean(discharge, na.rm=T)~'hi',
      TRUE~'lo')
  )%>%
  mutate(ID_q = paste(ID, threshold, sep = "_"))

ggplot(df_tail, aes(discharge, color=threshold))+geom_histogram()+facet_wrap(~ID, scales='free')

#Prepare data for two station sites#######

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

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
  group_by(ID_q) %>%
  group_split()

names(split_list) <- input %>%
  group_by(ID_q) %>%
  group_keys() %>%
  pull(ID_q)

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
sheet_names <- excel_sheets("04_Outputs/rC_K600_edited.xlsx")

list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/rC_K600_edited.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}
k600s <- bind_rows(list_of_ks, .id = "ID")%>%
  distinct(ID, k600_1.day, .keep_all = T)%>% filter(ID != 'Vent DO')%>%
  select(Date, ID, k600_1.day)%>%
  mutate(Date=mdy(Date))

prepped.k600s<-left_join(k600s, discharge)%>%
  mutate(discharge=if_else(discharge<0, NA, discharge))%>%
  filter(!is.na(discharge))

K600.hi <- prepped.k600s%>%
  mutate(ID_q=paste(ID, 'hi', sep = "_"))

K600.lo <- prepped.k600s%>%
  mutate(ID_q=paste(ID, 'lo', sep = "_"))

Ks<-rbind(K600.hi, K600.lo)

k_list <- Ks %>%
  group_by(ID_q) %>%
  group_split()

names(k_list) <- Ks %>%
  group_keys(ID_q) %>%
  pull(ID_q)

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
    group_by(ID_q) %>%
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
file.names<-file.names[c(2,4,6,11)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
OS <- reduce(data, full_join, by = c("ID", 'Date'))%>%filter(ID=='OS')

input.OS <- OS %>%
  rename(DO.obs = DO) %>%
  arrange(Date)%>%
  mutate(
    lat=29.585,
    lon=-82.93,
    temp.water = fahrenheit.to.celsius(Temp),
    DO.sat     = Cs(temp.water),
    solar.time = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    light      = calc_light(solar.time, lat, lon)
  )%>%select(-lat, -lon, -Date, -ID, -Temp, -velocity, -K600_1.d_daily)

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
endDate <- "2024-08-06"
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
master.met<-rbind(met_results_two, OS.edit, IU.edit)%>%
  filter(
    GPP_daily_mean>0, ER_daily_mean<0, ER_Rhat > 0.9 & ER_Rhat < 1.2,K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2)%>%
  separate(
    ID, into = c("ID", "q_sep"), sep = "_")%>%
  select(
    date, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ID, -q_sep)%>%
  arrange(ID, date)%>%
  rename(
    GPP=GPP_daily_mean, ER=ER_daily_mean, K600=K600_daily_mean)

ggplot(master.met, aes(x = date)) +
  #geom_line(aes(y = K600))+
  geom_line(aes(y = GPP), color='green')+
  geom_line(aes(y = ER), color='red')+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')

write_csv(master.met, "04_Outputs/one.station.metabolism.csv")





