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


file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,3,4)]
var.data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})


file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(3,6)]
K.data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})


OS <- reduce(var.data, left_join, by = c("ID", 'Date'))%>%
  filter(ID=='OS',complete.cases(DO, depth))%>%
  distinct(ID, Date, .keep_all=T)%>%
  mutate(ln.Q=log(discharge),
         split_q=case_when(depth>mean(depth, na.rm=T)~'hi',
                           depth<=mean(depth, na.rm=T)~'lo'),
         ID_q = paste(ID, split_q, sep = "_"))%>%
  filter(depth>0)%>%
  rename('DO.obs'='DO')%>%
  mutate(
    temp.water=fahrenheit.to.celsius(Temp),
    DO.sat=Cs(temp.water),
    solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
    light=calc_light(solar.time,  29.8, -82.6))

OS.split_list <- OS %>%
  group_by(ID_q) %>%
  group_split()

names(OS.split_list) <- OS %>%
  group_by(ID_q) %>%
  group_keys() %>%
  pull(ID_q)


OS_for_sm <- lapply(OS.split_list, function(df) {
  samplingperiod <- data.frame(solar.time = seq(from = as.POSIXct(min(df$solar.time)),
                                                to = as.POSIXct(max(df$solar.time)),
                                                by = "hour"))
  
  df <- left_join(samplingperiod, df, by = "solar.time") %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(as.numeric(solar.time)) > 0)) %>%
    select(solar.time, light, depth, DO.sat, DO.obs, temp.water) %>%
    distinct(solar.time, .keep_all = TRUE)
  
  return(df)
})



OS_K <- reduce(K.data, left_join, by = c("ID", 'Date'))%>%filter(ID=='OS')

OS.K600.hi <- OS_K%>%
  mutate(ID_q=paste(ID, 'hi', sep = "_"))

OS.K600.lo <- OS_K%>%
  mutate(ID_q=paste(ID, 'lo', sep = "_"))

OS.Ks<-rbind(OS.K600.hi, OS.K600.lo)

k_list <- OS.Ks %>%
  group_by(ID_q) %>%
  group_split()

names(k_list) <- OS.Ks %>%
  group_keys(ID_q) %>%
  pull(ID_q)


k600_mean_list <- lapply(k_list, function(k600_df) {
  k600 <- k600_df %>%
    group_by(ID_q) %>%
    summarise(
      K600=mean(k600_1d,na.rm=T),
      sd_vals=sd(k600_1d,na.rm=T))
  
  return(k600)
})

specs <- lapply(k600_mean_list, function(K_means) {
  site_id <- K_means$ID[1]
  K_vals <- K_means$K600[[1]]
  sd_vals <- K_means$sd_vals[[1]]
  
  
  # Handle missing or NA values in K_vals
  if (all(is.na(K_vals))) {
    warning(paste("Skipping site", site_id, "- K_vals all NA"))
    return(NULL)
  }
  
  # Build specs
  bayes_name <- mm_name(type='bayes',
                        pool_K600='normal',
                        err_obs_iid=TRUE, err_proc_iid=TRUE)
  
  bayes_specs <- specs(bayes_name,
                       K600_daily_meanlog_meanlog= log(K_vals),
                       K600_daily_meanlog_sdlog=log(2),
                       GPP_daily_lower=0,
                       burnin_steps=1000,
                       saved_steps=1000)
})


valid_ids <- names(specs)[!sapply(specs, is.null)]
valid_streams <- OS_for_sm[valid_ids]
valid_specs <- specs[valid_ids]

# Run streamMetabolizer on each valid site##############
metab_results_base <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)

met_list_base <- lapply(metab_results_base, function(metab_results) {
  prediction2 <- metab_results@fit$daily #%>%
  return(prediction2)
})

met_results <- bind_rows(met_list_base, .id = "ID")%>%
  filter(
    GPP_daily_mean>0, ER_daily_mean<0, ER_Rhat > 0.9 & ER_Rhat < 1.2,K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2)%>%
  separate(
    ID, into = c("ID", "q_sep"), sep = "_")%>%
  select(
    date, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ID, -q_sep)%>%
  arrange(ID, date)%>%
  rename(
    GPP=GPP_daily_mean, ER=ER_daily_mean, K600=K600_daily_mean)

