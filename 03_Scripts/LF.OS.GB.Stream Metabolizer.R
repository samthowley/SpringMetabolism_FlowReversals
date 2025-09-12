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

#Two Station: ID, LF, GB###########
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,3,4)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

merged_data <- reduce(data, left_join, by = c("ID", 'Date'))%>%
  filter(complete.cases(DO, depth))%>%
  distinct(ID, Date, .keep_all=T)%>%
  mutate(ln.Q=log(discharge))%>%
  group_by(ID)%>%
  mutate(split_q=case_when(depth>mean(depth, na.rm=T)~'hi',
                           depth<=mean(depth, na.rm=T)~'lo'))%>%
  mutate(ID_q = paste(ID, split_q, sep = "_"))


input <- merged_data %>%
  filter(depth > 0, !ID=='OS')%>%
  rename('DO.obs'='DO')%>%
  mutate(
    temp.water=fahrenheit.to.celsius(Temp))%>%
  mutate(
    DO.sat=Cs(temp.water),
    solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
  )%>%
  mutate(
    light=calc_light(solar.time,  29.8, -82.6))
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

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(3,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

K600_data <- reduce(data, left_join, by = c("ID", 'Date'))%>%filter(!ID=='OS')

K600.hi <- K600_data%>%
  mutate(ID_q=paste(ID, 'hi', sep = "_"))

K600.lo <- K600_data%>%
  mutate(ID_q=paste(ID, 'lo', sep = "_"))

Ks<-rbind(K600.hi, K600.lo)

k_list <- Ks %>%
  group_by(ID_q) %>%
  group_split()

names(k_list) <- Ks %>%
  group_keys(ID_q) %>%
  pull(ID_q)



bayes_specs <- function(site) {
  
  breaks <- unique(quantile(site$discharge, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
  
  site <- site %>%
    mutate(bin = cut(discharge, breaks = breaks, include.lowest = TRUE, labels = FALSE))
  
  summary <- site%>%
    group_by(bin) %>%
    summarise(
      Q_mean = mean(discharge, na.rm = TRUE),
      K_mean = mean(k600_1d, na.rm = TRUE)
    ) %>%
    arrange(bin)
  
  Q_vals <- summary$Q_mean
  K_vals <- summary$K_mean
  

  bayes_specs <- specs(bayes_name,
                       K600_lnQ_nodes_centers = Q_vals,
                       K600_lnQ_nodes_meanlog = log(K_vals),
                       K600_lnQ_nodes_sdlog = 0.1,
                       K600_lnQ_nodediffs_sdlog = 0.05,
                       K600_daily_sigma_sigma = 0.24,
                       burnin_steps = 1000, saved_steps = 1000)
  
  return(bayes_specs)
}
bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)

k600.specs <- lapply(k_list, function(k600_df) {
  k600 <- k600_df %>%
    group_by(ID_q) %>%
    bayes_specs()
    
  return(k600)
})

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

write_csv(met_results, "04_Output/master_metabolism.csv")
