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
  filter(depth > 0)%>%
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

rdy_for_sm<- lapply(split_list, function(df) {
  df<-df %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(solar.time) > 0))%>%
    select(solar.time, light, depth, DO.sat, DO.obs, temp.water)
  
  df<-left_join(samplingperiod, df)
})

#K600#############


bayes.specs<-bins(GB_K600)

bayes_specs <- function(site) {
  
  breaks <- quantile(site_positive$Q_m.s, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  site <- site %>%
    mutate(bin = cut(Q_m.s, breaks = breaks, include.lowest = TRUE, labels = FALSE))
  
  summary <- site%>%
    group_by(bin) %>%
    summarise(
      Q_mean = mean(Q_m.s, na.rm = TRUE),
      K_mean = mean(K600_1d, na.rm = TRUE)
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


mm <- metab(bayes.specs, data=site1)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                       GPP_Rhat,ER_Rhat,K600_daily_Rhat)
