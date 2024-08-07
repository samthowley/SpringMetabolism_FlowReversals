rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(grid)
library(weathermetrics)
library('StreamMetabolism')
library("hydroTSM")
library(imputeTS)
library(streamMetabolizer)
library(dataRetrieval)
library(lme4)

#functions####
bins<- function(site) {
  site<- site %>% mutate(Q_m.s=abs(Q_m.s), K600_1d=abs(K600_1d))

  IQR<-quantile(site$Q_m.s, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
  bin<-filter(site, Q_m.s<=IQR[1])
  (Q<-mean(bin$Q_m.s))
  (K<-mean(bin$K600_1d))

  bin2<-filter(site, Q_m.s<=IQR[2])
  (Q2<-mean(bin2$Q_m.s))
  (K2<-mean(bin2$K600_1d))

  bin3<-filter(site, Q_m.s<=IQR[3])
  (Q3<-mean(bin3$Q_m.s))
  (K3<-mean(bin3$K600_1d, na.rm=T))

  bin4<-filter(site, Q_m.s>=IQR[4])
  (Q4<-mean(bin4$Q_m.s))
  (K4<-mean(bin4$K600_1d, na.rm=T))

  bin5<-filter(site, Q_m.s>=IQR[5])
  (Q5<-mean(bin5$Q_m.s))
  (K5<-mean(bin5$K600_1d, na.rm=T))

  bayes_specs <- specs(bayes_name,
                       K600_lnQ_nodes_centers = c(Q,Q2,Q3,Q4),
                       K600_lnQ_nodes_meanlog= log(c(K,K2,K3,K4)),
                       K600_lnQ_nodes_sdlog= 0.1,
                       K600_lnQ_nodediffs_sdlog = 0.05,
                       K600_daily_sigma_sigma= 0.24,
                       burnin_steps=1000, saved_steps=1000)

  return(bayes_specs)}
metabolism <- function(site) {

  site<- site %>% mutate(Q_m.s=abs(Q_m.s), K600_1d=abs(K600_1d))

  samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct(min(site$Date)),
                                              to=as.POSIXct(max(site$Date)),by="hour")))
  site<-left_join(samplingperiod, site, by=c('Date'), relationship ="many-to-many")

  site$Temp[site$Temp < 65] <- mean(site$Temp, na.rm=T)
  site$Temp[site$Temp > 80] <- mean(site$Temp, na.rm=T)

  site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp)

  site<-rename(site,'DO.obs'='DO','temp.water'='Mouth_Temp_C','discharge'="Q_m.s")
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")

  y<-c("DO.obs","depth",'discharge',"temp.water", "DO.sat","solar.time","light" )

  site1<-site[,y]
  mm <- metab(bayes_specs, data=site1)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                         GPP_Rhat,ER_Rhat,K600_daily_Rhat)
  prediction2<- prediction2 %>% filter(GPP_Rhat> 0.9 & GPP_Rhat<1.05)%>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)

  return(prediction2)
}
compile<- function(site_output, site2) {
  site_output<-rename(site_output,'Date'='date','GPPavg'="GPP_daily_mean",
                      'ER'='ER_daily_mean','K600_1d'='K600_daily_mean')
  site_output$GPPavg[site_output$GPPavg<0] <- 0
  site_output$ER[site_output$ER>0] <- -3


  x<-c('Date', 'ER','GPPavg', 'K600_1d')
  site2<-site2[,x]
  site<-rbind(site2, site_output)
  names(site2)
  names(site_output)
  site$NEP<-site$GPPavg+site$ER
  return(site)}
notparsed_metabolism<- function(site) {
  keep<-c('Date', 'DO', 'depth', 'Temp', 'K600_1d', 'Q_m.s')
  site<-site[,keep]

  site<- site %>%mutate(Q_m.s=abs(Q_m.s), K600_1d=abs(K600_1d))

  bayes_specs<-bins(site)

  samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct(min(site$Date, na.rm=T)),
                                              to=as.POSIXct(max(site$Date,na.rm=T)),by="hour")))
  site<-left_join(samplingperiod, site)
  site <- site[!duplicated(site[c('Date')]),]
  site$Temp[site$Temp < 65] <- mean(site$Temp, na.rm=T)
  site$Temp[site$Temp > 80] <- mean(site$Temp, na.rm=T)

  site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp)

  site<-rename(site,'DO.obs'='DO','temp.water'='Mouth_Temp_C','discharge'="Q_m.s")
  site$DO.sat<-Cs(site$temp.water)
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site$light<-calc_light(site$solar.time,  29.8, -82.6)

  site<-site[,-c(1)]
  y<-c("DO.obs","depth",'discharge',"temp.water", "DO.sat","solar.time","light")
  site<-site[,y]
  #site <- site[complete.cases(site), ]

  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                         GPP_Rhat,ER_Rhat,K600_daily_Rhat,GPP_sd,GPP_97.5pct,GPP_2.5pct)
  
  site_output<- prediction2 %>% filter(GPP_Rhat> 0.9 & GPP_Rhat<1.05 | ER_Rhat> 0.9 & ER_Rhat<1.05 |
                                         K600_daily_Rhat> 0.9 & K600_daily_Rhat<1.05) %>% 
    select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,GPP_sd,GPP_97.5pct,GPP_2.5pct)
  
  
  site_output<-rename(prediction2,'Date'='date','GPPavg'="GPP_daily_mean",
                      'ER'='ER_daily_mean','K600_1d'='K600_daily_mean')
  site_output$GPPavg[site_output$GPPavg<0] <- 0
  site_output$ER[site_output$ER>0] <- -3
  
  site_output$NEP<-site_output$GPPavg+site_output$ER
  
  return(site_output)}

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)
options(mc.cores=6)

###GB######
GB_input <- read_csv("04_Outputs/one station inputs/GB.csv")
bayes_specs<-bins(GB_input)
GB_output<-metabolism(GB_input)
write_csv(GB_output, "04_Outputs/one station outputs/GB.csv")

GB_output<-read_csv("04_Outputs/one station outputs/GB.csv")
GB2 <- read_csv("04_Outputs/two station results/GB.csv")
GB<-compile(GB_output, GB2)
GB$ID<-'GB'
write_csv(GB, "04_Outputs/Stream metabolizer results/GB.csv")

#all one station
GB_input <- read_csv("04_Outputs/one station inputs/not parsed/GB.csv")
GB_output<-notparsed_metabolism(GB_input)
GB_output$ID<-'GB'
write_csv(GB_output, "04_Outputs/Stream metabolizer results/not parsed/GB.csv")


###OS######
OS_input <- read_csv("04_Outputs/one station inputs/OS.csv")
OS_input$light<-calc_light(OS_input$Date,  29.585, -82.937)
OS_output<-notparsed_metabolism(OS_input)

OS_output$ID<-'OS'
OS_output<-OS_output %>%mutate(NEP=GPPavg+ER)%>% select(Date, ER, GPPavg, K600_1d, NEP, ID)

write_csv(OS_output, "04_Outputs/Stream metabolizer results/not parsed/OS.csv")
write_csv(OS_output, "04_Outputs/Stream metabolizer results/OS.csv")

###AM######
AM_input <- read_csv("04_Outputs/one station inputs/AM.csv")
bayes_specs<-bins(AM_input)
AM_onestat<-metabolism(AM_input)
write_csv(AM_onestat, "04_Outputs/one station outputs/AM.csv")

AM_onestat <- read_csv("04_Outputs/one station outputs/AM.csv")
AM2 <- read_csv("04_Outputs/two station results/AM.csv")
AM<-compile(AM_onestat, AM2)
AM$ID<-'AM'
write_csv(AM, "04_Outputs/Stream metabolizer results/AM.csv")

AM_input <- read_csv("04_Outputs/one station inputs/not parsed/AM.csv")
AM_output<-notparsed_metabolism(AM_input)
AM_output$ID<-'AM'
write_csv(AM_output, "04_Outputs/Stream metabolizer results/not parsed/AM.csv")

###LF######
LF_input <- read_csv("04_Outputs/one station inputs/LF.csv")
bayes_specs<-bins(LF_input)
LF_output<-metabolism(LF_input)
write_csv(LF_output, "04_Outputs/one station outputs/LF.csv")

LF_output<-read_csv("04_Outputs/one station outputs/LF.csv")
LF2 <- read_csv("04_Outputs/two station results/LF.csv")
LF<-compile(LF_output, LF2)
LF$ID<-'LF'

ggplot(data=LF, aes(x=Date)) +geom_line(aes(y=GPPavg), size=1, color='darkgreen')
write_csv(LF, "04_Outputs/Stream metabolizer results/LF.csv")

#one station
LF_input <- read_csv("04_Outputs/one station inputs/not parsed/LF.csv")
LF_output<-notparsed_metabolism(LF_input)
LF_output$ID<-'LF'
write_csv(LF_output, "04_Outputs/Stream metabolizer results/not parsed/LF.csv")

##ID#######
ID_input <- read_csv("04_Outputs/one station inputs/ID.csv")
bayes_specs<-bins(ID_input)
ID_one<-metabolism(ID_input)
write_csv(ID_one, "04_Outputs/one station outputs/ID.csv")


ID_output<-read_csv("04_Outputs/one station outputs/ID.csv")
ID2 <- read_csv("04_Outputs/two station results/ID.csv")
ID<-compile(ID_output, ID2)
ID$ID<-'ID'

ggplot(data=ID, aes(x=Date)) +geom_line(aes(y=GPPavg), size=1, color='darkgreen')
write_csv(ID, "04_Outputs/Stream metabolizer results/ID.csv")

ID_input <- read_csv("04_Outputs/one station inputs/not parsed/ID.csv")
ID_output<-notparsed_metabolism(ID_input)
ID_output$ID<-'ID'
write_csv(ID_output, "04_Outputs/Stream metabolizer results/not parsed/ID.csv")

###IU#######
startDate <- "2024-06-17"
endDate <- "2024-07-25"
parameterCd <- c('00010','00300','00065')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime', 'temp.water'='X_00010_00000', 'DO.obs'='X_00300_00000')%>%
  mutate(depth=X_00065_00000-13.72)%>%
  mutate(min=minute(Date)) %>% filter(min==0) %>%
  mutate(DO.sat= Cs(temp.water), solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         light=calc_light(solar.time,  29.8, -82.6) )

y<-c("DO.obs","depth","temp.water", "DO.sat","solar.time","light" )
IU<-IU[,y]

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)
mm<- metab(bayes_specs, IU)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                       GPP_Rhat,ER_Rhat,K600_daily_Rhat,GPP_sd,GPP_97.5pct,GPP_2.5pct)
IU<- prediction2 %>% filter(GPP_Rhat> 0.9 & GPP_Rhat<1.05 | ER_Rhat> 0.9 & ER_Rhat<1.05 |
                                       K600_daily_Rhat> 0.9 & K600_daily_Rhat<1.05) 
write_csv(IU, "04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")


IU<- read_csv("04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")
IU<-IU %>%rename("GPPavg"='GPP_daily_mean', 'ER'='ER_daily_mean', 
       'K600_1d'='K600_daily_mean', 'Date'='date') %>%  
  mutate(NEP= GPPavg+ER)
IU$ID<-'IU'

#IU<-IU[,c(1,2,3,4,6,5)]
write_csv(IU, "04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")

file.names <- list.files(path="04_Outputs/Stream metabolizer results/not parsed/IU", pattern=".csv", full.names=TRUE)
IU_all<-data.frame()
for(fil in file.names){
  site <- read_csv(fil)
  IU_all<-rbind(IU_all,site)}
IU_all<-IU_all %>% select(Date, ER, GPPavg, K600_1d, NEP, ID)


write_csv(IU_all, "04_Outputs/Stream metabolizer results/IU.csv")
write_csv(IU_all, "04_Outputs/Stream metabolizer results/not parsed/IU.csv")


names(IU_all)
names(OS_all)
############################
#Curate Datasets#####
########################

#compile two station####
file.names <- list.files(path="04_Outputs/Stream metabolizer results", pattern=".csv", full.names=TRUE)
master_parsed <- data.frame()
for(fil in file.names){
  site <- read_csv(fil)
  master_parsed<-rbind(master_parsed,site)}
write_csv(master_parsed, "04_Outputs/master_metabolizer_parsed.csv")

#compile one station####
file.names <- list.files(path="04_Outputs/Stream Metabolizer results/not parsed", pattern=".csv", full.names=TRUE)
master_notparsed <- data.frame()
for(fil in file.names){
  site <- read_csv(fil)
  site<- site %>% select(Date,ER,GPPavg,K600_1d,NEP,ID)
  master_notparsed<-rbind(master_notparsed,site)}

write_csv(master_notparsed, "04_Outputs/master_metabolizer_onestation.csv")

#curate master dataset####
onestation<- read_csv("04_Outputs/master_metabolizer_onestation.csv")
onestation<-onestation %>%select(Date, ID, GPPavg, ER, K600_1d)%>%rename('GPP_1'='GPPavg', 'ER_1'='ER', 'K600_1'='K600_1d')

twostation<- read_csv("04_Outputs/master_metabolizer_parsed.csv")
twostation<-twostation %>%rename('GPP_2'='GPPavg', 'ER_2'='ER', 'K600_2'='K600_1d')

metabolism<-left_join( onestation, twostation, by=c('ID', 'Date'))
metabolism$GPP_2 <- ifelse(is.na(metabolism$GPP_2), metabolism$GPP_1, metabolism$GPP_2)
metabolism$ER_2 <- ifelse(is.na(metabolism$ER_2), metabolism$ER_1, metabolism$ER_2)

metabolism<-metabolism %>% mutate(GPP=(GPP_1+GPP_2)/2, ER=(ER_1+ER_2)/2,
                                  day=day(Date), month=month(Date), year=year(Date))

depth<-read_csv('02_Clean_data/master_depth2.csv')

metabolism<-left_join(metabolism, depth, by=c('Date', 'ID'))

write_csv(metabolism, "02_Clean_data/master_metabolism4.csv")

