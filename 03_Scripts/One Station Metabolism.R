rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(grid)
library(weathermetrics)
library('StreamMetabolism')
library("hydroTSM")
library(mmand)
library(imputeTS)
library(streamMetabolizer)
library(dataRetrieval)


samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2021-03-29 00:00", tz="UTC"),
                                            to=as.POSIXct("2024-02-05 00:00", tz="UTC"),by="hour")))
samplingperiod<-samplingperiod %>% mutate(hr=hour(Date),day=day(Date),mnth=month(Date),yr=year(Date))


metabolism <- function(samplingperiod,site) {
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
  y<-c("DO.obs","depth",'discharge',"temp.water", "DO.sat","solar.time","light" )
  site<-site[,y]
  #site <- site[complete.cases(site), ]

  mm <- metab(bayes_specs, data=site)
  prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
  return(prediction2)
}
bins<- function(site) {
  IQR<-quantile(site$Q_m.s, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
  bin<-filter(site, Q_m.s<=IQR[1])
  (Q_m.s<-mean(bin$Q_m.s))
  (K<-mean(bin$K600_avg))

  bin2<-filter(site, Q_m.s<=IQR[2])
  (Q_m.s2<-mean(bin2$Q_m.s))
  (K2<-mean(bin2$K600_avg))

  bin3<-filter(site, Q_m.s<=IQR[3])
  (Q_m.s3<-mean(bin3$Q_m.s))
  (K3<-mean(bin3$K600_avg, na.rm=T))

  bin4<-filter(site, Q_m.s>=IQR[4])
  (Q_m.s4<-mean(bin4$Q_m.s))
  (K4<-mean(bin4$K600_avg, na.rm=T))

  bin5<-filter(site, Q_m.s>=IQR[5])
  (Q_m.s5<-mean(bin5$Q_m.s))
  (K5<-mean(bin5$K600_avg, na.rm=T))

  bayes_specs <- specs(bayes_name,
                       K600_lnQ_nodes_centers = c(Q_m.s,Q_m.s2,Q_m.s3,Q_m.s4,Q_m.s5),
                       K600_lnQ_nodes_meanlog= log(c(K,K2,K3,K4,K5)),
                       K600_lnQ_nodes_sdlog= 0.1,
                       K600_lnQ_nodediffs_sdlog = 0.05,
                       K600_daily_sigma_sigma= 0.24,
                       burnin_steps=1000, saved_steps=1000)

  return(bayes_specs)}
compile<- function(site_output, site2) {
  site_output<-rename(site_output,'Date'='date','GPPavg'="GPP_daily_mean",
                      'ER'='ER_daily_mean','K600_1d'='K600_daily_mean')
  site_output$GPPavg[site_output$GPPavg<0] <- 0

  x<-c('Date', 'ER','GPPavg', 'K600_1d')
  site2<-site2[,x]
  site<-rbind(site2, site_output)
  site$NEP<-site$GPPavg+site$ER
  return(site)}

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)

###LF######
LF_input <- read_csv("04_Outputs/one_station_inputs/LF.csv")
bayes_specs<-bins(LF_input)
LF_output<-metabolism(samplingperiod,LF_input)
LF2 <- read_csv("04_Outputs/two_station/LF.csv")
LF<-compile(LF_output, LF2)
LF$ID<-'LF'

###post#####
LF<-filter(master_metabolism, ID=='LF')
ggplot(LF, aes(Date, GPPavg))+geom_line()
LF<- LF %>% filter(ER< -5) %>% filter(GPPavg <27)

###GB######
GB_input <- read_csv("04_Outputs/one_station_inputs/GB.csv")
bayes_specs<-bins(GB_input)
GB_output<-metabolism(samplingperiod,GB_input)
GB2 <- read_csv("04_Outputs/two_station/GB.csv")
GB<-compile(GB_output, GB2)
GB$ID<-'GB'

ggplot(GB, aes(Date, ER))+geom_line()

###OS######
OS_input <- read_csv("04_Outputs/one_station_inputs/OS.csv")
OS_input<-rename(OS_input, `Q_m.s`="Q_m/s", 'K600_avg'='K600_1d')
bayes_specs<-bins(OS_input)
OS<-metabolism(samplingperiod,OS_input)
OS<-rename(OS,'Date'='date','GPPavg'="GPP_daily_mean",
                    'ER'='ER_daily_mean','K600_1d'='K600_daily_mean')
OS$GPPavg[OS$GPPavg<0] <- 0
OS$NEP<-OS$GPPavg+OS$ER
OS$ID<-'OS'
ggplot(OS, aes(Date, ER))+geom_line()

###AM######
AM_input <- read_csv("04_Outputs/one_station_inputs/AM.csv")
bayes_specs<-bins(AM_input)
AM_output<-metabolism(samplingperiod,AM_input)

AM2 <- read_csv("04_Outputs/two_station/AM.csv")
AM<-compile(AM_output, AM2)
AM$ID<-'AM'
ggplot(AM, aes(Date, ER))+geom_line()

##ID#######
ID <- read_csv("04_Outputs/two_station/ID.csv")
x<-c('Date', 'ER','GPPavg', 'K600_1d')
ID<-ID[,x]
ID$NEP<-ID$GPPavg+ID$ER
ID$ID<-'ID'

###IU#######
startDate <- "2022-05-12"
endDate <- "2024-02-05"
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
IU <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)

IU<-rename(IU,'Date'='date','GPPavg'="GPP_daily_mean",
                    'ER'='ER_daily_mean','K600_1d'='K600_daily_mean')
IU$GPPavg[IU$GPPavg<0] <- 0

x<-c('Date', 'ER','GPPavg', 'K600_1d')
IU<-IU[,x]
IU$NEP<-IU$GPPavg+IU$ER
IU$ID<-'IU'

ggplot(IU, aes(Date)) +
  geom_point(aes(y=GPPavg,color='GPP'))


# master_metabolism<-read_csv("02_Clean_data/master_metabolism.csv")
# master_metabolism<-rbind(master_metabolism, IU)
# write_csv(master_metabolism, "02_Clean_data/master_metabolism.csv")
######
master_metabolism<-rbind(AM, OS, LF,GB, ID, IU)
write_csv(master_metabolism, "02_Clean_data/master_metabolism3.csv")

master_metabolism<-read_csv("02_Clean_data/master_metabolism3.csv")
master_metabolism<-master_metabolism %>%filter(ID !='LF') %>%
 rbind(LF)

master<-read_csv("02_Clean_data/master_depth2.csv")
master_all<-left_join(master_metabolism,master, by=c('ID','Date'))
master_all <- master_all[!duplicated(master_all[c('Date','ID')]),]
master_all$GPPavg[master_all$GPPavg>30] <- 30

ggplot(master_all, aes(Date)) +
  geom_line(aes(y=GPPavg,color='GPP'))+
 facet_wrap(~ ID, ncol=2)


write_csv(master_all, "02_Clean_data/master_met4.csv")
