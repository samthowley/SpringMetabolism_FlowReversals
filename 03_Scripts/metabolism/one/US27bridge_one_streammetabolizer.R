rm(list=ls())


library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library('StreamMetabolism')
library("hydroTSM")
library(ggnewscale)
library(streamMetabolizer)
library(tidyverse)
library(dataRetrieval)

startDate <- "2022-07-12"
endDate <- "2023-08-21"
parameterCd <- c('00010','00300','00065', '00060')
vent<-'02322700'

Ichetucknee<-readNWISuv(vent,parameterCd,startDate,endDate)
Ichetucknee<- Ichetucknee %>% mutate(minute = minute(dateTime))
Ichetucknee<-filter(Ichetucknee, minute==0)

names(Ichetucknee)
bridge<-Ichetucknee[,c(3,4,6,8,10)]
bridge<-rename(bridge, 'Date'="dateTime", 
               'temp.water'="X_00010_00000",
               "depth"="X_00065_00000",
               "DO.obs"="X_00300_00000")

bridge$depth<-conv_unit(bridge$depth, "ft", "m")
bridge$depth<-bridge$depth-(max(bridge$depth, na.rm=T)-2.5)
range(bridge$depth, na.rm=T)
bridge$DO.sat<-Cs(bridge$temp.water)

bridge$solar.time <-as.POSIXct(bridge$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
bridge<-bridge[,-c(1)]
bridge$light<-calc_light(bridge$solar.time,  29.8, -82.8)

x<-c("DO.obs", "DO.sat", "depth", "temp.water","solar.time","light")
bridge<-bridge[,x]



bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, 
                     K600_daily_meanlog_meanlog=0.1, 
                     K600_daily_meanlog_sdlog=0.001, 
                     GPP_daily_lower=0, 
                     burnin_steps=1000, 
                     saved_steps=1000)

mm <- metab(bayes_specs, data=bridge)



predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

names(prediction2)
ggplot(prediction2, aes(x=date)) + 
  geom_line(aes(y=GPP_daily_mean, color="GPP"),size=1)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Ichetucknee/DONE/US27bridge_streamMetabol.xlsx")



samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv")
samplingperiod<-filter(samplingperiod, Date>"2022-06-30")
samplingperiod<-filter(samplingperiod, Date<"2023-08-21")
samplingperiod$days <- as.Date(samplingperiod$Date)
samplingperiod.day <- aggregate(samplingperiod, by=list(samplingperiod$days), FUN='min')

prediction2<-rename(prediction2, 
                  'Date'='date',
                  'GPPavg'="GPP_daily_mean",
                  'ER'='ER_daily_mean',
                  'K600_avg'='K600_daily_mean')

prediction2$NEP<-prediction2$GPPavg+prediction2$ER


bridge<-readNWISuv(vent,parameterCd,startDate,endDate)
bridge<- bridge %>% mutate(minute = minute(dateTime))
bridge<-filter(bridge, minute==0)

y<-c('dateTime', 'X_00010_00000','X_00065_00000', 'X_00060_00000', 'X_00300_00000')
bridge<-bridge[,y]
bridge<-rename(bridge, 'Date'="dateTime", 
               'temp.water'="X_00010_00000",
               "depth"="X_00065_00000",
               "discharge"='X_00060_00000',
               "DO.obs"="X_00300_00000")

bridge<- bridge %>% mutate(day = day(Date),
                           month= month(Date),
                           year=year(Date))

prediction2<- prediction2 %>% mutate(day = day(Date),
                           month= month(Date),
                           year=year(Date))
prediction2<-prediction2[,-c(1)]


Ichetucknee<-left_join(prediction2, bridge, by=c('day','month','year'))

NEPmin<-min(Ichetucknee$NEP, na.rm = T)
NEPmax<-max(Ichetucknee$NEP, na.rm = T)

GPPmin<-min(Ichetucknee$GPPavg, na.rm = T)
GPPmax<-max(Ichetucknee$GPPavg, na.rm = T)

ERmin<-min(Ichetucknee$ER, na.rm = T)
ERmax<-max(Ichetucknee$ER, na.rm = T)

Ichetucknee$NEPnorm<- (Ichetucknee$NEP-NEPmin)/(NEPmax-NEPmin)
Ichetucknee$GPPnorm<- (Ichetucknee$GPPavg-GPPmin)/(GPPmax-GPPmin)
Ichetucknee$ERnorm<- (Ichetucknee$ER-ERmin)/(ERmax-ERmin)

names(Ichetucknee)
ggplot(Ichetucknee, aes(x=Date)) + 
  geom_line(aes(y=GPPavg, color="GPP"),size=1) + #make transparent cloud
 geom_line(aes(y=ER, color="ER"),size=1) +
  geom_line(aes(y=NEP, color="NEP"),size=1)

write_xlsx(Ichetucknee, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")

