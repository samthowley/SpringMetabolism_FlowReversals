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

width <-20

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry")

Ichetucknee <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/Ichetucknee.xlsx")
x<-c("Date", "DO", "Temp.x", "stage_avg")
Ichetucknee<-Ichetucknee[,x]

Ichetucknee<-Ichetucknee %>%
  mutate(Day = floor_date(Date, unit = "day")) %>%
  group_by(Day) %>%
  mutate(nObservation = n()) %>%
  filter(nObservation == max(nObservation))

Ichetucknee<-filter(Ichetucknee,nObservation>23 )


Ichetucknee<- Ichetucknee %>%
  mutate(hour = hour(Date),
         month=month(Date),
         day=day(Date),
         year=year(Date))

Ichetucknee$u<- -0.0773*Ichetucknee$stage_avg + 0.3319
Ichetucknee$Q<-width*Ichetucknee$stage_avg*Ichetucknee$u

(Ichetucknee$Mouth_Temp_C<- fahrenheit.to.celsius(Ichetucknee$Temp.x))
(Ichetucknee$Mouth_DO_sat<-Cs(Ichetucknee$Mouth_Temp_C))

Ichetucknee$'U/H'<-Ichetucknee$u/Ichetucknee$stage_avg

Ichetucknee$K600<- 9.8425*Ichetucknee$'U/H'+1.9726

Ichetucknee$days <- as.Date(Ichetucknee$Date)
Ichetucknee.day <- aggregate(Ichetucknee, by=list(Ichetucknee$days), FUN='mean')

u<-
  Ichetucknee.day %>%
  mutate( K600_avg= rollapply(K600,7,mean, fill=NA, partial=TRUE, align='left'))
x<-c("K600_avg", "month","day","year" )
u<-u[,x]

Ichetucknee<-left_join(Ichetucknee,u,by=c('day','month','year'))

bin<-filter(Ichetucknee, Date>'2022-08-30'& Date<'2022-09-02')
(Q<-mean(bin$Q))
(K<-mean(bin$K600_avg))

bin2<-filter(Ichetucknee, Date>'2022-09-28'& Date<'2022-10-10')
(Q2<-mean(bin2$Q))
(K2<-mean(bin2$K600_avg))

bin3<-filter(Ichetucknee, Date>'2022-12-02'& Date<'2023-01-26')
(Q3<-mean(bin3$Q))
(K3<-mean(bin3$K600_avg))

bin4<-filter(Ichetucknee, Date>'2023-07-25'& Date<'2023-08-13')
(Q4<-mean(bin4$Q))
(K4<-mean(bin4$K600_avg))

bin5<-filter(Ichetucknee, Date>'2023-01-30'& Date<'2023-02-15')
(Q5<-mean(bin5$Q))
(K5<-mean(bin5$K600_avg))

bin6<-filter(Ichetucknee, Date>'2023-05-11'& Date<'2023-06-13')
(Q6<-mean(bin6$Q))
(K6<-mean(bin6$K600_avg))

bin7<-filter(Ichetucknee, Date>'2023-06-20'& Date<'2023-06-25')
(Q7<-mean(bin7$Q))
(K7<-mean(bin7$K600_avg))

Ichetucknee<-rename(Ichetucknee, 
                  'DO.obs'='DO',
                  'depth'='stage_avg',
                  'temp.water'='Mouth_Temp_C',
                  'discharge'="Q")
Ichetucknee$DO.sat<-Cs(Ichetucknee$temp.water)

Ichetucknee$solar.time <-as.POSIXct(Ichetucknee$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
Ichetucknee<-Ichetucknee[,-c(1)]
Ichetucknee$light<-calc_light(Ichetucknee$solar.time,  29.8, -82.8)

Ichetucknee$temp.water[is.na(Ichetucknee$temp.water)]<-mean(Ichetucknee$temp.water,na.rm=TRUE)
Ichetucknee$DO.sat[is.na(Ichetucknee$DO.sat)]<-mean(Ichetucknee$DO.sat,na.rm=TRUE)


x<-c("DO.obs", "DO.sat", "depth", "discharge", "temp.water","solar.time","light")
Ichetucknee<-Ichetucknee[,x]

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)


bayes_specs <- specs(bayes_name, 
                     K600_lnQ_nodes_centers = c(1.7,1.46,3.03,4.93,1.97),
                     K600_lnQ_nodes_meanlog= log(c(20.48,12.45,6.08,5.02,7.88)),
                     K600_lnQ_nodes_sdlog= 0.1,
                     K600_lnQ_nodediffs_sdlog = 0.05,
                     K600_daily_sigma_sigma= 0.24,
                     burnin_steps=1000, saved_steps=1000)

mm <- metab(bayes_specs, data=Ichetucknee)

predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Ichetucknee/DONE/Ichone_streamMetabol.xlsx")



samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv")
samplingperiod<-filter(samplingperiod, Date>"2022-06-30")
samplingperiod<-filter(samplingperiod, Date<"2023-08-21")
samplingperiod$days <- as.Date(samplingperiod$Date)
samplingperiod.day <- aggregate(samplingperiod, by=list(samplingperiod$days), FUN='min')

Ichone_streamMetabol <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Ichetucknee/DONE/Ichone_streamMetabol.xlsx")

Ichone_streamMetabol<-rename(Ichone_streamMetabol, 
                  'Date'='date',
                  'GPPavg'="GPP_daily_mean",
                  'ER'='ER_daily_mean',
                  'K600_avg'='K600_daily_mean')

Ichone_streamMetabol$NEP<-Ichone_streamMetabol$GPPavg+Ichone_streamMetabol$ER


setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry")

Ichetucknee <- read_excel("Ichetucknee.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric"))
(Ichetucknee$Mouth_Temp_C<- fahrenheit.to.celsius(Ichetucknee$Temp.x))
Ichetucknee$Mouth_DO_sat<-Cs(Ichetucknee$Mouth_Temp_C)

Ichetucknee<-left_join(samplingperiod, Ichetucknee, by=c('Date'))

Ichetucknee<- Ichetucknee %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))

Ichone_streamMetabol<- Ichone_streamMetabol %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))
Ichone_streamMetabol<-Ichone_streamMetabol[,-c(1)]


master<-left_join(Ichetucknee,Ichone_streamMetabol, by=c('day','Month','year'))
master <- master[!duplicated(master[c('Date')]),]
names(Ichetucknee)
Ichetucknee<-master[,-c(2,9,11,19,20,21)]

NEPmin<-min(Ichetucknee$NEP, na.rm = T)
NEPmax<-max(Ichetucknee$NEP, na.rm = T)

GPPmin<-min(Ichetucknee$GPPavg, na.rm = T)
GPPmax<-max(Ichetucknee$GPPavg, na.rm = T)

ERmin<-min(Ichetucknee$ER, na.rm = T)
ERmax<-max(Ichetucknee$ER, na.rm = T)

Ichetucknee$NEPnorm<- (Ichetucknee$NEP-NEPmin)/(NEPmax-NEPmin)
Ichetucknee$GPPnorm<- (Ichetucknee$GPPavg-GPPmin)/(GPPmax-GPPmin)
Ichetucknee$ERnorm<- (Ichetucknee$ER-ERmin)/(ERmax-ERmin)


write_xlsx(Ichetucknee, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx")

