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
library(dataRetrieval)
library(tools)
library(cowplot)
library(streamMetabolizer)
library(writexl)

two_station_forRecovery<- function(spring) {
  #spring$K600_1d[spring$K600_1d<0]<-0.1
  #spring$Q_m.s[spring$K600_1d<0]<-0.1
  
  spring$K_reaeration<-spring$K600_1d*spring$depth*spring$DO_deficit
  spring$not<-spring$deltaDO_rate-spring$K_reaeration
  
  spring<- spring %>%
    mutate(hour = hour(Date),day=day(Date),Month=month(Date),year=year(Date))
  
  spring <- spring %>%mutate(time= case_when(light<=0~ 'ER',light>0~ 'AM'))
  
  springER<-spring%>% group_by(day,Month,year,time) %>%
    summarize(ER = mean(not, na.rm=T))
  
  springER<-filter(springER, time=='ER')
  spring<-left_join(spring, springER,by=c("day","Month","year"))
  
  spring$GPP<-spring$not-spring$ER
  spring$GPP[spring$GPP<0] <- 0
  
  spring_GPPavg<-spring%>% group_by(day,Month,year) %>%
    summarize(GPPavg = mean(GPP, na.rm=T))
  
  
  spring<-left_join(spring, spring_GPPavg,by=c("day","Month","year"))
  
  return(spring)}

master <- read_csv("02_Clean_data/master_depth2.csv")

data_retrieval <- function(parameterCd, ventID) {
  
  startDate <- "2022-04-12"
  endDate <- "2024-07-25"
  
  vent_15sec<-readNWISuv(ventID,parameterCd,startDate,endDate)
  vent_15sec<-vent_15sec[,-c(1,2,5,7,8)]
  vent_15sec<-rename(vent_15sec, 'VentTemp_C'="X_00010_00000", 'Date'="dateTime",'VentDO'="X_00300_00000")
  
  vent_15sec<- vent_15sec %>% mutate(minute = minute(Date))
  vent<-filter(vent_15sec, minute==0)
  vent$VentDO[is.na(vent$VentDO)]<-mean(vent$VentDO, na.rm = T)
  vent$VentTemp_C[is.na(vent$VentTemp_C)]<-mean(vent$VentTemp_C, na.rm = T)
  vent$VentTemp_F<-celsius.to.fahrenheit(vent$VentTemp_C)
  vent<-vent[,-c(2,4)]
  return(vent)}
prelim <- function(spring) {
  spring <- spring[complete.cases(spring[ , c('DO', 'Temp', 'depth')]), ]

  spring$Q_m.s<-width*spring$depth*spring$velocity_m.s #m3/s

  spring<-spring %>% 
    mutate(Mouth_Temp_C=fahrenheit.to.celsius(Temp))%>% #F to C
    mutate(Mouth_DO_sat=Cs(Mouth_Temp_C))%>% # calculate station 2 DO Saturation
    mutate(mouth_deficit=Mouth_DO_sat-DO) #DO deficit

  spring<-spring %>% 
    mutate(Vent_Temp_C=fahrenheit.to.celsius(VentTemp_F))%>%
    mutate(Vent_DO_sat=Cs(Vent_Temp_C))%>%
    mutate(vent_deficit=Vent_DO_sat-VentDO)%>%

    mutate(mean.DO.deficit=(mouth_deficit+vent_deficit)/2)%>% #average DO deficit between the two stations
    mutate(mean.DO.sat=(Vent_DO_sat+Mouth_DO_sat)/2)%>%
    mutate(DO.sat.def=mean.DO.deficit/mean.DO.sat)

  spring<-spring %>%
    mutate(velocity_m.h=velocity_m.s*60*60) %>%
    mutate(Q_m.h=Q_m.s*60*60)%>%
    mutate(u.h=velocity_m.s/depth)
  
  spring<-spring %>% 
    mutate(deltaDO=DO-VentDO) %>% #station2-station1
    mutate(deltaDO_rate=(deltaDO/area)*Q_m.h*24)

  return(spring)}
two_station<- function(spring) {
  
  spring<-spring %>% 
    mutate(K_reaeration=K600_1d*depth*mean.DO.deficit)%>%
    mutate(rearation=K_reaeration*DO.sat.def)%>%
    mutate(not=deltaDO_rate-rearation)

  spring<- spring %>%
    mutate(hour = hour(Date),day=day(Date),Month=month(Date),year=year(Date))
  
  spring <- spring %>%mutate(time= case_when(light<=200~ 'ER',light>200~ 'AM'))
  
  springER<-spring%>% group_by(day,Month,year,time) %>%
    summarize(ER = mean(not, na.rm=T))
  
  springER<-filter(springER, time=='ER')
  spring<-left_join(spring, springER,by=c("day","Month","year"))
  
  spring$GPP<-spring$not-spring$ER
  spring$GPP[spring$GPP<0] <- 0
  
  spring_GPPavg<-spring%>% group_by(day,Month,year) %>%
    summarize(GPPavg = mean(GPP, na.rm=T))
  
  spring<-left_join(spring, spring_GPPavg,by=c("day","Month","year"))
  
  spring<-spring %>% mutate(u_md=velocity_m.h*24) %>%
    mutate(L_max=(u_md/K600_1d)*3)%>%mutate(L_min=(u_md/K600_1d)*0.4)
  
  spring<- spring %>% mutate(demars=case_when(length>L_min & length<L_max~"two"))
  spring$demars[is.na(spring$demars)]<- "one"
    
  two<-spring %>% filter(demars=="two")  #%>% filter(ER< -2) %>% filter(ER> -30)
  one<-spring %>% filter(demars=="one")
  
  return(list(two,one))}

##GB####
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
GB <- master %>% filter(ID=='GB')

GB$light<-calc_light(GB$Date,  29.83, -82.68)
length<-310
width <-16.5
area<-length*width

x<-c('Date', 'DO', "depth", "Temp",'light', 'SpC')
GB<-GB[,x]

rel_u <- lm(u ~ depth, data=GB_rC)
(cf <- coef(rel_u))

GB$"velocity_m.s"<-(GB$depth*cf[2]+cf[1])

GB$VentDO<-mean(GB_rC$VentDO, na.rm=T)
GB$VentTemp_F<-mean(GB_rC$VentTemp, na.rm=T)

GB<-prelim(GB)

rel_k <- lm(k600_1d ~ uh, data=GB_rC)
(cf <- coef(rel_k))
GB$K600_1d<- (cf[2]*GB$u.h + cf[1])

GB1<- GB%>%filter(DO >3.5, Date>'2022-05-20', DO<8.5)#%>%mutate(depth=depth-0.5)
#ggplot(GB, aes(Date, DO)) + geom_line()

#GB_recov<-two_station_forRecovery(GB1)
#write_csv(GB_recov, "04_Outputs/one station inputs/not parsed/GB.csv")

met_output<-two_station(GB1)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(two, aes(x=depth)) +geom_point(aes(y=ER),size=1)#+geom_hline(yintercept = -30)
ggplot(two, aes(x=Date)) +geom_line(aes(y=ER),size=1)+geom_hline(yintercept = 0)#+geom_line(aes(y=depth*100),size=1,color='darkgreen')

two<-two%>%filter(ER<0)

write_csv(two, "04_Outputs/two station results/GB.csv")
write_csv(one, "04_Outputs/one station inputs/GB.csv")

##AM####
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
AllenMill <- master %>% filter(ID=='AM')

AllenMill$light<-calc_light(AllenMill$Date,  30.155, -83.238)
length<-950
width <-30
area<-length*width

rel_u <- lm(u ~ depth, data=AM_rC)
(cf <- coef(rel_u))
AllenMill$"velocity_m.s"<-(AllenMill$depth*cf[2]+cf[1])

AllenMill$VentDO<-mean(AM_rC$VentDO, na.rm=T)
AllenMill$VentDO<-AllenMill$VentDO+4
AllenMill$VentTemp_F<-mean(AM_rC$VentTemp, na.rm=T)

AllenMill<-prelim(AllenMill)

rel_k <- lm(k600_1d ~ uh, data=AM_rC)
(cf <- coef(rel_k))
AllenMill$K600_1d<- cf[2]*AllenMill$u.h + cf[1]

AllenMill1<- AllenMill %>% filter(DO<10)# %>% mutate(depth=depth-1.2)

# AM_recov<-two_station_forRecovery(AllenMill1)
# 
# write_csv(AM_recov, "04_Outputs/one station inputs/not parsed/AM.csv")
met_output<-two_station(AllenMill1)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

two<-two%>%filter(ER< -2)%>% filter(ER> -50)
ggplot(two, aes(x=Date))+ geom_line(aes(y=ER))

write_csv(two, "04_Outputs/two station results/AM.csv")
write_csv(one, "04_Outputs/one station inputs/AM.csv")

##LF####
LF <- master %>% filter(ID=='LF')
LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")

LF$light<-calc_light(LF$Date,  29.585, -82.937)
length<-350
width <-20
area<-length*width
LF$depth<-na_interpolation(LF$depth)

rel_u <- lm(u ~ depth, data=LF_rC)
(cf <- coef(rel_u))
LF$"velocity_m.s"<-(LF$depth*cf[2]+cf[1])

LF$VentDO<-mean(LF_rC$VentDO, na.rm=T)
LF$VentTemp_F<-mean(LF_rC$VentTemp, na.rm=T)

LF<-prelim(LF)

rel_k <- lm(k600_1d ~ uh, data=LF_rC)
(cf <- coef(rel_k))
LF$K600_1d<- cf[2]*LF$u.h + cf[1]

LF<-LF%>% filter(DO>1.2)
#ggplot(LF, aes(Date, DO)) + geom_line() 

#LF_recov<-two_station_forRecovery(LF)
#ggplot(LF_recov, aes(x=Date)) + geom_line(aes(y=ER),size=1)
#write_csv(LF_recov, "04_Outputs/one station inputs/not parsed/LF.csv")

met_output<-two_station(LF)
two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

 ggplot(two, aes(x=depth)) +geom_point(aes(y=ER, color="ER"),size=1)+
  geom_point(aes(y=GPPavg, color="GPP"),size=0.4)+geom_hline(yintercept = -30)
# ggplot(one, aes(x=Date)) +geom_line(aes(y=ER, color="ER"),size=1)+
#   geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+geom_hline(yintercept = -30)


write_csv(two, "04_Outputs/two station results/LF.csv")
write_csv(one, "04_Outputs/one station inputs/LF.csv")

##ID####
parameterCd <- c('00010','00300')
ventID<-'02322700'
vent<-data_retrieval(parameterCd, ventID)
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")

ID <- master %>% filter(ID=='ID')
ID$light<-calc_light(ID$Date,  29.93, -82.8)
length<-2800
width <-50
area<- length*width

x<-c('Date', 'DO', "depth", "Temp",'light')
ID<-ID[,x]
ID$depth<-na_interpolation(ID$depth)
ID<-left_join(ID, vent, by=c('Date'))

rel_u <- lm(u ~ depth, data=ID_rC)
(cf <- coef(rel_u))
ID$"velocity_m.s"<-(ID$depth*cf[2]+cf[1])

ID<-prelim(ID)

rel_k <- lm(k600_1d ~ uh, data=ID_rC)
(cf <- coef(rel_k))
ID$K600_1d<- cf[2]*ID$u.h + cf[1]

ID1<-ID %>% mutate(depth=depth-0.5) %>%filter(DO<10)
#ggplot(ID1, aes(Date, DO)) + geom_line() 

# ID_recov<-two_station_forRecovery(ID1)
# write_csv(ID_recov, "04_Outputs/one station inputs/not parsed/ID.csv")

met_output<-two_station(ID1)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(one, aes(x=depth)) +geom_point(aes(y=ER, color="ER"),size=1)+
  geom_point(aes(y=GPPavg, color="GPP"),size=0.4)+geom_hline(yintercept = -30)

write_csv(two, "04_Outputs/two station reswrite_csv")
write_csv(one, "04_Outputs/one station inputs/ID.csv")
##OS####
OS <- master %>% filter(ID=='OS')

width <-16.86
length<-1390
area<-width*length

x<-c('Date', 'DO', "depth", "Temp")
OS<-OS[,x]
OS$depth<-na_interpolation(OS$depth)
OS$VentTemp_F<-NA
OS$VentDO<-NA
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
rel_u <- lm(u ~ depth, data=OS_rC)
(cf <- coef(rel_u))
OS$"velocity_m.s"<-(OS$depth*cf[2]+cf[1])

OS<-prelim(OS)

rel_k <- lm(k600_1d ~ uh, data=OS_rC)
(cf <- coef(rel_k))
OS$K600_1d<- cf[2]*OS$u.h + cf[1]

#ggplot(OS, aes(Date, DO)) + geom_line() 

write_csv(OS, "04_Outputs/one station inputs/not parsed/OS.csv")
write_csv(OS, "04_Outputs/one station inputs/OS.csv")

####For recovery analysis####
x<-c('Date', 'GPPavg', 'ER','Q_m.s','ID')
AM_recov$ID<-'AM'
AM_recov<-AM_recov[,x]
GB_recov$ID<-'GB'
GB_recov<-GB_recov[,x]
LF_recov$ID<-'LF'
LF_recov<-LF_recov[,x]
ID_recov$ID<-'ID'
ID_recov<-ID_recov[,x]

recovery<-rbind(AM_recov, GB_recov, LF_recov,  ID_recov) #ID_recov

OSmet<-read_csv("04_outputs/Stream metabolizer results/OS.csv")
OSmet<-OSmet %>% mutate(day=day(Date), month=month(Date), year=year(Date))
OSdepth<-read_csv("02_Clean_data/master_depth2.csv")
OSdepth<-OSdepth %>% filter(ID=="OS") %>%mutate(day=day(Date), month=month(Date), year=year(Date))
OSdepth<-OSdepth[,-6]

rel_u <- lm(u ~ depth, data=OS_rC)
(cf <- coef(rel_u))
OSdepth$"velocity_m.s"<-(OSdepth$depth*cf[2]+cf[1])
OS_recov<-left_join(OSdepth, OSmet, by=c('day', 'month', 'year'))
OS_recov<-OS_recov %>% mutate(width=17, ID="OS") %>% mutate(Q_m.s=width*depth*velocity_m.s)
OS_recov<-OS_recov[,x]




IUmet<-read_csv("04_outputs/Stream metabolizer results/IU.csv")
IUmet<-IUmet %>% mutate(day=day(Date), month=month(Date), year=year(Date))

startDate <- "2022-05-02"
endDate <- "2024-07-25"
parameterCd <- c('00060','00065')
ventID<-'02322700'

library(measurements)
IUdepth<- readNWISuv(ventID,parameterCd, startDate, endDate)
IUdepth<-IUdepth %>% rename('Q_m.s'='X_00060_00000')%>%
  mutate(depth=X_00065_00000-13.72, Q_m.s=Q_m.s/35.3147)%>%
  mutate(min=minute(dateTime), day=day(dateTime), month=month(dateTime), year=year(dateTime)) %>% filter(min==0)
IU_recov<-left_join(IUdepth, IUmet, by=c('day', 'month', 'year'))
IU_recov<-IU_recov %>% mutate(width=28, ID="IU")
IU_recov<-IU_recov[,x]

recovery<-rbind(recovery,OS_recov,IU_recov) #ID_recov
write_csv(recovery, "02_Clean_data/discharge.csv")





