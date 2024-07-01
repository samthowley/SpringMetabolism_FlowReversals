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
library(streamMetabolizer)
library(tools)
library(cowplot)

master <- read_csv("02_Clean_data/master_depth2.csv")
LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")
prelim <- function(spring) {
  spring <- spring[complete.cases(spring[ , c('DO', 'Temp', 'depth')]), ]

  spring$Q_m.s<-width*spring$depth*spring$velocity_m.s

  (spring$Mouth_Temp_C<- fahrenheit.to.celsius(spring$Temp))
  spring$Mouth_DO_sat<-Cs(spring$Mouth_Temp_C)
  spring$Mouth_DO_sat[is.na(spring$Mouth_DO_sat)]<-mean(spring$Mouth_DO_sat, na.rm=T)

  spring$mouth_deficit<-spring$Mouth_DO_sat-spring$DO
  spring$Vent_Temp_C<- fahrenheit.to.celsius(spring$VentTemp_F)
  spring$Vent_DO_sat<-Cs(spring$Vent_Temp_C)
  spring$vent_deficit<-spring$Vent_DO_sat-spring$VentDO
  spring$DO_deficit<-(spring$mouth_deficit+spring$vent_deficit)/2

  spring$"velocity_m.h" <-spring$"velocity_m.s"*60*60
  spring$"Q_m.h" <-spring$"Q_m.s"*60*60

  spring$'U/H'<-spring$"velocity_m.s"/spring$depth

  spring$deltaDO<-(spring$DO-spring$VentDO)
  spring$deltaDO_rate<- (spring$deltaDO*spring$"Q_m.h"*24)/area

  return(spring)}
two_station<- function(spring) {

  spring$K_reaeration<-spring$K600_1d*spring$depth*spring$DO_deficit
  spring$not<-spring$deltaDO_rate-spring$K_reaeration
  spring$not[spring$not< -37]<- -37

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

  spring<-spring %>%rename('u_mh'='velocity_m.h')%>%
    mutate(u_md=u_mh*24) %>%
    mutate(L_max=(u_md*3)/K600_1d) %>%
    group_by(day,Month,year) %>%
    mutate(L_max=max(L_max, na.rm=T)- length*2)

  spring1 <- spring %>%
    mutate(method= case_when(
      length< L_max ~ '2'))
  spring1$method[is.na(spring1$method)]<-1

  two<-spring1 %>%group_by(day,Month,year) %>%
    filter(method== "2" & ER <= -2)
  one<-spring1 %>%group_by(day,Month,year) %>%
    filter(method== "1"  | ER > -2)

  return(list(two,one))}
two_station_ID<- function(spring) {
  
  spring$K_reaeration<-spring$K600_1d*spring$depth*spring$DO_deficit
  spring$not<-spring$deltaDO_rate-spring$K_reaeration
  spring$not[spring$not< -37]<- -37
  
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
  
  #for ID
  spring<-spring %>%rename('u_mh'='velocity_m.h')%>%
    mutate(u_md=u_mh*24) %>%
    mutate(L_max=(u_md*3)/K600_1d) %>%
    group_by(day,Month,year) %>%
    mutate(L_max=max(L_max, na.rm=T)- length*2)
  
  
  spring1 <- spring %>%
    mutate(method= case_when(
      length< L_max ~ '2'))
  spring1$method[is.na(spring1$method)]<-1
  
  two<-spring1 %>%group_by(day,Month,year) %>%
    filter(method== "2" & ER <= -2)
  one<-spring1 %>%group_by(day,Month,year) %>%
    filter(method== "1"  | ER > -2)
  
  return(list(two,one))}

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
data_retrieval <- function(parameterCd, ventID) {

  startDate <- "2022-04-12"
  endDate <- "2024-06-25"

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

##GB####
GB <- master %>% filter(ID=='GB')

GB$light<-calc_light(GB$Date,  29.83, -82.68)
length<-350
width <-23
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
GB$K600_1d<- cf[2]*GB$'U/H' + cf[1]

GB1<- GB%>%filter(DO >3.5, Date>'2022-05-20', DO<8.5)
#ggplot(GB1, aes(Date, DO)) + geom_line()

GB_recov<-two_station_forRecovery(GB1)

a<-ggplot(GB_recov, aes(x=Date)) +
  geom_line(aes(y=ER),size=1)
b<-ggplot(GB_recov, aes(x=Date)) +
  geom_line(aes(y=depth),size=1)
c<-ggplot(GB_recov, aes(x=Date)) +geom_line(aes(y=DO),size=1)
plot_grid(a,b,c, ncol=1)

write_csv(GB_recov, "04_Outputs/one station inputs/not parsed/GB.csv")

met_output<-two_station(GB)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

write_csv(two, "04_Outputs/two station results/GB.csv")
write_csv(one, "04_Outputs/one station inputs/GB.csv")

##AM####

AllenMill <- master %>% filter(ID=='AM')

AllenMill$light<-calc_light(AllenMill$Date,  30.155, -83.238)
length<-520
width <-24
area<-length*width

rel_u <- lm(u ~ depth, data=AM_rC)
(cf <- coef(rel_u))
AllenMill$"velocity_m.s"<-(AllenMill$depth*cf[2]+cf[1])

AllenMill$VentDO<-mean(AM_rC$VentDO, na.rm=T)
AllenMill$VentDO<-AllenMill$VentDO+3.5
AllenMill$VentTemp_F<-mean(AM_rC$VentTemp, na.rm=T)

AllenMill<-prelim(AllenMill)

rel_k <- lm(k600_1d ~ uh, data=AM_rC)
(cf <- coef(rel_k))
AllenMill$K600_1d<- cf[2]*AllenMill$'U/H' + cf[1]

AllenMill1<- AllenMill %>% filter(DO<10) %>% mutate(depth=depth-0.25)
#ggplot(AllenMill1, aes(Date, DO)) + geom_line() 

AM_recov<-two_station_forRecovery(AllenMill1)

write_csv(AM_recov, "04_Outputs/one station inputs/not parsed/AM.csv")
met_output<-two_station(AllenMill1)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(two, aes(x=Date)) +
  geom_line(aes(y=ER),size=1)+
 #geom_hline(yintercept=(0.4))
geom_line(aes(y=DO*2),size=1, color='darkred')
# 
# ggplot(one, aes(x=Date)) +
#   geom_line(aes(y=ER),size=1)+
#   geom_hline(yintercept=-35)

write_csv(two, "04_Outputs/two station results/AM.csv")
write_csv(one, "04_Outputs/one station inputs/AM.csv")

##LF####
LF <- master %>% filter(ID=='LF')

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
LF$K600_1d<- cf[2]*LF$'U/H' + cf[1]

LF<-LF%>% filter(DO>1.2)
#ggplot(LF, aes(Date, DO)) + geom_line() 

LF_recov<-two_station_forRecovery(LF)

ggplot(LF_recov, aes(x=Date)) +
  geom_line(aes(y=ER),size=1)

write_csv(LF_recov, "04_Outputs/one station inputs/not parsed/LF.csv")
met_output<-two_station(LF)
two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

# ggplot(two, aes(x=Date)) +
#   geom_line(aes(y=ER, color="ER"),size=1)+
#   geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
#   geom_hline(yintercept = -30)
# ggplot(one, aes(x=Date)) +
#   geom_line(aes(y=ER, color="ER"),size=1)+
#   geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
#   geom_hline(yintercept = -30)


write_csv(two, "04_Outputs/two station results/LF.csv")
write_csv(one, "04_Outputs/one station inputs/LF.csv")

##ID####
parameterCd <- c('00010','00300')
ventID<-'02322700'
vent<-data_retrieval(parameterCd, ventID)

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
ID$K600_1d<- cf[2]*ID$'U/H' + cf[1]

ID1<-ID %>% mutate(depth=depth-0.5) %>%filter(DO<10, DO>3.4)
#ggplot(ID1, aes(Date, DO)) + geom_line() 

ID_recov<-two_station_forRecovery(ID)

write_csv(ID_recov, "04_Outputs/one station inputs/not parsed/ID.csv")

met_output<-two_station_ID(ID1)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(one, aes(x=Date)) +
  geom_line(aes(y=L_max),size=0.4)

write_csv(two, "04_Outputs/two station results/ID.csv")
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
OS$K600_1d<- cf[2]*OS$'U/H' + cf[1]

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
endDate <- "2024-06-17"
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





