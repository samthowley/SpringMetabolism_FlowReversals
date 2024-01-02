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

master <- read_csv("02_Clean_data/master.csv")

prelim <- function(spring) {
  spring$"velocity_m/s_abs"<-abs(spring$"velocity_m/s")
  spring$"Q_m/s"<-width*spring$depth*spring$"velocity_m/s_abs"

  (spring$Mouth_Temp_C<- fahrenheit.to.celsius(spring$Temp))
  spring$Mouth_DO_sat<-Cs(spring$Mouth_Temp_C)
  spring$Mouth_DO_sat[is.na(spring$Mouth_DO_sat)] <- mean(spring$Mouth_DO_sat, na.rm=T)

  spring$mouth_deficit<-spring$Mouth_DO_sat-spring$DO
  spring$Vent_Temp_C<- fahrenheit.to.celsius(spring$VentTemp_F)
  spring$Vent_DO_sat<-Cs(spring$Vent_Temp_C)
  spring$vent_deficit<-spring$Vent_DO_sat-spring$VentDO
  spring$DO_deficit<-(spring$mouth_deficit+spring$vent_deficit)/2

  spring$"velocity_m/h_abs" <-spring$"velocity_m/s_abs"*3600
  spring$"velocity_m/h" <-spring$"velocity_m/s"*3600

  spring$"Q_m/h" <-spring$"Q_m/s"*3600
  spring$'U/H'<-spring$"velocity_m/s_abs"/spring$depth

  spring$deltaDO<-(spring$DO-spring$VentDO)
  spring$deltaDO_rate<- (spring$deltaDO*spring$"Q_m/s"*24*3600)/area

  return(spring)}
two_station <- function(spring) {
  spring$K600_avg<-gaussianSmooth(spring$K600_1d, 90)
  spring$K_reaeration<-spring$K600_1d*spring$depth*spring$DO_deficit
  spring$not<-spring$deltaDO_rate-spring$K_reaeration
  spring$season <- time2season(spring$Date, out.fmt = "seasons")

  spring<- spring %>%
    mutate(hour = hour(Date),
           Month=month(Date),
           day=day(Date),
           year=year(Date))

  two <-  spring %>%
    mutate(set= case_when(season=="spring"~ 'summer',
                          season=="summer"~ 'summer',
                          season=="autumm"~ 'autumm',
                          season=="winter"~ 'winter'))

  two <- two %>%
    mutate(time= case_when(hour>= 0 & hour<=8 & set=='summer'~ 'ER',
                           hour>= 21 & hour<=23 & set=='summer'~ 'ER',
                           hour>= 0 & hour<=8 & set=='autumm'~ 'ER',
                           hour>= 20 & hour<=23 & set=='autumm'~ 'ER',
                           hour>= 0 & hour<=4 & set=='winter'~ 'ER',
                           hour>= 20 & hour<=23 & set=='winter'~ 'ER'))
  two$time[is.na(two$time)] <- 'AM'

  twoER<-two%>% group_by(day,Month,year,time) %>%
    summarize(ER = mean(not, na.rm=T))

  twoER<-filter(twoER, time=='ER')
  two<-left_join(two, twoER,by=c("day","Month","year"))

  two$GPP<-two$not-two$ER
  two$GPP[two$GPP<0] <- 0

  two_GPPavg<-two%>% group_by(day,Month,year) %>%
    summarize(GPPavg = mean(GPP, na.rm=T))

  two<-left_join(two, two_GPPavg,by=c("day","Month","year"))

  one<-filter(two,  ER<= -35 | ER> 0)
  two<-filter(two, ER> -35 & ER< 0)

  #two_NEP$NEP<-two_NEP$GPPavg+two_NEP$ER
  #two_NEP<-filter(two_NEP, ER<0)

  return(list(two,one))}
data_retrieval <- function(parameterCd, ventID) {

  startDate <- "2022-04-12"
  endDate <- "2023-11-09"

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

##AM####
AllenMill <- master %>% filter(ID=='AM')

length<-900
width <-23.3
area<-length*width

x<-c('Date', 'DO', "depth", "Temp")
AllenMill<-AllenMill[,x]
AllenMill$depth<-na_interpolation(AllenMill$depth)

AllenMill$"velocity_m/s"<-(AllenMill$depth*-0.244+0.46)

AllenMill <- AllenMill %>% mutate(VentDO = case_when(Date <= '2022-07-20 13:00' ~1.42,
                                                     Date  <= '2022-08-15 12:00' ~ 0.947,
                                                     Date  <= '2022-09-12 12:00' ~  1.1,
                                                     Date  <= '2022-10-10 11:00' ~ 1.07,
                                                     Date  <= '2022-10-26 11:00' ~ 0.975,
                                                     Date  <= '2022-11-07 11:00' ~ 1.09704,
                                                     Date  <= '2022-11-21 12:00' ~ 1.09704,
                                                     Date  <= '2022-12-05 11:00' ~ 0.975,
                                                     Date  <= '2022-12-19 10:00' ~ 1.42,
                                                     Date  <= '2023-12-31 13:00' ~ 1.5,
                                                     Date  <= '2023-01-18 11:00' ~ 1.09704,
                                                     Date  <= '2023-02-01 11:00' ~ 1.09704,
                                                     Date  <= '2023-02-15 11:00' ~ 1.2884375,
                                                     Date  <= '2023-03-15 11:00' ~ 1.2884375,
                                                     Date  <= '2023-03-29 11:00' ~ 1.26,
                                                     Date  <= '2023-04-12 11:00' ~ 1.26,
                                                     Date  <= '2023-05-18 11:00' ~ 1.2884375,
                                                     Date  <= '2023-12-30 11:00' ~ 1.2884375))

AllenMill$VentDO<-AllenMill$VentDO +4

AllenMill <- AllenMill %>% mutate(VentTemp_F= case_when(Date <= '2022-07-20 13:00' ~72.05473,
                                                        Date  <= '2022-08-15 12:00' ~ 71.64678,
                                                        Date  <= '2022-09-12 12:00' ~  74.0883,
                                                        Date  <= '2022-10-10 11:00' ~ 72.69694,
                                                        Date  <= '2022-10-26 11:00' ~ 72.15675,
                                                        Date  <= '2022-11-07 11:00' ~ 72.15675,
                                                        Date  <= '2022-11-21 12:00' ~ 72.15675,
                                                        Date  <= '2022-12-05 11:00' ~ 71.9429,
                                                        Date  <= '2022-12-19 10:00' ~ 71.9429,
                                                        Date  <= '2023-01-03 13:00' ~ 71.99254,
                                                        Date  <= '2023-01-18 11:00' ~ 71.48696,
                                                        Date  <= '2023-02-01 11:00' ~ 71.14078431,
                                                        Date  <= '2023-02-15 11:00' ~ 71.42,
                                                        Date  <= '2023-03-15 11:00' ~ 71.67,
                                                        Date  <= '2023-04-12 11:00' ~72.60454545,
                                                        Date  <= '2023-04-26 11:00' ~73.47818182,
                                                        Date  <= '2023-05-18 11:00' ~71.76173913,
                                                        Date  <= '2023-12-30 11:00' ~73.85097561))
AllenMill$VentTemp_F<-mean(AllenMill$VentTemp_F, na.rm = T)

AllenMill<-prelim(AllenMill)

AllenMill$K600_1d<- 7.2*AllenMill$'U/H' + 4

met_output<-two_station(AllenMill)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(two, aes(x=Date)) +
  geom_line(aes(y=ER, color="ER"),size=1)+
  geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
  geom_hline(yintercept = 30)

write_csv(two, "04_Outputs/two_station/AM.csv")
write_csv(one, "04_Outputs/one_station_inputs/AM.csv")

##GB####
GB <- master %>% filter(ID=='GB')

length<-350
width <-21.21408
area<-length*width

x<-c('Date', 'DO', "depth", "Temp")
GB<-GB[,x]
GB$depth<-na_interpolation(GB$depth)

GB$'velocity_m/s'<- -0.39*GB$depth + 0.3

GB <- GB %>% mutate(VentDO= case_when(Date  <= '2022-06-13' ~ 6.253686,
                                      Date <= '2022-07-08' ~ 5.587224,
                                      Date  <= '2022-08-24' ~ 4.60795,
                                      Date  <= '2022-09-19' ~ 4.60795,
                                      Date  <= '2022-10-31' ~ 4.601677,
                                      Date  <= '2022-01-01' ~ 6.253686,
                                      Date  <= '2023-01-11' ~ 4.81268595,
                                      Date  <= '2023-03-08' ~ 5.587224,
                                      Date  <= '2023-03-15' ~ 4.685238095,
                                      Date  <= '2023-04-26' ~ 4.685238095,
                                      Date  <= '2023-05-04' ~4.81268595,
                                      Date  <= '2023-05-18' ~4.44175,
                                      Date  <= '2023-12-30' ~4.51725))



GB <- GB %>% mutate(VentTemp_F = case_when(Date <= '2022-07-08' ~ 77.2365,
                                           Date  <= '2022-08-24' ~ 72.34454,
                                           Date  <= '2022-09-19' ~ 76.55,
                                           Date  <= '2022-10-31' ~ 72.91894,
                                           Date  <= '2022-01-03' ~ 72.5453,
                                           Date  <= '2023-01-11' ~ 73.18875,
                                           Date  <= '2023-03-08' ~ 73.18875,
                                           Date  <= '2023-03-15' ~ 72.485,
                                           Date  <= '2023-05-03' ~ 72.6692562,
                                           Date  <= '2023-05-18' ~ 73.68,
                                           Date  <= '2023-12-30' ~ 73.80225))
GB$VentTemp_F<-mean(GB$VentTemp_F, na.rm = T)

GB<-prelim(GB)

GB$K600_1d<- 11*GB$'U/H'+4

met_output<-two_station(GB)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(two, aes(x=Date)) +
  geom_line(aes(y=ER, color="ER"),size=1)+
  geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
  geom_hline(yintercept = 30)

write_csv(two, "04_Outputs/two_station/GB.csv")
write_csv(one, "04_Outputs/one_station_inputs/GB.csv")









##LF####
LF <- master %>% filter(ID=='LF')

length<-320
width <-7.13
area<-length*width

x<-c('Date', 'DO', "depth", "Temp")
LF<-LF[,x]
LF$depth<-na_interpolation(LF$depth)

LF$"velocity_m/s"<- -0.041*LF$depth + 0.13

LF <- LF %>% mutate(VentDO= case_when(Date  <= '2022-06-13' ~ 1.93,
                                      Date <= '2022-07-08' ~ 1.68,
                                      Date  <= '2022-08-24' ~ 3.475384615,
                                      Date  <= '2022-09-19' ~ 3.475384615,
                                      Date  <= '2022-10-31' ~ 1.78,
                                      Date  <= '2022-01-03' ~ 1.76,
                                      Date  <= '2023-01-11' ~ 1.83,
                                      Date  <= '2023-03-08' ~ 1.93,
                                      Date  <= '2023-03-20' ~ 1.93,
                                      Date  <= '2023-04-13' ~ 1.53,
                                      Date  <= '2023-04-19' ~ 1.704,
                                      Date  <= '2023-05-03' ~ 3.475384615,
                                      Date  <= '2023-08-01' ~ 1.628292683))
LF$VentDO[is.na(LF$VentDO)]<-mean(LF$VentDO, na.rm = T)


LF <- LF %>% mutate(VentTemp_F = case_when(Date <= '2022-07-08' ~ 74.28,
                                           Date  <= '2022-08-24' ~ 74.034,
                                           Date  <= '2022-09-19' ~ 74.05,
                                           Date  <= '2022-10-31' ~ 73.43,
                                           Date  <= '2022-01-03' ~ 74.42,
                                           Date  <= '2023-01-11' ~ 73.29,
                                           Date  <= '2023-03-08' ~ 74.42,
                                           Date  <= '2023-03-20' ~ 74.42,
                                           Date  <= '2023-04-05' ~ 71.74,
                                           Date  <= '2023-04-19' ~ 73.278,
                                           Date  <= '2023-05-03' ~ 72.6692562,
                                           Date  <= '2023-08-01' ~ 73.679375))
LF$VentTemp_F[is.na(LF$VentTemp_F)]<-mean(LF$VentTemp_F, na.rm = T)

LF<-prelim(LF)

LF$K600_1d<-LF$'U/H'*24.6+ 8.6

met_output<-two_station(LF)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(one, aes(x=Date)) +
  geom_line(aes(y=ER, color="ER"),size=1)+
  geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
  geom_hline(yintercept = 30)

write_csv(two, "04_Outputs/two_station/LF.csv")
write_csv(one, "04_Outputs/one_station_inputs/LF.csv")









##ID####
library(dataRetrieval)
parameterCd <- c('00010','00300')
ventID<-'02322700'
vent<-data_retrieval(parameterCd, ventID)

ID <- master %>% filter(ID=='ID')

length<-2845
width <-20
area<- length*width

x<-c('Date', 'DO', "depth", "Temp")
ID<-ID[,x]
ID$depth<-na_interpolation(ID$depth)
ID<-left_join(ID, vent, by=c('Date'))

ID$'velocity_m/s'<- -0.13*ID$depth + 0.42

ID<-prelim(ID)

ID$K600_1d<- 7.2*ID$'U/H'+1.1

met_output<-two_station(ID)

two<-data.frame(met_output[1]) #date column
one<-data.frame(met_output[2]) #date column

ggplot(two, aes(x=Date)) +
  geom_line(aes(y=ER, color="ER"),size=1)+
  geom_line(aes(y=GPPavg, color="GPP"),size=0.4)+
  geom_hline(yintercept = 30)

write_csv(two, "04_Outputs/two_station/ID.csv")
write_csv(one, "04_Outputs/one_station_inputs/ID.csv")









##OS####
OS <- master %>% filter(ID=='OS')

width <-16.86

x<-c('Date', 'DO', "depth", "Temp")
OS<-OS[,x]
OS$depth<-na_interpolation(OS$depth)
OS$VentTemp_F<-NA
OS$VentDO<-NA
OS$'velocity_m/s'<-OS$depth*-0.087+ 0.16

OS<-prelim(OS)

OS$K600_1d<-OS$'U/H'*33.55+1.8

write_csv(OS, "04_Outputs/one_station_inputs/OS.csv")








