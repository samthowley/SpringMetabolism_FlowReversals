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

master <- read_csv("02_Clean_data/master_depth2.csv")
prelim <- function(spring) {
  spring <- spring[complete.cases(spring[ , c('DO', 'Temp', 'depth')]), ]
  
  #spring<-spring %>% mutate(velocity_m.s=abs(velocity_m.s))

  spring$Q_m.s<-width*spring$depth*spring$velocity_m.s

  (spring$Mouth_Temp_C<- fahrenheit.to.celsius(spring$Temp))
  spring$Mouth_DO_sat<-Cs(spring$Mouth_Temp_C)
  spring$Mouth_DO_sat[is.na(spring$Mouth_DO_sat)]<-mean(spring$Mouth_DO_sat, na.rm=T)

  spring$DO_deficit<-spring$Mouth_DO_sat-spring$DO

  spring$"velocity_m.h" <-spring$"velocity_m.s"*60*60
  spring$"Q_m.h" <-spring$"Q_m.s"*60*60

  spring$'U/H'<-spring$"velocity_m.s"/spring$depth

  spring$deltaDO<-(spring$DO-lag(spring$DO))
  spring$deltaDO_rate<- (spring$deltaDO*spring$"Q_m.h"*24)/area

  return(spring)}
one_station<- function(spring,min) {
  
  spring$K600_1d[spring$K600_1d<min]<-min-1
  
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

##AM####

AllenMill <- master %>% filter(ID=='AM')

AllenMill$light<-calc_light(AllenMill$Date,30.155, -83.238)
length<-520
width <-24
area<-length*width

AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
rel_u <- lm(u ~ depth, data=AM_rC)
(cf <- coef(rel_u))
AllenMill$"velocity_m.s"<-(AllenMill$depth*cf[2]+cf[1])

AllenMill<-prelim(AllenMill)

rel_k <- lm(k600_1d ~ uh, data=AM_rC)
(cf <- coef(rel_k))
AllenMill$K600_1d<- cf[2]*AllenMill$'U/H' + cf[1]
min<-min(AM_rC$k600_1d, na.rm=T)

AllenMill1<- AllenMill %>% filter(DO<10) %>% mutate(depth=depth-0.25)

AM_met<-one_station(AllenMill1, min)

test<-filter(AM_met, DO<2)
ggplot(test, aes(Date, ER)) + geom_line() 

write_csv(one, "04_Outputs/one station outputs/manual/AM.csv")

##OS####
OS <- master %>% filter(ID=='OS')

width <-16.86
length<-1390
area<-width*length

OS<-OS %>% select(Date,DO,depth,Temp) %>% mutate(depth=na_interpolation(depth),
                                                 light=calc_light(Date,30.155,-83.238))

OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
rel_u <- lm(u ~ depth, data=OS_rC)
(cf <- coef(rel_u))
OS$"velocity_m.s"<-(OS$depth*cf[2]+cf[1])

OS<-prelim(OS)

rel_k <- lm(k600_1d ~ uh, data=OS_rC)
(cf <- coef(rel_k))
OS$K600_1d<- cf[2]*OS$'U/H' + cf[1]
#min(OS_rC$k600_1d, na.rm=T)
min<-1.32
ggplot(OS_rC, aes(uh, k600_1d)) + geom_point() 

OS_met<-one_station(OS, min)

test<-filter(OS_met, DO<2)
ggplot(test, aes(Date, ER)) + geom_line() 

write_csv(one, "04_Outputs/one station outputs/manual/AM.csv")


