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
library(mmand)
library(dataRetrieval)
library(imputeTS)

####ft white####
site_id <- c('02321958','02322500')
parameterCd <- c('00065')
riverGB <- readNWISuv(site_id,parameterCd)
split<-split(riverGB, riverGB$site_no)

df02321958 <-split(riverGB, riverGB$site_no)[[2]]
df02321958<-df02321958[,c(3,4)]
df02321958<-rename(df02321958, 'Date'='dateTime', 'stage_down'='X_00065_00000')

df02322500 <-split(riverGB, riverGB$site_no)[[1]]
df02322500<-df02322500[,c(3,4)]
df02322500<-rename(df02322500, 'Date'='dateTime', 'stage_up'='X_00065_00000')

riverGB<-left_join(df02322500,df02321958, by='Date')

riverGB<- riverGB %>% mutate(minute = minute(Date))
riverGB<-filter(riverGB, minute==0)
riverGB$elevation<-(riverGB$stage_up-riverGB$stage_down)*0.79
riverGB<-riverGB[,c(1,5)]
###GB run####
######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master")

GB <- read_excel("GilchristBlue.xlsx")
GB<-filter(GB, stage>0.16)
GB$stage<-na_interpolation(GB$stage)
GB<-left_join(GB, riverGB)

ggplot(GB, aes(x=Date))+geom_line(aes(y=GPPavg))
ggplot(GB, aes(x=Date))+geom_line(aes(y=ER))

summary(modGPPavg<-lm(GPPavg ~ stage, data = GB))
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])
#ggplot(GB, aes(x=stage))+geom_point(aes(y=GPPavg))

summary(modER<-lm(ER ~ stage, data = GB))
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

#riverGB$elevation<-(riverGB$stage_up-riverGB$stage_down)*0.79

(relation_h<-ggplot(GB, aes(stage, elevation))+geom_point()+
  geom_smooth(method='lm')+theme_minimal()+ggtitle("GB"))

summary(mod_h<-lm(formula =  stage ~ elevation, data = GB))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = GB))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(GB$stage_interpolated<-(Slope_h*GB$elevation)+Intercept_h)
(riverGB$stage<-(Slope_h*riverGB$elevation)+Intercept_h)


compare_h<-ggplot(GB, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)+
  theme_minimal()+ggtitle("GB")

(riverGB$GPPavg<- SlopeGPPavg*riverGB$stage+InterceptGPPavg)
riverGB$GPPavg[riverGB$GPPavg<0] <- 0
(riverGB$ER<- SlopeER*riverGB$stage+InterceptER)


test<-left_join(GB, riverGB, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y
test<-filter(test, ratio_GPP<10)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_h<-test$stage.x/test$stage.y

(compare<-ggplot(test, aes(x=Date)) +
  geom_line(aes(y=ER.y),size=1, color="red")+
  geom_line(aes(y=ER.x),size=1, color="black")+
  geom_line(aes(y=GPPavg.y),size=1, color="green")+
  geom_line(aes(y=GPPavg.x),size=1,color="black")+
  ylab('flux')+ggtitle('GB')+
  theme_minimal())

a<-ggplot(test, aes(x=Date)) +
  geom_point(aes(y=ratio_ER),size=1, color="black")+ theme_minimal()+
  ggtitle("ER Correlation")+ylab("actual/interpolated")
b<-ggplot(test, aes(x=Date)) +
  geom_point(aes(y=ratio_GPP),size=1, color="black")+ theme_minimal()+
  ggtitle("GPP Correlation")+ylab("actual/interpolated")
c<-ggplot(test, aes(x=Date)) +
  geom_point(aes(y=ratio_h),size=1, color="black")+ theme_minimal()+
  ggtitle("Stage Correlation")+ylab("actual/interpolated")


bottom_h<-plot_grid(relation_h, c)
plot_grid(compare_h, bottom_h, nrow=2)
bottom<-plot_grid(a,b)
plot_grid(compare, bottom, nrow=2)


ggplot(riverGB, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=1)+theme_minimal()



