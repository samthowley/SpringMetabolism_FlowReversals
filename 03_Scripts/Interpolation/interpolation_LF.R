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
site_id <- '02323500'
parameterCd <- c('00065')
riverLF <- readNWISuv(site_id,parameterCd)
split<-split(riverLF, riverLF$site_no)

df02322500 <-split(riverLF, riverLF$site_no)[[1]]
df02322500<-df02322500[,c(3,4)]
df02322500<-rename(df02322500, 'Date'='dateTime', 'elevation'='X_00065_00000')

riverLF<-(df02322500)

riverLF<- riverLF %>% mutate(minute = minute(Date))
riverLF<-filter(riverLF, minute==0)
riverLF<-riverLF[,c(1,2)]
###LF run####
######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master")

LF <- read_excel("LittleFanning.xlsx")
LF<-filter(LF, stage>0.16)
LF$stage<-na_interpolation(LF$stage)
LF<-left_join(LF, riverLF)

ggplot(LF, aes(x=Date))+geom_line(aes(y=GPPavg))
ggplot(LF, aes(x=Date))+geom_line(aes(y=ER))

summary(modGPPavg<-lm(GPPavg ~ stage, data = LF))
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])
#ggplot(LF, aes(x=stage))+geom_point(aes(y=GPPavg))

summary(modER<-lm(ER ~ stage, data = LF))
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

#riverLF$elevation<-(riverLF$elevation-riverLF$stage_down)*0.79

(relation_h<-ggplot(LF, aes(stage, elevation))+geom_point()+
  geom_smooth(method='lm')+theme_minimal()+ggtitle("LF"))

summary(mod_h<-lm(formula =  stage ~ elevation, data = LF))
#summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = LF))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(LF$stage_interpolated<-(Slope_h*LF$elevation)+Intercept_h)
(riverLF$stage<-(Slope_h*riverLF$elevation)+Intercept_h)


compare_h<-ggplot(LF, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)+
  theme_minimal()+ggtitle("LF")

(riverLF$GPPavg<- SlopeGPPavg*riverLF$stage+InterceptGPPavg)
riverLF$GPPavg[riverLF$GPPavg<0] <- 0
(riverLF$ER<- SlopeER*riverLF$stage+InterceptER)

test<-left_join(LF, riverLF, by='Date')
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
  ylab('flux')+ggtitle('LF')+
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


ggplot(riverLF, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=1)+theme_minimal()



