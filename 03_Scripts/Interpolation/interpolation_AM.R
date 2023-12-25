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
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master")

AM <- read_excel("AllenMill.xlsx")
AM<-filter(AM, Date>"2022-10-11")
AM$stage<-na_interpolation(AM$stage)
AM<-
  AM %>%  mutate(day = day(Date), 
                    Month = month(Date),
                    year=year(Date)) %>%
  mutate( GPP=rollmean(GPPavg, k=12, fill=NA, align='left'),
          ER_avg=rollmean(ER, k=12, fill=NA, align='left'))

ggplot(AM, aes(x=Date))+geom_line(aes(y=GPPavg))+
  geom_line(aes(y=GPP), color='red')

modGPPavg<-lm(GPP ~ stage, data = AM)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER_avg ~ stage, data = AM)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

site_id <- c('02320000','02319800')
parameterCd <- c('00065')
riverAM <- readNWISuv(site_id,parameterCd)
split<-split(riverAM, riverAM$site_no)

df02323000 <-split(riverAM, riverAM$site_no)[[1]]
df02323000<-df02323000[,c(3,4)]
df02323000<-rename(df02323000, 'Date'='dateTime', 'stage_down'='X_00065_00000')

df02323500 <-split(riverAM, riverAM$site_no)[[2]]
df02323500<-df02323500[,c(3,4)]
df02323500<-rename(df02323500, 'Date'='dateTime', 'stage_up'='X_00065_00000')

riverAM<-left_join(df02323500,df02323000, by='Date')

riverAM<- riverAM %>% mutate(minute = minute(Date))
riverAM<-filter(riverAM, minute==0)

riverAM$elevation<-(riverAM$stage_up-riverAM$stage_down)*0.501
riverAM<-riverAM[,c(1,5)]
AM<-left_join(AM, riverAM)

(relation_h<-ggplot(AM, aes(stage, elevation))+geom_point()+
  geom_smooth(method='lm')+theme_minimal()+ggtitle("AM"))

summary(mod_h<-lm(formula =  stage ~ elevation, data = AM))
#summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = AM))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(AM$stage_interpolated<- (Slope_h*AM$elevation)+Intercept_h)
(riverAM$stage<- (Slope_h*riverAM$elevation)+Intercept_h)


compare_h<-ggplot(AM, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)+
  theme_minimal()+ggtitle("AM")

(riverAM$GPPavg<- SlopeGPPavg*riverAM$stage+InterceptGPPavg)
riverAM$GPPavg[riverAM$GPPavg<0] <- 0
(riverAM$ER<- SlopeER*riverAM$stage+InterceptER)


test<-left_join(AM, riverAM, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y
test<-filter(test, ratio_GPP<10)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_h<-test$stage.x/test$stage.y

compare<-ggplot(test, aes(x=Date)) +
  geom_line(aes(y=ER.y),size=1, color="red")+
  geom_line(aes(y=ER.x),size=1, color="black")+
  geom_line(aes(y=GPPavg.y),size=1, color="green")+
  geom_line(aes(y=GPPavg.x),size=1,color="black")+
  ylab('flux')+ggtitle('AM')+
  theme_minimal()

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


ggplot(riverAM, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=1)+theme_minimal()



