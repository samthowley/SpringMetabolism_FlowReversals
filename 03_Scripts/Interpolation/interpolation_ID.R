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
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master")

riverID <- read_excel("r02322703_Level.xlsx", 
                    col_types = c("skip", "skip", "skip", 
                                  "date", "numeric", "text"))
riverID<-riverID %>%
  mutate(day = day(Date),month = month(Date),year = year(Date))

riverID_mean<-riverID %>%
  mutate(day = day(Date),month = month(Date),year = year(Date))%>%
  group_by(day, month, year) %>%
  summarise(daily_NAVD88 = mean(`Level NAVD88`, na.rm=T))


ID<-read_excel("Ichetucknee.xlsx")
ID <- ID[!duplicated(ID[c('Date')]),]

modGPPavg<-lm(GPPavg ~ stage, data = ID)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER ~ stage, data = ID)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

ID<-ID %>%
  mutate(day = day(Date),month = month(Date),year = year(Date))

ID<-left_join(ID, riverID_mean, by=c('day', 'month', 'year'))
riverID<-left_join(riverID, riverID_mean, by=c('day', 'month', 'year'))
riverID<-filter(riverID, daily_NAVD88>5)

(relation_h<-ggplot(ID, aes(stage, daily_NAVD88))+geom_point()+
    geom_smooth(method='lm')+theme_minimal()+ggtitle("ID"))

summary(mod_h<-lm(formula =  stage ~ daily_NAVD88, data = ID))
#summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = ID))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(ID$stage_interpolated<-(Slope_h*ID$daily_NAVD88)+Intercept_h)
(riverID$stage<-(Slope_h*riverID$daily_NAVD88)+Intercept_h)

compare_h<-ggplot(ID, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)+
  theme_minimal()+ggtitle("ID")


(riverID$GPPavg<- SlopeGPPavg*riverID$stage+InterceptGPPavg)
riverID$GPPavg[riverID$GPPavg<0] <- 0

(riverID$ER<- SlopeER*riverID$stage+InterceptER)
riverID$ER[riverID$ER>=max(ID$ER, na.rm=T)] <- max(ID$ER, na.rm=T)

riverID$NEP<-riverID$GPPavg+riverID$ER


test<-left_join(ID, riverID, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
ggplot(test, aes(x=Date)) +
  geom_line(aes(y=ER.y, color="y"),size=1)+
  geom_line(aes(y=ER.x, color="x"),size=1)
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y
test<-filter(test, ratio_GPP<10)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_h<-test$stage.x/test$stage.y

(NEP_h<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y),size=1, color="red")+
    geom_line(aes(y=ER.x),size=1, color="black")+
    geom_line(aes(y=GPPavg.y),size=1, color="green")+
    geom_line(aes(y=GPPavg.x),size=1,color="black")+
    ylab('flux')+ggtitle('ID')+
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
plot_grid(NEP_h, bottom, nrow=2)


ggplot(riverID, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=1)+theme_minimal()



