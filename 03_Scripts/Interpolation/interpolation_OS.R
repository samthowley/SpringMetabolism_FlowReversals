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

Otter <- read_excel("Otter.xlsx")

Otter<-
  Otter %>%  mutate(day = day(Date), 
                    Month = month(Date),
                    year=year(Date)) %>%
  mutate( GPP=rollmean(GPPavg, k=7, fill=NA, align='left'),
          ER_avg=rollmean(ER, k=7, fill=NA, align='left'))

modGPPavg<-lm(GPP ~ stage, data = Otter)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER_avg ~ stage, data = Otter)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

site_id <- c('02323000','02323500')
parameterCd <- c('00065')
riverOS <- readNWISuv(site_id,parameterCd)
split<-split(riverOS, riverOS$site_no)

df02323000 <-split(riverOS, riverOS$site_no)[[1]]
df02323000<-df02323000[,c(3,4)]
df02323000<-rename(df02323000, 'Date'='dateTime', 'stage_down'='X_00065_00000')

df02323500 <-split(riverOS, riverOS$site_no)[[2]]
df02323500<-df02323500[,c(3,4)]
df02323500<-rename(df02323500, 'Date'='dateTime', 'stage_up'='X_00065_00000')

riverOS<-left_join(df02323500,df02323000, by='Date')

riverOS<- riverOS %>% mutate(minute = minute(Date))
riverOS<-filter(riverOS, minute==0)

riverOS$elevation<-(riverOS$stage_up-riverOS$stage_down)*0.72
riverOS<-riverOS[,c(1,5)]
Otter<-left_join(Otter, riverOS)


relation_h<-ggplot(Otter, aes(stage, elevation))+geom_point()+
  geom_smooth(method='lm')+theme_minimal()+ggtitle("OS")


summary(mod_h<-lm(formula =  stage ~ elevation, data = Otter))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = Otter))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(Otter$stage_interpolated<- (x2_h*Otter$elevation^2+Slope_h*Otter$elevation)+Intercept_h)
(riverOS$stage<- (x2_h*riverOS$elevation^2+Slope_h*riverOS$elevation)+Intercept_h)


compare_h<-ggplot(Otter, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)+
  theme_minimal()+ggtitle("OS")

(riverOS$GPPavg<- SlopeGPPavg*riverOS$stage+InterceptGPPavg)
riverOS$GPPavg[riverOS$GPPavg<0] <- 0
(riverOS$ER<- SlopeER*riverOS$stage+InterceptER)


test<-left_join(Otter, riverOS, by='Date')
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
  ylab('flux')+ggtitle('OS')+
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


ggplot(riverOS, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=1)+theme_minimal()



