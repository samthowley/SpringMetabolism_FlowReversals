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


#####OS#####
Otter <- read_excel("Otter.xlsx")

modGPPavg<-lm(GPPavg ~ stage, data = Otter)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER ~ stage, data = Otter)
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

ggplot(Otter, aes(stage, elevation))+geom_point()+geom_smooth(method='lm')

#summary(mod_h<-lm(formula =  stage ~ elevation, data = Otter))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = Otter))

cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(Otter$stage_interpolated<- (x2_h*Otter$elevation^2+Slope_h*Otter$elevation)+Intercept_h)
riverOS$stage<- (x2_h*riverOS$elevation^2)+(Slope_h*riverOS$elevation)+Intercept_h


ggplot(Otter, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)

(riverOS$GPPavg<- SlopeGPPavg*riverOS$stage+InterceptGPPavg)
riverOS$GPPavg[riverOS$GPPavg<0] <- 0
(riverOS$ER<- SlopeER*riverOS$stage+InterceptER)


ggplot(riverOS, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=0.4)+
  theme_minimal()

test<-left_join(Otter, riverOS, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y
test$ratio_ER<-test$ER.x/test$ER.y

(OS_ER<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y, color="interpolated"),size=1)+
    geom_line(aes(y=ER.x, color="model"),size=1)+theme_minimal()+
    ggtitle("LF, ER")+ylab("fllux"))

(OS_GPP<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=GPPavg.y, color="interpolated"),size=1)+
    geom_line(aes(y=GPPavg.x, color="model"),size=1)+theme_minimal()+
    ggtitle("LF, GPP")+ylab("fllux"))


(OS_ERratio<-ggplot(test, aes(x=Date)) +
  geom_point(aes(y=ratio_ER, color="ratio ER"),size=1)+theme_minimal()+
  ggtitle("Otter, ER")+ylab("Actual/Interpolated"))

test<-filter(test, ratio_GPP<10)
(OS_GPPratio<-ggplot(test, aes(x=Date)) +
  geom_point(aes(y=ratio_GPP, color="interpolated"),size=1)+
  ggtitle("Otter, GPP")+ylab("fllux"))+
  theme_minimal()

(OS_inter<-ggplot(riverOS, aes(x=Date)) +
    geom_line(aes(y=GPPavg, color="GPP"),size=1)+
    geom_line(aes(y=ER, color="ER"),size=1))+
  theme_minimal()+ggtitle("OS")

plot_grid(OS_ER, OS_GPP, OS_ERratio, OS_GPPratio, ncol=2)
####LF####

LF <- read_excel("LittleFanning.xlsx")

modGPPavg<-lm(GPPavg ~ stage, data = LF)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER ~ stage, data = LF)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])


site_id <- '02323500'
parameterCd <- '00065'
riverLF <- readNWISuv(site_id,parameterCd)
riverLF<-riverLF[,c(3,4)]
riverLF<-rename(riverLF, 'Date'='dateTime', 'elevation'='X_00065_00000')

riverLF<- riverLF %>%
  mutate(minute = minute(Date))

riverLF<-filter(riverLF, minute==0)
riverLF<-riverLF[,c(1,2)]

LF<-left_join(LF, riverLF)

summary(mod_h<-lm(formula =  stage ~ elevation, data = LF))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = LF))

mod_h<-lm(stage~elevation, data = LF)
cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(x2_h <- cf[3])
(riverLF$stage<- (Slope_h*riverLF$elevation)+Intercept_h)

(riverLF$GPPavg<- SlopeGPPavg*riverLF$stage+InterceptGPPavg)
riverLF$GPPavg[riverLF$GPPavg<0] <- 0
(riverLF$ER<- SlopeER*riverLF$stage+InterceptER)
riverLF$ER[riverLF$ER< -35] <- -35

ggplot(riverLF, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=0.4)

test<-left_join(LF, riverLF, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y
test$ratio_ER<-test$ER.x/test$ER.y

(LF_ER<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y, color="interpolated"),size=1)+
    geom_line(aes(y=ER.x, color="model"),size=1)+theme_minimal()+
    ggtitle("LF, ER")+ylab("fllux"))

(LF_GPP<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=GPPavg.y, color="interpolated"),size=1)+
    geom_line(aes(y=GPPavg.x, color="model"),size=1)+theme_minimal()+
    ggtitle("LF, GPP")+ylab("fllux"))

(LF_ERratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_ER, color="ratio ER"),size=1)+theme_minimal()+
    ggtitle("LF, ER")+ylab("Actual/Interpolated"))

test<-filter(test, ratio_GPP<10)
(LF_GPPratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_GPP, color="interpolated"),size=1)+
    theme_minimal()+
    ggtitle("LF, GPP")+ylab("fllux"))


(LF_inter<-ggplot(riverLF, aes(x=Date)) +
    geom_line(aes(y=GPPavg, color="GPP"),size=1)+
    geom_line(aes(y=ER, color="ER"),size=1))+
  theme_minimal()+ggtitle("LF")

plot_grid(LF_ER, LF_GPP, LF_ERratio, LF_GPPratio, ncol=2)


#Allen Mill####
AllenMill <- read_excel("AllenMill.xlsx")

modGPPavg<-lm(GPPavg ~ stage, data = AllenMill)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER ~ stage, data = AllenMill)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

site_id <- c('02320000','02319800')
parameterCd <- c('00065')
riverAM <- readNWISuv(site_id,parameterCd)
split<-split(riverAM, riverAM$site_no)

df02320000 <-split(riverAM, riverAM$site_no)[[2]]
df02320000<-df02320000[,c(3,4)]
df02320000<-rename(df02320000, 'Date'='dateTime', 'stage_down'='X_00065_00000')

df02319800 <-split(riverAM, riverAM$site_no)[[1]]
df02319800<-df02319800[,c(3,4)]
df02319800<-rename(df02319800, 'Date'='dateTime', 'stage_up'='X_00065_00000')

riverAM<-left_join(df02319800,df02320000, by='Date')

riverAM<- riverAM %>%mutate(minute = minute(Date))
riverAM<-filter(riverAM, minute==0)

riverAM$elevation<-(riverAM$stage_up-riverAM$stage_down)*0.5
riverAM<-riverAM[,c(1,5)]

AllenMill<-left_join(AllenMill, riverAM)

ggplot(AllenMill, aes(stage, elevation))+geom_point()+geom_smooth()

summary(mod_h<-lm(formula =  stage ~ elevation, data = AllenMill))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = AllenMill))

mod_h<-lm(stage~elevation, data = AllenMill)
cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(AllenMill$stage_interpolated<- (Slope_h*AllenMill$elevation)+Intercept_h)
(riverAM$stage<- (Slope_h*riverAM$elevation)+Intercept_h)


ggplot(AllenMill, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)

(riverAM$GPPavg<- SlopeGPPavg*riverAM$stage+InterceptGPPavg)
riverAM$GPPavg[riverAM$GPPavg<0] <- 0
(riverAM$ER<- SlopeER*riverAM$stage+InterceptER)


ggplot(riverAM, aes(x=Date)) +
  geom_line(aes(y=GPPavg, color="GPP"),size=1)+
  geom_line(aes(y=ER, color="ER"),size=0.4)

test<-left_join(AllenMill, riverAM, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y

(AM_ER<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y, color="interpolated"),size=1)+
    geom_line(aes(y=ER.x, color="model"),size=1)+theme_minimal()+
    ggtitle("AM, ER")+ylab("fllux"))

(AM_GPP<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=GPPavg.y, color="interpolated"),size=1)+
    geom_line(aes(y=GPPavg.x, color="model"),size=1)+theme_minimal()+
    ggtitle("AM, GPP")+ylab("fllux"))

(AM_ERratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_ER, color="ratio ER"),size=1)+theme_minimal()+
    ggtitle("AM, ER")+ylab("Actual/Interpolated"))

test<-filter(test, ratio_GPP<10)
(AM_GPPratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_GPP, color="interpolated"),size=1)+
    theme_minimal()+
    ggtitle("AM, GPP")+ylab("fllux"))

plot_grid(AM_ER, AM_GPP, AM_ERratio, AM_GPPratio, ncol=2)

(AM_inter<-ggplot(riverAM, aes(x=Date)) +
    geom_line(aes(y=GPPavg, color="GPP"),size=1)+
    geom_line(aes(y=ER, color="ER"),size=1))+
  theme_minimal()+ggtitle("AM")
####GB#####

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
riverGB<- riverGB %>%
  mutate(minute = minute(Date))
riverGB<-filter(riverGB, minute==0)

riverGB$elevation<-(riverGB$stage_up-riverGB$stage_down)*0.79
riverGB<-riverGB[,c(1,5)]

GB<-read_excel('GilchristBlue.xlsx')
GB<-left_join(GB, riverGB)

modGPPavg<-lm(GPPavg ~ stage, data = GB)
cf <- coef(modGPPavg)
(InterceptGPPavg <- cf[1])
(SlopeGPPavg <- cf[2])

modER<-lm(ER ~ stage, data = GB)
cf <- coef(modER)
(InterceptER <- cf[1])
(SlopeER <- cf[2])

summary(mod_h<-lm(formula =  stage ~ elevation, data = GB))
summary(mod_h<-lm(formula =  stage ~ elevation + I(elevation^2), data = GB))

mod_h<-lm(formula =  stage ~ elevation, data = GB)
cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])
(riverGB$stage<- (Slope_h*riverGB$elevation)+Intercept_h)
(GB$stage_interpolated<- (Slope_h*GB$elevation)+Intercept_h)


ggplot(GB, aes(x=Date)) +
  geom_line(aes(y=stage, color="h"),size=1)+
  geom_line(aes(y=stage_interpolated, color="interpolated"),size=0.4)

(riverGB$GPPavg<- SlopeGPPavg*riverGB$stage+InterceptGPPavg)
riverGB$GPPavg[riverGB$GPPavg<0] <- 0
(riverGB$ER<- SlopeER*riverGB$stage+InterceptER)
riverGB$ER[riverGB$ER< -32] <- -32

test<-left_join(GB, riverGB, by='Date')
x<-c('Date','stage.x','GPPavg.x','ER.x','stage.y','GPPavg.y','ER.y')
test<-test[,x]
test<-na.omit(test)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y

(GB_ER<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y, color="interpolated"),size=1)+
    geom_line(aes(y=ER.x, color="model"),size=1)+theme_minimal()+
    ggtitle("GB, ER")+ylab("fllux"))

(GB_GPP<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=GPPavg.y, color="interpolated"),size=1)+
    geom_line(aes(y=GPPavg.x, color="model"),size=1)+theme_minimal()+
    ggtitle("GB, GPP")+ylab("fllux"))

(GB_ERratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_ER, color="ratio ER"),size=1)+theme_minimal()+
    ggtitle("GB, ER")+ylab("Actual/Interpolated"))

test<-filter(test, ratio_GPP<10)
(GB_GPPratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_GPP, color="interpolated"),size=1)+
    theme_minimal()+
    ggtitle("GB, GPP")+ylab("fllux"))

plot_grid(GB_ER, GB_GPP, GB_ERratio, GB_GPPratio, ncol=2)

(GB_inter<-ggplot(riverGB, aes(x=Date)) +
    geom_line(aes(y=GPPavg, color="GPP"),size=1)+
    geom_line(aes(y=ER, color="ER"),size=1))+
  theme_minimal()+ggtitle("GB")

###ID#####

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

mod_h<-lm(stage~daily_NAVD88, data = ID)
cf <- coef(mod_h)
(Intercept_h<- cf[1])
(Slope_h <- cf[2])


(riverID$stage<- (Slope_h*riverID$daily_NAVD88)+Intercept_h)
riverID$stage<-gaussianSmooth(riverID$stage, 2)

(riverID$GPPavg<- SlopeGPPavg*riverID$stage+InterceptGPPavg)
riverID$GPPavg[riverID$GPPavg<0] <- 0

(riverID$ER<- SlopeER*riverID$stage+InterceptER)
riverID$ER[riverID$ER>=max(ID$ER, na.rm=T)] <- max(ID$ER, na.rm=T)

riverID$NEP<-riverID$GPPavg+riverID$ER


test<-left_join(ID, riverID, by='Date')
test<-test[,c(1,10,13,14,27,28,29)]
test<-na.omit(test)
test$ratio_ER<-test$ER.x/test$ER.y
test$ratio_GPP<-test$GPPavg.x/test$GPPavg.y

(ID_ER<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=ER.y, color="interpolated"),size=1)+
    geom_line(aes(y=ER.x, color="model"),size=1)+theme_minimal()+
    ggtitle("ID, ER")+ylab("fllux"))

(ID_GPP<-ggplot(test, aes(x=Date)) +
    geom_line(aes(y=GPPavg.y, color="interpolated"),size=1)+
    geom_line(aes(y=GPPavg.x, color="model"),size=1)+theme_minimal()+
    ggtitle("ID, GPP")+ylab("fllux"))


(ID_ERratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_ER, color="ratio ER"),size=1)+theme_minimal()+
    ggtitle("ID, ER")+ylab("Actual/Interpolated"))

test<-filter(test, ratio_GPP<10)
(ID_GPPratio<-ggplot(test, aes(x=Date)) +
    geom_point(aes(y=ratio_GPP, color="interpolated"),size=1)+
    theme_minimal()+
    ggtitle("ID, GPP")+ylab("fllux"))

plot_grid(ID_ER, ID_GPP, ID_ERratio, ID_GPPratio, ncol=2)

(ID_inter<-ggplot(riverID, aes(x=Date)) +
    geom_line(aes(y=GPPavg, color="GPP"),size=1)+
    geom_line(aes(y=ER, color="ER"),size=1))+
  theme_minimal()+ggtitle("ID")
