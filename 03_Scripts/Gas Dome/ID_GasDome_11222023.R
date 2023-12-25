rm(list=ls())

library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(xlsx)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(dataRetrieval)

dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38

x<-c('Date','CO2')
k600_sheet1<- vector("numeric") 
stage_sheet1 <- vector("numeric") 
date_sheet1 <-as.Date(character(0))

k600_sheet2<- vector("numeric") 
stage_sheet2 <- vector("numeric") 
date_sheet2 <-as.Date(character(0))


setwd("Z:/SpringsProject_Sam&Paul/Master/chemistry")
spring <- read_excel("Ichetucknee.xlsx")
spring$CO2<-spring$CO2*6
spring<-spring %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date))
spring<-spring[,-1]

setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome")
file.names <- list.files(path="Ichetucknee", pattern="xlsx", full.names=TRUE)

for(i in file.names){  
  
  gas<-read_excel(i, sheet="Sheet1")
  gas<-gas[,x]
  gas$CO2<-gas$CO2*6
  gas<-gas %>% mutate(day=day(Date), hour=hour(Date))
  gas<-left_join(gas, spring,by=c('day', 'hour'))
  gas <- gas[!duplicated(gas[c('Date')]),]
  gas$day <- as.Date(gas$Date)
  date_sheet1[i]<-gas$day[1]
  
  mouthTemp_F<-mean(gas$Temp, na.rm=T)
  mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3
  
  pCO2_water<-	mean(gas$CO2_enviro, na.rm=T)/1000000
  depth<-mean(gas$stage, na.rm=T)
  stage_sheet1[i] <- depth  
  
  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope)*2/1000000) #change in CO2 during float
  pCO2_air<-max(gas$CO2)/1000000
  
  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000
  
  KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
  
  (KO2_1d<-kO2/depth)
  (KCO2_1d<-KCO2_md/depth)
  (k600_1d<- k600_md/depth)
  
  k600_sheet1[i] <- k600_1d }

sheet1<-data.frame(date_sheet1,k600_sheet1, stage_sheet1)
sheet1<- sheet1 %>% rename('date'="date_sheet1",
                           'k600'="k600_sheet1", 'stage'="stage_sheet1")

for(i in file.names){  
  
  gas<-read_excel(i, sheet="Sheet2")
  gas<-gas[,x]
  gas$CO2<-gas$CO2*6
  gas<-gas %>% mutate(day=day(Date), hour=hour(Date))
  gas<-left_join(gas, spring,by=c('day', 'hour'))
  gas <- gas[!duplicated(gas[c('Date')]),]
  gas$day <- as.Date(gas$Date)
  date_sheet2[i]<-gas$day[1]
  
  mouthTemp_F<-mean(gas$Temp, na.rm=T)
  mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3
  
  pCO2_water<-	mean(gas$CO2_enviro, na.rm=T)/1000000
  depth<-mean(gas$stage, na.rm=T)
  stage_sheet2[i] <- depth  
  
  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope)*2/1000000) #change in CO2 during float
  pCO2_air<-max(gas$CO2)/1000000
  
  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000
  
  KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
  
  (KO2_1d<-kO2/depth)
  (KCO2_1d<-KCO2_md/depth)
  (k600_1d<- k600_md/depth)
  
  k600_sheet2[i] <- k600_1d 
  
}

sheet2<-data.frame(date_sheet2,k600_sheet2)
sheet2<- sheet2 %>% rename('date'="date_sheet2", 'k6002'="k600_sheet2")

k600_ID<-left_join(sheet1, sheet2)
