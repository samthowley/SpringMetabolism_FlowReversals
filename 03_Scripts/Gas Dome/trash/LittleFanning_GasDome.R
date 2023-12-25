
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

setwd("Z:/SpringsProject_Sam&Paul/Master/chemistry")
LittleFanning <- read_excel("LittleFanning.xlsx",
                            col_types = c("date", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", 
                                          "numeric", "numeric", "numeric",
                                          "numeric","numeric"))
LittleFanning <- LittleFanning %>% mutate(set = case_when(Date  <= '2022-06-13 11:00' ~ 1,
                                                          Date <= '2022-07-08 10:00' ~ 2,
                                                          Date  <= '2022-08-24 14:00' ~ 3,
                                                          Date  <= '2022-09-19 13:00' ~ 4,
                                                          Date  <= '2022-10-31 13:00' ~ 5,
                                                          Date  <= '2022-11-14 12:00' ~ 7,
                                                          Date  <= '2022-11-28 12:00' ~ 8,
                                                          Date  <= '2022-12-12 12:00' ~ 9,
                                                          Date  <= '2023-01-04 13:00' ~ 10,
                                                          Date  <= '2023-01-11 12:00' ~ 11,
                                                          Date  <= '2023-01-25 12:00' ~ 12,
                                                          Date  <= '2023-02-06 12:00' ~ 13,
                                                          Date  <= '2023-02-22 12:00' ~ 14,
                                                          Date  <= '2023-03-08 12:00' ~ 15,
                                                          Date  <= '2023-03-20 12:00' ~ 16,
                                                          Date  <= '2023-04-05 11:00' ~ 17,
                                                          Date  <= '2023-04-19 12:00' ~ 18,
                                                          Date  <= '2023-05-04 12:00' ~ 19,
                                                          Date  <= '2023-05-17 12:00' ~ 20,
                                                          Date  <= '2023-05-30 12:00' ~ 21,
                                                          Date  <= '2023-06-12 12:00' ~ 22))


LF_1031<-filter(LittleFanning, set==5)
LF_1114<-filter(LittleFanning, set==7)
LF_1128<-filter(LittleFanning, set==8)
LF_1212<-filter(LittleFanning, set==9)
LF_0103<-filter(LittleFanning, set==10)
LF_0111<-filter(LittleFanning, set==11)
LF_0125<-filter(LittleFanning, set==12)
LF_0206<-filter(LittleFanning, set==13)
LF_0222<-filter(LittleFanning, set==14)
LF_0308<-filter(LittleFanning, set==15)
LF_0320<-filter(LittleFanning, set==16)
LF_0405<-filter(LittleFanning, set==17)
LF_0419<-filter(LittleFanning, set==18)
LF_0503<-filter(LittleFanning, set==19)
LF_0517<-filter(LittleFanning, set==20)
LF_0530<-filter(LittleFanning, set==21)
LF_0612<-filter(LittleFanning, set==22)


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/LittleFanning")

mouthTemp_F<-mean(LF_1031$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-15023.62/1000000
depth<-0.3879644

LittleFanning_10312022 <- read_excel("LittleFanning_10312022.xlsx",
                                     sheet="mouth1")
LittleFanning_10312022$CO2<-LittleFanning_10312022$CO2*6
(mouth2<-lm(CO2~Date, data = LittleFanning_10312022))

deltaCO2_atm<-(2.060*2)/1000000#change in CO2 during float
pCO2_air<-max(LittleFanning_10312022$CO2)/1000000
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



LittleFanning_10312022 <- read_excel("LittleFanning_10312022.xlsx",
                                     sheet="mouth 2")
LittleFanning_10312022$CO2<-LittleFanning_10312022$CO2*6
(mouth2<-lm(CO2~Date, data = LittleFanning_10312022))

deltaCO2_atm<-(1.679*2)/1000000#change in CO2 during float
pCO2_air<-max(LittleFanning_10312022$CO2)/1000000
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





View(LF_1114)
mouthTemp_F<-mean(LF_1114$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7500/1000000
depth<-0.2578767

LF_11142022 <- read_excel("LittleFanning_11142022.xlsx",sheet = "mouth2")
LF_11142022$CO2<-LF_11142022$CO2*6
(mouth2<-lm(CO2~Date, data = LF_11142022))

deltaCO2_atm<-(9.589)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_11142022$CO2)/1000000
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

LF_11142022 <- read_excel("LittleFanning_11142022.xlsx",sheet = "mouth1")
LF_11142022$CO2<-LF_11142022$CO2*6
(mouth2<-lm(CO2~Date, data = LF_11142022))

deltaCO2_atm<-(7.114)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_11142022$CO2)/1000000
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





View(LF_1128)
mouthTemp_F<-mean(LF_1128$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-8754.00/1000000
depth<-0.3580278

LF_11282022 <- read_excel("LittleFanning_11282022.xlsx",sheet = "Sheet1")
LF_11282022$CO2<-LF_11282022$CO2*6
(mouth2<-lm(CO2~Date, data = LF_11282022))

deltaCO2_atm<-(6.943)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_11282022$CO2)/1000000
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

LF_11282022 <- read_excel("LittleFanning_11282022.xlsx",sheet = "Sheet2")
LF_11282022$CO2<-LF_11282022$CO2*6
(mouth2<-lm(CO2~Date, data = LF_11282022))

deltaCO2_atm<-(7.458)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_11282022$CO2)/1000000
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
############


View(LF_1212)
mouthTemp_F<-mean(LF_1212$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6876.0/1000000
depth<-0.2758665

LF_12122022 <- read_excel("LittleFanning_12122022.xlsx",sheet = "Sheet1")
LF_12122022$CO2<-LF_12122022$CO2*6
(mouth2<-lm(CO2~sec, data = LF_12122022))

deltaCO2_atm<-(1.591 )*2/1000000#change in CO2 during float
pCO2_air<-max(LF_12122022$CO2)/1000000
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


LF_12122022 <- read_excel("LittleFanning_12122022.xlsx",sheet = "Sheet2")
LF_12122022$CO2<-LF_12122022$CO2*6
(mouth2<-lm(CO2~Date, data = LF_12122022))

deltaCO2_atm<-(2.549)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_12122022$CO2)/1000000
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








View(LF_0103)
mouthTemp_F<-mean(LF_0103$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6450/1000000
depth<-0.2279499

LF_01032023 <- read_excel("LittleFanning_01032023.xlsx",sheet = "Sheet1")
LF_01032023$CO2<-LF_01032023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01032023))

deltaCO2_atm<-(2.510)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01032023$CO2)/1000000
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


LF_01032023 <- read_excel("LittleFanning_01032023.xlsx",sheet = "Sheet2")
LF_01032023$CO2<-LF_01032023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01032023))

deltaCO2_atm<-(2.569)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01032023$CO2)/1000000
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











View(LF_0111)
mouthTemp_F<-mean(LF_0111$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-4535.4/1000000

LF_01112023 <- read_excel("LittleFanning_01112023.xlsx",sheet = "Sheet1")
LF_01112023$CO2<-LF_01112023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01112023))

deltaCO2_atm<-(6.895)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01112023$CO2)/1000000
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

LF_01112023 <- read_excel("LittleFanning_01112023.xlsx",sheet = "Sheet2")
LF_01112023$CO2<-LF_01112023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01112023))

deltaCO2_atm<-(6.146)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01112023$CO2)/1000000
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











View(LF_0125)
mouthTemp_F<-mean(LF_0125$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7980.0/1000000
depth<-0.3665723

LF_01252023 <- read_excel("LittleFanning_01252023.xlsx",sheet = "Sheet1")
LF_01252023$CO2<-LF_01252023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01252023))

deltaCO2_atm<-(2.189)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01252023$CO2)/1000000
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

LF_01252023 <- read_excel("LittleFanning_01252023.xlsx",sheet = "Sheet2")
LF_01252023$CO2<-LF_01252023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_01252023))

deltaCO2_atm<-(2.138)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_01252023$CO2)/1000000
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





View(LF_0206)
mouthTemp_F<-mean(LF_0206$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6846.0/1000000
depth<-0.3938168

LF_02062023 <- read_excel("LittleFanning_02062023.xlsx",sheet = "Sheet1")
LF_02062023$CO2<-LF_02062023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_02062023))

deltaCO2_atm<-(8.094e-01)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_02062023$CO2)/1000000
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

LF_02062023 <- read_excel("LittleFanning_02062023.xlsx",sheet = "Sheet2")
LF_02062023$CO2<-LF_02062023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_02062023))

deltaCO2_atm<-(1.263)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_02062023$CO2)/1000000
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






View(LF_0222)
mouthTemp_F<-mean(LF_0222$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-14952/1000000
depth<-0.8988637

LF_02222023 <- read_excel("LittleFanning_02222023.xlsx",sheet = "Sheet1")
LF_02222023$CO2<-LF_02222023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_02222023))

deltaCO2_atm<-(2.068)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_02062023$CO2)/1000000
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

LF_02222023 <- read_excel("LittleFanning_02222023.xlsx",sheet = "Sheet2")
LF_02222023$CO2<-LF_02222023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_02222023))

deltaCO2_atm<-(1.882)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_02062023$CO2)/1000000
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





View(LF_0308)
mouthTemp_F<-mean(LF_0308$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-12732/1000000
depth<-0.6721763

LF_003382023 <- read_excel("LittleFanning_03082023.xlsx",sheet = "Sheet1")
LF_003382023$CO2<-LF_003382023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_003382023))

deltaCO2_atm<-(1.704)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_003382023$CO2)/1000000
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






View(LF_0405)
mouthTemp_F<-mean(LF_0405$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-13056/1000000
depth<-0.5215242

LF_04052023 <- read_excel("LittleFanning_04052023.xlsx",sheet = "Sheet1")
LF_04052023$CO2<-LF_04052023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04052023))

deltaCO2_atm<-(2.466)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04052023$CO2)/1000000
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

LF_04052023 <- read_excel("LittleFanning_04052023.xlsx",sheet = "Sheet2")
LF_04052023$CO2<-LF_04052023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04052023))

deltaCO2_atm<-(1.811)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04052023$CO2)/1000000
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







View(LF_0405)
mouthTemp_F<-mean(LF_0419$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-8082/1000000
depth<-0.3866326

LF_04192023 <- read_excel("LittleFanning_04192023.xlsx",sheet = "Sheet1")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(2.534)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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

LF_04192023 <- read_excel("LittleFanning_04192023.xlsx",sheet = "Sheet2")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(1.326)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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







View(LF_0503)
mouthTemp_F<-mean(LF_0503$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6894/1000000
depth<-0.3272159

LF_04192023 <- read_excel("LittleFanning_05032023.xlsx",sheet = "Sheet1")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(3.030)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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

LF_04192023 <- read_excel("LittleFanning_05032023.xlsx",sheet = "Sheet2")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(2.715)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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







View(LF_0503)
mouthTemp_F<-mean(LF_0517$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-4760.4/1000000
depth<-0.2902947

LF_04192023 <- read_excel("LittleFanning_05172023.xlsx",sheet = "Sheet1")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(3.525)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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







View(LF_0503)
mouthTemp_F<-mean(LF_0530$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-5214.0/1000000
depth<-0.2766577

LF_04192023 <- read_excel("LittleFanning_05302023.xlsx",sheet = "Sheet1")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(3.182)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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







View(LF_0612)
mouthTemp_F<-mean(LF_0612$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6696.0/1000000
depth<-0.3043318

LF_04192023 <- read_excel("LittleFanning_06122023.xlsx",sheet = "Sheet1")
LF_04192023$CO2<-LF_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = LF_04192023))

deltaCO2_atm<-(1.594)*2/1000000#change in CO2 during float
pCO2_air<-max(LF_04192023$CO2)/1000000
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
