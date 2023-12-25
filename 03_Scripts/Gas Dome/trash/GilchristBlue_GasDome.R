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

GilchristBlue <- read_excel("GilchristBlue.xlsx",
                            col_types = c("date", "numeric", "numeric",
                                                "numeric", "numeric", "numeric",
                                                "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric","numeric",
                                          "numeric","numeric",
                                          "numeric"))

GilchristBlue <- GilchristBlue %>% mutate(set = case_when(Date  <= '2022-06-06' ~ 1,
                                                          Date <= '2022-07-05' ~ 2,
                                                          Date  <= '2022-08-01' ~ 3,
                                                          Date  <= '2022-08-31' ~ 4,
                                                          Date  <= '2022-09-26' ~ 5,
                                                          Date  <= '2022-10-24' ~ 6,
                                                          Date  <= '2022-11-07' ~ 7,
                                                          Date  <= '2022-11-21' ~ 8,
                                                          Date  <= '2022-12-05' ~ 9,
                                                          Date  <= '2022-12-20' ~ 10,
                                                          Date  <= '2023-01-03 10:00' ~ 11,
                                                          Date  <= '2023-01-18 14:00' ~ 12,
                                                          Date  <= '2023-02-01 10:00' ~ 13,
                                                          Date  <= '2023-02-15 10:00' ~ 14,
                                                          Date  <= '2023-03-01 10:00' ~ 15,
                                                          Date  <= '2023-03-15 10:00' ~ 16,
                                                          Date  <= '2023-03-29 10:00' ~ 17,
                                                          Date  <= '2023-04-12 9:00' ~ 18,
                                                          Date  <= '2023-04-27 10:00' ~ 19,
                                                          Date  <= '2023-05-18 10:00' ~ 20,
                                                          Date  <= '2023-05-29 10:00' ~ 21))
GilchristBlue_0606<-filter(GilchristBlue, set==1)
GilchristBlue_0705<-filter(GilchristBlue, set==2)
GilchristBlue_0801<-filter(GilchristBlue, set==3)
GilchristBlue_0831<-filter(GilchristBlue, set==4)
GilchristBlue_0926<-filter(GilchristBlue, set==5)
GilchristBlue_1024<-filter(GilchristBlue, set==6)
GilchristBlue_1107<-filter(GilchristBlue, set==7)
GilchristBlue_1121<-filter(GilchristBlue, set==8)
GilchristBlue_1205<-filter(GilchristBlue, set==9)
GilchristBlue_1220<-filter(GilchristBlue, set==10)
GilchristBlue_0103<-filter(GilchristBlue, set==11)
GilchristBlue_0118<-filter(GilchristBlue, set==12)
GilchristBlue_0201<-filter(GilchristBlue, set==13)
GilchristBlue_0215<-filter(GilchristBlue, set==14)
GilchristBlue_0301<-filter(GilchristBlue, set==15)
GilchristBlue_0315<-filter(GilchristBlue, set==16)
GilchristBlue_0329<-filter(GilchristBlue, set==17)
GilchristBlue_0412<-filter(GilchristBlue, set==18)
GilchristBlue_0426<-filter(GilchristBlue, set==19)
GilchristBlue_0517<-filter(GilchristBlue, set==20)
GilchristBlue_0529<-filter(GilchristBlue, set==21)



setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Gilchrist Blue")
###########06062022#########

mouthTemp_F<-mean(GilchristBlue_0926$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-11656.55/1000000
depth<-0.8231176

GilchristBlue_09262022_m1<- read_excel("GilchristBlue_09262022.xlsx", 
                                       sheet = "mouthr1")
GilchristBlue_09262022_m1$CO2hi<-GilchristBlue_09262022_m1$CO2hi*6
(mouth1<-lm(CO2hi~Date, data = GilchristBlue_09262022_m1))
deltaCO2_atm<-(9.283)*6/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_09262022_m1$CO2hi)/1000000

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





########10242022#######
mouthTemp_F<-mean(GilchristBlue_1024$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-12239.29/1000000
depth<-0.4902573

GilchristBlue_10242022_v1 <- read_excel("GilchristBlue_10242022.xlsx",sheet = "vent r1")
GilchristBlue_10242022_v1$CO2<-GilchristBlue_10242022_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_10242022_v1))

deltaCO2_atm<-((2.021/30)*60/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_10242022_v1$CO2)/1000000

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



GilchristBlue_10242022_v2 <- read_excel("GilchristBlue_10242022.xlsx",sheet = "vent r2")
GilchristBlue_10242022_v2$CO2<-GilchristBlue_10242022_v2$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_10242022_v2))

deltaCO2_atm<-((2.339/30)*60 /1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_10242022_v1$CO2)/1000000

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

#1/d
#########11072022#####
mouthTemp_F<-mean(GilchristBlue_1107$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-10656.370/1000000
depth<-0.4514910

GilchristBlue_11072022_v1 <- read_excel("GilchristBlue_11072022.xlsx",sheet = "vent1")
GilchristBlue_11072022_v1$CO2<-GilchristBlue_11072022_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_11072022_v1))

deltaCO2_atm<-((7.731/30)*60/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_11072022_v1$CO2)/1000000

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


GilchristBlue_11072022_v2 <- read_excel("GilchristBlue_11072022.xlsx",sheet = "vent2")
GilchristBlue_11072022_v2$CO2<-GilchristBlue_11072022_v2$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_11072022_v2))

deltaCO2_atm<-((1.974e+01*2)/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_11072022_v2$CO2)/1000000

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






mouthTemp_F<-mean(GilchristBlue_1121$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-9764.393/1000000
depth<-0.4672603

GilchristBlue_11212022_v1 <- read_excel("GilchristBlue_11212022.xlsx",sheet = "Sheet1")
GilchristBlue_11212022_v1$CO2<-GilchristBlue_11212022_v1$CO2*6
(m<-lm(CO2~sec, data = GilchristBlue_11212022_v1))

deltaCO2_atm<-((2.194)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_11212022_v1$CO2)/1000000

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


GilchristBlue_11212022_v1 <- read_excel("GilchristBlue_11212022.xlsx",sheet = "Sheet2")
GilchristBlue_11212022_v1$CO2<-GilchristBlue_11212022_v1$CO2*6
(m<-lm(CO2~sec, data = GilchristBlue_11212022_v1))

deltaCO2_atm<-((13.6 )*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_11212022_v1$CO2)/1000000

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


mouthTemp_F<-mean(GilchristBlue_1205$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	11455.600/1000000
depth<-0.4380986

GilchristBlue_12052022_v1 <- read_excel("GilchristBlue_12052022.xlsx",sheet = "mouth1")
GilchristBlue_12052022_v1$CO2<-GilchristBlue_12052022_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_12052022_v1))

deltaCO2_atm<-((2.149)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_12052022_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_12052022_v1 <- read_excel("GilchristBlue_12052022.xlsx",sheet = "mouth2")
GilchristBlue_12052022_v1$CO2<-GilchristBlue_12052022_v1$CO2*6
(m<-lm(CO2~sec, data = GilchristBlue_12052022_v1))

deltaCO2_atm<-((3.015)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_12052022_v1$CO2)/1000000

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
(k600_1db<- k600_md/depth)

(k600_1da+k600_1db)/2






#############3
mouthTemp_F<-mean(GilchristBlue_1220$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	10459.750/1000000
depth<-0.4212485

GilchristBlue_12202022_v1 <- read_excel("GilchristBlue_12202022.xlsx",sheet = "Sheet1")
GilchristBlue_12202022_v1$CO2<-GilchristBlue_12202022_v1$CO2*6
(m<-lm(CO2~sec, data = GilchristBlue_12202022_v1))

deltaCO2_atm<-((1.201)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_12202022_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_12202022_v1 <- read_excel("GilchristBlue_12202022.xlsx",sheet = "Sheet2")
GilchristBlue_12202022_v1$CO2<-GilchristBlue_12202022_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_12202022_v1))

deltaCO2_atm<-((2.135)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_12202022_v1$CO2)/1000000

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
(k600_1db<- k600_md/depth)

(k600_1da+k600_1db)/2










#############3
(mouthTemp_F<-mean(GilchristBlue_0103$Temp, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	9919.908/1000000
depth<-0.4075028

GilchristBlue_01032023_v1 <- read_excel("GilchristBlue_01032023.xlsx",sheet = "r2")
GilchristBlue_01032023_v1$CO2<-GilchristBlue_01032023_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_01032023_v1))

deltaCO2_atm<-((3.766)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_01032023_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_01032023_v1 <- read_excel("GilchristBlue_01032023.xlsx",sheet = "r1")
GilchristBlue_01032023_v1$CO2<-GilchristBlue_01032023_v1$CO2*6
(m<-lm(CO2~...2, data = GilchristBlue_01032023_v1))

deltaCO2_atm<-((2.893)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_01032023_v1$CO2)/1000000

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
(k600_1db<- k600_md/depth)













#############3
(mouthTemp_F<-mean(GilchristBlue_0118$Temp, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8799.727/1000000
depth<-0.3699016

GilchristBlue_01182023_v1 <- read_excel("GilchristBlue_01182023.xlsx",sheet = "Sheet1")
GilchristBlue_01182023_v1$CO2<-GilchristBlue_01182023_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_01182023_v1))

deltaCO2_atm<-((2.788)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_01182023_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_01182023_v1 <- read_excel("GilchristBlue_01182023.xlsx",sheet = "Sheet2")
GilchristBlue_01182023_v1$CO2<-GilchristBlue_01182023_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_01182023_v1))

deltaCO2_atm<-((2.023)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_01182023_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)













#############3
(mouthTemp_F<-mean(GilchristBlue_0201$Temp, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8937.165/1000000
depth<-0.3934939
  
GilchristBlue_0201_v1 <- read_excel("GilchristBlue_02012023.xlsx",sheet = "Sheet1")
GilchristBlue_0201_v1$CO2<-GilchristBlue_0201_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0201_v1))

deltaCO2_atm<-((4.528)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0201_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_0201_v1 <- read_excel("GilchristBlue_02012023.xlsx",sheet = "Sheet2")
GilchristBlue_0201_v1$CO2<-GilchristBlue_0201_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0201_v1))

deltaCO2_atm<-((3.539)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0201_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)












View(GilchristBlue_0215)
#############
(mouthTemp_F<-mean(GilchristBlue_0215$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8056.021/1000000
depth<-0.3383848

GilchristBlue_0215_v1 <- read_excel("GilchristBlue_02152023.xlsx",sheet = "Sheet1")
GilchristBlue_0215_v1$CO2<-GilchristBlue_0215_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0215_v1))

deltaCO2_atm<-((4.922)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0215_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_0215_v1 <- read_excel("GilchristBlue_02152023.xlsx",sheet = "Sheet2")
GilchristBlue_0215_v1$CO2<-GilchristBlue_0215_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0215_v1))

deltaCO2_atm<-((2.259)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0215_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)











View(GilchristBlue_0301_v1)
#############
(mouthTemp_F<-mean(GilchristBlue_0301$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8957.649/1000000
depth<-0.3833544

GilchristBlue_0301_v1 <- read_excel("GilchristBlue_03012023.xlsx",sheet = "Sheet1")
GilchristBlue_0301_v1$CO2<-GilchristBlue_0301_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0301_v1))
summary(m)

deltaCO2_atm<-((2.462)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0301_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)


GilchristBlue_0301_v1 <- read_excel("GilchristBlue_03012023.xlsx",sheet = "Sheet2")
GilchristBlue_0301_v1$CO2<-GilchristBlue_0301_v1$CO2*6
(m<-lm(CO2~sec, data = GilchristBlue_0301_v1))

deltaCO2_atm<-((1.993)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0301_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)







(mouthTemp_F<-mean(GilchristBlue_0315$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8251.671/1000000
depth<-0.4153631

GilchristBlue_0315_v1 <- read_excel("GilchristBlue_03152023.xlsx",sheet = "Sheet1")
GilchristBlue_0315_v1$CO2<-GilchristBlue_0315_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0315_v1))
summary(m)

deltaCO2_atm<-((3.709)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0315_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)







(mouthTemp_F<-mean(GilchristBlue_0329$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	7701.022/1000000
depth<-0.3952735

GilchristBlue_0329_v1 <- read_excel("GilchristBlue_03292023.xlsx")
GilchristBlue_0329_v1$CO2<-GilchristBlue_0329_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_0329_v1))
summary(m)

deltaCO2_atm<-((4.632)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_0329_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)








(mouthTemp_F<-mean(GilchristBlue_0412$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8295.693/1000000
depth<-0.3481904
GilchristBlue_v1 <- read_excel("GilchristBlue_04122023.xlsx")
GilchristBlue_v1$CO2<-GilchristBlue_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_v1))

deltaCO2_atm<-((5.532)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)








(mouthTemp_F<-mean(GilchristBlue_0426$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	8511.282/1000000
depth<-0.3062842
GilchristBlue_v1 <- read_excel("GilchristBlue_04262023.xlsx")
GilchristBlue_v1$CO2<-GilchristBlue_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_v1))

deltaCO2_atm<-((7.31)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)








(mouthTemp_F<-mean(GilchristBlue_0517$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	7806.448/1000000
depth<-0.2980401
GilchristBlue_v1 <- read_excel("GilchristBlue_05172023.xlsx")
GilchristBlue_v1$CO2<-GilchristBlue_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_v1))

deltaCO2_atm<-((5.030)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)









(mouthTemp_F<-mean(GilchristBlue_0529$Temp, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-	7856.389/1000000
depth<-0.3406949
GilchristBlue_v1 <- read_excel("GilchristBlue_05292023_GD.xlsx")
GilchristBlue_v1$CO2<-GilchristBlue_v1$CO2*6
(m<-lm(CO2~Date, data = GilchristBlue_v1))

deltaCO2_atm<-((3.011)*2/1000000) #change in CO2 during float
pCO2_air<-max(GilchristBlue_v1$CO2)/1000000

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
(k600_1da<- k600_md/depth)
