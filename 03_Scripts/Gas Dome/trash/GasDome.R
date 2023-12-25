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


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
######
vent07202022 <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/AllenMill/DO/ROVING_AllenMill_DO_07202022.xlsx")
Temp_F<-mean(vent07202022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3

roving_07202022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/AllenMill/AllenMill_07202022.xlsx")
pCO2_water<-max(roving_07202022$CO2hi)*4.8/1000000

AllenMill_07202022_v1<- read_excel("AllenMill_07202022.xlsx",sheet = "vent r1")
AllenMill_07202022_v1$CO2hi<-AllenMill_07202022_v1$CO2hi
vent1<-lm(CO2hi~Date, data = AllenMill_07202022_v1)

deltaCO2_atm<-(3.047)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_07202022_v1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.585

KCO2_1d_07202022_v1<-KCO2_md/0.585 #1/d
KO2_1d_07202022_v1<-kO2/0.585#1/d
##############################################
AllenMill_07202022_v2<- read_excel("AllenMill_07202022.xlsx",sheet = "vent r2")
AllenMill_07202022_v2$CO2hi<-AllenMill_07202022_v2$CO2hi
vent1<-lm(CO2hi~Date, data = AllenMill_07202022_v2)

deltaCO2_atm<-((2.169))/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_07202022_v2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.585

KCO2_1d_07202022_v2<-KCO2_md/0.585 #1/d
KO2_1d_07202022_v2<-kO2/0.585 #1/d
##############################################
SpC_07202022 <- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Allen Mill/AllenMillPond_SpC_07202022.csv")
Temp_F<-mean(SpC_07202022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3
View(AllenMill_07202022)
AllenMill_07202022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/AllenMill/edited/AllenMill_08012022.xlsx")
AllenMill_07202022$CO2hi<-(AllenMill_08012022$CO2hi/5.6829)-375.97
pCO2_water<-mean(AllenMill_07202022$CO2hi)/1000000

AllenMill_07202022_m1<- read_excel("AllenMill_07202022.xlsx",sheet = "mouth r1")
AllenMill_07202022_m1$CO2hi<-AllenMill_07202022_m1$CO2hi
vent1<-lm(CO2hi~Date, data = AllenMill_07202022_m1)

deltaCO2_atm<-((1.673)/1000000) #change in CO2 during float
pCO2_air<-max(AllenMill_07202022_m1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.46908549

KCO2_1d_07202022_m1<-KCO2_md/0.585 #1/d
KO2_1d_07202022_m1<-kO2/0.585 #1/d

########################################
AllenMill_07202022_m2<- read_excel("AllenMill_07202022.xlsx",sheet = "mouth r2")
AllenMill_07202022_m2$CO2hi<-AllenMill_07202022_m2$CO2hi
vent1<-lm(CO2hi~Date, data = AllenMill_07202022_m2)

deltaCO2_atm<-(1.978)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_07202022_m2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.562248769

KCO2_1d_07202022_m2<-KCO2_md/0.585 #1/d
KO2_1d_07202022_m2<-kO2/0.585 #1/d
###############################
vent0816022 <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/AllenMill/DO/ROVING_AllenMill_DO_08152022.xlsx")
Temp_F<-mean(vent0816022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3

roving_08162022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/AllenMill/AllenMill_08152022.xlsx")
pCO2_water<-max(roving_08162022$CO2hi)*4.8/1000000

AllenMill_08162022_v1<- read_excel("AllenMill_08162022.xlsx",sheet = "vent_r1")
AllenMill_08162022_v1$CO2hi<-AllenMill_08162022_v1$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_08162022_v1)

deltaCO2_atm<-((5.823 /10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_08162022_v1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.562248769

KCO2_1d_08162022_v1<-KCO2_md/0.75 #1/d
KO2_1d_08162022_v1<-kO2/0.75 #1/d
##############################################
AllenMill_08162022_v2<- read_excel("AllenMill_08162022.xlsx",sheet = "vent_r2")
AllenMill_08162022_v2$CO2hi<-AllenMill_08162022_v2$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_08162022_v2)

deltaCO2_atm<-((4.691/10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_08162022_v2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.75

KCO2_1d_08162022_v2<-KCO2_md/0.75 #1/d
KO2_1d_08162022_v2<-kO2/0.75 #1/d
##############################################
SpC_08152022 <- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Allen Mill/AllenMillPond_SpC_08152022.csv")
Temp_F<-mean(SpC_08152022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3

AllenMill_08162022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/AllenMill/edited/AllenMill_08152022.xlsx")
AllenMill_08162022$CO2hi<-(AllenMill_08162022$CO2hi/5.6829)-375.97
pCO2_water<-mean(AllenMill_08162022$CO2hi)/1000000

AllenMill_08152022_m1<- read_excel("AllenMill_08162022.xlsx",sheet = "mouth_r1")
AllenMill_08152022_m1$CO2hi<-AllenMill_08152022_m1$CO2hi
vent1<-lm(CO2hi~Date, data = AllenMill_08152022_m1)

deltaCO2_atm<-((2.764/10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_08152022_m1$CO2hi*4.8)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.46908549

KCO2_1d_08162022m1<-KCO2_md/0.75 #1/d
KO2_1d_08162022m1<-kO2/0.75 #1/d

########################################
AllenMill_08152022_m2<- read_excel("AllenMill_08162022.xlsx",sheet = "mout_r2")
AllenMill_08152022_m2$CO2hi<-AllenMill_08152022_m2$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_08152022_m2)

deltaCO2_atm<-((2.075/10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_08152022_m2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.562248769

KCO2_1d_08152022_m2<-KCO2_md/0.75 #1/d
KO2_1d_08152022_m2<-kO2/0.75 #1/d

#####################################################
vent0912022 <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/AllenMill/SpC/ROVING_SpC_09122022.xlsx")
Temp_F<-mean(vent0912022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3


roving_09122022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/AllenMill/AllenMill_09122022.xlsx")
pCO2_water<-max(roving_09122022$CO2hi)*6/1000000


AllenMill_09122022_v1<- read_excel("AllenMill_09122022.xlsx",sheet = "ventr1")
AllenMill_09122022_v1$CO2hi<-AllenMill_09122022_v1$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_09122022_v1)

deltaCO2_atm<-((4.658e+01 /10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_09122022_v1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.4

KCO2_1d_09122022v1<-KCO2_md/0.4 #1/d
KO2_1d_09122022v1<-kO2/0.4 #1/d
##############################################
AllenMill_09122022_v2<- read_excel("AllenMill_09122022.xlsx",sheet = "ventr2")
AllenMill_09122022_v2$CO2hi<-AllenMill_09122022_v2$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_09122022_v2)

deltaCO2_atm<-((2.452e+01/10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_09122022_v2$CO2hi*4.8)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.4

KCO2_1d_09122022v2<-KCO2_md/0.4 #1/d
KO2_1d_09122022v2<-kO2/0.4 #1/d
##############################################
SpC_09122022 <- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Allen Mill/AllenMillPond_SpC_09122022.csv")
Temp_F<-mean(SpC_09122022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3

AllenMill_09122022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/AllenMill/edited/AllenMill_09122022.xlsx")
AllenMill_09122022$CO2hi<-AllenMill_09122022$CO2hi*13
pCO2_water<-mean(AllenMill_09122022$CO2hi)/1000000

AllenMill_09122022_m1<- read_excel("AllenMill_09122022.xlsx",sheet = "mouthr2")
AllenMill_09122022_m1$CO2hi<-AllenMill_09122022_m1$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_09122022_m1)

deltaCO2_atm<-((6.831/10)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_09122022_m1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.4

KCO2_1d_09122022m<-KCO2_md/0.4 #1/d
KO2_1d_09122022m<-kO2/0.4 #1/d

###################################################
vent101022 <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/AllenMill/SpC/ROVING_SpC_09262022.xlsx")
Temp_F<-mean(vent101022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3

roving_10102022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/AllenMill/AllenMill_10102022.xlsx")
pCO2_water<-max(roving_10102022$CO2*4.8)/1000000

AllenMill_10102022_v1<- read_excel("AllenMill_10102022.xlsx",sheet = "vent r1")
AllenMill_10102022_v1$CO2<-AllenMill_10102022_v1$CO2*6
vent1<-lm(CO2~Date, data = AllenMill_10102022_v1)

deltaCO2_atm<-((1.357/30)*60)/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_10102022_v1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/Temp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/Temp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/0.32

KCO2_1d_10102022<-KCO2_md/0.32 #1/d
KO2_1d_10102022<-kO2/0.32 #1/d
###################################################
SpC_1012022 <- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Allen Mill/AllenMillPond_SpC_10102022.csv")
Temp_F<-mean(SpC_1012022$Temp)
Temp_C<-fahrenheit.to.celsius(Temp_F)
Temp_K<-Temp_C+273.15
SchmidtO2hi<-1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3
SchmidtCO2hi<-1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3
View(AllenMill_10102022)
AllenMill_10102022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/AllenMill/edited/AllenMill_10102022.xlsx")
AllenMill_10102022$CO2hi<-AllenMill_10102022$CO2hi*6
pCO2_water<-mean(AllenMill_10102022$CO2hi)/1000000

AllenMill_10102022_m1<- read_excel("AllenMill_10102022.xlsx",sheet = "mouthr2")
AllenMill_09122022_m1$CO2hi<-AllenMill_09122022_m1$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = AllenMill_09122022_m1)



setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Gilchrist Blue")

ventTemp <- read_xlsx("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO/ROVING_DO_06062022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent06062022<-read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/GilchristBlue/GilchristBlue_06172022.xlsx")
pCO2_water<-(max(vent06062022$CO2hi)*4.8)/1000000

GilchristBlue_60602022_ventr1 <- read_excel("GilchristBlue_60602022.xlsx",sheet = "vent r1")
GilchristBlue_60602022_ventr1$CO2hi<-GilchristBlue_60602022_ventr1$CO2hi*4.8
vent1<-lm(CO2hi~Date, data = GilchristBlue_60602022_ventr1)
deltaCO2_atm<-(2.149)/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_60602022_ventr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d_0606v1<-KCO2_md/0.4 #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606v1<-kO2/0.4
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/0.4


GilchristBlue_60602022_ventr2 <- read_excel("GilchristBlue_60602022.xlsx",sheet = "vent r2")
GilchristBlue_60602022_ventr2$CO2hi<-GilchristBlue_60602022_ventr2$CO2hi*4.8
vent2<-lm(CO2hi~Date, data = GilchristBlue_60602022_ventr2)
deltaCO2_atm<-(-2.322)*-1/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_60602022_ventr2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/0.4
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/0.4

KO2_1d_0606v2<-kO2/0.4
KCO2_1d_0606v2<-KCO2_md/0.4 #1/d

#####################
mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/GilchristBlue/DO/GilchristBlue_DO_06062022.csv")
mouthTemp_F<-mean(mouthtemp$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

mouth06062022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited/GilchristBlue_06062022.xlsx")
pCO2_water<-mean(mouth06062022$CO2hi)/1000000

GilchristBlue_60602022_m <- read_excel("GilchristBlue_60602022.xlsx",sheet = "mouth")
GilchristBlue_60602022_m$CO2hi<-GilchristBlue_60602022_m$CO2hi*4.2
m<-lm(CO2hi~Date, data = GilchristBlue_60602022_m)

deltaCO2_atm<-(2.071)/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_60602022_m$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0705v1<-kO2/0.4
KCO2_1d_0705v1<-KCO2_md/0.4 #1/d

####
ventTemp <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO/ROVING_DO_07052022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent07052022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/GilchristBlue/GilchristBlue_07052022.xlsx")
vent07052022$CO2hi<-vent07052022$CO2hi*4.8
pCO2_water<-max(vent07052022$CO2hi)/1000000

GilchristBlue_07052022_ventr1 <- read_excel("GilchristBlue_07052022.xlsx", sheet = "vent r1")
GilchristBlue_07052022_ventr1$CO2hi<-GilchristBlue_07052022_ventr1$CO2hi*4.2
vent1<-lm(CO2hi~Date, data = GilchristBlue_07052022_ventr1)
deltaCO2_atm<-(1.121)/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_07052022_ventr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0705v1<-kO2/0.55
KCO2_1d_0705v1<-KCO2_md/0.55 #1/d

GilchristBlue_07052022_vent2 <- read_excel("GilchristBlue_07052022.xlsx", sheet = "vent r2")
GilchristBlue_07052022_vent2$CO2hi<-GilchristBlue_07052022_vent2$CO2hi*4.2
vent2<-lm(CO2hi~Date, data = GilchristBlue_07052022_vent2)
pCO2_air<-max(GilchristBlue_07052022_vent2$CO2hi)/1000000
deltaCO2_atm<-(1.109)/1000000 #change in CO2 during float

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0705v2<-kO2/0.55
KCO2_1d_0705v2<-KCO2_md/0.55 #1/d

#mouth
mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/GilchristBlue/DO/GilchristBlue_DO_07052022.csv")
mouthTemp_F<-mean(mouthtemp$Temp)
mouthTemp_C<-fahrenheit.to.celsius(mean(mouthtemp$Temp))
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

GilchristBlue_07052022_mouthr1 <- read_excel("GilchristBlue_07052022.xlsx", sheet = "mouth r1")
GilchristBlue_07052022_mouthr1$CO2hi<-GilchristBlue_07052022_mouthr1$CO2hi*4.2
mouth1<-lm(CO2hi~Date, data = GilchristBlue_07052022_mouthr1)
deltaCO2_atm<-(5.729)/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_07052022_mouthr1$CO2hi)/1000000

mouth07052022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited/GilchristBlue_07052022.xlsx")
pCO2_water<-8973.244/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0705m1<-kO2/0.55
KCO2_1d_0705m1<-KCO2_md/0.55 #1/d

GilchristBlue_07052022_mouthr2 <- read_excel("GilchristBlue_07052022.xlsx", sheet = "mouth r2")
GilchristBlue_07052022_mouthr2$CO2hi<-GilchristBlue_07052022_mouthr2$CO2hi*4.2
mouth2<-lm(CO2hi~Date, data = GilchristBlue_07052022_mouthr2)
deltaCO2_atm<-(7.467)/1000000#change in CO2 during float
pCO2_air<-max(GilchristBlue_07052022_mouthr2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0705m2<-kO2/0.55
KCO2_1d_0705m2<-KCO2_md/0.55 #1/d

##
ventTemp <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO/ROVING_DO_08012022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent08122022<-read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/GilchristBlue/GilchristBlue_07202022.xlsx")
vent08122022$CO2hi<-vent08122022$CO2hi*6
pCO2_water<-(max(vent08122022$CO2hi))/1000000

GilchristBlue_08012022_ventr1 <- read_excel("GilchristBlue_08012022.xlsx", sheet = "vent1")
GilchristBlue_08012022_ventr1$CO2hi<-GilchristBlue_08012022_ventr1$CO2hi*4.2
vent1<-lm(CO2hi~Date, data = GilchristBlue_08012022_ventr1)
deltaCO2_atm<-(-1.278e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(GilchristBlue_08012022_ventr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d_0606<-kO2/2.733276219
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733276219

KO2_1d_0812v1<-kO2/0.675
KCO2_1d_0812v1<-KCO2_md/0.675 #1/d

GilchristBlue_08012022_ventr2 <- read_excel("GilchristBlue_08012022.xlsx", sheet = "vent2")
GilchristBlue_08012022_ventr2$CO2hi<-GilchristBlue_08012022_ventr2$CO2hi*4.2
vent2<-lm(CO2hi~Date, data = GilchristBlue_08012022_ventr2)
deltaCO2_atm<-(2.183e+01 /10)*60/1000000#change in CO2 during float
pCO2_air<-max(GilchristBlue_08012022_ventr2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.736860197
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.736860197

KO2_1d_0812v2<-kO2/0.675
KCO2_1d_0812v2<-KCO2_md/0.675 #1/d

mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/GilchristBlue/DO/GilchristBlue_DO_08012022.csv")
mouthTemp_F<-mean(mouthtemp$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

mouth08082022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited/GilchristBlue_08012022.xlsx")
pCO2_water<-9016.405/1000000

GilchristBlue_08012022_mouthr1 <- read_excel("GilchristBlue_08012022.xlsx", sheet = "mouth1")
GilchristBlue_08012022_mouthr1$CO2hi<-GilchristBlue_08012022_mouthr1$CO2hi*4.2
mouth1<-lm(CO2hi~Date, data = GilchristBlue_08012022_mouthr1)
deltaCO2_atm<-(-1.772e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(GilchristBlue_08012022_mouthr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.736860197
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.736860197

KO2_1d_0812m1<-kO2/0.675
KCO2_1d_0812m1<-KCO2_md/0.675 #1/d

GilchristBlue_08012022_mouthr2 <- read_excel("GilchristBlue_08012022.xlsx", sheet = "mouth2")
GilchristBlue_08012022_mouthr2$CO2hi<-GilchristBlue_08012022_mouthr2$CO2hi*4.8
mouth2<-lm(CO2hi~Date, data = GilchristBlue_08012022_mouthr2)
deltaCO2_atm<-(1.512e+01/10)*60/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_08012022_mouthr2$CO2hi)/100000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.736860197
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.736860197

KO2_1d_0812m2<-kO2/0.65
KCO2_1d_0812m2<-KCO2_md/0.65 #1/d

mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/GilchristBlue/DO/GilchristBlue_DO_08292022.csv")
mouthTemp_F<-mean(mouthtemp$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

mouth08292022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited/GilchristBlue_08292022.xlsx")
pCO2_water<-(9761.717)/1000000

GilchristBlue_08292022_mouthr1 <- read_excel("GilchristBlue_08312022.xlsx", sheet = "mouth_r1")
GilchristBlue_08292022_mouthr1$CO2hi<-GilchristBlue_08292022_mouthr1$CO2hi*2.4
mouth1<-lm(CO2hi~Date, data = GilchristBlue_08292022_mouthr1)
deltaCO2_atm<-(-6.014/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(GilchristBlue_08292022_mouthr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.733083331
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733083331

KO2_1d_0829m1<-kO2/0.7
KCO2_1d_0829m1<-KCO2_md/0.7 #1/d

GilchristBlue_08292022_mouthr2 <- read_excel("GilchristBlue_08312022.xlsx", sheet = "mouth_r2")
GilchristBlue_08292022_mouthr2$CO2hi<-GilchristBlue_08292022_mouthr2$CO2hi*2.4
mouth2<-lm(CO2hi~Date, data = GilchristBlue_08292022_mouthr2)
deltaCO2_atm<-(-5.618/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(GilchristBlue_08292022_mouthr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.733083331
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733083331

KO2_1d_0829m2<-kO2/0.7
KCO2_1d_0829m2<-KCO2_md/0.7 #1/d

mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/GilchristBlue/DO/GilchristBlue_DO_09262022.csv")
mouthTemp_F<-mean(mouthtemp$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

mouth09262022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited/GilchristBlue_09262022.xlsx")
pCO2_water<-11773.90/1000000

GilchristBlue_09262022_m1<- read_excel("GilchristBlue_09262022.xlsx", 
                                     sheet = "mouthr1")
GilchristBlue_09262022_m1$CO2hi<-GilchristBlue_09262022_m1$CO2hi*6
mouth1<-lm(CO2lo~secs, data = GilchristBlue_09262022_m1)
deltaCO2_atm<-(1.701)*60/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_09262022_m1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.733083331
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733083331

KO2_1d_0829m2<-kO2/1
KCO2_1d_0829m2<-KCO2_md/1#1/d

ventTemp <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO/ROVING_DO_09262022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent09262022<-read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/GilchristBlue/GilchristBlue_09262022.xlsx")
vent09262022$CO2hi<-vent09262022$CO2hi*6
pCO2_water<-(max(vent08122022$CO2hi))/1000000

GilchristBlue_09262022_ventr1 <- read_excel("GilchristBlue_09262022.xlsx", sheet = "ventr1")
GilchristBlue_09262022_ventr1$CO2hi<-GilchristBlue_09262022_ventr1$CO2hi*6
vent1<-lm(CO2hi~Date, data = GilchristBlue_09262022_ventr1)
deltaCO2_atm<-((2.448)/10)*60/1000000 #change in CO2 during float
pCO2_air<-max(GilchristBlue_09262022_ventr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.733083331
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733083331

KO2_1d_0926v1<-kO2/1
KCO2_1d_0926v1<-KCO2_md/1 #1/d

GilchristBlue_09262022_ventr2 <- read_excel("GilchristBlue_09262022.xlsx", sheet = "ventr2")
vent2<-lm(CO2hi~Date, data = GilchristBlue_09262022_ventr2)
deltaCO2_atm<-((-4.08e-01)/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(GilchristBlue_09262022_ventr1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/2.733083331
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/2.733083331

KO2_1d_0926v2<-kO2/2.733083331
KCO2_1d_0926v2<-KCO2_md/2.733083331 #1/d

ventTemp <- read_xlsx("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO/ROVING_DO_09262022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(VentTemp_F)
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Ichetucknee")
Ichetucknee_06282022 <- read_excel("Ichetucknee_06282022.xlsx",sheet = "mouth r2",
                                   col_types = c("date","numeric", "numeric",
                                                 "numeric","numeric", "numeric"))
mouth2<-lm(CO2hi~Date, data = Ichetucknee_06282022_mouthr2)
summary(mouth2)

Ichetucknee_06282022_ventr1 <- read_excel("Ichetucknee_06282022.xlsx",sheet = "bridge r1")
a<-Ichetucknee_06282022_ventr1$Rep < '679'
b<-which(a)
Ichetucknee_06282022_ventr1<-Ichetucknee_06282022_ventr1[b,]
vent1<-lm(CO2hi~Date, data = Ichetucknee_06282022_ventr1)
summary(vent1)

Ichetucknee_06282022_ventr2 <- read_excel("Ichetucknee_06282022.xlsx",sheet = "bridge r2")
a<-Ichetucknee_06282022_ventr2$Rep < '712'
b<-which(a)
Ichetucknee_06282022_ventr2<-Ichetucknee_06282022_ventr2[b,]
vent2<-lm(CO2hi~Date, data = Ichetucknee_06282022_ventr2)
summary(vent2)

Ichetucknee_07262022_bridge <- read_excel("Ichetucknee_07262022.xlsx", sheet='bridge')
bridge<-lm(CO2hi~Date, data = Ichetucknee_07262022_bridge)

Ichetucknee_07262022_mouth1 <- read_excel("Ichetucknee_07262022.xlsx", sheet='mouth1')
mouth1<-lm(CO2hi~Date, data = Ichetucknee_07262022_mouth1)

Ichetucknee_07262022_mouth2 <- read_excel("Ichetucknee_07262022.xlsx", sheet='mouth2')
mouth2<-lm(CO2hi~Date, data = Ichetucknee_07262022_mouth2)

Ichetucknee_08302022_mouth1 <- read_excel("Ichetucknee_08312022.xlsx", sheet='mouth_r1')
mouth1<-lm(CO2hi~Date, data = Ichetucknee_08302022_mouth1)

Ichetucknee_08302022_mouth2 <- read_excel("Ichetucknee_08312022.xlsx", sheet='mouth_r2')
mouth2<-lm(CO2hi~Date, data = Ichetucknee_08302022_mouth2)

Ichetucknee_09142022_mouth1<- read_excel("Ichetucknee_09142022.xlsx", sheet='mouthr1')
mouth1<-lm(CO2hi~Date, data = Ichetucknee_09142022_mouth1)

Ichetucknee_09142022_mouth2<- read_excel("Ichetucknee_09142022.xlsx", sheet='mouthr2')
mouth2<-lm(CO2hi~Date, data = Ichetucknee_09142022_mouth2)

Ichetucknee_09162022_mouth2<- read_excel("Ichetucknee_09162022.xlsx", sheet='mouthr2')
mouth2<-lm(CO2hi~Date, data = Ichetucknee_09162022_mouth2)


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/LittleFanning")
LittleFanning_06132022_ventr1 <- read_excel("LittleFanning_06132022.xlsx", sheet = "vent r1")
vent1<-lm(CO2hi~Date, data = LittleFanning_06132022_ventr1)
summary(vent1)

LittleFanning_06132022_mouthr1 <- read_excel("LittleFanning_06132022.xlsx", sheet = "mouth r1")
a<-LittleFanning_06132022_mouthr1$Rep > '556'
b<-which(a)
LittleFanning_06132022_mouthr1<-LittleFanning_06132022_mouthr1[b,]
mouth1<-lm(CO2hi~Date, data = LittleFanning_06132022_mouthr1)
summary(mouth1)

LittleFanning_06132022_mouthr2 <- read_excel("LittleFanning_06132022.xlsx", sheet = "mouth r2")
mouth2<-lm(CO2hi~Date, data = LittleFanning_06132022_mouthr2)
summary(mouth2)

LittleFanning_07082022_r1 <- read_excel("LittleFanning_07082022.xlsx", sheet = "rep1")
r1<-lm(CO2hi~Date, data = LittleFanning_07082022_r1)
summary(r1)

LittleFanning_07082022_r2 <- read_excel("LittleFanning_07082022.xlsx", sheet = "rep 2")
r2<-lm(CO2hi~Date, data = LittleFanning_07082022_r2)
summary(r2)

LittleFanning_08242022_r1 <- read_excel("LittleFanning_08242022.xlsx", sheet = "vent_r1")
vent1<-lm(CO2hi~Date, data = LittleFanning_08242022_r1)
summary(vent1)

LittleFanning_08242022_r2 <- read_excel("LittleFanning_08242022.xlsx", sheet = "vent_r2")
vent2<-lm(CO2hi~Date, data = LittleFanning_08242022_r2)
summary(vent2)

LittleFanning_08242022_r3 <- read_excel("LittleFanning_08242022.xlsx", sheet = "mouth_r1")
mouth1<-lm(CO2hi~Date, data = LittleFanning_08242022_r3)
summary(mouth1)

LittleFanning_08242022_r4 <- read_excel("LittleFanning_08242022.xlsx", sheet = "mouth_r2")
mouth2<-lm(CO2hi~Date, data = LittleFanning_08242022_r4)
summary(mouth2)

LittleFanning_09192022_r1 <- read_excel("LittleFanning_09192022.xlsx", sheet = "ventr1")
vent1<-lm(CO2hi~Date, data = LittleFanning_09192022_r1)
summary(vent1)

LittleFanning_09192022_r2 <- read_excel("LittleFanning_09192022.xlsx", sheet = "ventr2")
vent2<-lm(CO2hi~Date, data = LittleFanning_09192022_r2)
summary(vent2)

LittleFanning_09192022_r3 <- read_excel("LittleFanning_09192022.xlsx", sheet = "mouthr1")
mouth1<-lm(CO2hi~Date, data = LittleFanning_09192022_r3)
summary(mouth1)

LittleFanning_09192022_r4 <- read_excel("LittleFanning_09192022.xlsx", sheet = "mouthr2")
mouth2<-lm(CO2hi~Date, data = LittleFanning_09192022_r4)
summary(mouth2)



#####
stage <- read_excel("Z:/SpringsProject_Sam&Paul/Master/Otter.xlsx",
                        col_types = c("date", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))
setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter")
ventTemp <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/Otter/ROVING_DO_08082022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

stage0808<-filter(stage, Date< '2022-08-08')
depth0808<-	0.2085869+1

vent08082022 <- read_excel(
  "Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/Otter/Otter_08082022.xlsx")
vent08082022$CO2hi<-vent08082022$CO2hi*4.2
pCO2_water<-(max(vent08082022$CO2hi))/1000000

Otter_08082022_v1 <- read_excel("Otter_08082022.xlsx", sheet = 'vent1')
Otter_08082022_v1$CO2hi<-Otter_08082022_v1$CO2lo*4
vent1<-lm(CO2hi~Date, data = Otter_08082022_v1)
deltaCO2_atm<-(2.829e+01 /10)*60/1000000 #change in CO2 during float
pCO2_air<-max(Otter_08082022_v1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent1_d<- k600_vent1/depth0808

KCO2_1d<-KCO2_md/depth0808 #1/d
KO2_1d<-kO2/depth0808

Otter_08082022_v2 <- read_excel("Otter_08082022.xlsx", sheet = 'vent2')
Otter_08082022_v2$CO2hi<-Otter_08082022_v2$CO2lo*4
vent2<-lm(CO2hi~Date, data = Otter_08082022_v2)
deltaCO2_atm<-(1.593e+01/10)*60/1000000 #change in CO2 during float
pCO2_air<-max(Otter_08082022_v2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth0808 #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth0808 #1/d
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/depth0808

##
mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Otter/Otter_DO_08082022.csv")
mouthTemp_F<-mean(mouthtemp$Temp)
mouthTemp_C<-fahrenheit.to.celsius(mean(mouthtemp$Temp))
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

mouth08082022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Otter/edited/Otter_08242022.xlsx",
                            sheet= 'CO2')
mouth08082022$CO2<-mouth08082022$CO2*6
pCO2_water<-13740/1000000

Otter_08082022_m1 <- read_excel("Otter_08082022.xlsx", sheet = 'mouth1')
Otter_08082022_m1$CO2hi<-Otter_08082022_m1$CO2lo*4
mouth1<-lm(CO2hi~Date, data = Otter_08082022_m1)
deltaCO2_atm<-(-2.477e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(Otter_08082022_m1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth0808 #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth0808
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_mouth1_d<- k600_vent1/depth0808

Otter_08082022_m2 <- read_excel("Otter_08082022.xlsx", sheet = 'mouth2')
Otter_08082022_m2$CO2hi<-Otter_08082022_m2$CO2lo*4
mouth2<-lm(CO2hi~Date, data = Otter_08082022_m2)
deltaCO2_atm<-(-1.118e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(Otter_08082022_m2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth0808 #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth0808
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_mouth2_d<- k600_vent1/depth0808

####################################
mouthtemp<- read_xlsx("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/Otter/Otter_DO_09072022.xlsx")
mouthTemp_F<-mean(mouthtemp$Temp)
mouthTemp_C<-fahrenheit.to.celsius(mean(mouthtemp$Temp))
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

stage0907<-filter(stage,Date < '2022-09-07' & Date > '2022-08-08')
depth<-0.7027645+1

mouth09072022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Otter/edited/Otter_09072022.xlsx",
                            sheet= 'CO2')
mouth09072022$CO2<-mouth09072022$CO2*6
pCO2_water<-17328/1000000

Otter_09072022_m <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_09072022.xlsx", 
                               sheet = "mouth")
Otter_09072022_m$CO2hi<-Otter_09072022_m$CO2lo*4
mouth<-lm(CO2hi~Date, data = Otter_09072022_m)
deltaCO2_atm<-(1.547/10)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_09072022_m$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth #1/d
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_mouth1_d<- k600_vent1/depth
####
ventTemp <- read_excel("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/Otter/ROVING_DO_09072022.xlsx")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent09072022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/Otter/Otter_09072022.xlsx"
                            )
vent09072022$CO2<-vent09072022$CO2*6
pCO2_water<-max(vent09072022$CO2)/1000000 

Otter_09072022_V2 <- read_excel("Otter_09072022.xlsx", sheet = "vent2")
Otter_09072022_V2$CO2hi<-Otter_09072022_V2$CO2hi*2
vent2<-lm(CO2hi~Date, data = Otter_09072022_V2)
deltaCO2_atm<-(-1.034e+01/10)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_09072022_V2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/depth #1/d

k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/depth
#######################################
mouthtemp<- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Otter/Otter_DO_10172022.csv")
mouthTemp_F<-mean(mouthtemp$Temp, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

stage1010<-filter(stage,Date > '2022-09-07')
depth<-0.16996793+1

mouth10172022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Otter/edited/Otter_10172022.xlsx",
                            )
mouth10172022$CO2<-mouth10172022$CO2*6
pCO2_water<-12582/1000000

Otter_10172022_m <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_10172022.xlsx", 
                               sheet = "mouth")
Otter_10172022_m$CO2<-Otter_10172022_m$CO2*6
mouth<-lm(CO2~Date, data = Otter_10172022_m)
deltaCO2_atm<-(6.202e-01 /30)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_10172022_m$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth #1/d
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_mouth1_d<- k600_vent1/depth
####
ventTemp <- read_csv("Z:/SpringsProject_Sam&Paul/Hobo/Roving_edited/Otter/RovingDO_Otter_10172022.csv")
VentTemp_F<-mean(ventTemp$Temp)
VentTemp_C<-fahrenheit.to.celsius(mean(ventTemp$Temp))
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

vent09072022 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/Roving/Roving_edited/Otter/Otter_09072022.xlsx"
)
vent09072022$CO2<-vent09072022$CO2*6
pCO2_water<-max(vent09072022$CO2)/1000000 

Otter_10172022_V2 <- read_excel("Otter_10172022.xlsx", sheet = "vent2")
Otter_10172022_V2$CO2<-Otter_10172022_V2$CO2*6
vent2<-lm(CO2~sec, data = Otter_10172022_V2)
deltaCO2_atm<-(-10.38 /30)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_10172022_V2$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/depth #1/d

k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/depth
###################

Otter_10172022_V1<- read_excel("Otter_10172022.xlsx", sheet = "vent1")
Otter_10172022_V1$CO2<-Otter_10172022_V1$CO2*6
vent2<-lm(CO2~Date, data = Otter_10172022_V1)
deltaCO2_atm<-(-2.736 /30)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_10172022_V1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
KH<-0.034*exp(exp) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
KO2_1d<-kO2/depth #1/d

k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/depth

