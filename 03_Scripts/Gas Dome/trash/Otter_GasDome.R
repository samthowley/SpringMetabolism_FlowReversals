
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

Otter <- read_excel("Otter.xlsx",
                            col_types = c("date", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric","numeric", "numeric",
                                          "numeric", "numeric"))

Otter<- Otter %>% mutate(set = case_when( Date  <= '2022-08-08' ~ 1,
                                          Date  <= '2022-09-07' ~ 2,
                                          Date  <= '2022-10-17' ~ 3,
                                          Date  <= '2022-10-31' ~ 4,
                                          Date  <= '2022-11-14' ~ 5,
                                          Date  <= '2022-11-28' ~ 6,
                                          Date  <= '2022-12-12' ~ 7,
                                          Date  <= '2023-01-04 11:00' ~ 8,
                                          Date  <= '2023-01-25 11:00' ~ 9,
                                          Date  <= '2023-02-06 11:00' ~ 10,
                                          Date  <= '2023-02-22 11:00' ~ 11,
                                          Date  <= '2023-03-08 11:00' ~ 12,
                                          Date  <= '2023-03-20 11:00' ~ 13,
                                          Date  <= '2023-04-05 13:00' ~ 14,
                                          Date  <= '2023-04-19 13:00' ~ 15,
                                          Date  <= '2023-05-03 13:00' ~ 16,
                                          Date  <= '2023-05-17 11:00' ~ 17))
Otter_0824<-filter(Otter, set==1)
Otter_0907<-filter(Otter, set==2)
Otter_1017<-filter(Otter, set==3)
Otter_1031<-filter(Otter, set==4)
Otter_1114<-filter(Otter, set==5)
Otter_1128<-filter(Otter, set==6)
Otter_1212<-filter(Otter, set==7)
Otter_0104<-filter(Otter, set==8)
Otter_0125<-filter(Otter, set==9)
Otter_0206<-filter(Otter, set==10)
Otter_0222<-filter(Otter, set==11)
Otter_0308<-filter(Otter, set==12)
Otter_0320<-filter(Otter, set==13)
Otter_0405<-filter(Otter, set==14)
Otter_0419<-filter(Otter, set==15)
Otter_0503<-filter(Otter, set==16)
Otter_0517<-filter(Otter, set==17)


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter")

VentTemp_F<-mean(RovingDO_0824$Temp, na.rm=T)
VentTemp_C<-fahrenheit.to.celsius(VentTemp_F)
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

pCO2_water<-max(RovingCO2_0808$CO2,na.rm=T)*2.2/1000000


Otter_08082022_v1 <- read_excel("Otter_08082022.xlsx", sheet = 'vent1')
Otter_08082022_v1$CO2hi<-Otter_08082022_v1$CO2lo*4
(vent1<-lm(CO2hi~Date, data = Otter_08082022_v1))
deltaCO2_atm<-(2.829e+01 /10)*60/1000000 #change in CO2 during float
pCO2_air<-max(Otter_08082022_v1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0824$stage))
(KCO2_1d<-KCO2_md/depth)#1/d
(KO2_1d<-kO2/depth)
(k600_vent1_d<- k600_vent1/depth)

Otter_08082022_v2 <- read_excel("Otter_08082022.xlsx", sheet = 'vent2')
Otter_08082022_v2$CO2hi<-Otter_08082022_v2$CO2lo*4
(vent2<-lm(CO2hi~Date, data = Otter_08082022_v2))
deltaCO2_atm<-(1.593e+01/10)*60/1000000 #change in CO2 during float
pCO2_air<-max(Otter_08082022_v2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_vent2_d<- k600_vent1/depth

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth)#1/d


##
mouthTemp_F<-mean(Otter_0824$Temp.x,na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

View(Otter_0907)
pCO2_water<-15000.0/1000000


Otter_08082022_m1 <- read_excel("Otter_08082022.xlsx", sheet = 'mouth1')
Otter_08082022_m1$CO2hi<-Otter_08082022_m1$CO2lo*4
(mouth1<-lm(CO2hi~Date, data = Otter_08082022_m1))
deltaCO2_atm<-(-2.477e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(Otter_08082022_m1$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth)#1/d


Otter_08082022_m2 <- read_excel("Otter_08082022.xlsx", sheet = 'mouth2')
Otter_08082022_m2$CO2hi<-Otter_08082022_m2$CO2lo*4
mouth2<-lm(CO2hi~Date, data = Otter_08082022_m2)
deltaCO2_atm<-(-1.118e+01/10)*60/1000000*-1 #change in CO2 during float
pCO2_air<-max(Otter_08082022_m2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
k600_mouth2_d<- k600_vent1/depth0808

(KCO2_1d<-KCO2_md/depth)#1/d
(kO2_1d<-kO2/depth)
(k600_mouth1_d<- k600_vent1/depth)


####################################
mouthTemp_F<-mean(Otter_0907$Temp.x,na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3
View(Otter_0907)
(depth<-mean(Otter_0907$stage))

pCO2_water<-17328/1000000

Otter_09072022_m <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_09072022.xlsx", 
                               sheet = "mouth")
Otter_09072022_m$CO2hi<-Otter_09072022_m$CO2lo*4
(mouth<-lm(CO2hi~Date, data = Otter_09072022_m))
deltaCO2_atm<-(1.547/10)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_09072022_m$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d
(k600_mouth1_d<- k600_vent1/depth)



####
VentTemp_F<-mean(RovingDO_0907$Temp, na.rm=T)
VentTemp_C<-fahrenheit.to.celsius(VentTemp_F)
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3

pCO2_water<-max(RovingCO2_0907$CO2)/1000000 

Otter_09072022_V2 <- read_excel("Otter_09072022.xlsx", sheet = "vent2")
Otter_09072022_V2$CO2hi<-Otter_09072022_V2$CO2hi*2
(vent2<-lm(CO2hi~Date, data = Otter_09072022_V2))
deltaCO2_atm<-(-1.034e+01/10)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_09072022_V2$CO2hi)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm)
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d
(k600_mouth1_d<- k600_vent1/depth)


#######################################
mouthTemp_F<-mean(Otter_1017$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

(depth<-mean(Otter_1017$stage))

pCO2_water<-12702/1000000

Otter_10172022_m <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_10172022.xlsx", 
                               sheet = "mouth")
Otter_10172022_m$CO2<-Otter_10172022_m$CO2*6
(mouth<-lm(CO2~Date, data = Otter_10172022_m))
deltaCO2_atm<-(6.202e-01 /30)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_10172022_m$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d
(k600_mouth1_d<- k600_vent1/depth)


####
VentTemp_F<-mean(RovingDO_1017$Temp)
VentTemp_C<-fahrenheit.to.celsius(VentTemp_F)
VentTemp_K<-VentTemp_C+273.15
SchmidtO2hi<-1568-86.04*VentTemp_C+2.142*VentTemp_C^2-0.0216*VentTemp_C^3
SchmidtCO2hi<-1742-91.24*VentTemp_C+2.208*VentTemp_C^2-0.0219*VentTemp_C^3


pCO2_water<-mean(RovingCO2_1017$CO2)*6/1000000 

Otter_10172022_V2 <- read_excel("Otter_10172022.xlsx", sheet = "vent2")
Otter_10172022_V2$CO2<-Otter_10172022_V2$CO2*6
(vent2<-lm(CO2~sec, data = Otter_10172022_V2))
deltaCO2_atm<-(-10.38 /30)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_10172022_V2$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent2_d<- k600_vent1/depth

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d
(k600_mouth1_d<- k600_vent1/depth)



Otter_10172022_V2 <- read_excel("Otter_10172022.xlsx", sheet = "vent1")
Otter_10172022_V2$CO2<-Otter_10172022_V2$CO2*6
(vent2<-lm(...6~secs, data = Otter_10172022_V2))
deltaCO2_atm<-(-0.103 /30)*60*-1/1000000 #change in CO2 during float
pCO2_air<-max(Otter_10172022_V2$...6)/1000000

n<-(deltaCO2_atm*domeVol_L/R/VentTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/VentTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent2_d<- k600_vent1/depth

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d
(k600_mouth1_d<- k600_vent1/depth)

############10312022



mouthTemp_F<-mean(Otter_1031$Temp.x)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

depth<-mean(Otter_1031$stage)

View(Otter_1031)
pCO2_water<-17562/1000000

Otter_10312022_mouth2 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_10312022.xlsx", 
                             sheet = "mouth 2")
Otter_10312022_mouth2$CO2<-Otter_10312022_mouth2$CO2...4*6
(mouth2<-lm(CO2~Date, data = Otter_10312022_mouth2))

deltaCO2_atm<-(7.921/30)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_10312022_mouth2$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth #1/d
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
(k600_mouth1_d<- k600_vent1/depth)

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d

Otter_10312022_mouth2 <- read_excel("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/Otter/Otter_10312022.xlsx", 
                                    sheet = "mouth 1")
Otter_10312022_mouth2$CO2<-Otter_10312022_mouth2$CO2...4*6
(mouth2<-lm(CO2~Date, data = Otter_10312022_mouth2))
deltaCO2_atm<-(5.126/30)*60/1000000#change in CO2 during float
pCO2_air<-max(Otter_10312022_mouth2$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
KCO2_1d<-KCO2_md/depth #1/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
kO2_1d<-kO2/depth #1/d
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d
(k600_mouth1_d<- k600_vent1/depth)

(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


###just mouth

mouthTemp_F<-mean(Otter_1114$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

depth<-0.687324

pCO2_water<-17934/1000000

Otter_11142022_mouth1 <- read_excel("Otter_11142022.xlsx",sheet = "mouth1")
Otter_11142022_mouth1$CO2<-Otter_11142022_mouth1$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_11142022_mouth1))

deltaCO2_atm<-(4.876)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_11142022_mouth1$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_11142022_mouth2<- read_excel("Otter_11142022.xlsx",sheet = "mouth2")
Otter_11142022_mouth2$CO2<-Otter_11142022_mouth2$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_11142022_mouth2))

deltaCO2_atm<-(2.438)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_11142022_mouth2$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


###just mouth

mouthTemp_F<-mean(Otter_1128$Temp.y)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-14118/1000000

Otter_11282022 <- read_excel("Otter_11282022.xlsx",sheet = "mouth1")
Otter_11282022$CO2<-Otter_11282022$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_11282022))

deltaCO2_atm<-(3.953)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_11282022$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

depth<-mean(Otter_1128$stage)
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_11282022 <- read_excel("Otter_11282022.xlsx",sheet = "mouth2")
Otter_11282022$CO2<-Otter_11282022$CO2*6
(mouth2<-lm(CO2~minute, data = Otter_11282022))

deltaCO2_atm<-(6.493)/2/1000000#change in CO2 during float
pCO2_air<-max(Otter_11282022$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

depth<-mean(Otter_1128$stage)
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d





(mouthTemp_F<-mean(Otter_1212$Temp.y, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-8964/1000000

Otter_12122022 <- read_excel("Otter_12122022.xlsx",sheet = "Sheet1")
Otter_12122022$CO2<-Otter_12122022$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_12122022))

deltaCO2_atm<-(4.358e-01 )*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_12122022$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_1212$stage,na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_12122022 <- read_excel("Otter_12122022.xlsx",sheet = "Sheet2")
Otter_12122022$CO2<-Otter_12122022$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_12122022))

deltaCO2_atm<-(5.808e-01)/2/1000000#change in CO2 during float
pCO2_air<-max(Otter_12122022$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_1212$stage))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d









mouthTemp_F<-mean(Otter_0104$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-490.8*6/1000000

Otter_01032023 <- read_excel("Otter_01032023.xlsx",sheet = "Sheet1")
Otter_01032023$CO2<-Otter_01032023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_01032023))

deltaCO2_atm<-(1.882)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_01032023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

depth<-mean(Otter_0104$stage, na.rm=T)
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_01032023 <- read_excel("Otter_01032023.xlsx",sheet = "Sheet2")
Otter_01032023$CO2<-Otter_01032023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_01032023))

deltaCO2_atm<-(1.581)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_01032023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d










View(Otter_0125)
mouthTemp_F<-mean(Otter_0125$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7566/1000000

Otter_01252023 <- read_excel("Otter_01252023.xlsx",sheet = "Sheet1")
Otter_01252023$CO2<-Otter_01252023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_01252023))

deltaCO2_atm<-(3.955)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_01252023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0125$stage, na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_01252023 <- read_excel("Otter_01252023.xlsx",sheet = "Sheet2")
Otter_01252023$CO2<-Otter_01252023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_01252023))

deltaCO2_atm<-(1.232)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_01252023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d











(mouthTemp_F<-mean(Otter_0222$Temp.x, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2636.4/1000000

Otter_02222023 <- read_excel("Otter_02222023.xlsx",sheet = "Sheet1")
Otter_02222023$CO2<-Otter_02222023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_02222023))

deltaCO2_atm<-(9.603)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_02222023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0222$stage, na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_02222023 <- read_excel("Otter_02222023.xlsx",sheet = "Sheet2")
Otter_02222023$CO2<-Otter_02222023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_02222023))

deltaCO2_atm<-(5.457)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_02222023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d











(mouthTemp_F<-mean(Otter_0308$Temp.x, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-15330/1000000

Otter_03082023 <- read_excel("Otter_03082023.xlsx",sheet = "Sheet1")
Otter_03082023$CO2<-Otter_03082023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_03082023))

deltaCO2_atm<-(3.446)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_03082023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0308$stage, na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


Otter_03082023 <- read_excel("Otter_03082023.xlsx",sheet = "Sheet2")
Otter_03082023$CO2<-Otter_03082023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_03082023))

deltaCO2_atm<-(2.12)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_03082023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d





(mouthTemp_F<-mean(Otter_0320$Temp.y, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7884/1000000

Otter_03202023 <- read_excel("Otter_03202023.xlsx",sheet = "Sheet1")
Otter_03202023$CO2<-Otter_03202023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_03202023))

deltaCO2_atm<-(2.376)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_03202023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0320$stage, na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d







(mouthTemp_F<-mean(Otter_0405$Temp.y, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-10506/1000000

Otter_04052023 <- read_excel("Otter_04052023.xlsx",sheet = "Sheet1")
Otter_04052023$CO2<-Otter_04052023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_04052023))

deltaCO2_atm<-(1.945)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_04052023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(depth<-mean(Otter_0405$stage, na.rm=T))
(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d









(mouthTemp_F<-mean(Otter_0419$Temp.y, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7200/1000000
depth<-0.838894818

Otter_04192023 <- read_excel("Otter_04192023.xlsx",sheet = "Sheet1")
Otter_04192023$CO2<-Otter_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_04192023))

deltaCO2_atm<-(2.181)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_04192023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d









(mouthTemp_F<-mean(Otter_0503$Temp.x, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-9582.0/1000000
depth<-0.7554098

Otter_04192023 <- read_excel("Otter_05032023.xlsx",sheet = "Sheet1")
Otter_04192023$CO2<-Otter_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_04192023))

deltaCO2_atm<-(1.627)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_04192023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d











(mouthTemp_F<-mean(Otter_0517$Temp.x, na.rm=T)) #replace
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-11718/1000000
depth<-0.7383014

Otter_04192023 <- read_excel("Otter_05172023.xlsx",sheet = "Sheet1")
Otter_04192023$CO2<-Otter_04192023$CO2*6
(mouth2<-lm(CO2~Date, data = Otter_04192023))

deltaCO2_atm<-(1.804)*2/1000000#change in CO2 during float
pCO2_air<-max(Otter_04192023$CO2)/1000000
n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm

KH_1000<-KH*1000
KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(k600_mouth1_d<- k600_vent1/depth)
(KCO2_1d<-KCO2_md/depth) #1/d
(kO2_1d<-kO2/depth) #1/d


