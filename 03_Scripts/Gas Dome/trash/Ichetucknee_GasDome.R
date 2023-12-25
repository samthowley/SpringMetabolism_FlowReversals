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

############

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry")

Ichetucknee <- read_excel("Ichetucknee.xlsx",
                          col_types = c("date", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric"))

Ichetucknee <- Ichetucknee %>% mutate(set = case_when(Date  <= '2022-06-28' ~ 1,
                                                          Date <= '2022-07-26 09:00' ~ 2,
                                                          Date  <= '2022-08-30 08:00' ~ 3,
                                                          Date  <= '2022-09-12 12:00' ~ 4,
                                                          Date  <= '2022-10-10 10:00' ~ 5,
                                                          Date  <= '2022-10-26 8:00' ~ 6,
                                                          Date  <= '2022-11-07 12:00' ~ 7,
                                                          Date  <= '2022-11-21 12:00' ~ 8,
                                                          Date  <= '2022-12-05 11:00' ~ 9,
                                                          Date  <= '2022-12-19 12:00' ~ 10,
                                                      Date  <= '2023-01-03 12:00' ~ 11,
                                                      Date  <= '2023-01-18 13:00' ~ 12,
                                                      Date  <= '2023-02-01 12:00' ~ 13,
                                                      Date  <= '2023-02-15 12:00' ~ 14,
                                                      Date  <= '2023-03-01 12:00' ~ 15,
                                                      Date  <= '2023-03-15 12:00' ~ 16,
                                                      Date  <= '2023-03-29 12:00' ~ 17,
                                                      Date  <= '2023-04-12 12:00' ~ 18,
                                                      Date  <= '2023-04-26 12:00' ~ 19,
                                                      Date  <= '2023-05-30 12:00' ~ 20))
Ichetucknee_1010<-filter(Ichetucknee, set==5)
Ichetucknee_1026<-filter(Ichetucknee, set==6)
Ichetucknee_1107<-filter(Ichetucknee, set==7)
Ichetucknee_1121<-filter(Ichetucknee, set==8)
Ichetucknee_1205<-filter(Ichetucknee, set==9)
Ichetucknee_1219<-filter(Ichetucknee, set==10)
Ichetucknee_0103<-filter(Ichetucknee, set==11)
Ichetucknee_0118<-filter(Ichetucknee, set==12)
Ichetucknee_0201<-filter(Ichetucknee, set==13)
Ichetucknee_0215<-filter(Ichetucknee, set==14)
Ichetucknee_0301<-filter(Ichetucknee, set==15)
Ichetucknee_0315<-filter(Ichetucknee, set==16)
Ichetucknee_0329<-filter(Ichetucknee, set==17)
Ichetucknee_0412<-filter(Ichetucknee, set==18)
Ichetucknee_0426<-filter(Ichetucknee, set==19)
Ichetucknee_0530<-filter(Ichetucknee, set==20)




setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/CampbellSci/GasDome/Ichetucknee")
View(Ichetucknee_1010)

(mouthTemp_F<-mean(Ichetucknee_1010$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2697.631/1000000
Ichetucknee_1010[which(Ichetucknee_1010$Date== '2022-10-10 10:00'),]
depth<-1.44  
depth1010<-1.44  

  
Ichetucknee_10102022 <- read_excel("Ichetucknee_10102022.xlsx",
                                   sheet = "r1")
Ichetucknee_10102022$CO2<-Ichetucknee_10102022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_10102022))
deltaCO2_atm<-(3.439*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_10102022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k6001010<- k600_md/depth)


Ichetucknee_10102022 <- read_excel("Ichetucknee_10102022.xlsx",
                                   sheet = "r2")
Ichetucknee_10102022$CO2<-Ichetucknee_10102022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_10102022))
deltaCO2_atm<-(1.06*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_10102022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)














(mouthTemp_F<-mean(Ichetucknee_1026$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3302.882/1000000
Ichetucknee_1026[which(Ichetucknee_1026$Date== '2022-10-26 8:00'),]
depth<-1.13  

Ichetucknee_10262022 <- read_excel("Ichetucknee_10262022.xlsx",
                                   sheet = "vent 1")
Ichetucknee_10262022$CO2<-Ichetucknee_10262022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_10262022))
deltaCO2_atm<-(1.050e+01*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_10262022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)


Ichetucknee_10262022 <- read_excel("Ichetucknee_10262022.xlsx",
                                   sheet = "vent 2")
Ichetucknee_10262022$CO2<-Ichetucknee_10262022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_10262022))
deltaCO2_atm<-(1.068e+01*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_10262022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)













View(Ichetucknee_1107)
(mouthTemp_F<-mean(Ichetucknee_1107$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2001.206/1000000
Ichetucknee_1107[which(Ichetucknee_1107$Date== '2022-11-07 12:00'),]
depth<- 1.00  

  
Ichetucknee_11072022 <- read_excel("Ichetucknee_11072022.xlsx", sheet = "mouth1")
Ichetucknee_11072022$CO2<-Ichetucknee_11072022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_11072022))
deltaCO2_atm<-(8.532*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_11072022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)


Ichetucknee_11072022 <- read_excel("Ichetucknee_11072022.xlsx", sheet = "mouth2")
Ichetucknee_11072022$CO2<-Ichetucknee_11072022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_11072022))
deltaCO2_atm<-(2.213e+01*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_11072022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



















View(Ichetucknee_1121)

(mouthTemp_F<-mean(Ichetucknee_1121$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2319.452/1000000
Ichetucknee_1121[which(Ichetucknee_1121$Date== '2022-11-21 10:00'),]
depth<-0.999  
  
Ichetucknee_112120222 <- read_excel("Ichetucknee_112120222.xlsx",sheet = "Sheet1")
Ichetucknee_112120222$CO2<-Ichetucknee_112120222$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_112120222))
deltaCO2_atm<-(8.016*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_112120222$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)


Ichetucknee_112120222 <- read_excel("Ichetucknee_112120222.xlsx",sheet = "Sheet2")
Ichetucknee_112120222$CO2<-Ichetucknee_112120222$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_112120222))
deltaCO2_atm<-(7.524*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_112120222$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



















View(Ichetucknee_1205)

(mouthTemp_F<-mean(Ichetucknee_1205$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-(2515.396)/1000000
Ichetucknee_1205[which(Ichetucknee_1205$Date== '2022-12-05 11:00'),]
depth<- 0.956  
  
Ichetucknee_12052022 <- read_excel("Ichetucknee_12052022.xlsx",sheet = "mouth1")
Ichetucknee_12052022$CO2<-Ichetucknee_12052022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_12052022))
deltaCO2_atm<-(3.453*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_12052022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)


Ichetucknee_12052022 <- read_excel("Ichetucknee_12052022.xlsx",sheet = "Sheet2")
Ichetucknee_12052022$CO2<-Ichetucknee_12052022$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_12052022))
deltaCO2_atm<-(4.014*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_12052022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



















View(Ichetucknee_1219)

(mouthTemp_F<-mean(Ichetucknee_1219$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-1910.468/1000000
Ichetucknee_1219[which(Ichetucknee_1219$Date== '2022-12-19 10:00'),]

depth<-0.922  

Ichetucknee_12192022 <- read_excel("Ichetucknee_12192022.xlsx", sheet = "Sheet1")
Ichetucknee_12192022$CO2<-Ichetucknee_12192022$CO2*6
(mouth1<-lm(CO2~sec, data = Ichetucknee_12192022))
deltaCO2_atm<-(3.554*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_12052022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)


Ichetucknee_12192022 <- read_excel("Ichetucknee_12192022.xlsx", sheet = "Sheet2")
Ichetucknee_12192022$CO2<-Ichetucknee_12192022$CO2*6
(mouth1<-lm(CO2~sec...4, data = Ichetucknee_12192022))
deltaCO2_atm<-(7.006*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_12192022$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)




















View(Ichetucknee_0118)

(mouthTemp_F<-mean(Ichetucknee_0103$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2651.380/1000000
Ichetucknee_0103[which(Ichetucknee_0103$Date== '2023-01-03 10:00'),]

depth<-0.911  

Ichetucknee_01032023 <- read_excel("Ichetucknee_01032023.xlsx", sheet = "Sheet1")
Ichetucknee_01032023$CO2<-Ichetucknee_01032023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_01032023))
deltaCO2_atm<-(2.585*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_01032023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



Ichetucknee_01032023 <- read_excel("Ichetucknee_01032023.xlsx", sheet = "Sheet2")
Ichetucknee_01032023$CO2<-Ichetucknee_01032023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_01032023))
deltaCO2_atm<-(2.958*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_01032023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)






##########






View(Ichetucknee_0201)
(mouthTemp_F<-mean(Ichetucknee_0118$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2530.550/1000000
Ichetucknee_0118[which(Ichetucknee_0118$Date== '2023-01-18 10:00'),]

depth<-0.926  

Ichetucknee_01182023 <- read_excel("Ichetucknee_01182023.xlsx", sheet = "Sheet1")
Ichetucknee_01182023$CO2<-Ichetucknee_01182023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_01182023))
deltaCO2_atm<-(2.883*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_01182023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



Ichetucknee_01182023 <- read_excel("Ichetucknee_01182023.xlsx", sheet = "Sheet2")
Ichetucknee_01182023$CO2<-Ichetucknee_01182023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_01182023))
deltaCO2_atm<-(3.707*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_01182023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)






##########







(mouthTemp_F<-mean(Ichetucknee_0201$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2833.006/1000000
depth<-1.32  
Ichetucknee_0201[which(Ichetucknee_0201$Date== '2023-02-01 10:00'),]

Ichetucknee_02012023 <- read_excel("Ichetucknee_02012023.xlsx", sheet = "Sheet1")
Ichetucknee_02012023$CO2<-Ichetucknee_02012023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_02012023))
deltaCO2_atm<-(3.799*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_02012023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



Ichetucknee_02012023 <- read_excel("Ichetucknee_02012023.xlsx", sheet = "Sheet2")
Ichetucknee_02012023$CO2<-Ichetucknee_02012023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_02012023))
deltaCO2_atm<-(3.799*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_02012023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)





##########
##########






View(Ichetucknee_0215)
(mouthTemp_F<-mean(Ichetucknee_0215$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2651.160/1000000
Ichetucknee_0215[which(Ichetucknee_0215$Date== '2023-02-15 10:00'),]

depth<- 2.07  
  
Ichetucknee_02152023 <- read_excel("Ichetucknee_02152023.xlsx", sheet = "Sheet1")
Ichetucknee_02152023$CO2<-Ichetucknee_02152023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_02152023))
deltaCO2_atm<-(7.228*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_02152023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



Ichetucknee_02152023 <- read_excel("Ichetucknee_02152023.xlsx", sheet = "Sheet2")
Ichetucknee_02152023$CO2<-Ichetucknee_02152023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_02152023))
deltaCO2_atm<-(5.643*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_02152023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)





##########






(mouthTemp_F<-mean(Ichetucknee_0301$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3740.594/1000000
Ichetucknee_0301[which(Ichetucknee_0301$Date== '2023-03-01 10:00'),]

depth<-2.38  

Ichetucknee_03012023 <- read_excel("Ichetucknee_03012023.xlsx", sheet = "Sheet1")
Ichetucknee_03012023$CO2<-Ichetucknee_03012023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_03012023))
deltaCO2_atm<-(2.699*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_03012023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)



Ichetucknee_03012023 <- read_excel("Ichetucknee_03012023.xlsx", sheet = "Sheet2")
Ichetucknee_03012023$CO2<-Ichetucknee_03012023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_03012023))
deltaCO2_atm<-(1.916*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_03012023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)





View(Ichetucknee_0315)
(mouthTemp_F<-mean(Ichetucknee_0315$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2545.463/1000000
Ichetucknee_0315[which(Ichetucknee_0315$Date== '2023-03-15 10:00'),]

depth<- 1.75  
  
Ichetucknee_03152023 <- read_excel("Ichetucknee_03152023.xlsx", sheet = "Sheet1")
Ichetucknee_03152023$CO2<-Ichetucknee_03152023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_03152023))
deltaCO2_atm<-(4.964*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_03152023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)






View(Ichetucknee_0329)
(mouthTemp_F<-mean(Ichetucknee_0329$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3271.327/1000000
depth<-1.692208

Ichetucknee_03292023 <- read_excel("Ichetucknee_03292023.xlsx", sheet = "Sheet1")
Ichetucknee_03292023$CO2<-Ichetucknee_03292023$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_03292023))
deltaCO2_atm<-(5.321*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_03292023$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)









View(Ichetucknee_0412)
(mouthTemp_F<-mean(Ichetucknee_0412$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3150.865/1000000
Ichetucknee_0412[which(Ichetucknee_0412$Date== '2023-04-12 10:00'),]

depth<-1.41  

Ichetucknee_ <- read_excel("Ichetucknee_04122023.xlsx", sheet = "Sheet1")
Ichetucknee_$CO2<-Ichetucknee_$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_))
deltaCO2_atm<-(1.086*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)











View(Ichetucknee_0426)
(mouthTemp_F<-mean(Ichetucknee_0426$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3301.8510/1000000
Ichetucknee_0426[which(Ichetucknee_0426$Date== '2023-04-26 10:00'),]

depth<-0.988  

Ichetucknee_ <- read_excel("Ichetucknee_04262023.xlsx", sheet = "Sheet1")
Ichetucknee_$CO2<-Ichetucknee_$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_))
deltaCO2_atm<-(5.476*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)











View(Ichetucknee_0530)
(mouthTemp_F<-mean(Ichetucknee_0530$Temp.x, na.rm=T))
mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2073/1000000
Ichetucknee_0530[which(Ichetucknee_0530$Date== '2023-05-30 10:00'),]

depth<-0.889  

Ichetucknee_ <- read_excel("Ichetucknee_05292023_GD.xlsx", sheet = "Sheet1")
Ichetucknee_$CO2<-Ichetucknee_$CO2*6
(mouth1<-lm(CO2~Date, data = Ichetucknee_))
deltaCO2_atm<-(4.917*2)/1000000 #change in CO2 during float
pCO2_air<-max(Ichetucknee_$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp))#mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_md<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d<-kO2/depth)
(KCO2_1d<-KCO2_md/depth)
(k600<- k600_md/depth)
