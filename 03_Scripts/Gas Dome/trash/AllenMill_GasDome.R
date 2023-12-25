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

AllenMill <- read_excel("AllenMill.xlsx",
                        col_types = c("date", "numeric", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric","numeric",
                                      "numeric","numeric", "numeric","numeric",
                                      "numeric"))
AllenMill <- AllenMill %>% mutate(set = case_when(Date <= '2022-07-20 13:00' ~ 2,
                                                  Date  <= '2022-08-15 12:00' ~ 3,
                                                  Date  <= '2022-09-12 12:00' ~ 4,
                                                  Date  <= '2022-10-10 11:00' ~ 5,
                                                  Date  <= '2022-10-26 11:00' ~ 6,
                                                  Date  <= '2022-11-07 11:00' ~ 7,
                                                  Date  <= '2022-11-21 12:00' ~ 8,
                                                  Date  <= '2022-12-05 11:00' ~ 9,
                                                  Date  <= '2022-12-19 10:00' ~ 10,
                                                  Date  <= '2023-01-03 13:00' ~ 11,
                                                  Date  <= '2023-01-18 11:00' ~ 12,
                                                  Date  <= '2023-02-01 13:00' ~ 13,
                                                  Date  <= '2023-02-15 13:00' ~ 14,
                                                  Date  <= '2023-03-01 13:00' ~ 15,
                                                  Date  <= '2023-03-15 13:00' ~ 16,
                                                  Date  <= '2023-03-29 13:00' ~ 17,
                                                  Date  <= '2023-04-12 13:00' ~ 18,
                                                  Date  <= '2023-04-26 13:00' ~ 19,
                                                  Date  <= '2023-05-04 13:00' ~ 20,
                                                  Date  <= '2023-05-18 13:00' ~ 21,
                                                  Date  <= '2023-05-29 13:00' ~ 22,
                                                  Date  <= '2023-06-09 13:00' ~ 23))


AllenMill_1010<-filter(AllenMill, set==5)
AllenMill_1026<-filter(AllenMill, set==6)
AllenMill_1107<-filter(AllenMill, set==7)
AllenMill_1121<-filter(AllenMill, set==8)
AllenMill_1205<-filter(AllenMill, set==9)
AllenMill_1219<-filter(AllenMill, set==10)
AllenMill_010323<-filter(AllenMill, set==11)
AllenMill_011823<-filter(AllenMill, set==12)
AllenMill_020123<-filter(AllenMill, set==13)
AllenMill_021523<-filter(AllenMill, set==14)
AllenMill_030123<-filter(AllenMill, set==15)
AllenMill_031523<-filter(AllenMill, set==16)
AllenMill_032923<-filter(AllenMill, set==17)
AllenMill_041223<-filter(AllenMill, set==18)
AllenMill_042623<-filter(AllenMill, set==19)
AllenMill_050323<-filter(AllenMill, set==20)
AllenMill_051823<-filter(AllenMill, set==21)
AllenMill_052923<-filter(AllenMill, set==22)
AllenMill_060823<-filter(AllenMill, set==23)


setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
Temp_F<-mean(AllenMill_1010$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-8622/1000000
depth<-0.8225421


AllenMill_10102022_mouthr1<- read_excel("AllenMill_10102022.xlsx",sheet = " mouth r2")
AllenMill_10102022_mouthr1$CO2<-AllenMill_10102022_mouthr1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_10102022_mouthr1))
deltaCO2_atm<-(3.144e-01)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_10102022_mouthr1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_10102022_mouthr3<- read_excel("AllenMill_10102022.xlsx", sheet = "mouth r3")
AllenMill_10102022_mouthr3$CO2<-AllenMill_10102022_mouthr3$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_10102022_mouthr3))
deltaCO2_atm<-(5.830e-01 )*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_10102022_mouthr3 $CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)


###########10262022######

depth<-mean(AllenMill_1026$depth, na.rm=T)



########11072022#####

Temp_F<-mean(AllenMill_1107$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3972.6/1000000
depth<-0.6609395

AllenMill_11072022_mouth_r1<- read_excel("AllenMill_11072022.xlsx",sheet = "mouth1")
AllenMill_11072022_mouth_r1$CO2<-AllenMill_11072022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_11072022_mouth_r1))
deltaCO2_atm<-(5.140)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_11072022_mouth_r1$CO2)/1000000


n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)


AllenMill_11072022_mouth_r2<- read_excel("AllenMill_11072022.xlsx",sheet = "mouth2")
AllenMill_11072022_mouth_r2$CO2<-AllenMill_11072022_mouth_r2$CO2*6
(vent1<-lm(CO2~secs, data = AllenMill_11072022_mouth_r2))
deltaCO2_atm<-(7.899)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_11072022_mouth_r2$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)





###############
Temp_F<-mean(AllenMill_1121$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3648.0/1000000
depth<-0.6703303
  
AllenMill_11212022_mouth_r1<- read_excel("AllenMill_11212022.xlsx",sheet = "Sheet1")
AllenMill_11212022_mouth_r1$CO2<-AllenMill_11212022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_11212022_mouth_r1))
deltaCO2_atm<-(2.695)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_11212022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000


(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)



AllenMill_11212022_mouth_r1<- read_excel("AllenMill_11212022.xlsx",sheet = "Sheet2")
AllenMill_11212022_mouth_r1$CO2<-AllenMill_11212022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_11212022_mouth_r1))
deltaCO2_atm<-(8.95)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_11212022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

#####
View(AllenMill_1205)
Temp_F<-mean(AllenMill_1205$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3639.6/1000000
depth<-0.6516353

AllenMill_12052022_mouth_r1<- read_excel("AllenMill_12052022.xlsx",sheet = "Sheet1")
AllenMill_12052022_mouth_r1$CO2<-AllenMill_12052022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_12052022_mouth_r1))
deltaCO2_atm<-(3.111)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_12052022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_12052022_mouth_r1<- read_excel("AllenMill_12052022.xlsx",sheet = "Sheet2")
AllenMill_12052022_mouth_r1$CO2<-AllenMill_12052022_mouth_r1$CO2*6
(vent1<-lm(CO2~time, data = AllenMill_12052022_mouth_r1))
deltaCO2_atm<-(5.422)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_12052022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)


###############
View(AllenMill_1219)
Temp_F<-mean(AllenMill_1219$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-4056.6/1000000
depth<- 0.6522113
  
AllenMill_12192022_mouth_r1<- read_excel("AllenMill_12192022.xlsx",sheet = "Sheet1")
AllenMill_12192022_mouth_r1$CO2<-AllenMill_12192022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_12192022_mouth_r1))
deltaCO2_atm<-(3.143)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_12192022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_12192022_mouth_r1<- read_excel("AllenMill_12192022.xlsx",sheet = "Sheet2")
AllenMill_12192022_mouth_r1$CO2<-AllenMill_12192022_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_12192022_mouth_r1))
deltaCO2_atm<-(5.105)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_12192022_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)




View(AllenMill_010323)
Temp_F<-mean(AllenMill_010323$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-490.8*6/1000000
depth<-0.6507326

AllenMill_010323_mouth_r1<- read_excel("AllenMill_01032023.xlsx",sheet = "Sheet1")
AllenMill_010323_mouth_r1$CO2<-AllenMill_010323_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_010323_mouth_r1))
deltaCO2_atm<-(9.345e-01)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_010323_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_010323_mouth_r1<- read_excel("AllenMill_01032023.xlsx",sheet = "Sheet2")
AllenMill_010323_mouth_r1$CO2<-AllenMill_010323_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_010323_mouth_r1))
deltaCO2_atm<-(4.229)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_010323_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)



View(AllenMill_011823)
Temp_F<-mean(AllenMill_011823$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2261.40/1000000
depth<-0.6157468

AllenMill_011823_mouth_r1<- read_excel("AllenMill_01182023.xlsx",sheet = "Sheet1")
AllenMill_011823_mouth_r1$CO2<-AllenMill_011823_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_011823_mouth_r1))
deltaCO2_atm<-(5.907)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_011823_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)


AllenMill_011823_mouth_r1<- read_excel("AllenMill_01182023.xlsx",sheet = "Sheet2")
AllenMill_011823_mouth_r1$CO2<-AllenMill_011823_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_011823_mouth_r1))
deltaCO2_atm<-(6.215)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_011823_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)





View(AllenMill_020123)
Temp_F<-mean(AllenMill_020123$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-5886.0/1000000
depth<-1.1493393

AllenMill_020123_mouth_r1<- read_excel("AllenMill_02012023.xlsx",sheet = "Sheet1")
AllenMill_020123_mouth_r1$CO2<-AllenMill_020123_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_020123_mouth_r1))
deltaCO2_atm<-(1.351)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_020123_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_020123_mouth_r1<- read_excel("AllenMill_02012023.xlsx",sheet = "Sheet2")
AllenMill_020123_mouth_r1$CO2<-AllenMill_020123_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_020123_mouth_r1))
deltaCO2_atm<-(9.074e-01 )*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_020123_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)





View(AllenMill_021523)
Temp_F<-mean(AllenMill_021523$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2434.8/1000000
depth<-1.961475

AllenMill_021523_mouth_r1<- read_excel("AllenMill_02152023.xlsx",sheet = "Sheet1")
AllenMill_021523_mouth_r1$CO2<-AllenMill_021523_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_021523_mouth_r1))
deltaCO2_atm<-(8.874)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_021523_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)


AllenMill_021523_mouth_r1<- read_excel("AllenMill_02152023.xlsx",sheet = "Sheet2")
AllenMill_021523_mouth_r1$CO2<-AllenMill_021523_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_021523_mouth_r1))
deltaCO2_atm<-(8.666)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_021523_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)




Temp_F<-mean(AllenMill_030123$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
(depth<-mean(AllenMill_030123$depth, na.rm=T))



(depth<-mean(AllenMill_031523$depth, na.rm=T))
Temp_F<-mean(AllenMill_031523$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm





View(AllenMill_032923)
Temp_F<-mean(AllenMill_032923$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-7134/1000000
depth<-1.290305

AllenMill_032923_mouth_r1<- read_excel("AllenMill_03292023.xlsx",sheet = "Sheet1")
AllenMill_032923_mouth_r1$CO2<-AllenMill_032923_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_032923_mouth_r1))
deltaCO2_atm<-(2.197)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_032923_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)



AllenMill_032923_mouth_r1<- read_excel("AllenMill_03292023.xlsx",sheet = "Sheet2")
AllenMill_032923_mouth_r1$CO2<-AllenMill_032923_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_032923_mouth_r1))
deltaCO2_atm<-(1.890)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_032923_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)






View(AllenMill_041223)
Temp_F<-mean(AllenMill_041223$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-6696/1000000
depth<-1.115436

AllenMill_mouth_r1<- read_excel("AllenMill_04122023.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(3.607)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)



AllenMill_mouth_r2<- read_excel("AllenMill_04122023.xlsx",sheet = "Sheet2")
AllenMill_mouth_r2$CO2<-AllenMill_mouth_r2$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r2))
deltaCO2_atm<-(1.493)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r2$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)










View(AllenMill_042623)
Temp_F<-mean(AllenMill_042623$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-4690.8/1000000
depth<-0.7170969

AllenMill_mouth_r1<- read_excel("AllenMill_04262023.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(3.587)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)








View(AllenMill_050323)
setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
Temp_F<-mean(AllenMill_050323$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-3336.6/1000000
depth<-0.6848208

AllenMill_mouth_r1<- read_excel("AllenMill_05032023.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(9.337)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)










setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
Temp_F<-mean(AllenMill_051823$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2882.4/1000000
depth<-0.6176438

AllenMill_mouth_r1<- read_excel("AllenMill_05172023.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(6.033)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)











setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
Temp_F<-mean(AllenMill_052923$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-2928.0/1000000
depth<-0.8190202

AllenMill_mouth_r1<- read_excel("AllenMill_05292023_GD.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(2.664)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_mouth_r1<- read_excel("AllenMill_05292023_GD.xlsx",sheet = "Sheet2")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(6.023)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)











setwd("Z:/SpringsProject_Sam&Paul/CampbellSci/GasDome/AllenMill")
Temp_F<-mean(AllenMill_060823$Temp.x, na.rm=T)
mouthTemp_C<-fahrenheit.to.celsius(Temp_F)
mouthTemp_K<-mouthTemp_C+273.15
SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

pCO2_water<-5418.0/1000000
depth<-0.6518525

AllenMill_mouth_r1<- read_excel("AllenMill_0608023.xlsx",sheet = "Sheet1")
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2~Date, data = AllenMill_mouth_r1))
deltaCO2_atm<-(1.113e+01)*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)

AllenMill_mouth_r1<-read_excel("AllenMill_0608023.xlsx", 
                                   sheet = "Sheet2", col_types = c("date", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric"))
AllenMill_mouth_r1$CO2<-AllenMill_mouth_r1$CO2*6
(vent1<-lm(CO2avg~sec, data = AllenMill_mouth_r1))
deltaCO2_atm<-(13.52 )*2/1000000 #change in CO2 during float
pCO2_air<-max(AllenMill_mouth_r1$CO2)/1000000

n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
FCO2<-n/domeFoot_m2*60
exp<-2400*((1/mouthTemp_K)-(1/298.15))
(KH<-0.034*exp(exp)) #mol/L/atm
KH_1000<-KH*1000

(KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24) #m/d
kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
k600_vent1<-(KCO2_md*(600/SchmidtCO2hi))^(-2/3) #m/d

(KO2_1d_0829m2<-kO2/depth)
(KCO2_1d_0829m2<-KCO2_md/depth)
(k600_vent1_d<- k600_vent1/depth)
