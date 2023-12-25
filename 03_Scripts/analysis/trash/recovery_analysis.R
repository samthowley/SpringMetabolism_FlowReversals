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
library(measurements)
library(ggnewscale)
library(StreamMetabolism)
library(ggpmisc)

####GB####
GB <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric","numeric"))
GB$u<-abs(GB$stage_avg*-0.4032+0.3189)

GB<- GB %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-GB %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
GB<-left_join(GB,DO, by=c("day","month","year"))

GB$days <- as.Date(GB$Date)

GB<- GB %>% mutate(RI = case_when(
  stage_avg<0.55 ~ "low",
  stage_avg>=0.55 ~ "high"))
GB_low<-filter(GB, RI== 'low')

GBNEP0<-mean(GB_low$NEPnorm, na.rm=T)
GBER0<-mean(GB_low$ERnorm, na.rm=T)
GBGPP0<-mean(GB_low$GPPnorm, na.rm=T)
GBu0<-mean(GB_low$u, na.rm=T)


GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-26" & Date<"2022-10-20"~ 2))
GBFR<-filter(GBFR, RI== "2")


GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_avg, na.rm=T)
GBFR$disturb_count<-GBFR$consec-20

ggplot(GBFR, aes(x=disturb_count))+
  geom_point(aes(y=stage_avg, color=disturb_count), size=3)
  

ggplot(GBFR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(GBFR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-2)+
  geom_vline(xintercept=3)

x<-filter(GBFR, disturb_count>=-2& disturb_count <=3)
u9<-mean(x$u, na.rm = T)  
NEP9<-mean(x$NEPnorm, na.rm = T)  
ER9<-mean(x$ERnorm, na.rm = T)  
GPP9<-mean(x$GPPnorm, na.rm = T)  

GBrecov<-filter(GBFR,disturb_count>0)

GBrecov$NEP_RR<- GBrecov$NEP/mean(GB_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = GBrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov<-(1-InterceptNEP)/SlopeNEP)

GBrecov$O2_RR<- GBrecov$O2avg/mean(GB_low$O2avg, na.rm=T)
lm(O2_RR ~ disturb_count, data = GBrecov)
modO2<-lm(O2_RR ~ disturb_count, data = GBrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov<-(1-InterceptO2)/SlopeO2)


GBrecov$ER_RR<- GBrecov$ER/mean(GB_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = GBrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov<-(1-InterceptER)/SlopeER)


GBrecov$GPP_RR<- GBrecov$GPPavg/mean(GB_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = GBrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov<-(1-InterceptGPP)/SlopeGPP)


GBrecov$stage_RR<- GBrecov$stage_avg/mean(GB_low$stage_avg, na.rm=T)
modH<-lm(stage_RR ~ disturb_count, data = GBrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov<-(1-InterceptH)/SlopeH)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-25" & Date<"2023-8-17"~ 2))
GBFR<-filter(GBFR, RI== "2")


GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_avg, na.rm=T)
GBFR$disturb_count<-GBFR$consec-12

ggplot(GBFR, aes(x=disturb_count))+
  geom_line(aes(y=stage_avg), size=1)


ggplot(GBFR, aes(x=DOavg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(GBFR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-10)+
  geom_vline(xintercept=-3)

x<-filter(GBFR, disturb_count>=-10& disturb_count <=-3)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)  

GBrecov<-filter(GBFR,disturb_count>0)


GBrecov$NEP_RR<- GBrecov$NEP/mean(GB_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = GBrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov09<-(1-InterceptNEP)/SlopeNEP)

GBrecov$O2_RR<- GBrecov$O2avg/mean(GB_low$O2avg, na.rm=T)
lm(O2_RR ~ disturb_count, data = GBrecov)
modO2<-lm(O2_RR ~ disturb_count, data = GBrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov09<-(1-InterceptO2)/SlopeO2)


GBrecov$ER_RR<- GBrecov$ER/mean(GB_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = GBrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov09<-(1-InterceptER)/SlopeER)


GBrecov$GPP_RR<- GBrecov$GPPavg/mean(GB_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = GBrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov09<-(1-InterceptGPP)/SlopeGPP)


GBrecov$stage_RR<- GBrecov$stage_avg/mean(GB_low$stage_avg, na.rm=T)
modH<-lm(stage_RR ~ disturb_count, data = GBrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov09<-(1-InterceptH)/SlopeH)


event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(GBNEP0, NEP8, "", "", NEP9))
GPPnorm<-as.numeric(c(GBGPP0, GPP8, "", "", GPP9))
ERnorm<-as.numeric(c(GBER0, ER8, "", "", ER9))
u<-as.numeric(c(GBu0, u8, "","", u9))
stage_recovery<-as.numeric(c("", H_recov, "","",H_recov09))
NEP_recovery<-as.numeric(c("", NEP_recov, "","", NEP_recov09))
GPP_recovery<-as.numeric(c("", GPP_recov, "","",GPP_recov09))
ER_recovery<-as.numeric(c("", ER_recov, "","",ER_recov09))
NEP_reduction<-as.numeric(c("",NEP8/GBNEP0, "","",NEP9/GBNEP0))
GPP_reduction<-as.numeric(c("",GPP8/GBGPP0, "","",GPP9/GBGPP0))
ER_reduction<-as.numeric(c("",GBER0/ER8, "","",GBER0/ER9))


GB<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,GPP_recovery,ER_recovery,
                NEP_reduction,GPP_reduction,ER_reduction)
GB$site<-"GB"

####Otter#####

Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"))

Otter$u<-Otter$stage_avg*-0.0686+0.16

Otter$mouthTemp_K<-Otter$Mouth_Temp_C+273.15
Otter$exp<-2400*((1/Otter$mouthTemp_K)-(1/298.15))
Otter$KH<-0.034*2.178^(Otter$exp)#mol/L/atm

Otter$DO_mol<-Otter$DO/32000
Otter$DO_Sat_mol<-Otter$Mouth_DO_sat/32000

Otter$K600_md<-Otter$K600_avg*Otter$stage_avg

Otter$mouthTemp_K<-Otter$Mouth_Temp_C+273.15
Otter$SchmidtO2hi<-1568-86.04*Otter$Mouth_Temp_C+2.142*Otter$Mouth_Temp_C^2-0.0216*Otter$Mouth_Temp_C^3
Otter$SchmidtCO2hi<-1742-91.24*Otter$Mouth_Temp_C+2.208*Otter$Mouth_Temp_C^2-0.0219*Otter$Mouth_Temp_C^3

Otter$x<-(600/Otter$SchmidtCO2hi)^(-2/3)

Otter$KCO2_md<-Otter$K600_md/Otter$x

Otter$KO2_md<-Otter$KCO2_md*(Otter$SchmidtCO2hi/Otter$SchmidtO2hi)^(-2/3)
Otter$KCO2_1d<-Otter$KCO2_md/Otter$stage_avg
Otter$KO2_1d<-Otter$KO2_md/Otter$stage_avg

Otter$'O2_mol_m2_1d'<-Otter$stage_avg*Otter$KO2_1d*(Otter$DO_mol-Otter$DO_Sat_mol)
Otter$'O2_mmol_m2_1d'<-Otter$'O2_mol_m2_1d'*10^3

Otter<- Otter %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-Otter %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
Otter<-left_join(Otter,DO, by=c("day","month","year"))

Otter$days <- as.Date(Otter$Date)

Otter<- Otter %>% mutate(RI = case_when(
  stage_avg<0.82 ~ "low",
  stage_avg<1.12 ~ "moderate",
  stage_avg>=1.12 ~ "high"))

Otter_low<-filter(Otter, RI== 'low')

u0<-mean(Otter_low$u, na.rm=T)
NEP0<-mean(Otter_low$NEPnorm, na.rm=T)
ER0<-mean(Otter_low$ERnorm, na.rm=T)
GPP0<-mean(Otter_low$GPPnorm, na.rm=T)


OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-18"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-26

ggplot(OtBO, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=O2avg), size=1)+
  geom_vline(xintercept=-7)+
  geom_vline(xintercept=2)
x<-filter(OtBO, disturb_count>=-7& disturb_count <=2)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)  

Otrecov<-filter(OtBO,disturb_count>0)

Otrecov$NEP_RR<- Otrecov$NEP/mean(Otter_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)


Otrecov$DO_RR<- Otrecov$O2_mmol_m2_1d/mean(Otter_low$O2_mmol_m2_1d, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Otrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov8<-(1-InterceptO2)/SlopeO2)

Otrecov$ER_RR<- Otrecov$ER/mean(Otter_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

Otrecov$GPP_RR<- Otrecov$GPPavg/mean(Otter_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)


Otrecov$stage_RR<- Otrecov$stage_avg/mean(Otter_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-(1-InterceptH)/SlopeH)


OtFR<- Otter %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-5-10"~ 2))
OtFR<-filter(OtFR, RI== "2")

OtFR <- OtFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

OtFR$disturb_count<-OtFR$consec-26

ggplot(OtFR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(OtFR, aes(x=disturb_count))+
  geom_line(aes(y=O2avg), size=1)+
  geom_vline(xintercept=-1)+
  geom_vline(xintercept=8)

x<-filter(OtFR, disturb_count>=-1& disturb_count <=8)
u2<-mean(x$u, na.rm = T)  
NEP2<-mean(x$NEPnorm, na.rm = T)  
ER2<-mean(x$ERnorm, na.rm = T)  
GPP2<-mean(x$GPPnorm, na.rm = T)  

Otrecov<-filter(OtFR,disturb_count>0)

Otrecov$NEP_RR<- Otrecov$NEP/mean(Otter_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

Otrecov$DO_RR<- Otrecov$O2_mmol_m2_1d/mean(Otter_low$O2_mmol_m2_1d, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Otrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov2<-(1-InterceptO2)/SlopeO2)

Otrecov$ER_RR<- Otrecov$ER/mean(Otter_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

Otrecov$GPP_RR<- Otrecov$GPPavg/mean(Otter_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)

Otrecov$stage_RR<- Otrecov$stage_avg/mean(Otter_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-(1-InterceptH)/SlopeH)


OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2023-06-17" & Date<"2023-08-10"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-19

ggplot(OtBO, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=stage_avg), size=1)+
  geom_vline(xintercept=-3)+
  geom_vline(xintercept=3)
x<-filter(OtBO, disturb_count>=-3& disturb_count <=3)
u6<-mean(x$u, na.rm = T)  
NEP6<-mean(x$NEPnorm, na.rm = T)  
ER6<-mean(x$ERnorm, na.rm = T)  
GPP6<-mean(x$GPPnorm, na.rm = T)  

Otrecov<-filter(OtBO,disturb_count>0)


Otrecov$NEP_RR<- Otrecov$NEP/mean(Otter_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

Otrecov$DO_RR<- Otrecov$O2_mmol_m2_1d/mean(Otter_low$O2_mmol_m2_1d, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Otrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov6<-(1-InterceptO2)/SlopeO2)

Otrecov$ER_RR<- Otrecov$ER/mean(Otter_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-(1-InterceptER)/SlopeER)

Otrecov$GPP_RR<- Otrecov$GPPavg/mean(Otter_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-(1-InterceptGPP)/SlopeGPP)

Otrecov$stage_RR<- Otrecov$stage_avg/mean(Otter_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-(1-InterceptH)/SlopeH)

event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(NEP0, NEP8, NEP2,NEP6,""))
GPPnorm<-as.numeric(c(GPP0, GPP8, GPP2, GPP6,""))
ERnorm<-as.numeric(c(ER0, ER8, ER2, ER6,""))
u<-as.numeric(c(u0, u8, u2,u6,""))
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,""))
NEP_reduction<-as.numeric(c("",NEP8/NEP0, NEP2/NEP0,NEP6/NEP0,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER0/ER8, ER0/ER2,ER0/ER6,""))


Otter<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,
                   GPP_recovery,ER_recovery,NEP_reduction,GPP_reduction,ER_reduction)
Otter$site<-"Otter"


####AllenMill##########
AllenMill <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))
AllenMill$"u"<-(AllenMill$stage_avg*-0.2136+0.4426)

AllenMill$mouthTemp_K<-AllenMill$Mouth_Temp_C+273.15
AllenMill$exp<-2400*((1/AllenMill$mouthTemp_K)-(1/298.15))
AllenMill$KH<-0.034*2.178^(AllenMill$exp)#mol/L/atm

AllenMill$CO2_atm<-AllenMill$CO2/1000000
AllenMill$CO2_mol<-AllenMill$CO2_atm*AllenMill$KH
AllenMill$DO_mol<-AllenMill$DO/32000

AllenMill$mouthTemp_K<-AllenMill$Mouth_Temp_C+273.15
AllenMill$SchmidtO2hi<-1568-86.04*AllenMill$Mouth_Temp_C+2.142*AllenMill$Mouth_Temp_C^2-0.0216*AllenMill$Mouth_Temp_C^3
AllenMill$SchmidtCO2hi<-1742-91.24*AllenMill$Mouth_Temp_C+2.208*AllenMill$Mouth_Temp_C^2-0.0219*AllenMill$Mouth_Temp_C^3

AllenMill$x<-(600/AllenMill$SchmidtCO2hi)^(-2/3)
AllenMill$K600_md<-AllenMill$K600_avg*AllenMill$stage_avg

AllenMill$KCO2_md<-AllenMill$K600_md/AllenMill$x
AllenMill$KO2_md<-AllenMill$KCO2_md*(AllenMill$SchmidtCO2hi/AllenMill$SchmidtO2hi)^(-2/3)
AllenMill$KCO2_1d<-AllenMill$KCO2_md/AllenMill$stage_avg
AllenMill$KO2_1d<-AllenMill$KO2_md/AllenMill$stage_avg

AllenMill$CO2_atm<-AllenMill$CO2/1000000
AllenMill$CO2_mol<-AllenMill$CO2_atm*AllenMill$KH
AllenMill$DO_mol<-AllenMill$DO/32000

AllenMill$Do_Sat<-Cs(AllenMill$Mouth_Temp_C)
AllenMill$DO_Sat_mol<-AllenMill$Do_Sat/32000
(AllenMill$CO2_Sat_mol<-(420/1000000)*AllenMill$KH)

AllenMill$'O2_mol_m2_1d'<-AllenMill$stage_avg*AllenMill$KO2_1d*(AllenMill$DO_mol-AllenMill$DO_Sat_mol)
AllenMill$'CO2_mol_m2_1d'<-AllenMill$KCO2_1d*AllenMill$stage_avg*(AllenMill$CO2_mol-AllenMill$CO2_Sat_mol)

AllenMill$'O2_mmol_m2_1d'<-AllenMill$'O2_mol_m2_1d'*10^3
AllenMill$'CO2_mmol_m2_1d'<-AllenMill$'CO2_mol_m2_1d'*10^3

AllenMill<- AllenMill %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-AllenMill %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
AllenMill<-left_join(AllenMill,DO, by=c("day","month","year"))

c<-AllenMill %>% group_by(day,month,year) %>% summarize(metCO2avg = mean(CO2_mmol_m2_1d, na.rm = TRUE))
AllenMill<-left_join(AllenMill,c, by=c("day","month","year"))

AllenMill$days <- as.Date(AllenMill$Date)

AM<- AllenMill %>% mutate(RI = case_when(
  stage_avg<0.93 ~ "low",
  stage_avg<1.37 ~ "moderate",
  stage_avg>=1.37 ~ "high"))
AM_low<-filter(AM, RI== 'low')
u0<-mean(AM_low$u, na.rm=T)
NEP0<-mean(AM_low$NEPnorm, na.rm=T)
ER0<-mean(AM_low$ERnorm, na.rm=T)
GPP0<-mean(AM_low$GPPnorm, na.rm=T)



AMBO<- AM %>% mutate(RI = case_when(
  Date> "2022-08-26" & Date<"2022-10-17"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-20

ggplot(AMBO, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-8)+
  geom_vline(xintercept=9)
x<-filter(AMBO, disturb_count>=-8& disturb_count <=9)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)  

AMBOrecov<-filter(AMBO,disturb_count>0)

AMBOrecov$NEP_RR<- AMBOrecov$NEP/mean(AM_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)


AMBOrecov$DO_RR<- AMBOrecov$O2avg/mean(AM_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov8<-(1-InterceptO2)/SlopeO2)


AMBOrecov$ER_RR<- AMBOrecov$ER/mean(AM_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

AMBOrecov$GPP_RR<- AMBOrecov$GPPavg/mean(AM_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

AMBOrecov$stage_RR<- AMBOrecov$stage_avg/mean(AM_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = AMBOrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-(1-InterceptH)/SlopeH)


AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-04-10"~ 2))
AMFR<-filter(AMFR, RI== "2")

AMFR <- AMFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMFR$stage_avg, na.rm=T)
AMFR$disturb_count<-AMFR$consec-20

ggplot(AMFR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(AMFR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=5)+
  geom_vline(xintercept=9)

x<-filter(AMFR, disturb_count>=6& disturb_count <=9)
u2<-mean(x$u, na.rm = T)  
NEP2<-mean(x$NEPnorm, na.rm = T)  
ER2<-mean(x$ERnorm, na.rm = T)  
GPP2<-mean(x$GPPnorm, na.rm = T)  

AMFRrecov<-filter(AMFR,disturb_count>0)

AMFRrecov$NEP_RR<- AMFRrecov$NEP/mean(AM_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = AMFRrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)


AMFRrecov$O2_RR<- AMFRrecov$O2_mmol_m2_1d/mean(AM_low$O2_mmol_m2_1d, na.rm=T)
modO2<-lm(O2_RR ~ disturb_count, data = AMFRrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov2<-(1-InterceptO2)/SlopeO2)


AMFRrecov$ER_RR<- AMFRrecov$ER/mean(AM_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = AMFRrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

modGPP<-AMFRrecov$GPP_RR<- AMFRrecov$GPPavg/mean(AM_low$GPPavg, na.rm=T)
lm(GPP_RR ~ disturb_count, data = AMFRrecov)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)


AMFRrecov$stage_RR<- AMFRrecov$stage_avg/mean(AM_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = AMFRrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-(1-InterceptH)/SlopeH)




AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-06-16" & Date<"2023-08-01"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-12

AMBOrecov<-filter(AMBO,disturb_count>0)

ggplot(AMFR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=0.4)+
  geom_vline(xintercept=-4)+
  geom_vline(xintercept=7)
x<-filter(AMBO, disturb_count>=-4& disturb_count <=7)
u6<-mean(x$u, na.rm = T)  
NEP6<-mean(x$NEPnorm, na.rm = T)  
ER6<-min(x$ERnorm, na.rm = T)  
GPP6<-mean(x$GPPnorm, na.rm = T)  

AMBOrecov<-filter(AMBO,disturb_count>0)

AMBOrecov$NEP_RR<- AMBOrecov$NEP/mean(AM_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

AMBOrecov$DO_RR<- AMBOrecov$O2avg/mean(AM_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov6<-(1-InterceptO2)/SlopeO2)

AMBOrecov$ER_RR<- AMBOrecov$ER/mean(AM_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-abs((1-InterceptER)/SlopeER))

AMBOrecov$GPP_RR<- AMBOrecov$GPPavg/mean(AM_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = AMBOrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-(1-InterceptGPP)/SlopeGPP)

AMBOrecov$stage_RR<- AMBOrecov$stage_avg/mean(AM_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = AMBOrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-(1-InterceptH)/SlopeH)


event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(NEP0, NEP8, NEP2,NEP6,""))
GPPnorm<-as.numeric(c(GPP0, GPP8, GPP2, GPP6,""))
ERnorm<-as.numeric(c(ER0, ER8, ER2, ER6,""))
u<-as.numeric(c(u0, u8, u2,u6,""))
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,""))
NEP_reduction<-as.numeric(c("",NEP8/NEP0, NEP2/NEP0,NEP6/NEP0,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER8/ER0, ER2/ER0,ER6/ER0,""))


AM<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,GPP_recovery,
                ER_recovery,NEP_reduction,GPP_reduction,ER_reduction)
AM$site<-"AM"


####LF############
LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))
LF$u<- -0.12*LF$stage_avg + 0.18

LF$mouthTemp_K<-LF$Mouth_Temp_C+273.15
LF$exp<-2400*((1/LF$mouthTemp_K)-(1/298.15))
LF$KH<-0.034*2.178^(LF$exp)#mol/L/atm

LF$DO_mol<-LF$DO/32000
LF$DO_Sat_mol<-LF$Mouth_DO_sat/32000

LF$K600_md<-LF$K600_avg*LF$stage_avg

LF$mouthTemp_K<-LF$Mouth_Temp_C+273.15
LF$SchmidtO2hi<-1568-86.04*LF$Mouth_Temp_C+2.142*LF$Mouth_Temp_C^2-0.0216*LF$Mouth_Temp_C^3
LF$SchmidtCO2hi<-1742-91.24*LF$Mouth_Temp_C+2.208*LF$Mouth_Temp_C^2-0.0219*LF$Mouth_Temp_C^3

LF$x<-(600/LF$SchmidtCO2hi)^(-2/3)

LF$KCO2_md<-LF$K600_md/LF$x
LF$KO2_md<-LF$KCO2_md*(LF$SchmidtCO2hi/LF$SchmidtO2hi)^(-2/3)
LF$KO2_1d<-LF$KO2_md/LF$stage_avg

LF$'O2_mol_m2_1d'<-LF$stage_avg*LF$KO2_1d*(LF$DO_mol-LF$DO_Sat_mol)
LF$'O2_mmol_m2_1d'<-LF$'O2_mol_m2_1d'*10^3

LF<- LF %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-LF %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
LF<-left_join(LF,DO, by=c("day","month","year"))

LF$days <- as.Date(LF$Date)

LF<- LF %>% mutate(RI = case_when(
  stage_avg<0.33 ~ "low",
  stage_avg<0.61 ~ "moderate",
  stage_avg>=0.61 ~ "high"))
LF_low<-filter(LF, RI== 'low')
u0<-mean(LF_low$u, na.rm = T)
NEP0<-mean(LF_low$NEPnorm, na.rm = T)
ER0<-mean(LF_low$ERnorm, na.rm = T)
GPP0<-mean(LF_low$GPPnorm, na.rm = T)





LFRR<- LF %>% mutate(RI = case_when(
  Date> "2022-08-18" & Date<"2022-10-16"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_avg, na.rm=T)
LFRR$disturb_count<-LFRR$consec-28

ggplot(LFRR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-10)+
  geom_vline(xintercept=10)
x<-filter(LFRR, disturb_count>=-10& disturb_count <=10)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)

LFrecov<-filter(LFRR,disturb_count>0)

LFrecov$NEP_RR<- LFrecov$NEP/mean(LF_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

LFrecov$DO_RR<- LFrecov$O2avg/mean(LF_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = LFrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov8<-(1-InterceptO2)/SlopeO2)

LFrecov$ER_RR<- LFrecov$ER/mean(LF_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

LFrecov$GPP_RR<- LFrecov$GPPavg/mean(LF_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

LFrecov$stage_RR<- LFrecov$stage_avg/mean(LF_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))



LFFR<- LF %>%mutate(RI = case_when(
  Date> "2023-01-30" & Date<"2023-04-21"~ 2))
LFFR<-filter(LFFR, RI== "2")

LFFR <- LFFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFFR$stage_avg, na.rm=T)
LFFR$disturb_count<-LFFR$consec-23

ggplot(LFFR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=1)+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=18)

x<-filter(LFFR, disturb_count>=0 & disturb_count <=18)
u2<-mean(x$u, na.rm = T)  
NEP2<-mean(x$NEPnorm, na.rm = T)  
ER2<-mean(x$ERnorm, na.rm = T)  
GPP2<-mean(x$GPPnorm, na.rm = T)  

LFrecov<-filter(LFFR,disturb_count>0)

LFrecov$NEP_RR<- LFrecov$NEP/mean(LF_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

LFrecov$DO_RR<- LFrecov$O2avg/mean(LF_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = LFrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov2<-(1-InterceptO2)/SlopeO2)

LFrecov$ER_RR<- LFrecov$ER/mean(LF_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

LFrecov$GPP_RR<- LFrecov$GPPavg/mean(LF_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)

LFrecov$stage_RR<- LFrecov$stage_avg/mean(LF_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))





LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-18" & Date<"2023-08-03"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_avg, na.rm=T)
LFRR$disturb_count<-LFRR$consec-14

ggplot(LFRR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")



ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-4)+
  geom_vline(xintercept=5)
x<-filter(LFRR, disturb_count>=-4& disturb_count <=5)
u6<-mean(x$u, na.rm = T)  
NEP6<-mean(x$NEPnorm, na.rm = T)  
ER6<-mean(x$ERnorm, na.rm = T)  
GPP6<-mean(x$GPPnorm, na.rm = T)  
LFrecov<-filter(LFRR,disturb_count>0)

LFrecov$NEP_RR<- LFrecov$NEP/mean(LF_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

LFrecov$DO_RR<- LFrecov$O2avg/mean(LF_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = LFrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov6<-(1-InterceptO2)/SlopeO2)

LFrecov$ER_RR<- LFrecov$ER/mean(LF_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-(1-InterceptER)/SlopeER)

LFrecov$GPP_RR<- LFrecov$GPPavg/mean(LF_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-(1-InterceptGPP)/SlopeGPP)

LFrecov$stage_RR<- LFrecov$stage_avg/mean(LF_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-abs((1-InterceptH)/SlopeH))

event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(NEP0, NEP8, NEP2,NEP6,""))
GPPnorm<-as.numeric(c(GPP0, GPP8, GPP2, GPP6,""))
ERnorm<-as.numeric(c(ER0, ER8, ER2, ER6,""))
u<-as.numeric(c(u0, u8, u2,u6,""))
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,""))
NEP_reduction<-as.numeric(c("",NEP8/NEP0, NEP2/NEP0,NEP6/NEP0,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER0/ER8, ER0/ER2,ER0/ER6,""))

LF<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,
                GPP_recovery,ER_recovery,NEP_reduction,GPP_reduction,ER_reduction)
LF$site<-"LF"

######Ichetucknee#######

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))

ggplot(Ich, aes(x=Date))+
  geom_line(aes(y=NEP))
  
Ich$u<-Ich$stage_avg*-0.0773+0.3319

Ich$mouthTemp_K<-Ich$Mouth_Temp_C+273.15
Ich$exp<-2400*((1/Ich$mouthTemp_K)-(1/298.15))
Ich$KH<-0.034*2.178^(Ich$exp)#mol/L/atm

Ich$CO2_atm<-Ich$CO2hi/1000000
Ich$CO2_mol<-Ich$CO2_atm*Ich$KH
Ich$DO_mol<-Ich$DO/32000

Ich$mouthTemp_K<-Ich$Mouth_Temp_C+273.15
Ich$SchmidtO2hi<-1568-86.04*Ich$Mouth_Temp_C+2.142*Ich$Mouth_Temp_C^2-0.0216*Ich$Mouth_Temp_C^3
Ich$SchmidtCO2hi<-1742-91.24*Ich$Mouth_Temp_C+2.208*Ich$Mouth_Temp_C^2-0.0219*Ich$Mouth_Temp_C^3

Ich$x<-(600/Ich$SchmidtCO2hi)^(-2/3)
Ich$K600_md<-Ich$K600_avg*Ich$stage_avg

Ich$KCO2_md<-Ich$K600_md/Ich$x
Ich$KO2_md<-Ich$KCO2_md*(Ich$SchmidtCO2hi/Ich$SchmidtO2hi)^(-2/3)
Ich$KCO2_1d<-Ich$KCO2_md/Ich$stage_avg
Ich$KO2_1d<-Ich$KO2_md/Ich$stage_avg

Ich$CO2_atm<-Ich$CO2hi/1000000
Ich$CO2_mol<-Ich$CO2_atm*Ich$KH
Ich$DO_mol<-Ich$DO/32000

Ich$Do_Sat<-Cs(Ich$Mouth_Temp_C)
Ich$DO_Sat_mol<-Ich$Do_Sat/32000
(Ich$CO2_Sat_mol<-(420/1000000)*Ich$KH)

Ich$'O2_mol_m2_1d'<-Ich$stage_avg*Ich$KO2_1d*(Ich$DO_mol-Ich$DO_Sat_mol)
Ich$'CO2_mol_m2_1d'<-Ich$KCO2_1d*Ich$stage_avg*(Ich$CO2_mol-Ich$CO2_Sat_mol)

Ich$'O2_mmol_m2_1d'<-Ich$'O2_mol_m2_1d'*10^3
Ich$'CO2_mmol_m2_1d'<-Ich$'CO2_mol_m2_1d'*10^3

Ich<- Ich %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-Ich %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
Ich<-left_join(Ich,DO, by=c("day","month","year"))
names(Ich)
Ich$days <- as.Date(Ich$Date)

Ich<- Ich %>% mutate(RI = case_when(
  stage_avg<0.93 ~ "low",
  stage_avg<1.37 ~ "moderate",
  stage_avg>=1.37 ~ "high"))

Ich_low<-filter(Ich, RI== 'low')
u0<-mean(Ich_low$u, na.rm=T)
NEP0<-mean(Ich_low$NEPnorm, na.rm=T)
ER0<-mean(Ich_low$ERnorm, na.rm=T)
GPP0<-mean(Ich_low$GPPnorm, na.rm=T)


IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2022-08-17" & Date<"2022-10-18"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_avg, na.rm=T)
IchRR$disturb_count<-IchRR$consec-31

ggplot(IchRR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-9)+
  geom_vline(xintercept=4)

x<-filter(IchRR, disturb_count>=-9 & disturb_count <=4)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)  

Ichrecov<-filter(IchRR,disturb_count>0)

Ichrecov$NEP_RR<- Ichrecov$NEP/mean(Ich_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

Ichrecov$DO_RR<- Ichrecov$O2avg/mean(Ich_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov8<-(1-InterceptO2)/SlopeO2)

Ichrecov$ER_RR<- Ichrecov$ER/mean(Ich_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

Ichrecov$GPP_RR<- Ichrecov$GPPavg/mean(Ich_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-abs((1-InterceptGPP)/SlopeGPP))

Ichrecov$stage_RR<- Ichrecov$stage_avg/mean(Ich_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))



IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-04-12"~ 2))
IchRR<-filter(IchRR, RI== "2")

  
IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_avg, na.rm=T)
IchRR$disturb_count<-IchRR$consec-23


ggplot(IchRR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-5)+
  geom_vline(xintercept=5)

x<-filter(IchRR, disturb_count>=-8 & disturb_count <=0)
u2<-mean(x$u, na.rm = T)  
NEP2<-mean(x$NEPnorm, na.rm = T)  
ER2<-mean(x$ERnorm, na.rm = T)  
GPP2<-mean(x$GPPnorm, na.rm = T)  

Ichrecov<-filter(IchRR,disturb_count>0)

IchRR$NEP_RR<- IchRR$NEP/mean(Ich_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = IchRR)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-abs((1-InterceptNEP)/SlopeNEP))

Ichrecov$DO_RR<- Ichrecov$O2avg/mean(Ich_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov2<-(1-InterceptO2)/SlopeO2)

IchRR$ER_RR<- IchRR$ER/mean(Ich_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = IchRR)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-abs((1-InterceptER)/SlopeER))

Ichrecov$GPP_RR<- Ichrecov$GPPavg/mean(Ich_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)

Ichrecov$stage_RR<- Ichrecov$stage_avg/mean(Ich_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))




IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-06-20" & Date<"2023-08-11"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_avg, na.rm=T)
IchRR$disturb_count<-IchRR$consec-11

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=stage_avg), size=3)

ggplot(IchRR, aes(x=stage_avg))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=6)+
  geom_vline(xintercept=10)

x<-filter(IchRR, disturb_count>=6 & disturb_count <=10)
u6<-mean(x$u, na.rm = T)  
NEP6<-mean(x$NEPnorm, na.rm = T)  
ER6<-mean(x$ERnorm, na.rm = T)  
GPP6<-mean(x$GPPnorm, na.rm = T)  

Ichrecov<-filter(IchRR,disturb_count>0)

Ichrecov$NEP_RR<- Ichrecov$NEP/mean(Ich_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

Ichrecov$DO_RR<- Ichrecov$O2avg/mean(Ich_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov6<-(1-InterceptO2)/SlopeO2)

Ichrecov$ER_RR<- Ichrecov$ER/mean(Ich_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-abs((1-InterceptER)/SlopeER))

Ichrecov$GPP_RR<- Ichrecov$GPPavg/mean(Ich_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-abs((1-InterceptGPP)/SlopeGPP))

Ichrecov$stage_RR<- Ichrecov$stage_avg/mean(Ich_low$stage_avg, na.rm=T)
modH<-lm(stage_avg ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-abs((1-InterceptH)/SlopeH))


event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(NEP0, NEP8, NEP2,NEP6,""))
GPPnorm<-as.numeric(c(GPP0, GPP8, GPP2, GPP6,""))
ERnorm<-as.numeric(c(ER0, ER8, ER2, ER6,""))
u<-as.numeric(c(u0, u8, u2,u6,""))
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", GPP_recov8, "",GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, "",ER_recov6,""))
NEP_reduction<-as.numeric(c("",NEP8/NEP0, NEP2/NEP0,NEP6/NEP0,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, ""," ",""))
ER_reduction<-as.numeric(c("",ER0/ER8, "",ER0/ER6,""))

Ichetucknee<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,
                         GPP_recovery,ER_recovery,NEP_reduction,GPP_reduction,ER_reduction)
Ichetucknee$site<-"ICHE_down"

### US 27####
US27 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
US27$day <- as.Date(US27$Date)
US27 <- aggregate(US27, by=list(US27$day), FUN='mean')

US27$depth<-conv_unit(US27$depth, "ft", "m")
US27$depth<-US27$depth-(max(US27$depth, na.rm=T)-1.5)
US27$u<-US27$discharge/(US27$depth*15)/100

min(US27$u, na.rm = T)

US27$mouthTemp_K<-US27$temp.water+273.15
US27$exp<-2400*((1/US27$mouthTemp_K)-(1/298.15))
US27$KH<-0.034*2.178^(US27$exp)#mol/L/atm

US27$DO_mol<-US27$DO.obs/32000

US27$mouthTemp_K<-US27$temp.water+273.15
US27$SchmidtO2hi<-1568-86.04*US27$temp.water+2.142*US27$temp.water^2-0.0216*US27$temp.water^3
US27$SchmidtCO2hi<-1742-91.24*US27$temp.water+2.208*US27$temp.water^2-0.0219*US27$temp.water^3

US27$x<-(600/US27$SchmidtCO2hi)^(-2/3)
US27$K600_md<-US27$K600_avg*US27$depth

US27$KCO2_md<-US27$K600_md/US27$x
US27$KO2_md<-US27$KCO2_md*(US27$SchmidtCO2hi/US27$SchmidtO2hi)^(-2/3)
US27$KCO2_1d<-US27$KCO2_md/US27$depth
US27$KO2_1d<-US27$KO2_md/US27$depth

US27$DO_mol<-US27$DO.obs/32000

US27$Do_Sat<-Cs(US27$temp.water)
US27$DO_Sat_mol<-US27$Do_Sat/32000
(US27$CO2_Sat_mol<-(420/1000000)*US27$KH)

US27$'O2_mol_m2_1d'<-US27$depth*US27$KO2_1d*(US27$DO_mol-US27$DO_Sat_mol)
US27$'O2_mmol_m2_1d'<-US27$'O2_mol_m2_1d'*10^3

US27<- US27 %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-US27 %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
US27<-left_join(US27,DO, by=c("day","month","year"))
names(US27)
US27$days <- as.Date(US27$Date)

US27<- US27 %>% mutate(RI = case_when(
  depth<1.12 ~ "low",
  depth>=1.12 ~ "high"))

US27_low<-filter(US27, RI== 'low')
u0<-mean(US27_low$u, na.rm=T)
NEP0<-mean(US27_low$NEPnorm, na.rm=T)
ER0<-mean(US27_low$ERnorm, na.rm=T)
GPP0<-mean(US27_low$GPPnorm, na.rm=T)


US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2022-08-17" & Date<"2022-10-18"~ 2))
US27RR<-filter(US27RR, RI== "2")

US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$depth, na.rm=T)
US27RR$disturb_count<-US27RR$consec-31

ggplot(US27RR, aes(x=depth))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")

ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-9)+
  geom_vline(xintercept=4)

x<-filter(US27RR, disturb_count>=-9 & disturb_count <=4)
u8<-mean(x$u, na.rm = T)  
NEP8<-mean(x$NEPnorm, na.rm = T)  
ER8<-mean(x$ERnorm, na.rm = T)  
GPP8<-mean(x$GPPnorm, na.rm = T)  

US27recov<-filter(US27RR,disturb_count>0)

US27recov$NEP_RR<- US27recov$NEP/mean(US27_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

US27recov$DO_RR<- US27recov$O2avg/mean(US27_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = US27recov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov8<-(1-InterceptO2)/SlopeO2)

US27recov$ER_RR<- US27recov$ER/mean(US27_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = US27recov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

US27recov$GPP_RR<- US27recov$GPPavg/mean(US27_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-abs((1-InterceptGPP)/SlopeGPP))


US27recov$stage_RR<- US27recov$depth/mean(US27_low$depth, na.rm=T)
modH<-lm(stage_RR ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))



US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-04-12"~ 2))
US27RR<-filter(US27RR, RI== "2")


US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$depth, na.rm=T)
US27RR$disturb_count<-US27RR$consec-24


ggplot(US27RR, aes(x=depth))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-3)+
  geom_vline(xintercept=5)

x<-filter(US27RR, disturb_count>=-8 & disturb_count <=0)
u2<-mean(x$u, na.rm = T)  
NEP2<-mean(x$NEPnorm, na.rm = T)  
ER2<-mean(x$ERnorm, na.rm = T)  
GPP2<-mean(x$GPPnorm, na.rm = T)  

US27recov<-filter(US27RR,disturb_count>0)

US27recov$NEP_RR<- US27recov$NEP/mean(US27_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-abs((1-InterceptNEP)/SlopeNEP))

US27recov$DO_RR<- US27recov$O2avg/mean(US27_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = US27recov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov2<-(1-InterceptO2)/SlopeO2)

US27RR$ER_RR<- US27RR$ER/mean(US27RR$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = US27RR)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

US27recov$GPP_RR<- US27recov$GPPavg/mean(US27_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)


US27recov$stage_RR<- US27recov$depth/mean(US27_low$depth, na.rm=T)
modH<-lm(stage_RR ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))



US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2023-06-20" & Date<"2023-08-15"~ 2))
US27RR<-filter(US27RR, RI== "2")


US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$depth, na.rm=T)
US27RR$disturb_count<-US27RR$consec-13

ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=depth), size=3)

ggplot(US27RR, aes(x=depth))+
  geom_point(aes(y=NEP, color=disturb_count), size=3)+
  scale_color_gradient(low="red", high="blue")


ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=6)+
  geom_vline(xintercept=10)

x<-filter(US27RR, disturb_count>=6 & disturb_count <=10)
u6<-mean(x$u, na.rm = T)  
NEP6<-mean(x$NEPnorm, na.rm = T)  
ER6<-mean(x$ERnorm, na.rm = T)  
GPP6<-mean(x$GPPnorm, na.rm = T)  

US27recov<-filter(US27RR,disturb_count>0)

US27recov$NEP_RR<- US27recov$NEP/mean(US27_low$NEP, na.rm=T)
modNEP<-lm(NEP_RR ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-abs((1-InterceptNEP)/SlopeNEP))

US27recov$DO_RR<- US27recov$O2avg/mean(US27_low$O2avg, na.rm=T)
modO2<-lm(DO_RR ~ disturb_count, data = US27recov)
cf <- coef(modO2)
(InterceptO2<- cf[1])
(SlopeO2 <- cf[2])
(O2_recov6<-(1-InterceptO2)/SlopeO2)

US27recov$ER_RR<- US27recov$ER/mean(US27_low$ER, na.rm=T)
modER<-lm(ER_RR ~ disturb_count, data = US27recov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-abs((1-InterceptER)/SlopeER))

US27recov$GPP_RR<- US27recov$GPPavg/mean(US27_low$GPPavg, na.rm=T)
modGPP<-lm(GPP_RR ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-abs((1-InterceptGPP)/SlopeGPP))


US27recov$stage_RR<- US27recov$depth/mean(US27_low$depth, na.rm=T)
modH<-lm(stage_RR ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-abs((1-InterceptH)/SlopeH))


event<-c('0','08','02','06','09')
NEPnorm<-as.numeric(c(NEP0, NEP8, NEP2,NEP6,''))
GPPnorm<-as.numeric(c(GPP0, GPP8, GPP2, GPP6,''))
ERnorm<-as.numeric(c(ER0, ER8, ER2, ER6,''))
u<-as.numeric(c(u0, u8, u2,u6,''))
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,"",''))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,"",''))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,"",''))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,"",''))
NEP_reduction<-as.numeric(c("",NEP8/NEP0, NEP2/NEP0,"",''))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,"",''))
ER_reduction<-as.numeric(c("",ER8/ER0, "","",''))

US27<- data.frame(event,NEPnorm,GPPnorm,ERnorm,stage_recovery,u, NEP_recovery,
                  GPP_recovery,ER_recovery,NEP_reduction,GPP_reduction,ER_reduction)
US27$site<-"ICHE_up"

#
###############

R_R<-rbind(US27,Ichetucknee, LF, GB, AM, Otter)
View(R_R)
R_R$NEP_reduction<- (1-R_R$NEP_reduction)*100
R_R$GPP_reduction<- (1-R_R$GPP_reduction)*100
R_R$ER_reduction<- (1-R_R$ER_reduction)*100
names(R_R)
R_R$avg_recovery<- (R_R$GPP_recovery+R_R$ER_recovery+R_R$NEP_recovery)/3
  

write_xlsx(R_R, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/recovery.xlsx")





recovery <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/recovery.xlsx")


recovery$a<-'a'
(f<-ggplot(recovery, aes(u, shape=site))+
    geom_point(aes(y=ER_reduction), size=5,color="darkred")+
    geom_point(aes(y=GPP_reduction), size=5, color='darkgreen' )+
    geom_smooth(aes(x=u, y=GPP_reduction, group=a), color='darkgreen', size=0.75,
                data=recovery, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER_reduction, group=a), color='darkred', size=0.75,
                data=recovery, se = FALSE, method='lm')+
    geom_vline(xintercept = 0, linetype='dashed')+
    geom_vline(xintercept = 0.07, linetype='dashed')+
    
    
    ggtitle("NEP Reduction During Flood Disturbance")+
    xlab("velocity (m/s)")+
    ylab("%")+
    theme(axis.text.x = element_text(size = 19, angle=0),
          axis.text.y = element_text(size = 19, angle=0),
          axis.title.y =element_text(size = 19),
          axis.title.x =element_text(size = 19),
          plot.title = element_text(size = 19),
          legend.position = "bottom",
          legend.text= element_text(size = 12),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

summary(lm(u ~ ER_reduction, data = recovery))
summary(lm(u ~ GPP_reduction, data = recovery))
summary(lm(u ~ NEP_reduction, data = recovery))

disturbed<-rbind(Ichetucknee, LF, GB, AM, Otter)
names(disturbed)
disturbed$NEP_reduction<- (1-disturbed$NEP_reduction)*100
disturbed$GPP_reduction<- (1-disturbed$GPP_reduction)*100

summary(lm(u ~ ER_reduction, data = disturbed))
summary(lm(u ~ GPP_reduction, data = disturbed))
summary(lm(u ~ NEP_reduction, data = disturbed))


recovery$ratio_GPP<- recovery$GPP_recovery/recovery$stage_recovery
recovery$ratio_ER<- recovery$ER_recovery/recovery$stage_recovery
recovery$ratio<- recovery$avg_recovery/recovery$stage_recovery

names(recovery)
(f<-ggplot(recovery, aes(u, shape=site))+
    geom_point(aes(y=ratio_ER), size=5,color="darkred")+
    geom_point(aes(y=ratio_GPP), size=5, color='darkgreen' )+
    
    
    geom_vline(xintercept = 0, linetype='dashed')+
    geom_vline(xintercept = 0.07, linetype='dashed')+
    
    
    ggtitle("NEP Reduction During Flood Disturbance")+
    xlab("velocity (m/s)")+
    ylab(" metabolic recovery: stage recovery")+
    theme(axis.text.x = element_text(size = 19, angle=0),
          axis.text.y = element_text(size = 19, angle=0),
          axis.title.y =element_text(size = 19),
          axis.title.x =element_text(size = 19),
          plot.title = element_text(size = 19),
          legend.position = "bottom",
          legend.text= element_text(size = 12),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

summary(lm(u ~ ratio_ER, data = recovery))
summary(lm(u ~ ratio_GPP, data = recovery))
summary(lm(u ~ NEP_reduction, data = recovery))






