rm(list=ls())

####packages######
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
library(weathermetrics)
library(measurements)
library(ggnewscale)
library(StreamMetabolism)
library(ggpmisc)
library("hydroTSM")


####Otter#####

Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Otter/DONE/Otter_not.xlsx")
Otter$stage_diff<-Otter$stage_avg-min(Otter$stage_avg, na.rm=T)

Otter<- Otter %>%
  mutate(hour = hour(Date),
         month=month(Date),
         day=day(Date))

Otter$season <- time2season(Otter$Date, out.fmt = "seasons")

Otter <-  Otter%>% 
  mutate(set= case_when(season=="spring"~ 'summer',
                        season=="summer"~ 'summer',
                        season=="autumm"~ 'summer',
                        season=="winter"~ 'winter'))
Otter <- Otter %>% 
  mutate(time= case_when(hour>= 0 & hour<=7 & set=='summer'~ 'ER',
                         hour>= 20 & hour<=23 & set=='summer'~ 'ER',
                         hour>= 0 & hour<=9 & set=='winter'~ 'ER',
                         hour>= 21 & hour<=23 & set=='winter'~ 'ER',))
Otter$time[is.na(Otter$time)] <- 'AM'

OtterER<-Otter %>% group_by(day,time,month) %>% 
  summarize(ER = mean(not, na.rm = TRUE))
OtterER<-filter(OtterER, time=='ER')
Otter<-left_join(Otter, OtterER,by=c("day","month"))

Otter$GPP<-Otter$not-Otter$ER
Otter$GPP[Otter$GPP<0] <- 0

Otter_GPPavg<-Otter %>% group_by(day,month) %>% 
  summarize(GPPavg = mean(GPP, na.rm=T))
Otter<-left_join(Otter, Otter_GPPavg,by=c("day","month"))

Otter$NEP<-Otter$GPPavg+Otter$ER

Otter$days <- as.Date(Otter$Date)
Otter <- aggregate(Otter, by=list(Otter$days), FUN='mean')

x<-c("Date","DO","Temp","stage_avg","Mouth_Temp_C","Mouth_DO_sat","K600_avg","K_reaeration","ER","GPPavg","NEP")
Otter<-Otter[,x]

ggplot(Otter, aes(x=Date))+
  geom_line(aes(y=GPPavg, color="GPP"), size=0.8)+
  geom_line(aes(y=stage_avg, color="DO"), size=0.8)

Otter<- Otter %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

Otter$days <- as.Date(Otter$Date)

#08
OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-11-29"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-46

(d08<-ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="GPPavg"), size=0.8)+
    geom_vline(xintercept=0)+geom_vline(xintercept=-20))

prior<-filter(OtBO, disturb_count< -20)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

OtBO$GPPavg<-OtBO$GPPavg/GPP_prior
OtBO$ER<-OtBO$ER/ER_prior
OtBO$stage_avg<-OtBO$stage_avg/h_prior

u<-
  OtBO %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(19,1)]
OtBO<-left_join(OtBO,u,by=c('Date'))

v<-
  OtBO %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(20,1)]
OtBO<-left_join(OtBO,v,by=c('Date'))

w<-
  OtBO %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(21,1)]
OtBO<-left_join(OtBO,w,by=c('Date'))

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="GPPavg"), size=0.8)+
  geom_vline(xintercept=20)+geom_vline(xintercept=38)

Otrecov<-filter(OtBO,disturb_count>0 & disturb_count<38)
modNEP<-lm(NEPmean ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-(1-InterceptH)/SlopeH)


##02
OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2022-12-10" & Date<"2023-5-01"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-79

(d02<-ggplot(OtBO, aes(x=disturb_count))+
    geom_line(aes(y=ER, color="DO"), size=0.8)+
    geom_vline(xintercept = 0)+geom_vline(xintercept = -10))

prior<-filter(OtBO, disturb_count< -10)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

OtBO$GPPavg<-OtBO$GPPavg/GPP_prior
OtBO$ER<-OtBO$ER/ER_prior
OtBO$stage_avg<-OtBO$stage_avg/h_prior


u<-
  OtBO %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))
ggplot(u, aes(x=Date))+
  geom_line(aes(y=ER, color="K1d"), size=0.8)+
  geom_line(aes(y=ERmean, color="K_avg"), size=0.8)
u<-u[,c(19,1)]
OtBO<-left_join(OtBO,u,by=c('Date'))

v<-
  OtBO %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(20,1)]
OtBO<-left_join(OtBO,v,by=c('Date'))

w<-
  OtBO %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(21,1)]
OtBO<-left_join(OtBO,w,by=c('Date'))

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="GPPavg"), size=0.8)+
  geom_vline(xintercept=10)+geom_vline(xintercept=60)

Otrecov<-filter(OtBO,disturb_count>0 & disturb_count<60)
modNEP<-lm(NEPmean ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-(1-InterceptH)/SlopeH)


##09
OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2023-5-10" & Date<"2023-9-15"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-57

(d02<-ggplot(OtBO, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color="DO"), size=0.8)+
    geom_vline(xintercept = 0)+geom_vline(xintercept = -30))

prior<-filter(OtBO, disturb_count< -30)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

OtBO$GPPavg<-OtBO$GPPavg/GPP_prior
OtBO$ER<-OtBO$ER/ER_prior
OtBO$stage_avg<-OtBO$stage_avg/h_prior


u<-
  OtBO %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))
ggplot(u, aes(x=Date))+
  geom_line(aes(y=ER, color="K1d"), size=0.8)+
  geom_line(aes(y=ERmean, color="K_avg"), size=0.8)
u<-u[,c(19,1)]
OtBO<-left_join(OtBO,u,by=c('Date'))

v<-
  OtBO %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(20,1)]
OtBO<-left_join(OtBO,v,by=c('Date'))

w<-
  OtBO %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(21,1)]
OtBO<-left_join(OtBO,w,by=c('Date'))

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="GPPavg"), size=0.8)+
  geom_vline(xintercept=10)+geom_vline(xintercept=60)

Otrecov<-filter(OtBO,disturb_count>0)
modNEP<-lm(NEPmean ~ disturb_count, data = Otrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov9<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Otrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov9<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = Otrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov9<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = Otrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov9<-(1-InterceptH)/SlopeH)

#####
event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,"",H_recov9))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,"",NEP_recov9))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,"",GPP_recov9))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,"",ER_recov9))


Otter<- data.frame(event,stage_recovery,NEP_recovery,
                   GPP_recovery,ER_recovery)
Otter$site<-"Otter"

####AllenMill##########

AllenMill<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Allen Mill/DONE/AllenMilltwo_NOT.xlsx", 
                               col_types = c("date", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"))
AllenMill$stage_diff<-AllenMill$stage_avg-min(AllenMill$stage_avg, na.rm=T)

AllenMill<- AllenMill %>%
  mutate(hour = hour(Date),
         month=month(Date),
         day=day(Date))

AllenMill$season <- time2season(AllenMill$Date, out.fmt = "seasons")

AllenMill <-  AllenMill%>% 
  mutate(set= case_when(season=="spring"~ 'summer',
                        season=="summer"~ 'summer',
                        season=="autumm"~ 'summer',
                        season=="winter"~ 'winter'))
AllenMill <- AllenMill %>% 
  mutate(time= case_when(hour>= 0 & hour<=7 & set=='summer'~ 'ER',
                         hour>= 20 & hour<=23 & set=='summer'~ 'ER',
                         hour>= 0 & hour<=9 & set=='winter'~ 'ER',
                         hour>= 21 & hour<=23 & set=='winter'~ 'ER',))
AllenMill$time[is.na(AllenMill$time)] <- 'AM'

AllenMillER<-AllenMill %>% group_by(day,time,month) %>% 
  summarize(ER = mean(not, na.rm = TRUE))
AllenMillER<-filter(AllenMillER, time=='ER')
AllenMill<-left_join(AllenMill, AllenMillER,by=c("day","month"))

AllenMill$GPP<-AllenMill$not-AllenMill$ER
AllenMill$GPP[AllenMill$GPP<0] <- 0

AllenMill_GPPavg<-AllenMill %>% group_by(day,month) %>% 
  summarize(GPPavg = mean(GPP, na.rm=T))
AllenMill<-left_join(AllenMill, AllenMill_GPPavg,by=c("day","month"))

AllenMill$NEP<-AllenMill$GPPavg+AllenMill$ER

AllenMill$days <- as.Date(AllenMill$Date)
AllenMill <- aggregate(AllenMill, by=list(AllenMill$days), FUN='mean')

x<-c("Date","DO","stage_avg","Mouth_Temp_C","Mouth_DO_sat","K600_avg","K_reaeration","ER","GPPavg","NEP")
AllenMill<-AllenMill[,x]

ggplot(AllenMill, aes(x=Date))+
  geom_line(aes(y=stage_avg, color="K1d"), size=0.8)

AllenMill<- AllenMill %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

AllenMill$days <- as.Date(AllenMill$Date)


AMBO<- AllenMill %>% mutate(RI = case_when(
  Date> "2022-07-11" & Date<"2022-11-30"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-65

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=0.8)+
  geom_vline(xintercept = 0)+geom_vline(xintercept = -30)

prior<-filter(AMBO, disturb_count< -30)
GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

AMBO$GPPavg<-AMBO$GPPavg/GPP_prior
AMBO$ER<-AMBO$ER/ER_prior
AMBO$stage_avg<-AMBO$stage_avg/h_prior

u<- AMBO %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))
u<-u[,c(18,1)]
AMBO<-left_join(AMBO,u,by=c('Date'))

v<-AMBO %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
AMBO<-left_join(AMBO,v,by=c('Date'))

w<- AMBO %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
AMBO<-left_join(AMBO,w,by=c('Date'))

(d08<-ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="NEP"), size=0.8)+
  geom_vline(xintercept=0)+  geom_vline(xintercept=14))

AMBO_fil<-filter(AMBO, disturb_count>-45)
ggplot(AMBO_fil, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="NEP"), size=0.8)+
  geom_vline(xintercept=0)+  geom_vline(xintercept=14)


AMrecov<-filter(AMBO,disturb_count>0 & disturb_count<15)
modGPP<-lm(GPPmean ~ disturb_count, data = AMrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

AMrecov<-filter(AMBO,disturb_count>0 & disturb_count<34)
modNEP<-lm(NEPmean ~ disturb_count, data = AMrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = AMrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

AMrecov<-filter(AMBO,disturb_count>0 & disturb_count<22)
modH<-lm(stage_avg ~ disturb_count, data = AMrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-(1-InterceptH)/SlopeH)



AMFR<- AllenMill %>% mutate(RI = case_when(
  Date> "2022-12-01" & Date<"2023-04-12"~ 2))
AMFR<-filter(AMFR, RI== "2")

AMFR <- AMFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

ggplot(AMFR, aes(x=Date))+
  geom_line(aes(y=NEP, color="NEP"), size=1)+
  geom_vline(xintercept = -2)+geom_vline(xintercept = 11)

AMFR$disturb_count<-AMFR$consec-83

ggplot(AMFR, aes(x=disturb_count))+
  geom_line(aes(y=ER, color="NEP"), size=1)+
  geom_vline(xintercept = -40)+geom_vline(xintercept = 11)

prior<-filter(AMFR, disturb_count< -40)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

AMFR$GPPavg<-AMFR$GPPavg/GPP_prior
AMFR$ER<-AMFR$ER/ER_prior
AMFR$stage_avg<-AMFR$stage_avg/h_prior


u<- AMFR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(18,1)]
AMFR<-left_join(AMFR,u,by=c('Date'))

v<-AMFR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
AMFR<-left_join(AMFR,v,by=c('Date'))

w<- AMFR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
AMFR<-left_join(AMFR,w,by=c('Date'))

AMFR<-filter(AMFR, ER<20)
ggplot(AMFR, aes(x=disturb_count))+
  geom_line(aes(y=ER, color="GPPavg"), size=0.2)+
  geom_vline(xintercept=0)+  geom_vline(xintercept=19)

AMrecov<-filter(AMFR,disturb_count>0)
modNEP<-lm(NEPmean ~ disturb_count, data = AMrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)


modH<-lm(stage_avg ~ disturb_count, data = AMrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-(1-InterceptH)/SlopeH)

ggplot(AMFR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="GPPavg"), size=0.2)+
  geom_vline(xintercept=7)+  geom_vline(xintercept=19)

AMrecov<-filter(AMFR,disturb_count>7)
modGPP<-lm(GPPavg ~ disturb_count, data = AMrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)


AMFR_ER<-filter(AMFR, ERmean<10)
AMrecov_ER<-filter(AMFR_ER,disturb_count>-2)

modER<-lm(ERmean ~ disturb_count, data = AMrecov_ER)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-abs((1-InterceptER)/SlopeER))


AMBO<- AllenMill %>% mutate(RI = case_when(
  Date> "2023-05-30" & Date<"2023-08-11"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-28

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg, color="NEP"), size=1)+
  geom_vline(xintercept = -10)+geom_vline(xintercept = 11)

prior<-filter(AMBO, disturb_count< -10)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

AMBO$GPPavg<-AMBO$GPPavg/GPP_prior
AMBO$ER<-AMBO$ER/ER_prior
AMBO$stage_avg<-AMBO$stage_avg/h_prior

u<- AMBO %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(18,1)]
AMBO<-left_join(AMBO,u,by=c('Date'))

v<-AMBO %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
AMBO<-left_join(AMBO,v,by=c('Date'))

w<- AMBO %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
AMBO<-left_join(AMBO,w,by=c('Date'))

(d06<-ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=-1)+  geom_vline(xintercept=20))

AMrecov<-filter(AMBO,disturb_count>0 )
modNEP<-lm(NEPmean ~ disturb_count, data = AMrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)


modER<-lm(ERmean ~ disturb_count, data = AMrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-(1-InterceptER)/SlopeER)

AMrecov<-filter(AMBO,disturb_count>0 & disturb_count<23)
modH<-lm(stage_avg ~ disturb_count, data = AMrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-(1-InterceptH)/SlopeH)

modGPP<-lm(GPPmean ~ disturb_count, data = AMrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-(1-InterceptGPP)/SlopeGPP)

#############

event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,""))


AM<- data.frame(event,stage_recovery, NEP_recovery,GPP_recovery,
                ER_recovery)
AM$site<-"AM"


####LF############
LF<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/LittleFanning/DONE/LittleFanning_not.xlsx")
LF$stage_diff<-LF$stage_avg-min(LF$stage_avg, na.rm=T)

LF<- LF %>%
  mutate(hour = hour(Date),
         month=month(Date),
         day=day(Date))

LF$season <- time2season(LF$Date, out.fmt = "seasons")

LF <-  LF%>% 
  mutate(set= case_when(season=="spring"~ 'summer',
                        season=="summer"~ 'summer',
                        season=="autumm"~ 'summer',
                        season=="winter"~ 'winter'))
LF <- LF %>% 
  mutate(time= case_when(hour>= 0 & hour<=7 & set=='summer'~ 'ER',
                         hour>= 20 & hour<=23 & set=='summer'~ 'ER',
                         hour>= 0 & hour<=9 & set=='winter'~ 'ER',
                         hour>= 21 & hour<=23 & set=='winter'~ 'ER',))
LF$time[is.na(LF$time)] <- 'AM'

LFER<-LF %>% group_by(day,time,month) %>% 
  summarize(ER = mean(not, na.rm = TRUE))
LFER<-filter(LFER, time=='ER')
LF<-left_join(LF, LFER,by=c("day","month"))

LF$GPP<-LF$not-LF$ER
LF$GPP[LF$GPP<0] <- 0

LF_GPPavg<-LF %>% group_by(day,month) %>% 
  summarize(GPPavg = mean(GPP, na.rm=T))
LF<-left_join(LF, LF_GPPavg,by=c("day","month"))

LF$NEP<-LF$GPPavg+LF$ER

LF$days <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$days), FUN='mean')

x<-c("Date","DO","stage_avg","Mouth_Temp_C","Mouth_DO_sat","K600_1d","K_reaeration","ER","GPPavg","NEP")
LF<-LF[,x]

ggplot(LF, aes(x=Date))+
  geom_line(aes(y=NEP, color="met"), size=0.8)+
  geom_line(aes(y=DO, color="DO"), size=0.8)

LF<- LF %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

LF$days <- as.Date(LF$Date)

##08
LFFR<- LF %>%mutate(RI = case_when(
  Date> "2022-06-01" & Date<"2022-11-21"~ 2))
LFFR<-filter(LFFR, RI== "2")

LFFR <- LFFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFFR$stage_avg, na.rm=T)
LFFR$disturb_count<-LFFR$consec-75

ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="met"), size=0.8)+
  geom_vline(xintercept = -24)

prior<-filter(LFFR, disturb_count< -24)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

LFFR$GPPavg<-LFFR$GPPavg/GPP_prior
LFFR$ER<-LFFR$ER/ER_prior
LFFR$stage_avg<-LFFR$stage_avg/h_prior


u<- LFFR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(18,1)]
LFFR<-left_join(LFFR,u,by=c('Date'))

v<-LFFR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
LFFR<-left_join(LFFR,v,by=c('Date'))

w<- LFFR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
LFFR<-left_join(LFFR,w,by=c('Date'))

(d08<-ggplot(LFFR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color="NEP"), size=0.8))

LFrecov<-filter(LFFR,disturb_count>0 )
modNEP<-lm(NEPmean ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))

ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=GPPmean, color="NEP"), size=0.8)+
  geom_vline(xintercept=-30)
LFrecov<-filter(LFFR,disturb_count>-30)

modGPP<-lm(GPPmean ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-abs((1-InterceptGPP)/SlopeGPP))


##02
LFFR<- LF %>%mutate(RI = case_when(
  Date> "2022-12-01" & Date<"2023-05-08"~ 2))
LFFR<-filter(LFFR, RI== "2")

LFFR <- LFFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFFR$stage_avg, na.rm=T)
LFFR$disturb_count<-LFFR$consec-90

ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=DO, color="met"), size=0.8)+
  geom_vline(xintercept = -30)

prior<-filter(LFFR, disturb_count< -30)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

LFFR$GPPavg<-LFFR$GPPavg/GPP_prior
LFFR$ER<-LFFR$ER/ER_prior
LFFR$stage_avg<-LFFR$stage_avg/h_prior


u<- LFFR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(18,1)]
LFFR<-left_join(LFFR,u,by=c('Date'))

v<-LFFR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
LFFR<-left_join(LFFR,v,by=c('Date'))

w<- LFFR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
LFFR<-left_join(LFFR,w,by=c('Date'))





(d02<-ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8))

LFrecov<-filter(LFFR,disturb_count>0 )
modNEP<-lm(NEPmean ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))

ggplot(LFFR, aes(x=disturb_count))+
    geom_line(aes(y=GPPmean, color="NEP"), size=0.8)+
    geom_vline(xintercept=-64)
LFrecov<-filter(LFFR,disturb_count>-64)

modGPP<-lm(GPPmean ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-abs((1-InterceptGPP)/SlopeGPP))

###06
LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-04-20" & Date<"2023-08-31"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_avg, na.rm=T)
LFRR$disturb_count<-LFRR$consec-75

ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=DO, color="met"), size=0.8)+
  geom_vline(xintercept = 27)

prior<-filter(LFRR, disturb_count< 25)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage_avg, na.rm=T)

LFRR$GPPavg<-LFRR$GPPavg/GPP_prior
LFRR$ER<-LFRR$ER/ER_prior
LFRR$stage_avg<-LFRR$stage_avg/h_prior

u<- LFRR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))
ggplot(u, aes(x=Date))+
  geom_line(aes(y=ER, color="K1d"), size=0.8)+
  geom_line(aes(y=ERmean, color="K_avg"), size=0.8)

u<-u[,c(18,1)]
LFRR<-left_join(LFRR,u,by=c('Date'))

v<-LFRR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(19,1)]
LFRR<-left_join(LFRR,v,by=c('Date'))

w<- LFRR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(20,1)]
LFRR<-left_join(LFRR,w,by=c('Date'))





(d06<-ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=0)+  geom_vline(xintercept=49))

LFrecov<-filter(LFRR,disturb_count>0 & disturb_count<49)
modNEP<-lm(NEPmean ~ disturb_count, data = LFrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

modGPP<-lm(GPPmean ~ disturb_count, data = LFrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-abs((1-InterceptGPP)/SlopeGPP))

modH<-lm(stage_avg ~ disturb_count, data = LFrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-abs((1-InterceptH)/SlopeH))

(d06<-ggplot(LFRR, aes(x=disturb_count))+
    geom_line(aes(y=ERmean, color="NEP"), size=0.8)+
    geom_vline(xintercept=-38)+  geom_vline(xintercept=0))

LFrecov<-filter(LFRR,disturb_count>-38 & disturb_count<0)
modER<-lm(ERmean ~ disturb_count, data = LFrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-abs((1-InterceptER)/SlopeER))




##########



event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("",NEP_recov8, NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("",GPP_recov8, GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,""))

LF<- data.frame(event,stage_recovery, NEP_recovery,
                GPP_recovery,ER_recovery)
LF$site<-"LF"

######Ichetucknee#######

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
Ich$stage_diff<-Ich$stage-min(Ich$stage, na.rm=T)

Ich<- Ich %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

Ich$days <- as.Date(Ich$Date)

###08
IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2022-07-06" & Date<"2022-11-30"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage, na.rm=T)
IchRR$disturb_count<-IchRR$consec-72

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=-28)


prior<-filter(IchRR, disturb_count< -28)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage, na.rm=T)

IchRR$GPPavg<-IchRR$GPPavg/GPP_prior
IchRR$ER<-IchRR$ER/ER_prior
IchRR$stage<-IchRR$stage/h_prior


u<- IchRR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(24,1)]
IchRR<-left_join(IchRR,u,by=c('Date'))

v<-IchRR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(25,1)]
IchRR<-left_join(IchRR,v,by=c('Date'))

w<- IchRR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(26,1)]
IchRR<-left_join(IchRR,w,by=c('Date'))




(d08<-ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=ERmean, color="NEP"), size=0.8)+
  geom_vline(xintercept=-50)+  geom_vline(xintercept=0))

Ichrecov<-filter(IchRR,disturb_count>-50 & disturb_count<0)
modNEP<-lm(NEPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Ichrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))





##02
IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-01-10" & Date<"2023-05-11"~ 2))
IchRR<-filter(IchRR, RI== "2")

  
IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage, na.rm=T)
IchRR$disturb_count<-IchRR$consec-44

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=-20)

prior<-filter(IchRR, disturb_count< -20)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage, na.rm=T)

IchRR$GPPavg<-IchRR$GPPavg/GPP_prior
IchRR$ER<-IchRR$ER/ER_prior
IchRR$stage<-IchRR$stage/h_prior


u<- IchRR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(24,1)]
IchRR<-left_join(IchRR,u,by=c('Date'))

v<-IchRR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(25,1)]
IchRR<-left_join(IchRR,v,by=c('Date'))

w<- IchRR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(26,1)]
IchRR<-left_join(IchRR,w,by=c('Date'))







(d02<-ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=ERmean, color="NEP"), size=0.8)+
  geom_vline(xintercept=13)+  geom_vline(xintercept=75))

Ichrecov<-filter(IchRR,disturb_count>-13 & disturb_count<75)
modNEP<-lm(NEPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Ichrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

modH<-lm(stage ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))

modGPP<-lm(GPPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)



##06
IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-05-07" & Date<"2023-08-11"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage, na.rm=T)
IchRR$disturb_count<-IchRR$consec-55

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=stage, color="NEP"), size=0.8)+
  geom_vline(xintercept=-0)+  geom_vline(xintercept=14)

prior<-filter(IchRR, disturb_count< -14)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage, na.rm=T)

IchRR$GPPavg<-IchRR$GPPavg/GPP_prior
IchRR$ER<-IchRR$ER/ER_prior
IchRR$stage<-IchRR$stage/h_prior


ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=stage))
names(u)
u<- IchRR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))
u<-u[,c(24,1)]
IchRR<-left_join(IchRR,u,by=c('Date'))

v<-IchRR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(25,1)]
IchRR<-left_join(IchRR,v,by=c('Date'))

w<- IchRR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(26,1)]
IchRR<-left_join(IchRR,w,by=c('Date'))





(d06<-ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=ERmean, color="NEP"), size=0.8)+
  geom_vline(xintercept=8)+  geom_vline(xintercept=28))

Ichrecov<-filter(IchRR,disturb_count>8 & disturb_count<28)
modNEP<-lm(NEPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = Ichrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-(1-InterceptER)/SlopeER)

modH<-lm(stage ~ disturb_count, data = Ichrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))

(d06<-ggplot(IchRR, aes(x=disturb_count))+
    geom_line(aes(y=GPPavg, color="NEP"), size=0.8)+
    geom_vline(xintercept=-0)+  geom_vline(xintercept=14))

Ichrecov<-filter(IchRR,disturb_count>-25 & disturb_count<0)

modGPP<-lm(GPPmean ~ disturb_count, data = Ichrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-abs((1-InterceptGPP)/SlopeGPP))

###########

event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", "", H_recov2,H_recov6,""))
NEP_recovery<-as.numeric(c("", "", NEP_recov2,NEP_recov6,""))
GPP_recovery<-as.numeric(c("", "", GPP_recov2,GPP_recov6,""))
ER_recovery<-as.numeric(c("", "", ER_recov2 ,ER_recov6,""))

Ichetucknee<- data.frame(event,stage_recovery, NEP_recovery,
                         GPP_recovery,ER_recovery)
Ichetucknee$site<-"ID"


######GB#######
GB <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))
GB$stage_diff<-GB$stage-min(GB$stage, na.rm=T)

GB<- GB %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))


GB$days <- as.Date(GB$Date)

GBRR<- GB %>% mutate(RI = case_when(
  Date> "2023-05-01" & Date<"2023-09-20"~ 2))
GBRR<-filter(GBRR, RI== "2")

GBRR <- GBRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBRR$stage, na.rm=T)
GBRR$disturb_count<-GBRR$consec-100

ggplot(GBRR, aes(x=disturb_count))+
  geom_line(aes(y=stage, color="NEP"), size=0.8)+
  geom_vline(xintercept=-50)


prior<-filter(GBRR, disturb_count< -50)

GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage, na.rm=T)

GBRR$GPPavg<-GBRR$GPPavg/GPP_prior
GBRR$ER<-GBRR$ER/ER_prior
GBRR$stage<-GBRR$stage/h_prior


u<- GBRR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(24,1)]
GBRR<-left_join(GBRR,u,by=c('Date'))

v<-GBRR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(25,1)]
GBRR<-left_join(GBRR,v,by=c('Date'))

w<- GBRR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(26,1)]
GBRR<-left_join(GBRR,w,by=c('Date'))




(d08<-ggplot(GBRR, aes(x=disturb_count))+
    geom_line(aes(y=ER, color="NEP"), size=0.8)+
    geom_vline(xintercept=0)+  geom_vline(xintercept=38))

GBrecov<-filter(GBRR,disturb_count>0)
modNEP<-lm(NEPmean ~ disturb_count, data = GBrecov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov9<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = GBrecov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov9<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = GBrecov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov9<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage ~ disturb_count, data = GBrecov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov9<-abs((1-InterceptH)/SlopeH))


###########

event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", "", "","",H_recov9))
NEP_recovery<-as.numeric(c("","", "","",NEP_recov9))
GPP_recovery<-as.numeric(c("", "", "","",""))
ER_recovery<-as.numeric(c("","","","",ER_recov9))

GB<- data.frame(event,stage_recovery, NEP_recovery,
                GPP_recovery,ER_recovery)
GB$site<-"GB"

plot_grid(d08+ggtitle("ID: 08/2022"),d02+ggtitle("ID: 02/2023"),
          d06+ggtitle("ID: 06/2023"))





### US 27####

US27 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx", 
                   col_types = c("numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
US27$depth<-conv_unit(US27$depth, "ft", "m")
US27$depth<-US27$depth-(max(US27$depth, na.rm=T)-1.5)
US27$u<-US27$discharge/(US27$depth*15)/100
US27$stage_diff<-US27$depth-min(US27$depth, na.rm=T)

US27<-rename(US27, 'stage_avg'='depth')

US27$day <- as.Date(US27$Date)
US27 <- aggregate(US27, by=list(US27$day), FUN='mean')

US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2022-07-17" & Date<"2022-11-18"~ 2))
US27RR<-filter(US27RR, RI== "2")

US27RR <- US27RR %>%
  arrange(day) %>% 
  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% 
  ungroup()

US27RR$disturb_count<-US27RR$consec-63

u<- US27RR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(22,9)]
US27RR<-left_join(US27RR,u,by=c('Date'))

v<-US27RR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(23,9)]
US27RR<-left_join(US27RR,v,by=c('Date'))

w<- US27RR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(24,9)]
US27RR<-left_join(US27RR,w,by=c('Date'))


d08<-ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=0)+  geom_vline(xintercept=28)

US27recov<-filter(US27RR,disturb_count>0 & disturb_count<28)
modNEP<-lm(NEPmean ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov8<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = US27recov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov8<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov8<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov8<-abs((1-InterceptH)/SlopeH))


US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2022-12-15" & Date<"2023-06-12"~ 2))
US27RR<-filter(US27RR, RI== "2")

US27RR <- US27RR %>%
  arrange(day) %>% 
  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% 
  ungroup()

max(US27RR$stage_avg, na.rm=T)
US27RR$disturb_count<-US27RR$consec-71

u<- US27RR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(22,9)]
US27RR<-left_join(US27RR,u,by=c('Date'))

v<-US27RR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(23,9)]
US27RR<-left_join(US27RR,v,by=c('Date'))

w<- US27RR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(24,9)]
US27RR<-left_join(US27RR,w,by=c('Date'))

d02<-ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)+
  geom_vline(xintercept=18)+  geom_vline(xintercept=55)

US27recov<-filter(US27RR,disturb_count>0 & disturb_count<55)
modNEP<-lm(NEPmean ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov2<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = US27recov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov2<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov2<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov2<-abs((1-InterceptH)/SlopeH))


US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2023-06-02" & Date<"2023-08-15"~ 2))
US27RR<-filter(US27RR, RI== "2")

US27RR <- US27RR %>%
  arrange(day) %>% 
  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% 
  ungroup()

US27RR$disturb_count<-US27RR$consec-29

u<- US27RR %>%
  mutate( ERmean= rollapply(ER,4,mean, fill=NA, partial=TRUE, align='left'))

u<-u[,c(22,9)]
US27RR<-left_join(US27RR,u,by=c('Date'))

v<-US27RR %>%
  mutate( GPPmean= rollapply(GPPavg,4,mean, fill=NA, partial=TRUE, align='left'))
v<-v[,c(23,9)]
US27RR<-left_join(US27RR,v,by=c('Date'))

w<- US27RR %>%
  mutate( NEPmean= rollapply(NEP,4,mean, fill=NA, partial=TRUE, align='left'))
w<-w[,c(24,9)]
US27RR<-left_join(US27RR,w,by=c('Date'))


d06<-ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP, color="NEP"), size=0.8)

US27recov<-filter(US27RR,disturb_count>0 & disturb_count<12)
modNEP<-lm(NEPmean ~ disturb_count, data = US27recov)
cf <- coef(modNEP)
(InterceptNEP <- cf[1])
(SlopeNEP <- cf[2])
(NEP_recov6<-(1-InterceptNEP)/SlopeNEP)

modER<-lm(ERmean ~ disturb_count, data = US27recov)
cf <- coef(modER)
(InterceptER<- cf[1])
(SlopeER <- cf[2])
(ER_recov6<-(1-InterceptER)/SlopeER)

modGPP<-lm(GPPmean ~ disturb_count, data = US27recov)
cf <- coef(modGPP)
(InterceptGPP<- cf[1])
(SlopeGPP <- cf[2])
(GPP_recov6<-(1-InterceptGPP)/SlopeGPP)

modH<-lm(stage_avg ~ disturb_count, data = US27recov)
cf <- coef(modH)
(InterceptH<- cf[1])
(SlopeH <- cf[2])
(H_recov6<-abs((1-InterceptH)/SlopeH))

###########
event<-c('0','08','02','06','09')
stage_recovery<-as.numeric(c("", H_recov8, H_recov2,H_recov6,''))
NEP_recovery<-as.numeric(c("", NEP_recov8, NEP_recov2,NEP_recov6,''))
GPP_recovery<-as.numeric(c("", GPP_recov8, GPP_recov2,GPP_recov6,''))
ER_recovery<-as.numeric(c("", ER_recov8, ER_recov2,ER_recov6,''))

US27<- data.frame(event,stage_recovery, NEP_recovery,
                  GPP_recovery,ER_recovery)
US27$site<-"IU"

plot_grid(d08+ggtitle("IU: 08/2022"),d02+ggtitle("IU: 02/2023"),
          d06+ggtitle("IU: 06/2023"))


###############

R_R<-rbind(US27, Ichetucknee, LF, AM, Otter, GB)
R_R$GPP_recovery[R_R$GPP_recovery<0] <- NA
R_R$ER_recovery[R_R$ER_recovery<0] <- NA

R_R$GPP_recovery[R_R$GPP_recovery>100] <- NA
R_R$ER_recovery[R_R$ER_recovery>100] <- NA

R_R$ratio_GPP<- (R_R$stage_recovery/R_R$GPP_recovery)
R_R$ratio_ER<- (R_R$stage_recovery/R_R$ER_recovery)


reduction <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")
recovery<-left_join(R_R, reduction, by=c('event','site'))
recovery <- recovery[!duplicated(recovery[c('event','site')]),]



write_xlsx(recovery, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/recovery.xlsx")

recovery <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/recovery.xlsx")
title<-expression(paste(recovery[stage]/recovery[metabolic]))
h<-expression(paste( h[i]-h[min]))
u<-"velocity m/s"
names(recovery)

recovery$a<-'a'
cols<-c(
  "h_high"="deepskyblue3",
  "h_brown"="burlywood4",
  "h_rev"="black")
recovery$IF <- factor(recovery$IF  , levels=c("h_high","h_brown","h_rev"))

(f<-ggplot(recovery, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=ratio_GPP), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("GPP Recovery")+
    xlab(h)+
    ylab(title)+scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 25, color='darkgreen'),
          axis.title.x =element_text(size = 27),
          plot.title = element_text(size = 27, color='darkgreen'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(g<-ggplot(recovery, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=ratio_ER), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("ER Recovery")+
    xlab(h)+
    ylab(title)+scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 25, color='darkred'),
          axis.title.x =element_text(size = 27),
          plot.title = element_text(size = 27, color='darkred'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

h<-plot_grid(f,g, ncol=2)
ggsave(filename="recovery.jpeg", 
       plot = h, 
       width =12, 
       height = 5, 
       units = "in")


(i<-ggplot(recovery, aes(ID, shape=site))+
    geom_point(aes(y=ratio_ER), size=5,color="darkred")+
    geom_point(aes(y=ratio_GPP), size=5, color='darkgreen' )+
    geom_hline(yintercept = 1, linetype='dashed')+
    
    ggtitle(" ", subtitle = "by Site")+
    xlab(" ")+
    ylab(title)+scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_blank(),
          axis.title.x =element_text(size = 17),
          plot.title = element_text(size = 17),
          legend.position = "none",
          legend.text= element_text(size = 12),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

names(recovery)

GPPbox<-recovery[,c(4,6)]
GPPbox<-rename(GPPbox, 'recovery'="GPP_recovery")
GPPbox$type<-"GPP"

ERbox<-recovery[,c(5,6)]
ERbox<-rename(ERbox, 'recovery'="ER_recovery")
ERbox$type<-"ER"

stagebox<-recovery[,c(2,6)]
stagebox<-rename(stagebox, 'recovery'="stage_recovery")
stagebox$type<-"Stage"
box<-rbind(GPPbox, ERbox, stagebox)
box<-filter(box, recovery<200 & recovery>0)

cols<-c(ER='darkred', GPP='darkgreen', NEP='blue')
j<-ggplot(box, aes(x=type, y=recovery)) + 
  geom_boxplot(outlier.colour="black", outlier.size=1,fill=cols)+
  ggtitle(" ")+
  xlab(' ')+
  ylab('Recovery (days)')+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_text(size = 17),
        plot.title = element_text(size = 17),
        legend.position = "none",
        legend.text= element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

reduction<-plot_grid(f,i,j, nrow=1)

ggsave(filename="reduction.jpeg",
       plot = reduction,
       width =12,
       height = 5,
       units = "in")

dev.new()
