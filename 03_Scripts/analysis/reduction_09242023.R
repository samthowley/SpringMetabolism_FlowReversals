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
                               "numeric", "numeric", "numeric"))
GB$stage_diff<-GB$stage-min(GB$stage, na.rm=T)
GB$u<-abs(GB$stage*-0.483+0.35)
GB$days <- as.Date(GB$Date)

ggplot(GB) + geom_line(aes(Date, ER))+geom_hline(yintercept = 0.38)

GB<- GB %>% mutate(RI = case_when(
  stage<0.38 ~ "low",
  stage>=0.38 ~ "high"))
GB_low<-filter(GB, RI== 'low')

NEP0<-mean(GB_low$NEP, na.rm=T)
ER0<-mean(GB_low$ER, na.rm=T)
GPP0<-mean(GB_low$GPPavg, na.rm=T)
h0<-mean(GB_low$stage_diff, na.rm=T)
u0<-mean(GB_low$u, na.rm=T)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-06-01" & Date<"2022-9-20"~ 2))

GBFR<-filter(GBFR, RI== "2")

GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

GBFR$disturb_count<-GBFR$consec-20

ggplot(GBFR, aes(x=disturb_count))+geom_line(aes(y=ER), size=1)+geom_vline(xintercept=-4)+geom_vline(xintercept=5)

x<-filter(GBFR, disturb_count>=-4 & disturb_count <=5)
h9<-mean(x$stage_diff, na.rm = T)  
NEP9<-mean(x$NEP, na.rm = T)  
ER9<-mean(x$ER, na.rm = T)  
GPP9<-mean(x$GPPavg, na.rm = T)
u9<-mean(x$u, na.rm = T)  


GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-25" & Date<"2023-8-25"~ 2))

GBFR<-filter(GBFR, RI== "2")


GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_diff, na.rm=T)
GBFR$disturb_count<-GBFR$consec-12

ggplot(GBFR, aes(x=Date))+
  geom_line(aes(y=stage_diff), size=1)

ggplot(GBFR, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-1)+
  geom_vline(xintercept=5)

x<-filter(GBFR, disturb_count>=-1& disturb_count <=5)
h8<-mean(x$stage_diff, na.rm = T)  
NEP8<-mean(x$NEP, na.rm = T)  
ER8<-mean(x$ER, na.rm = T)  
GPP8<-mean(x$GPPavg, na.rm = T)
stage8<-mean(x$stage_diff, na.rm = T)  
u8<-mean(x$u, na.rm = T)  
########
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, "", "", NEP9))
GPP<-as.numeric(c(GPP0, GPP8, "", "", GPP9))
ER<-as.numeric(c(ER0, ER8, "", "", ER9))
stage<-as.numeric(c(h0, h8, "", "", h9))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, "","",NEP0/NEP9))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, "","",GPP9/GPP0))
ER_reduction<-as.numeric(c("",ER8/ER0, "","",ER9/ER0))
stage_reduction<-as.numeric(c("",h8/h0, "","",h9/h0))
u<-as.numeric(c(u0, u8, "","", u9))

GB<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
GB$IF<-'h_high'
GB$site<-"GB"

GB$ID<-3
####Otter#####

Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
Otter$stage_diff<-Otter$stage-min(Otter$stage, na.rm=T)

Otter$u<-Otter$stage*-0.0868+0.16

ggplot(Otter) + geom_line(aes(Date, stage))+geom_hline(yintercept = 1.27)

Otter$days <- as.Date(Otter$Date)

Otter<- Otter %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

Otter<- Otter %>% mutate(RI = case_when(
  stage<0.84 ~ "low",
  stage<1.27 ~ "moderate",
  stage>=1.27 ~ "high"))

Otter_low<-filter(Otter, RI== 'low')

h0<-mean(Otter_low$stage_diff, na.rm=T)
NEP0<-mean(Otter_low$NEP, na.rm=T)
ER0<-mean(Otter_low$ER, na.rm=T)
GPP0<-mean(Otter_low$GPPavg, na.rm=T)
u0<-mean(Otter_low$u, na.rm=T)

OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-18"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO$days <- as.Date(OtBO$Date)

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_diff, na.rm=T)
OtBO$disturb_count<-OtBO$consec-26

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-7)+
  geom_vline(xintercept=2)
x<-filter(OtBO, disturb_count>=-7& disturb_count <=2)
h8<-mean(x$stage_diff, na.rm = T)  
NEP8<-mean(x$NEP, na.rm = T)  
ER8<-mean(x$ER, na.rm = T)  
GPP8<-mean(x$GPPavg, na.rm = T)  
u8<-mean(x$u, na.rm = T)  

OtFR<- Otter %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-5-10"~ 2))
OtFR<-filter(OtFR, RI== "2")

OtFR <- OtFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

OtFR$disturb_count<-OtFR$consec-26

ggplot(OtFR, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-1)+
  geom_vline(xintercept=8)

x<-filter(OtFR, disturb_count>=-1& disturb_count <=8)
h2<-mean(x$stage_diff, na.rm = T)  
NEP2<-mean(x$NEP, na.rm = T)  
ER2<-mean(x$ER, na.rm = T)  
GPP2<-mean(x$GPPavg, na.rm = T)  
u2<-mean(x$u, na.rm = T)  

OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2023-06-25" & Date<"2023-08-10"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_diff, na.rm=T)
OtBO$disturb_count<-OtBO$consec-9

ggplot(OtBO, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-3)+
  geom_vline(xintercept=2)

x<-filter(OtBO, disturb_count>=-3& disturb_count <=2)
h6<-mean(x$stage_diff, na.rm = T)  
NEP6<-mean(x$NEP, na.rm = T)  
ER6<-mean(x$ER, na.rm = T)  
GPP6<-mean(x$GPPavg, na.rm = T)  
u6<-mean(x$u, na.rm = T)  
################
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, NEP2, NEP6, ""))
GPP<-as.numeric(c(GPP0, GPP8, GPP2, GPP6, ""))
ER<-as.numeric(c(ER0, ER8, ER2, ER6, ""))
stage<-as.numeric(c(h0, h8, h2, h6, ""))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER8/ER0,ER2/ER0,ER6/ER0,""))
stage_reduction<-as.numeric(c("",h8/h0,h2/h0,h6/h0,""))
u<-as.numeric(c(u0, u8, u2,u6,""))

Otter<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
Otter$IF<-'h_brown'
Otter$site<-"Otter"
Otter$ID<-5
####Otter mod######
Otterx<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"))



Otterx$stage_diff<-Otterx$stage-min(Otterx$stage, na.rm=T)
Otterx$u<-Otterx$stage*-0.0868+0.16

ggplot(Otterx) + geom_line(aes(Date, stage))+geom_hline(yintercept = 1)+geom_hline(yintercept = 1.2)

Otterx<- Otterx %>% mutate(RI = case_when(
  stage<0.84 ~ "low",
  stage<1.27 ~ "moderate",
  stage>=1.27 ~ "high"))

Otter_low<-filter(Otterx, RI== 'low') 

Otter_mod<-filter(Otterx, stage>1 & stage< 1.2)

h0<-mean(Otter_low$stage_diff, na.rm=T)
NEP0<-mean(Otter_low$NEP, na.rm=T)
ER0<-mean(Otter_low$ER, na.rm=T)
GPP0<-mean(Otter_low$GPPavg, na.rm=T)
u0<-mean(Otter_low$u, na.rm=T)

ggplot(Otter_mod) + geom_line(aes(Date, stage))+geom_hline(yintercept = 1)+geom_hline(yintercept = 1.2)


OSBO<-filter(Otter_mod, Date<'2023-01-01')
h8<-mean(OSBO$stage_diff, na.rm = T)  
NEP8<-mean(OSBO$NEP, na.rm = T)  
ER8<-mean(OSBO$ER, na.rm = T)  
GPP8<-mean(Otter_mod$GPPavg, na.rm = T)  
u8<-mean(OSBO$u, na.rm = T)    

OSBO<-filter(Otter_mod, Date>'2023-01-01'& Date<'2023-06-01' )
h6<-mean(OSBO$stage_diff, na.rm = T)  
NEP6<-mean(OSBO$NEP, na.rm = T)  
ER6<-mean(OSBO$ER, na.rm = T)  
GPP6<-mean(Otter_mod$GPPavg, na.rm = T)  
u6<-mean(OSBO$u, na.rm = T)    

OSBO<-filter(Otter_mod, Date>'2023-06-01' )
h9<-mean(OSBO$stage_diff, na.rm = T)  
NEP9<-mean(OSBO$NEP, na.rm = T)  
ER9<-mean(OSBO$ER, na.rm = T)  
GPP9<-mean(Otter_mod$GPPavg, na.rm = T)  
u9<-mean(OSBO$u, na.rm = T)    
Otter_mod$ID<-5
############

event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, "", NEP6, NEP9))
GPP<-as.numeric(c(GPP0, GPP8, "", GPP6, GPP9))
ER<-as.numeric(c(ER0, ER8, "", ER6, ER9))
stage<-as.numeric(c(h0, h8, "", h6, h9))
u<-as.numeric(c(u0, u8, "", u6, u9))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, "",GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("",ER8/ER0,"",ER6/ER0,ER9/ER0))
stage_reduction<-as.numeric(c("",h8/h0,"",h6/h0,h9/h0))

Otter_mod<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
Otter_mod$IF<-'h_high'
Otter_mod$site<-"Otter" 
Otter_mod$ID<-5


####AllenMill##########
AllenMill <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))

AllenMill$days <- as.Date(AllenMill$Date)
AllenMill$stage_diff<-AllenMill$stage-min(AllenMill$stage, na.rm=T)
AllenMill$"u"<-(AllenMill$stage*-0.24+0.46)

ggplot(AllenMill) + geom_line(aes(Date, stage))+geom_hline(yintercept = 1.37)

AM<- AllenMill %>% mutate(RI = case_when(
  stage<0.8 ~ "low",
  stage<1.37 ~ "moderate",
  stage>=1.37 ~ "high"))
AM_low<-filter(AM, RI== 'low')
h0<-mean(AM_low$stage_diff, na.rm=T)
NEP0<-mean(AM_low$NEP, na.rm=T)
(ER0<-mean(AM_low$ER, na.rm=T))
(GPP0<-mean(AM_low$GPPavg, na.rm=T))
(u0<-mean(AM_low$u, na.rm=T))

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-03-01"~ 2))

AMFR<-filter(AMFR, RI== "2")

AMFR <- AMFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMFR$stage_diff, na.rm=T)
AMFR$disturb_count<-AMFR$consec-21

ggplot(AMFR, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=0.4)+
  geom_line(aes(y=GPPavg), size=0.4)+
  geom_vline(xintercept=-10)+
  geom_vline(xintercept=-2)

x<-filter(AMFR, disturb_count>=-7 & disturb_count <=5)
h2<-mean(x$stage_diff, na.rm = T)  
NEP2<-mean(x$NEP, na.rm = T)  
(GPP2<-mean(x$GPPavg, na.rm = T))
u2<-mean(x$u, na.rm = T)  
x<-filter(AMFR, disturb_count>=-10 & disturb_count <=-2)
((ER2<-mean(x$ER, na.rm = T)))


AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-05-01" & Date<"2023-08-01"~ 2))

AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_diff, na.rm=T)
AMBO$disturb_count<-AMBO$consec-61

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=0.4)+
  geom_line(aes(y=stage), size=0.4)+
  geom_vline(xintercept=-7)+
  geom_vline(xintercept=5)
x<-filter(AMBO, disturb_count>=-7 & disturb_count <=5)
h6<-mean(x$stage_diff, na.rm = T)  
NEP6<-mean(x$NEP, na.rm = T)  
(GPP6<-mean(x$GPPavg, na.rm = T))
u6<-mean(x$u, na.rm = T)  
x<-filter(AMBO, disturb_count>=-40 & disturb_count <=-20)
((ER6<-mean(x$ER, na.rm = T)))

AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-08-22" & Date<"2023-10-01"~ 2))

AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_diff, na.rm=T)
AMBO$disturb_count<-AMBO$consec-16

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=0.4)+
  geom_line(aes(y=stage), size=0.4)+
  geom_vline(xintercept=-5)+
  geom_vline(xintercept=-3)

x<-filter(AMBO, disturb_count>=-2& disturb_count <=2)
h9<-mean(x$stage_diff, na.rm = T)  
NEP9<-mean(x$NEP, na.rm = T)  
(GPP9<-mean(x$GPPavg, na.rm = T))
u9<-mean(x$u, na.rm = T)  
x<-filter(AMBO, disturb_count>=-5& disturb_count <=-3)
((ER9<-mean(x$ER, na.rm = T)))

#############
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, " ", NEP2, NEP6, NEP9))
GPP<-as.numeric(c(GPP0, " ", GPP2, GPP6, NEP6))
ER<-as.numeric(c(ER0, " ", ER2, ER6, ER9))
stage<-as.numeric(c(h0, " ", h2, h6, h9))
u<-as.numeric(c(u0, "", u2, u6, u9))
NEP_reduction<-as.numeric(c("","", NEP0/NEP2, NEP0/NEP6,NEP0/NEP9))
GPP_reduction<-as.numeric(c("","", GPP2/GPP0,GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("","",ER2/ER0,ER6/ER0,ER9/ER0))
stage_reduction<-as.numeric(c("","",h2/h0,h6/h0,h9/h0))
IF<-c("", "", 'h_rev', 'h_brown', 'h_brown')

AM<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction, IF)
AM$site<-"AM"
AM$ID<-6
######AM mod########

#ggplot(AllenMill) + geom_line(aes(Date, stage))+geom_hline(yintercept = 1.4)+geom_hline(yintercept = 1.1)
  
AMx<- AllenMill %>% mutate(RI = case_when(
  stage<0.7 ~ "low",
  stage<1.37 ~ "moderate",
  stage>=1.37 ~ "high"))

AM_low<-filter(AMx, RI== 'low')
h0<-mean(AM_low$stage_diff, na.rm=T)
NEP0<-mean(AM_low$NEP, na.rm=T)
ER0<-mean(AM_low$ER, na.rm=T)
GPP0<-mean(AM_low$GPPavg, na.rm=T)
u0<-mean(AM_low$u, na.rm=T)

AM_mod<-filter(AMx, stage>=1.1 & stage<1.4)
ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=stage), size=1)


AMFR<- AM_mod %>% mutate(RI = case_when( Date<"2023-02-09"~ 2))
AMFR<-filter(AMFR, RI== "2")


ggplot(AMFR, aes(x=Date))+
  geom_line(aes(y=ER), size=1)
h2<-mean(AMFR$stage_diff, na.rm = T)  
NEP2<-mean(AMFR$NEP, na.rm = T)  
(ER2<-min(AMFR$ER, na.rm = T))
(GPP2<-mean(AMFR$GPPavg, na.rm = T))
(u2<-mean(AMFR$u, na.rm = T))


#ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=stage), size=1)


AMBO<- AM_mod %>% mutate(RI = case_when(
  Date>"2023-06-15" &  Date<"2023-08-01"~ 2))
AMBO<-filter(AMBO, RI== "2")

ggplot(AMBO, aes(x=Date))+geom_line(aes(y=ER), size=0.4)


h6<-mean(AMBO$stage_diff, na.rm = T)  
NEP6<-mean(AMBO$NEP, na.rm = T)  
(ER6<-mean(AMBO$ER, na.rm = T))
(GPP6<-mean(AMBO$GPPavg, na.rm = T) )
(u6<-mean(AMBO$u, na.rm = T))

#ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=stage), size=1)


AMBO<- AM_mod %>% mutate(RI = case_when(
  Date>"2023-07-31"~ 2))
AMBO<-filter(AMBO, RI== "2")

ggplot(AMBO, aes(x=Date))+geom_line(aes(y=ER), size=0.4)

h9<-mean(AMBO$stage_diff, na.rm = T)  
NEP9<-mean(AMBO$NEP, na.rm = T)  
(ER9<-mean(AMBO$ER, na.rm = T))
(GPP9<-mean(AMBO$GPPavg, na.rm = T) )
(u9<-mean(AMBO$u, na.rm = T))


########
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, "", "", NEP6, NEP9))
GPP<-as.numeric(c(GPP0, "", "", GPP6, GPP9))
ER<-as.numeric(c(ER0, "", "", ER6, ER9))
stage<-as.numeric(c(h0, "", "", h6, h9))
u<-as.numeric(c(u0, "", "", u6, u9))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("","",  "",GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("","","",ER6/ER0,ER9/ER0))
stage_reduction<-as.numeric(c("","","",h6/h0,h9/h0))

AM_mod<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
AM_mod$IF<-'h_high'
AM_mod$site<-"AM"
AM_mod$ID<-6

####LF############
LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))
LF$days <- as.Date(LF$Date)
LF$stage_diff<-LF$stage-min(LF$stage, na.rm=T)

LF$u<-LF$stage*-0.115 + 0.25
ggplot(LF) + geom_line(aes(Date, stage))+geom_hline(yintercept = 0.61)

LF<- LF %>% mutate(RI = case_when(
  stage<0.4 ~ "low",
  stage<0.61 ~ "moderate",
  stage>=0.61 ~ "high"))
LF_low<-filter(LF, RI== 'low')
h0<-mean(LF_low$stage_diff, na.rm = T)
NEP0<-mean(LF_low$NEP, na.rm = T)
ER0<-mean(LF_low$ER, na.rm = T)
GPP0<-mean(LF_low$GPPavg, na.rm = T)
u0<-mean(LF_low$u, na.rm = T)



LFFR<- LF %>%mutate(RI = case_when(
  Date> "2023-01-30" & Date<"2023-04-21"~ 2))
LFFR<-filter(LFFR, RI== "2")

LFFR <- LFFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFFR$stage_diff, na.rm=T)
LFFR$disturb_count<-LFFR$consec-34

ggplot(LFFR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=1)+
  geom_vline(xintercept=-11)+
  geom_vline(xintercept=3)

x<-filter(LFFR, disturb_count>=-11 & disturb_count <=3)
h2<-mean(x$stage_diff, na.rm = T)  
NEP2<-mean(x$NEP, na.rm = T)  
ER2<-mean(x$ER, na.rm = T)  
GPP2<-mean(x$GPPavg, na.rm = T)  
u2<-mean(x$u, na.rm = T)  


LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-18" & Date<"2023-08-03"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_diff, na.rm=T)
LFRR$disturb_count<-LFRR$consec-14

ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=1)+
  geom_vline(xintercept=-7)+
  geom_vline(xintercept=5)
x<-filter(LFRR, disturb_count>=-7& disturb_count <=5)
h6<-mean(x$stage_diff, na.rm = T)  
NEP6<-mean(x$NEP, na.rm = T)  
ER6<-mean(x$ER, na.rm = T)  
GPP6<-mean(x$GPPavg, na.rm = T)  
u6<-mean(x$u, na.rm = T)  

LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-08-19" & Date<"2023-10-20"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_diff, na.rm=T)
LFRR$disturb_count<-LFRR$consec-20

ggplot(LFRR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=1)+
  geom_line(aes(y=stage), size=1)+
  geom_vline(xintercept=-8)+
  geom_vline(xintercept=1)
x<-filter(LFRR, disturb_count>=-8 & disturb_count <=1)
h9<-mean(x$stage_diff, na.rm = T)  
NEP9<-mean(x$NEP, na.rm = T)  
ER9<-mean(x$ER, na.rm = T)  
GPP9<-mean(x$GPPavg, na.rm = T)  
u9<-mean(x$u, na.rm = T)  

############
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, "", NEP2, NEP6, NEP9))
GPP<-as.numeric(c(GPP0, "", GPP2, GPP6, GPP9))
ER<-as.numeric(c(ER0, "", ER2, ER6, ER9))
stage<-as.numeric(c(h0, "", h2, h6, h9))
NEP_reduction<-as.numeric(c("", "", NEP0/NEP2, NEP0/NEP6,NEP0/NEP9))
GPP_reduction<-as.numeric(c("","", GPP2/GPP0,GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("","",ER2/ER0,ER6/ER0,ER9/ER0))
stage_reduction<-as.numeric(c("","",h2/h0,h6/h0,h9/h0))
u<-as.numeric(c(u0, "", u2,u6,u9))

LF<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
LF$IF<-'h_high'
LF$site<-"LF"
LF$ID<-4
View(LF)
######Ichetucknee#######

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
Ich$stage_diff<-Ich$stage-min(Ich$stage, na.rm=T)

Ich$days <- as.Date(Ich$Date)
Ich$u<-Ich$stage*-0.128+0.416
ggplot(Ich) + geom_line(aes(Date, stage))+geom_hline(yintercept = 0.76)

Ich<- Ich %>% mutate(RI = case_when(
  stage<0.76 ~ "low",
  stage<1.37 ~ "moderate",
  stage>=1.37 ~ "high"))

Ich_low<-filter(Ich, RI== 'low')
h0<-mean(Ich_low$stage_diff, na.rm=T)
NEP0<-mean(Ich_low$NEP, na.rm=T)
ER0<-mean(Ich_low$ER, na.rm=T)
GPP0<-mean(Ich_low$GPPavg, na.rm=T)
u0<-mean(Ich_low$u, na.rm=T)


IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2022-07-01" & Date<"2022-10-10"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_diff, na.rm=T)
IchRR$disturb_count<-IchRR$consec-11

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=stage), size=1)+
  geom_line(aes(y=GPPavg), size=1)+
  geom_vline(xintercept=-4)+
  geom_vline(xintercept=6)


x<-filter(IchRR, disturb_count>=-4 & disturb_count <=6)
h8<-mean(x$stage_diff, na.rm = T)  
NEP8<-mean(x$NEP, na.rm = T)  
ER8<-mean(x$ER, na.rm = T)  
GPP8<-mean(x$GPPavg, na.rm = T)  
u8<-mean(x$u, na.rm = T)  

IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-01-21" & Date<"2023-04-30"~ 2))
IchRR<-filter(IchRR, RI== "2")


IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

IchRR$disturb_count<-IchRR$consec-45
ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-16)+
  geom_vline(xintercept=-9)

x<-filter(IchRR, disturb_count>=-6 & disturb_count <=8)
h2<-mean(x$stage_diff, na.rm = T)  
NEP2<-mean(x$NEP, na.rm = T)  
ER2<-mean(x$ER, na.rm = T)  
u2<-mean(x$u, na.rm = T)  
x<-filter(IchRR, disturb_count>=-16 & disturb_count <=-9)
GPP2<-mean(x$GPPavg, na.rm = T)  


IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-05-30" & Date<"2023-08-01"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_diff, na.rm=T)
IchRR$disturb_count<-IchRR$consec-28

ggplot(IchRR, aes(x=disturb_count))+
  geom_line(aes(y=ER), size=1)+
  geom_vline(xintercept=-10)+
  geom_vline(xintercept=-22)

x<-filter(IchRR, disturb_count>=-2 & disturb_count <=2)
h6<-mean(x$stage_diff, na.rm = T)  
NEP6<-mean(x$NEP, na.rm = T)  
ER6<-mean(x$ER, na.rm = T)  
u6<-mean(x$u, na.rm = T)  
x<-filter(IchRR, disturb_count>=-22 & disturb_count <=-10)
GPP6<-mean(x$GPPavg, na.rm = T)  
#############
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, NEP2, NEP6, ""))
GPP<-as.numeric(c(GPP0, GPP8, GPP2, GPP6, ""))
ER<-as.numeric(c(ER0, ER8, ER2, ER6, ""))
stage<-as.numeric(c(h0, h8, h2, h6, ""))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER8/ER0,ER2/ER0,ER6/ER0,""))
stage_reduction<-as.numeric(c("",h8/h0,h2/h0,h6/h0,""))
u<-as.numeric(c(u0, u8, u2,u6,""))

Ichetucknee<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
Ichetucknee$IF<-'h_high'
Ichetucknee$site<-"ID"
Ichetucknee$ID<-2
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
US27$depth<-US27$depth-(max(US27$depth, na.rm=T)-1.5)


US27$days <- as.Date(US27$Date)

US27<- US27 %>% mutate(RI = case_when(
  depth<1.12 ~ "low",
  depth>=1.12 ~ "high"))
US27<-rename(US27, 'stage_diff'='depth')

US27_low<-filter(US27, RI== 'low')
h0<-mean(US27_low$stage_diff, na.rm=T)
NEP0<-mean(US27_low$NEP, na.rm=T)
ER0<-mean(US27_low$ER, na.rm=T)
GPP0<-mean(US27_low$GPPavg, na.rm=T)
u0<-mean(US27_low$u, na.rm=T)


US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-18"~ 2))
US27RR<-filter(US27RR, RI== "2")

US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$stage_diff, na.rm=T)
US27RR$disturb_count<-US27RR$consec-31
ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-4)+
  geom_vline(xintercept=4)

x<-filter(US27RR, disturb_count>=-4& disturb_count <=4)
h8<-mean(x$stage_diff, na.rm = T)  
NEP8<-mean(x$NEP, na.rm = T)  
ER8<-mean(x$ER, na.rm = T)  
GPP8<-mean(x$GPPavg, na.rm = T)  
u8<-mean(x$u, na.rm = T)  

US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-04-12"~ 2))
US27RR<-filter(US27RR, RI== "2")


US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$stage_diff, na.rm=T)
US27RR$disturb_count<-US27RR$consec-24

ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=NEP), size=1)+
  geom_vline(xintercept=-2)+
  geom_vline(xintercept=4)

x<-filter(US27RR, disturb_count>=-2 & disturb_count <=4)
h2<-mean(x$stage_diff, na.rm = T)  
NEP2<-mean(x$NEP, na.rm = T)  
ER2<-mean(x$ER, na.rm = T)  
GPP2<-mean(x$GPPavg, na.rm = T)  
u2<-mean(x$u, na.rm = T)  

US27RR<- US27 %>% mutate(RI = case_when(
  Date> "2023-06-10" & Date<"2023-08-15"~ 2))
US27RR<-filter(US27RR, RI== "2")


US27RR <- US27RR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(US27RR$stage_diff, na.rm=T)
US27RR$disturb_count<-US27RR$consec-13

ggplot(US27RR, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=1)+
  geom_vline(xintercept=-2)+
  geom_vline(xintercept=1)

x<-filter(US27RR, disturb_count>=-2 & disturb_count <=1)
h6<-mean(x$stage_diff, na.rm = T)  
NEP6<-mean(x$NEP, na.rm = T)  
ER6<-mean(x$ER, na.rm = T)  
GPP6<-mean(x$GPPavg, na.rm = T)  
u6<-mean(x$u, na.rm = T)  


############
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, NEP2, NEP6, ""))
GPP<-as.numeric(c(GPP0, GPP8, GPP2, GPP6, ""))
ER<-as.numeric(c(ER0, ER8, ER2, ER6, ""))
stage<-as.numeric(c(h0, h8, h2, h6, ""))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, GPP2/GPP0,GPP6/GPP0,""))
ER_reduction<-as.numeric(c("",ER8/ER0,ER2/ER0,ER6/ER0,""))
stage_reduction<-as.numeric(c("",h8/h0,h2/h0,h6/h0,""))
u<-as.numeric(c(u0, u8, u2,u6,''))

US27<- data.frame(event,NEP,GPP,ER, stage,u, NEP_reduction,GPP_reduction,ER_reduction,stage_reduction)
US27$IF<-'h_high'
US27$site<-"IU"
US27$ID<-1
###########


R_R<-rbind(US27, Ichetucknee, LF, GB, AM, Otter, Otter_mod, AM_mod )
R<-rbind(LF, GB, AM, Otter, Otter_mod, AM_mod )

R_R$GPP_reduction_percent<- (1-R_R$GPP_reduction)*100
R_R$ER_reduction_percent<- (1-R_R$ER_reduction)*-100

R_R$GPP_reduction[R_R$GPP_reduction<1] <- NA
R_R$ER_reduction[R_R$ER_reduction<1] <- NA
R_R$GPP_reduction_percent[R_R$GPP_reduction_percent<0] <- NA


R$GPP_reduction_percent<- (1-R$GPP_reduction)*100
R$ER_reduction_percent<- (1-R$ER_reduction)*-100

R$GPP_reduction[R$GPP_reduction<1] <- NA
R$ER_reduction[R$ER_reduction<1] <- NA




write_xlsx(R, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction_ammended.xlsx")

write_xlsx(R_R, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")

R_R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")
R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction_ammended.xlsx")
R_R$a<-'a'
R$a<-'a'
cols<-c(
  "h_high"="deepskyblue3",
  "h_brown"="burlywood4",
  "h_rev"="black")
R$IF <- factor(R$IF  , levels=c("h_high","h_brown","h_rev"))

mean(R$GPP_reduction_percent, na.rm=T)
h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

(b<-ggplot(R_R, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=GPP_reduction_percent), size=6)+
    geom_smooth(aes(x=stage, y=GPP_reduction_percent, group=a), color='darkgreen', size=0.75,
                data=R, se = FALSE, method='lm')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on GPP")+
    xlab(hdiff)+ylab("GPP Reduction (%)")+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 27, color="darkgreen"),
          axis.title.y.right =element_text(size = 27, color='darkred'),
          axis.title.x =element_text(size = 27),
          plot.title = element_text(size = 22, color="darkgreen"),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
  
  
(b1<-ggplot(R_R, aes(stage, shape=site, color= IF))+
    geom_point(aes(y=ER_reduction_percent), size=6)+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(" ")+ylab("|ER| Increase (%)")+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 27, color="darkred"),
          axis.title.y.right =element_text(size = 27, color='darkred'),
          axis.title.x =element_text(size = 27),
          plot.title = element_text(size = 22,color='darkred'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

#summary(lm(GPP_reduction ~ stage, data = R))
hs<-plot_grid(b, b1, nrow=1)

ggsave(filename="reduced.jpeg", 
       plot = hs, 
       width =12, 
       height = 5.5, 
       units = "in")



c<-ggplot(R_R, aes(ID, shape=site))+
  geom_point(aes(y=ER_reduction_percent), size=3,color="darkred")+
  geom_point(aes(y=GPP_reduction_percent), size=3, color='darkgreen' )+
  ggtitle("Inverted Flood Impacts by Site")+
  xlab("RR Fequency")+
  theme(axis.text.x = element_text(size = 13, angle=0),
        axis.text.y = element_text(size = 13, angle=0),
        axis.title.y =element_text(size = 15, color="darkgreen"),
        axis.title.y.right =element_text(size = 15, color='darkred'),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 15),
        legend.position = "none",
        legend.text= element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_y_continuous(
    name = "GPP Reduction %",
    sec.axis = sec_axis( trans=~., name="|ER| Increase %"))






plot_grid(c,hs,ncol=1)

ggsave(filename="reduced.jpeg", 
       plot = hs, 
       width =12, 
       height = 6, 
       units = "in")
