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
library(ggnewscale)

h<-expression(paste( h[actual]-h[minimum]))
slope<-expression(paste( "Slope"~(O[2]/CO[2])))
x<-expression(CO[2]~'departure'~('mmol'~L^-1))
y<-expression(O[2]~'departure'~('mmol'~L^-1))
  
  
cols<-c(
  "hn"="lightblue",
  "hh"="blue",
  "hb"="burlywood4",
  "hrev"="black")
##data#####


Otter <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-09-07')

Otter<-rename(Otter, 'stage_avg'='stage')
Otter$days <- as.Date(Otter$Date)
Otter$u<-Otter$stage_avg*-0.0686+0.16

ggplot(Otter, aes(Date))+
  geom_point(aes(y=stage_avg), size=1,color="darkred")+
  geom_hline(yintercept=0.82)+  geom_hline(yintercept=1.3)
  
Otter<- Otter %>% mutate(RI = case_when(
  stage_avg<=0.82 ~ "hn",
  stage_avg<1.3 ~ "hh",
  stage_avg>=1.3 ~ "hb"))
Otter$RI[is.na(Otter$RI)] <- 'hh'


Otter$day <- as.Date(Otter$Date)
Otterx <- aggregate(Otter, by=list(Otter$day), FUN='mean')
Otter$stage_diff<-Otter$stage_avg-min(Otter$stage_avg, na.rm=T)
Otter$EQ<-1/Otter$slope



GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))

GB$days <- as.Date(GB$Date)

GB$CO2perO2<-1/GB$slope


GB<- GB %>%
  mutate(hour= hour(Date),
         day= day(Date),
         month= month(Date),
         year=year(Date))

GB<-rename(GB, 'stage_avg'='stage')
GB<- GB %>% mutate(RI = case_when(
  stage_avg<0.55 ~ "hn",
  stage_avg>=0.55 ~ "hh"))

GB$day <- as.Date(GB$Date)
GBx <- aggregate(GB, by=list(GB$day), FUN='mean')
GB$stage_diff<-GB$stage_avg-min(GB$stage_avg, na.rm=T)
GB$u<-abs(GB$stage_avg*-0.4032+0.3189)
GB$EQ<-1/GB$slope


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric"))
names(LF)
LF$days <- as.Date(LF$Date)


LF<- LF %>% mutate(RI = case_when(
  stage_avg<0.42 ~ "hn",
  stage_avg<0.65 ~ "hn",
  stage_avg>=0.65 ~ "hh"))
LF$u<- -0.12*LF$stage_avg + 0.18

LF$stage_diff<-LF$stage_avg-min(LF$stage_avg, na.rm=T)
LF$EQ<-1/LF$slope

ggplot(LF, aes(Date))+
  geom_point(aes(y=stage_avg), size=1)+
  geom_hline(yintercept=0.42)+  geom_hline(yintercept=0.65)

AM<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric"))
AM$CO2perO2<-1/AM$slope

AM$days <- as.Date(AM$Date)


AM$u<-(AM$stage*-0.2136+0.4426)

AM<- AM %>% mutate(RI = case_when(
  stage<0.75 ~ "hn",
  stage<1.37 ~ "hh",
  stage<2.14 ~ "hb",
  stage>=2.14 ~"hrev"))
AM$RI[is.na(AM$RI)] <- 'hb'
AM$EQ<-1/AM$slope


AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric","numeric"))


Ich$days <- as.Date(Ich$Date)



Ich$u<-Ich$stage*-0.0773+0.3319

Ich<- Ich %>% mutate(RI = case_when(
  stage<=0.93 ~ "hn",
  stage<1.37 ~ "hh",
  stage>=1.37 ~ "hh"))
Ich$RI[is.na(Ich$RI)] <- 'hn'

Ich$stage_diff<-Ich$stage-min(Ich$stage, na.rm=T)
Ich$EQ<-1/Ich$slope









#####Otter#######
Otter<- Otter %>% mutate(RR = case_when(
  stage_avg<0.86 ~ "low",
  stage_avg<1.18 ~ "moderate",
  stage_avg>=1.18 ~ "high"))

Otter_low<-filter(Otter, RR== 'low')
quantile(Otter_low$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(Otter, u<=0.1010098 )
(slope<-mean(bin$slope, na.rm = T))

bin2<-filter(Otter, slope<=0.1083297     )
(slope2<-mean(bin2$slope, na.rm = T))

bin3<-filter(Otter, slope<=0.1088434     )
(slope3<-mean(bin3$slope, na.rm = T))

bin4<-filter(Otter, slope>=0.1093417     )
(slope4<-mean(bin4$slope, na.rm = T))

bin5<-filter(Otter, slope>=0.1114760      )
(slope5<-mean(bin5$slope, na.rm = T))


Otter_hi<-filter(Otter, RR== 'high')
quantile(Otter_hi$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin6<-filter(Otter, u<=0.04428368  )
(slope6<-mean(bin6$slope, na.rm = T))

bin7<-filter(Otter, u<=0.05466986      )
(slope7<-mean(bin7$slope, na.rm = T))

bin8<-filter(Otter, u<=0.06114160      )
(slope8<-mean(bin8$slope, na.rm = T))

bin9<-filter(Otter, u<=0.06947260)
(slope9<-mean(bin9$slope, na.rm = T))

bin10<-filter(Otter, u<=0.07901615 )
(slope10<-mean(bin10$slope, na.rm = T))

bin11<-filter(Otter, u<=0.07901615 )
(slope11<-mean(bin11$slope, na.rm = T))



(Ot_va<-ggplot()+
  scale_colour_manual(name="", values = cols, 
                      labels=c(expression(h[b]), expression(h[h]),expression(h[n])))+
    ggtitle("OS")+
  geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_point(data=Otter, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    
    xlab(x) + ylab(y)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
#####GB#######

GB<- GB %>% mutate(RR = case_when(
  stage_avg<0.55 ~ "low",
  stage_avg>=0.55 ~ "high"))
GB_low<-filter(GB, RR== 'low')

quantile(GB_low$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(GB, u<=0.09715939    )
(slope<-mean(bin$slope, na.rm = T))

bin2<-filter(GB, slope<=0.12670602       )
(slope2<-mean(bin2$slope, na.rm = T))

bin3<-filter(GB, slope<=0.15064903        )
(slope3<-mean(bin3$slope, na.rm = T))

bin4<-filter(GB, slope>=0.17243320        )
(slope4<-mean(bin4$slope, na.rm = T))

bin5<-filter(GB, slope>=0.28678908         )
(slope5<-mean(bin5$slope, na.rm = T))


GB_hi<-filter(GB, RR== 'high')
quantile(GB_hi$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin6<-filter(GB, u<=1.109450e-06  )
(slope6<-mean(bin6$slope, na.rm = T))

bin7<-filter(GB, u<=3.033397e-02      )
(slope7<-mean(bin7$slope, na.rm = T))

bin8<-filter(GB, u<=5.539553e-02      )
(slope8<-mean(bin8$slope, na.rm = T))

bin9<-filter(GB, u<=7.830321e-02)
(slope9<-mean(bin9$slope, na.rm = T))

bin10<-filter(GB, u<=1.215145e-01 )
(slope10<-mean(bin10$slope, na.rm = T))

bin11<-filter(GB, u<=1.215145e-01 )
(slope11<-mean(bin11$slope, na.rm = T))



(GB_va<-ggplot()+
    geom_point(data=GB, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]), expression(h[n]),expression(h[n])))+
    ggtitle("GB")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


####LF######
LF<- LF %>% mutate(RR = case_when(
  stage_avg<0.33 ~ "low",
  stage_avg>=0.61 ~ "high"))

LF_low<-filter(LF, RR== 'low')
quantile(LF_low$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(LF, u<=0.1404017   )
(slope<-mean(bin$slope, na.rm = T))

bin2<-filter(LF, slope<=0.1447047      )
(slope2<-mean(bin2$slope, na.rm = T))

bin3<-filter(LF, slope<=0.1492033       )
(slope3<-mean(bin3$slope, na.rm = T))

bin4<-filter(LF, slope>=0.1537834       )
(slope4<-mean(bin4$slope, na.rm = T))

bin5<-filter(LF, slope>=0.1575903        )
(slope5<-mean(bin5$slope, na.rm = T))


LF_hi<-filter(LF, RR== 'high')
quantile(LF_hi$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin6<-filter(LF, u<=0.06933720   )
(slope6<-mean(bin6$slope, na.rm = T))

bin7<-filter(LF, u<=0.07871486       )
(slope7<-mean(bin7$slope, na.rm = T))

bin8<-filter(LF, u<=0.08979422       )
(slope8<-mean(bin8$slope, na.rm = T))

bin9<-filter(LF, u<=0.10104927 )
(slope9<-mean(bin9$slope, na.rm = T))

bin10<-filter(LF, u<=0.10679518  )
(slope10<-mean(bin10$slope, na.rm = T))

bin11<-filter(LF, u<=0.10679518 )
(slope11<-mean(bin11$slope, na.rm = T))



(LF_va<-ggplot()+
    geom_point(data=LF, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]),expression(h[n])))+
    ggtitle("LF")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



#####AM#######
AM<- AM %>% mutate(RR = case_when(
  stage<0.93 ~ "low",
  stage>=1.37 ~ "high"))
AM_low<-filter(AM, RR== 'low')

AM_low<-filter(AM, RR== 'low')
quantile(AM_low$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(AM, u<=0.2446519   )
(slope<-mean(bin$slope, na.rm = T))

bin2<-filter(AM, slope<=0.2986026      )
(slope2<-mean(bin2$slope, na.rm = T))

bin3<-filter(AM, slope<=0.3012696       )
(slope3<-mean(bin3$slope, na.rm = T))

bin4<-filter(AM, slope>=0.3033891       )
(slope4<-mean(bin4$slope, na.rm = T))

bin5<-filter(AM, slope>=0.3088734        )
(slope5<-mean(bin5$slope, na.rm = T))


AM_hi<-filter(AM, RR== 'high')
quantile(AM_hi$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)


bin7<-filter(AM, u<=-0       )
(slope7<-mean(bin7$slope, na.rm = T))

bin8<-filter(AM, u<=-0.01183316          )
(slope8<-mean(bin8$slope, na.rm = T))

bin9<-filter(AM, u<=0.05238239    )
(slope9<-mean(bin9$slope, na.rm = T))

bin10<-filter(AM, u<=0.13592203     )
(slope10<-mean(bin10$slope, na.rm = T))

bin11<-filter(AM, u<=0.14992565  )
(slope11<-mean(bin11$slope, na.rm = T))



(AM_va<-ggplot()+
    geom_point(data=AM, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[b]), 
                                 expression(h[h]), 
                                 expression(h[n]),
                                 expression(h[rev])))+
    ggtitle("AM")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(1, 'cm'),
          legend.text=element_text(size=15),
          legend.title =element_text(size=18),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

####Ich######

Ich<- Ich %>% mutate(RR = case_when(
  stage<0.93 ~ "low",
  stage>=1.37 ~ "high"))
Ich_low<-filter(Ich, RR== 'low')

quantile(Ich_low$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(Ich, u<=0.2602366     )
(slope<-mean(bin$slope, na.rm = T))

bin2<-filter(Ich, slope<=0.2712245        )
(slope2<-mean(bin2$slope, na.rm = T))

bin3<-filter(Ich, slope<=0.2987929         )
(slope3<-mean(bin3$slope, na.rm = T))

bin4<-filter(Ich, slope>=0.3155533         )
(slope4<-mean(bin4$slope, na.rm = T))

bin5<-filter(Ich, slope>=0.3218407          )
(slope5<-mean(bin5$slope, na.rm = T))


Ich_hi<-filter(Ich, RR== 'high')
quantile(Ich_hi$u, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin6<-filter(Ich, u<=0.1811302   )
(slope6<-mean(bin6$slope, na.rm = T))

bin7<-filter(Ich, u<=0.1926013  )
(slope7<-mean(bin7$slope, na.rm = T))

bin8<-filter(Ich, u<=0.2069502  )
(slope8<-mean(bin8$slope, na.rm = T))

bin9<-filter(Ich, u<=0.2138134  )
(slope9<-mean(bin9$slope, na.rm = T))

bin10<-filter(Ich, u<=0.2246090 )
(slope10<-mean(bin10$slope, na.rm = T))

bin11<-filter(Ich, u<=0.2246090 )
(slope11<-mean(bin11$slope, na.rm = T))



(Ich_va<-ggplot()+
    geom_point(data=Ich, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]), expression(h[n])))+
    ggtitle("ID")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlab(x) + ylab(y)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))











#####
plot_grid(Ich_va, GB_va, LF_va, Ot_va, AM_va)

######
slope<-expression("EQ"~ (CO[2]:O[2]))
AM<-rename(AM, 'stage_avg'='stage')
Ich<-rename(Ich, 'stage_avg'='stage')

AM$RI <- factor(AM$RI  , levels=c("hn","hh","hb", "hrev"))
Otter$RI <- factor(Otter$RI  , levels=c("hn","hh","hb"))
LF$RI <- factor(LF$RI  , levels=c("hn","hh"))
GB$RI <- factor(GB$RI  , levels=c("hn","hh"))
Ich$RI <- factor(Ich$RI  , levels=c("hn","hh"))


Otter$Spring<-'OS'
GB$Spring<-'GB'
LF$Spring<-'LF'
AM$Spring<-'AM'
Ich$Spring<-'ID'

columns<-c("Date","stage_avg","ER","GPPavg","NEP",
           "O2_mmol_L","CO2_mmol_L","Spring",
           "RI", "slope","offset","u", "EQ")

Otter1<-Otter[,columns]
GB1<-GB[,columns]
LF1<-LF[,columns]
Ich1<-Ich[,columns]
AM1<-AM[,columns]

Otter1 <- Otter1[complete.cases(Otter1$RI), ] 
OS2<-ggplot(Otter1, aes(x=RI, y=EQ, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("OS")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[b]),
                            "hh" = expression(h[h]),
                            "hn" = expression(h[n])))+
  theme(axis.text.x = element_text(size = 18, angle=0),
        axis.text.y = element_text(size = 18, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 18, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=18),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

GB1 <- GB1[complete.cases(GB1$RI), ] 
GB2 <-ggplot(GB1, aes(x=RI, y=EQ, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("GB")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[b]),
                            "hh" = expression(h[h]),
                            "hn" = expression(h[n])))+
  theme(axis.text.x = element_text(size = 18, angle=0),
        axis.text.y = element_text(size = 18, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 18, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=18),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


LF1 <- LF1[complete.cases(LF1$RI), ] 
LF2 <-ggplot(LF1, aes(x=RI, y=EQ, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("LF")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[b]),
                            "hh" = expression(h[h]),
                            "hn" = expression(h[n])))+
  theme(axis.text.x = element_text(size = 18, angle=0),
        axis.text.y = element_text(size = 18, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 18, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=18),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

AM1 <- AM1[complete.cases(AM1$RI), ] 
AM2 <-ggplot(AM1, aes(x=RI, y=EQ, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("AM")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[b]),
                            "hrev" = expression(h[rev]),
                            "hh" = expression(h[h]),
                            "hn" = expression(h[n])))+
  theme(axis.text.x = element_text(size = 18, angle=0),
        axis.text.y = element_text(size = 18, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 18, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=18),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))




Ich1 <- Ich1[complete.cases(Ich1$RI), ] 
Ich2 <- ggplot(Ich1, aes(x=RI, y=EQ, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("ID")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[b]),
                            "hh" = expression(h[h]),
                            "hn" = expression(h[n])))+
  theme(axis.text.x = element_text(size = 18, angle=0),
        axis.text.y = element_text(size = 18, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 18, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=18),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



plot_grid(Ich2, GB2, LF2, OS2, AM2)












