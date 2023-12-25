rm(list=ls())
###packages####
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
library(ggnewscale)
####constants#####
h<-expression(paste( h[actual]-h[minimum]))
slope<-expression(paste( "Slope"~(CO[2]/O[2])))
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
                                      "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-09-07')

Otter<-rename(Otter, 'stage'='stage')
Otter$days <- as.Date(Otter$Date)
Otter$u<-Otter$stage*-0.0686+0.16

ggplot(Otter, aes(Date))+
  geom_point(aes(y=stage), size=1,color="darkred")+
  geom_hline(yintercept=0.82)+  geom_hline(yintercept=1.3)
  
Otter<- Otter %>% mutate(RI = case_when(
  stage<=0.82 ~ "hn",
  stage<1.3 ~ "hh",
  stage>=1.3 ~ "hb"))
Otter$RI[is.na(Otter$RI)] <- 'hh'


Otter$day <- as.Date(Otter$Date)
Otterx <- aggregate(Otter, by=list(Otter$day), FUN='mean')
Otter$stage_diff<-Otter$stage-min(Otter$stage, na.rm=T)
Otter$EQ<-1/abs(Otter$slope)



GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))

GB$days <- as.Date(GB$Date)

GB$EQ<-1/abs(GB$slope)


GB<- GB %>%
  mutate(hour= hour(Date),
         day= day(Date),
         month= month(Date),
         year=year(Date))

GB<-rename(GB, 'stage'='stage')
GB<- GB %>% mutate(RI = case_when(
  stage<0.55 ~ "hn",
  stage>=0.55 ~ "hh"))

GB$day <- as.Date(GB$Date)
GBx <- aggregate(GB, by=list(GB$day), FUN='mean')
GB$stage_diff<-GB$stage-min(GB$stage, na.rm=T)
GB$u<-abs(GB$stage*-0.4032+0.3189)


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
LF$days <- as.Date(LF$Date)


LF<- LF %>% mutate(RI = case_when(
  stage<0.42 ~ "hn",
  stage<0.65 ~ "hn",
  stage>=0.65 ~ "hh"))
LF$u<- -0.12*LF$stage + 0.18

LF$stage_diff<-LF$stage-min(LF$stage, na.rm=T)
LF$EQ<-1/abs(LF$slope)

ggplot(LF, aes(Date))+
  geom_point(aes(y=stage), size=1)+
  geom_hline(yintercept=0.42)+  geom_hline(yintercept=0.65)

AM<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))

AM$days <- as.Date(AM$Date)

AM$u<-(AM$stage*-0.2136+0.4426)

AM<- AM %>% mutate(RI = case_when(
  stage<0.75 ~ "hn",
  stage<1.37 ~ "hh",
  stage<2.14 ~ "hb",
  stage>=2.14 ~"hrev"))
AM$RI[is.na(AM$RI)] <- 'hb'
AM$EQ<-1/abs(AM$slope)

AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))


Ich$days <- as.Date(Ich$Date)

Ich$u<-Ich$stage*-0.0773+0.3319

Ich<- Ich %>% mutate(RI = case_when(
  stage<=0.93 ~ "hn",
  stage<1.37 ~ "hh",
  stage>=1.37 ~ "hh"))
Ich$RI[is.na(Ich$RI)] <- 'hn'

Ich$stage_diff<-Ich$stage-min(Ich$stage, na.rm=T)
Ich$EQ<-1/abs(Ich$slope)


#####Otter#######
Otter<- Otter %>% mutate(RR = case_when(
  stage<0.86 ~ "low",
  stage<1.18 ~ "moderate",
  stage>=1.18 ~ "high"))

Otter_low<-filter(Otter, RR== 'low')
Otter_mod<-filter(Otter, RR== 'moderate')
Otter_hi<-filter(Otter, RR== 'high')

(Ot_va<-ggplot()+
  scale_colour_manual(name="", values = cols, 
                      labels=c(expression(h[b]), expression(h[h]),expression(h[n])))+
    ggtitle("OS")+
  geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_point(data=Otter, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    
    geom_point(aes(x=mean(Otter_low$CO2_mmol_L, na.rm=T),
                    y=mean(Otter_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_low$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    
    geom_point(aes(x=mean(Otter_mod$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_mod$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_mod$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_mod$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(Otter_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "burlywood4",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size = 17),
          legend.title =element_text(size = 17),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
#####GB#######

GB<- GB %>% mutate(RR = case_when(
  stage<0.55 ~ "low",
  stage>=0.55 ~ "high"))

GB_low<-filter(GB, RR== 'low')
GB_hi<-filter(GB, RR== 'high')

(GB_va<-ggplot()+
    geom_point(data=GB, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]), expression(h[n]),expression(h[n])))+
    geom_abline(slope=0, intercept=0, size=0.5)+
    ggtitle("GB")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    geom_point(aes(x=mean(GB_low$CO2_mmol_L, na.rm=T),
                   y=mean(GB_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(GB_low$CO2_mmol_L, na.rm=T),
                   y=mean(GB_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    
    geom_point(aes(x=mean(GB_hi$CO2_mmol_L, na.rm=T),
                   y=mean(GB_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(GB_hi$CO2_mmol_L, na.rm=T),
                   y=mean(GB_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size = 17),
          legend.title =element_text(size = 17),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


####LF######
LF<- LF %>% mutate(RR = case_when(
  stage<0.33 ~ "low",
  stage>=0.61 ~ "high"))

LF_low<-filter(LF, RR== 'low')
LF_hi<-filter(LF, RR== 'high')


(LF_va<-ggplot()+
    geom_point(data=LF, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]),expression(h[n])))+
    geom_point(aes(x=mean(LF_low$CO2_mmol_L, na.rm=T),
                   y=mean(LF_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(LF_low$CO2_mmol_L, na.rm=T),
                   y=mean(LF_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_point(aes(x=mean(LF_hi$CO2_mmol_L, na.rm=T),
                   y=mean(LF_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(LF_hi$CO2_mmol_L, na.rm=T),
                   y=mean(LF_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("LF")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size = 17),
          legend.title =element_text(size = 17),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



#####AM#######
AM<- AM %>% mutate(RI = case_when(
  stage<0.75 ~ "hn",
  stage<1.37 ~ "hh",
  stage<2.14 ~ "hb",
  stage>=2.14 ~"hrev"))
AM$RI[is.na(AM$RI)] <- 'hb'

AM_hn<-filter(AM, RI== 'hn')
AM_hh<-filter(AM, RI== 'hh')
AM_hb<-filter(AM, RI== 'hb')
AM_hrev<-filter(AM, RI== 'hrev')


(AM_va<-ggplot()+
    geom_point(data=AM, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[brown]), 
                                 expression(h[high]), 
                                 expression(h[norm]),
                                 expression(h[reversal])))+
    geom_point(aes(x=mean(AM_hn$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hn$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hn$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hn$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    
    geom_point(aes(x=mean(AM_hh$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hh$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hh$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hh$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(AM_hb$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hb$O2_mmol_L, na.rm=T)),
               shape=21, fill = "burlywood4",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hb$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hb$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(AM_hrev$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hrev$O2_mmol_L, na.rm=T)),
               shape=21, fill = "black",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hrev$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hrev$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("AM")+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+  
    scale_x_continuous(n.breaks=4)+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.key.size = unit(1, 'cm'),
          legend.text=element_text(size = 17),
          legend.title =element_text(size = 17),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

####Ich######

Ich<- Ich %>% mutate(RR = case_when(
  stage<0.93 ~ "low",
  stage>=1.37 ~ "high"))
Ich_low<-filter(Ich, RR== 'low')
Ich_hi<-filter(Ich, RR== 'high')



(Ich_va<-ggplot()+
    geom_point(data=Ich, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols, 
                        labels=c(expression(h[h]), expression(h[n])))+
    geom_point(aes(x=mean(Ich_low$CO2_mmol_L, na.rm=T),
                   y=mean(Ich_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(Ich_low$CO2_mmol_L, na.rm=T),
                   y=mean(Ich_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    
    geom_point(aes(x=mean(Ich_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Ich_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(Ich_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Ich_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("ID")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    xlab(x) + ylab(y)+
    scale_y_continuous(n.breaks=4)+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size = 17),
          legend.title =element_text(size = 17),
          legend.position ="none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



#####
ellipse<-plot_grid(Ich_va, GB_va, LF_va, Ot_va, AM_va)
ggsave(filename="ellipse.jpeg", 
       plot = ellipse, 
       width =15, 
       height = 9, 
       units = "in")

##slope####
slope<-expression("Slope"~ (O[2]:CO[2]))
AM<-rename(AM, 'stage'='stage')

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

columns<-c("Date","stage","ER","GPPavg","NEP",
           "O2_mmol_L","CO2_mmol_L","Spring",
           "RI", "slope","offset","u", "EQ")

Otter1<-Otter[,columns]
GB1<-GB[,columns]
LF1<-LF[,columns]
Ich1<-Ich[,columns]
AM1<-AM[,columns]

Otter1 <- Otter1[complete.cases(Otter1$RI), ] 
OS2<-ggplot(Otter1, aes(x=RI, y=slope, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("OS")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[brown]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size = 17),
        legend.title =element_text(size = 17),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

GB1 <- GB1[complete.cases(GB1$RI), ] 
GB2 <-ggplot(GB1, aes(x=RI, y=slope, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("GB")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[brown]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size = 17),
        legend.title =element_text(size = 17),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


LF1 <- LF1[complete.cases(LF1$RI), ] 
LF2 <-ggplot(LF1, aes(x=RI, y=slope, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("LF")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[brown]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size = 17),
        legend.title =element_text(size = 17),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

AM1 <- AM1[complete.cases(AM1$RI), ] 
AM2 <-ggplot(AM1, aes(x=RI, y=slope, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("AM")+ylim(-2,0)+
  scale_x_discrete(labels=c("hrev" = expression(h[reversal]),
                            "hb" = expression(h[brown]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size = 17),
        legend.title =element_text(size = 17),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))




Ich1 <- Ich1[complete.cases(Ich1$RI), ] 
Ich2 <- ggplot(Ich1, aes(x=RI, y=slope, group=RI))+ 
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("ID")+ylim(-2,0)+
  scale_x_discrete(labels=c("hb" = expression(h[brown]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size = 17),
        legend.title =element_text(size = 17),
        legend.position ="none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



cloudbox<-plot_grid(Ich2, GB2, LF2, OS2, AM2)
ggsave(filename="cloudbox.jpeg", 
       plot = cloudbox, 
       width =15, 
       height = 9, 
       units = "in")

####slope tests########

summary(lm(slope~stage, data = Otter))
summary(aov(slope ~ RI,
    data = Otter))

summary(lm(slope~stage, data = AM))
summary(aov(slope ~ RI,
    data = AM))

summary(lm(slope~stage, data = LF))
t.test(slope ~ RI, data = LF)

summary(lm(slope~stage, data = GB))
t.test(slope ~ RI, data = GB)

summary(lm(slope~stage, data = Ich))
t.test(slope ~ RI, data = Ich)

