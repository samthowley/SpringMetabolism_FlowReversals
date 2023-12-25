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
Otter<- Otter %>%
  mutate(hour = hour(Date))

Otter_pm<-filter(Otter, hour==4)
x<-c("Date","CO2_mmol_m2_1d", "O2_mmol_m2_1d" )
Otter_pm<-Otter_pm[,x]
names(Otter_pm)
Otter_pm<-rename(Otter_pm, 'O2_pm'="O2_mmol_m2_1d", 'CO2_pm'="CO2_mmol_m2_1d" )
Otter<-left_join(Otter, Otter_pm, by='Date')

Otter$CO2perO2<-Otter$"CO2_mmol_m2_1d"/Otter$"O2_mmol_m2_1d"

Otter$days <- as.Date(Otter$Date)

Otter<- Otter %>% mutate(RI = case_when(
  stage_avg<0.82 ~ "hn",
  stage_avg<1.12 ~ "hh",
  stage_avg>=1.12 ~ "hb"))
Otter$day <- as.Date(Otter$Date)
Otterx <- aggregate(Otter, by=list(Otter$day), FUN='mean')
Otter$stage_diff<-Otter$stage_avg-min(Otter$stage_avg, na.rm=T)




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

GB_pm<-filter(GB, hour==4)
names(GB_pm)
GB_pm<-GB_pm[,x]
GB_pm<-rename(GB_pm, 'O2_pm'="O2_mmol_m2_1d", 'CO2_pm'="CO2_mmol_m2_1d" )

GB<-left_join(GB, GB_pm, by='Date')


GB<-rename(GB, 'stage_avg'='stage')
GB<- GB %>% mutate(RI = case_when(
  stage_avg<0.55 ~ "hn",
  stage_avg>=0.55 ~ "hh"))

GB$day <- as.Date(GB$Date)
GBx <- aggregate(GB, by=list(GB$day), FUN='mean')
GB$stage_diff<-GB$stage_avg-min(GB$stage_avg, na.rm=T)


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
LF$CO2perO2<-1/LF$slope

LF$days <- as.Date(LF$Date)

LF<- LF %>%
  mutate(hour= hour(Date),
         day= day(Date),
         month= month(Date),
         year=year(Date))

LF_pm<-filter(LF, hour==6)
LF_pm<-LF_pm[,x]
names(LF_pm)
LF_pm<-rename(LF_pm, 'O2_pm'="O2_mmol_m2_1d", 'CO2_pm'="CO2_mmol_m2_1d" )

LF<-left_join(LF, LF_pm, by='Date')


LF<- LF %>% mutate(RI = case_when(
  stage_avg<0.33 ~ "hn",
  stage_avg<0.61 ~ "hn",
  stage_avg>=0.61 ~ "hh"))

LF$stage_diff<-LF$stage_avg-min(LF$stage_avg, na.rm=T)


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

AM<- AM %>%
  mutate(hour= hour(Date),
         day= day(Date),
         month= month(Date),
         year=year(Date))

AM_pm<-filter(AM, hour==4)
AM_pm<-AM_pm[,x]
names(AM_pm)
AM_pm<-rename(AM_pm, 'O2_pm'="O2_mmol_m2_1d", 'CO2_pm'="CO2_mmol_m2_1d" )

AM<-left_join(AM, AM_pm, by='Date')

AM<- AM %>% mutate(RI = case_when(
  stage_avg<0.93 ~ "hn",
  stage_avg<1.37 ~ "hh",
  stage_avg>=1.37 ~ "hb",
  stage_avg>=2.1 ~ "hrev",))
AM$stage_diff<-AM$stage_avg-min(AM$stage_avg, na.rm=T)

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))


Ich$days <- as.Date(Ich$Date)

Ich<- Ich %>%
  mutate(hour= hour(Date),
         day= day(Date),
         month= month(Date),
         year=year(Date))

Ich_pm<-filter(Ich, hour==6)
Ich_pm<-Ich_pm[,x]
names(Ich_pm)
Ich_pm<-rename(Ich_pm, 'O2_pm'="O2_mmol_m2_1d", 'CO2_pm'="CO2_mmol_m2_1d" )

Ich<-left_join(Ich, Ich_pm, by='Date')


Ich<- Ich %>% mutate(RI = case_when(
  stage_avg<0.63 ~ "hn",
  stage_avg<1.88 ~ "hn",
  stage_avg>=1.88 ~ "hh"))

Ich$stage_diff<-Ich$stage_avg-min(Ich$stage_avg, na.rm=T)




GB$u<-abs(GB$stage_avg*-0.4032+0.3189)
Otter$u<-Otter$stage_avg*-0.0686+0.16
AM$u<-(AM$stage_avg*-0.2136+0.4426)
LF$u<- -0.12*LF$stage_avg + 0.18
Ich$u<-Ich$stage_avg*-0.0773+0.3319





#####Otter#######

(Ot_g<-ggplot(data=Otter, aes(x=u,y=slope, color="NEP")) + 
  geom_point(size=1)+
  geom_hline(yintercept=0, linetype='dotted', size=1) + 
  geom_hline(yintercept=-1, linetype='dotted', size=1) + 
  stat_correlation(mapping = use_label(c('P')), size=5,
                   label.y = 0.03,)+
  stat_poly_line(formula = y ~ x, color='blue') +
  stat_poly_eq(mapping = use_label(c("eq")), label.y = 0.1, size=5)+
  scale_color_manual(values='black')+
  xlab("Velocity (m/s)")+
  ylab(expression(paste("Slope"~O[2]/CO[2])))+
  ggtitle("Otter")+
  ylim(-2,0.2)+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_blank(),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(Ot_va<-ggplot()+
  geom_point(data=Otter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=RI))+
  scale_colour_manual(name="", values = cols)+
    ggtitle("Otter")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white')))
####GB#####

(GB_g<-ggplot(data=GB, aes(x=u,y=slope, color="NEP")) + 
   geom_point(size=1)+
   geom_hline(yintercept=0, linetype='dotted', size=1) + 
   geom_hline(yintercept=-1, linetype='dotted', size=1) + 
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")), label.y = 0.1, size=5)+
   scale_color_manual(values='black')+
   xlab("Velocity (m/s)")+
   ylab(expression(paste("Slope"~O[2]/CO[2])))+
   ggtitle("GB")+
   ylim(-2,0.2)+
   theme(axis.text.x = element_text(size = 16, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_blank(),
         axis.title.x =element_text(size = 18),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

GB<-filter(GB, CO2_mmol_m2_1d<1)
(GB_va<-ggplot()+
    geom_point(data=GB, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=RI))+
    scale_colour_manual(name="", values = cols)+
    ggtitle('GB')+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white')))


####LF######

(LF_g<-ggplot(data=LF, aes(x=u,y=slope, color="NEP")) + 
   geom_point(size=1)+
   geom_hline(yintercept=0, linetype='dotted', size=1) + 
   geom_hline(yintercept=-1, linetype='dotted', size=1) + 
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")), label.y = 0.1, size=5)+
   scale_color_manual(values='black')+
   xlab("Velocity (m/s)")+
   ylab(expression(paste("Slope"~O[2]/CO[2])))+
   ggtitle("LF")+
   ylim(-2,0.2)+
   theme(axis.text.x = element_text(size = 16, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_blank(),
         axis.title.x =element_text(size = 18),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(LF_va<-ggplot()+
    geom_point(data=LF, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=RI))+
    scale_colour_manual(name="", values = cols)+
    ggtitle('LF')+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white')))


####AM######

(AM_g<-ggplot(data=AM, aes(x=u,y=slope, color="NEP")) + 
   geom_point(size=1)+
   geom_hline(yintercept=0, linetype='dotted', size=1) + 
   geom_hline(yintercept=-1, linetype='dotted', size=1) + 
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")), label.y = 0.1, size=5)+
   scale_color_manual(values='black')+
   xlab("Velocity (m/s)")+
   ylab(expression(paste("Slope"~O[2]/CO[2])))+
   ggtitle("AM")+
   ylim(-2,0.2)+
   theme(axis.text.x = element_text(size = 16, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_blank(),
         axis.title.x =element_text(size = 18),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(AM_va<-ggplot()+
    geom_point(data=AM, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=RI))+
    scale_colour_manual(name="", values = cols)+
    ggtitle('AM')+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white')))



####Ich######

(ID_g<-ggplot(data=Ich, aes(x=u,y=slope, color="NEP")) + 
   geom_point(size=1)+
   geom_hline(yintercept=0, linetype='dotted', size=1) + 
   geom_hline(yintercept=-1, linetype='dotted', size=1) + 
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")), label.y = 0.1, size=5)+
   scale_color_manual(values='black')+
   xlab("Velocity (m/s)")+
   ylab(expression(paste("Slope"~O[2]/CO[2])))+
   ggtitle("ID")+
   ylim(-2,0.2)+
   theme(axis.text.x = element_text(size = 16, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_blank(),
         axis.title.x =element_text(size = 18),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(ID_va<-ggplot()+
    geom_point(data=Ich, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=RI))+
    scale_colour_manual(name="", values = cols)+
    ggtitle('ID')+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position ="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white')))











#####
plot_grid(ID_g, GB_g, LF_g, Ot_g, AM_g)
plot_grid(ID_va, GB_va, LF_va, Ot_va, AM_va)

######
GB$u<-abs(GB$stage_avg*-0.4032+0.3189)
Otter$u<-Otter$stage_avg*-0.0686+0.16
AM$u<-(AM$stage_avg*-0.2136+0.4426)
LF$u<- -0.12*LF$stage_avg + 0.18
Ich$u<-Ich$stage_avg*-0.0773+0.3319


Otter$Spring<-'Otter'
GB$Spring<-'GB'
LF$Spring<-'LF'
AM$Spring<-'AM'
Ich$Spring<-'Ichetucknee'

columns<-c("Date","stage_avg","ER","GPPavg","NEP",
           "O2_mmol_m2_1d","CO2_mmol_m2_1d","Spring",
           "RI", "slope","offset","u")

Otter1<-Otter[,columns]
GB1<-GB[,columns]
LF1<-LF[,columns]
Ich1<-Ich[,columns]
AM1<-AM[,columns]


all<-rbind(Otter1,AM1,GB1,LF1,Ich1)

ggplot()+
  geom_point(data=all, aes(x=u, y=slope, color=Spring), size=2)+
  geom_smooth(aes(x=u, y=slope), color="black",
              size=0.75, data=all, se = FALSE, method='lm')+
  
  scale_colour_manual(name="", values = c("black") )+
  new_scale_color() +
  xlab("Velocity (m/s)")+
  ylab(expression(paste("Slope"~O[2]/CO[2])))+
  geom_hline(yintercept=0, linetype='dotted', size=1) + 
  geom_hline(yintercept=-1, linetype='dotted', size=1) + 
  ggtitle(expression(paste("Paired"~O[2]-CO[2]~"vs. Disturbed Velocity")))+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1, 'cm'),
        legend.position = 'bottom',
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color=guide_legend(title=""))


















