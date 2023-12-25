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

flux<-expression(paste('NEP'~~'g'~O[2]/m^2/'day'))
O2<-expression('O'[2]~'mg/L')


####GB####
GB <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric"))

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

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-12-20"~ 2))
GBFR<-filter(GBFR, RI== "2")

GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_avg, na.rm=T)
GBFR$disturb_count<-GBFR$consec-46

GBbase<-filter(GBFR, disturb_count< -30)
GBbaseNEP<-mean(GBFR$NEP, na.rm=T)
GBFR$NEP_DR<-GBFR$NEP/GBbaseNEP

GBbase<-filter(GBFR, disturb_count< -30)
GBbaseO2<-mean(GBFR$O2avg, na.rm=T)
GBFR$O2_DR<-GBFR$O2avg/GBbaseO2


(GB_NEP<-ggplot(GBFR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(GB_NEPdots<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=GBFR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(GB_stage<-ggplot()+
    geom_line(data=GBFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Little Fanning')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

GB1<-plot_grid(GB_stage, GB_NEP,GB_NEPdots)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-18" & Date<"2023-08-17"~ 2))
GBFR<-filter(GBFR, RI== "2")

GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_avg, na.rm=T)
GBFR$disturb_count<-GBFR$consec-20

(GB_NEP<-ggplot(GBFR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(GB_NEPdots<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=GBFR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(GB_stage<-ggplot()+
    geom_line(data=GBFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Gilchrist Blue')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

GB2<-plot_grid(GB_stage, GB_NEP,GB_NEPdots)


####LF#####
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
names(LF)

LF$days <- as.Date(LF$Date)


LF<- LF %>% mutate(RI = case_when(
  stage_avg<0.33 ~ "low",
  stage_avg<0.61 ~ "moderate",
  stage_avg>=0.61 ~ "high"))
LF_low<-filter(LF, RI== 'low')



LFRR<- LF %>% mutate(RI = case_when(
  Date> "2022-08-10" & Date<"2022-10-16"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_avg, na.rm=T)
LFRR$disturb_count<-LFRR$consec-36

(LF_NEP<-ggplot(LFRR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_NEPdots<-ggplot(LFRR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=LFRR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_stage<-ggplot()+
    geom_line(data=LFRR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Little Fanning')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

LF1<-plot_grid(LF_stage, LF_NEP,LF_NEPdots)




LFFR<- LF %>% mutate(RI = case_when(
  Date> "2022-12-30" & Date<"2023-06-14"~ 2))
LFFR<-filter(LFFR, RI== "2")


LFFR <- LFFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFFR$stage_avg, na.rm=T)
LFFR$disturb_count<-LFFR$consec-55

LFFRbase<-filter(LFFR, disturb_count< -30)
LFbase<-mean(LFFRbase$NEP, na.rm=T)
LFFR$NEP_DR<-LFFR$NEP/LFbase

(LF_NEP<-ggplot(LFFR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
  ylab(flux) + 
  xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_NEPdots<-ggplot(LFFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=LFFR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_stage<-ggplot()+
    geom_line(data=LFFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Little Fanning')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

LF1<-plot_grid(LF_stage, LF_NEP,LF_NEPdots)



LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-05-10" & Date<"2023-08-20"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR <- LFRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(LFRR$stage_avg, na.rm=T)
LFRR$disturb_count<-LFRR$consec-52

LFRRbase<-filter(LFRR, disturb_count< -30)
LFbase<-mean(LFRRbase$NEP, na.rm=T)
LFRR$NEP_DR<-LFRR$NEP/LFbase

(LF_NEP<-ggplot(LFRR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_NEPdots<-ggplot(LFRR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=LFFR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_stage<-ggplot()+
    geom_line(data=LFRR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Little Fanning')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))


LF2<-plot_grid(LF_stage, LF_NEP,LF_NEPdots)

####Ich#####
Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric" ))


Ich<- Ich %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-Ich %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
Ich<-left_join(Ich,DO, by=c("day","month","year"))


Ich<- Ich %>% mutate(RI = case_when(
  stage_avg<0.63 ~ "low",
  stage_avg<1.88 ~ "moderate",
  stage_avg>=1.88 ~ "high"))
Ich_low<-filter(Ich, RI== 'low')


Ich$days <- as.Date(Ich$Date)
names(Ich)
IchFR<- Ich %>% mutate(RI = case_when(
  Date> "2022-11-20" & Date<"2023-5-30"~ 2))
IchFR<-filter(IchFR, RI== "2")

IchFR <- IchFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchFR$stage_avg, na.rm=T)
IchFR$disturb_count<-IchFR$consec-47

(ICH_NEP<-ggplot(IchFR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_NEPdots<-ggplot(IchFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=IchFR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_stage<-ggplot()+
    geom_line(data=IchFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Ichetucknee')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

ICH1<-plot_grid(ICH_stage, ICH_NEP,ICH_NEPdots)



IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2022-07-23" & Date<"2022-10-30"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_avg, na.rm=T)
IchRR$disturb_count<-IchRR$consec-56



(ICH_NEP<-ggplot(IchRR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_NEPdots<-ggplot(IchRR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=IchRR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_stage<-ggplot()+
    geom_line(data=IchRR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Ichetucknee')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

ICH2<-plot_grid(ICH_stage, ICH_NEP,ICH_NEPdots)


IchRR<- Ich %>% mutate(RI = case_when(
  Date> "2023-06-14" & Date<"2023-08-11"~ 2))
IchRR<-filter(IchRR, RI== "2")

IchRR <- IchRR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(IchRR$stage_avg, na.rm=T)
IchRR$disturb_count<-IchRR$consec-21

(ICH_NEP<-ggplot(IchRR, aes(x=disturb_count))+
    geom_line(aes(y=NEP, color=stage_avg))+
    scale_color_gradient(low="red", high="blue", name= "Disturbance Count")+
    ylab(flux) + 
    xlab("Stage (m)")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_NEPdots<-ggplot(IchRR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=IchRR, se = FALSE, method='lm', color="gray")+
    scale_fill_gradient(low="red", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(ICH_stage<-ggplot()+
    geom_line(data=IchRR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('Ichetucknee')+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

ICH3<-plot_grid(ICH_stage, ICH_NEP,ICH_NEPdots)
####

