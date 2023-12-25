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

flux<-expression(paste('NEP'~~'g'~O[2]/m^2/'day'))


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

AllenMill$mouthTemp_K<-AllenMill$Mouth_Temp_C+273.15
AllenMill$exp<-2400*((1/AllenMill$mouthTemp_K)-(1/298.15))
AllenMill$KH<-0.034*2.178^(AllenMill$exp)#mol/L/atm

AllenMill$CO2_atm<-AllenMill$CO2/1000000
AllenMill$CO2_mol<-AllenMill$CO2_atm*AllenMill$KH
AllenMill$DO_mol<-AllenMill$DO/32000

AllenMill$SchmidtO2hi<-1568-86.04*AllenMill$Mouth_Temp_C+2.142*AllenMill$Mouth_Temp_C^2-0.0216*AllenMill$Mouth_Temp_C^3
AllenMill$SchmidtCO2hi<-1742-91.24*AllenMill$Mouth_Temp_C+2.208*AllenMill$Mouth_Temp_C^2-0.0219*AllenMill$Mouth_Temp_C^3

AllenMill$x<-(600/AllenMill$SchmidtCO2hi)^(-2/3)
AllenMill$K600_md<-AllenMill$K600_avg*AllenMill$stage_avg

AllenMill$KCO2_md<-AllenMill$K600_md/AllenMill$x
AllenMill$KO2_md<-AllenMill$KCO2_md*(AllenMill$SchmidtCO2hi/AllenMill$SchmidtO2hi)^(-2/3)
AllenMill$KCO2_1d<-AllenMill$KCO2_md/AllenMill$stage_avg
AllenMill$KO2_1d<-AllenMill$KO2_md/AllenMill$stage_avg

AllenMill$Do_Sat<-Cs(AllenMill$Mouth_Temp_C)
AllenMill$DO_Sat_mol<-AllenMill$Do_Sat/32000
(AllenMill$CO2_Sat_mol<-(420/1000000)*AllenMill$KH)

AllenMill$'O2_mol_m2_1d'<-AllenMill$stage_avg*AllenMill$KO2_1d*(AllenMill$DO_mol-AllenMill$DO_Sat_mol)
AllenMill$'CO2_mol_m2_1d'<-AllenMill$KCO2_1d*AllenMill$stage_avg*(AllenMill$CO2_mol-AllenMill$CO2_Sat_mol)

AllenMill$'O2_mmol_m2_1d'<-AllenMill$'O2_mol_m2_1d'*10^3
AllenMill$'CO2_mmol_m2_1d'<-AllenMill$'CO2_mol_m2_1d'*10^3


AM<- AllenMill %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

DO<-AM %>% group_by(day,month,year) %>% summarize(O2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
AM<-left_join(AM,DO, by=c("day","month","year"))

C<-AM %>% group_by(day,month,year) %>% summarize(metCO2avg = mean(O2_mmol_m2_1d, na.rm = TRUE))
AM<-left_join(AM,C, by=c("day","month","year"))


AM$days <- as.Date(AM$Date)

AM<- AM %>% mutate(RI = case_when(
  stage_avg<0.93 ~ "low",
  stage_avg<1.37 ~ "moderate",
  stage_avg>=1.37 ~ "high"))
AM_low<-filter(AM, RI== 'low')



AMBO<- AM %>% mutate(RI = case_when(
  Date> "2022-07-25" & Date<"2022-12-01"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-54


(AM_NEP<-ggplot(AMBO, aes(x=disturb_count))+
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

(AM_NEPdots<-ggplot(AMBO, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=AMBO, se = FALSE, method='lm', color="gray")+
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

(AM_stage<-ggplot()+
    geom_line(data=AMBO, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('AMetucknee')+
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

AM1<-plot_grid(AM_stage, AM_NEP,AM_NEPdots)



#######################################
AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-01-11" & Date<"2023-05-20"~ 2))
AMFR<-filter(AMFR, RI== "2")

AMFR <- AMFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMFR$stage_avg, na.rm=T)
which(AMFR$stage_avg == max(AMFR$stage_avg, na.rm=T)) 
AMFR$disturb_count<-AMFR$consec-42



(AM_NEP<-ggplot(AMFR, aes(x=disturb_count))+
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

(AM_NEPdots<-ggplot(AMFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=AMFR, se = FALSE, method='lm', color="gray")+
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

(AM_stage<-ggplot()+
    geom_line(data=AMFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('AMetucknee')+
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

AM2<--plot_grid(AM_stage, AM_NEP,AM_NEPdots)


#############get new data for this###############


AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-08-01"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(AMBO$stage_avg, na.rm=T)
AMBO$disturb_count<-AMBO$consec-12


(AM_NEP<-ggplot(AMBO, aes(x=disturb_count))+
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

(AM_NEPdots<-ggplot(AMBO, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=AMBO, se = FALSE, method='lm', color="gray")+
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

(AM_stage<-ggplot()+
    geom_line(data=AMBO, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('AMetucknee')+
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

AM3<-plot_grid(AM_stage, AM_NEP,AM_NEPdots)



