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
#############
######

O2y<-(expression(paste(O[2]~('mmol'/m^2/'day'))))
  
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric"))

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


OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2022-07-29" & Date<"2022-11-18"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-53


(OTTER_NEP<-ggplot(OtBO, aes(x=disturb_count))+
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

(OTTER_NEPdots<-ggplot(OtBO, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=OtBO, se = FALSE, method='lm', color="gray")+
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

(OTTER_stage<-ggplot()+
    geom_line(data=OtBO, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('OTTERetucknee')+
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

OTTER1<-plot_grid(OTTER_stage, OTTER_NEP,OTTER_NEPdots)


OtFR<- Otter %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-5-20"~ 2))
OtFR<-filter(OtFR, RI== "2")

OtFR <- OtFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtFR$stage_avg, na.rm=T)
OtFR$disturb_count<-OtFR$consec-58

Otbase<-filter(OtFR, disturb_count< -24)
Otbase<-mean(OtFR$NEP, na.rm=T)
OtFR$NEP_DR<-OtFR$NEP/Otbase

(OTTER_NEP<-ggplot(OtFR, aes(x=disturb_count))+
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

(OTTER_NEPdots<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=OtFR, se = FALSE, method='lm', color="gray")+
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

(OTTER_stage<-ggplot()+
    geom_line(data=OtFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('OTTERetucknee')+
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

OTTER2<-plot_grid(OTTER_stage, OTTER_NEP,OTTER_NEPdots)



OtBO<- Otter %>% mutate(RI = case_when(
  Date> "2023-05-17" & Date<"2023-08-21"~ 2))
OtBO<-filter(OtBO, RI== "2")

OtBO <- OtBO %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtBO$stage_avg, na.rm=T)
OtBO$disturb_count<-OtBO$consec-52


(OTTER_NEP<-ggplot(OtBO, aes(x=disturb_count))+
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

(OTTER_NEPdots<-ggplot(OtBO, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=3, shape=21)+
    geom_smooth(aes(x=stage_avg, y=NEP), 
                size=0.75, data=OtBO, se = FALSE, method='lm', color="gray")+
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

(OTTER_stage<-ggplot()+
    geom_line(data=OtBO, aes(x=disturb_count, y=stage_avg), size=1.5)+
    ylab("Stage (m)")+
    xlab('Disturbance Count')+
    ggtitle('OTTERetucknee')+
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

OTTER3<-plot_grid(OTTER_stage, OTTER_NEP,OTTER_NEPdots)















