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


setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Gilchrist Blue")
GB <- read_excel("Z:/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
GB$days <- as.Date(GB$Date)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-17" & Date<"2022-11-21"~ 2))
GBFR<-filter(GBFR, RI== "2")

GBFR <- GBFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(GBFR$stage_avg, na.rm=T)
GBFR$disturb_count<-GBFR$consec-29


ggplot()+
  geom_line(data=GBFR, aes(x=disturb_count, y=stage_avg), size=1.5)+
  ylab("Stage (m)")+
  xlab('Disturbance Count')+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_text(size = 10),
        plot.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))


(Otter_CO2<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=CO2avg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name= "Disturbance Count"))+
  ylab(expression('CO'[2]~'ppm')) + 
  xlab("Stage (m)")+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_text(size = 10),
        plot.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))

(Otter_DO<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=DOavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name= "Disturbance Count"))+
  ylab(expression('O'[2]~'ppm')) + 
  xlab("Stage (m)")+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_text(size = 10),
        plot.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))


(Otter_GPP<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=GPPavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name='Disturbance Count')+
    ylab(expression(paste('GPP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(Otter_ER<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=ER, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name='Disturbance Count')+
    ylab(expression(paste('ER'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

(Otter_NEP<-ggplot(GBFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name='Disturbance Count')+
    ylab(expression(paste('NEP'~'(g'~O[2]/m^2/'day)')))+
    xlab('Stage (m)')+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

(Otter_NEP<-ggplot(AMFR, aes(ER))+
    geom_point(aes(y=GPPavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="blue", name='Disturbance Count')+
    ylab(expression(paste('GPP'~'(g'~O[2]/m^2/'day)')))+
    xlab(expression(paste('ER'~'(g'~O[2]/m^2/'day)')))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

GBFR$GPP_RR<- GBFR$GPPavg/max(AM$GPPavg, na.rm=T)
GBFR$GPP_reduc<- (1-(GBFR$GPPavg/max(AM$GPPavg, na.rm=T)))*100

GBFR$ER_RR<- GBFR$ER/max(AM$ER, na.rm=T)
GBFR$ER_reduc<- 1-(GBFR$ER/mean(AM$ER, na.rm=T))*100

GBFR$DO_RR<- GBFR$DOavg/max(AM$DOavg, na.rm=T)
GBFR$DO_reduc<- (1-(GBFR$DOavg/max(AM$DOavg, na.rm=T)))*100

GBFR$CO2_RR<- GBFR$CO2avg/max(AM$CO2avg, na.rm=T)
GBFR$CO2_reduc<- 1-(GBFR$CO2avg/max(AM$CO2avg, na.rm=T))*100

