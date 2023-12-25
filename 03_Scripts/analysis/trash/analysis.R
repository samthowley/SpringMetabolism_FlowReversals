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

dev.new()
samplingperiod <- read_csv("Z:/SpringsProject_Sam&Paul/samplingperiod.csv")
setwd("Z:/SpringsProject_Sam&Paul/Master")
LF <- read_excel("LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric",
                               "numeric"))
Ich <- read_excel("Ichetucknee.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
GB <- read_excel("GilchristBlue_.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))
AM <- read_excel("AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))
Otter <- read_excel("Otter.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

names(LF)
names(Ich)
LF<-LF[,c(1,2,3,5,6,9,11,17,18,19,20,21,22)]
LF$site<-'Little Fanning'

AM<-AM[,c(1,2,3,4,6,7,9,16,17,18,19,20,21)]
AM<-AM[,c(1,2,4,3,7,6,5,8,9,10,11,12,13)]
AM$site<-'Allen Mill'

GB<-GB[,c(1,3,4,5,8,9,10,16,17,18,19,20,21)]
GB<-rename(GB,'CO2'="CO2hi", 'depth'="stage")
GB<-GB[,c(1,2,7,3,4,5,6,8,9,10,11,12,13)]
GB$site<-'Gilchrist Blue'

Ich<-Ich[,c(1,3,4,5,6,8,10,16,17,18,19,20,21)]
Ich<-rename(Ich,'CO2'="CO2hi", 'depth'="stage","FullRangeSpC"="LowRangeSpC")
Ich$site<-'Ichetucknee'

Otter<-Otter[,c(1,2,3,4,5,7,10,16,17,18,19,20,21)]
Otter<-rename(Otter,'depth'="stage",'K600'="K600_1d")
Otter$site<-'Otter'

springs<-rbind(LF, AM, GB, Ich, Otter)
names(springs)
springs<-rename(springs, 'u'="velocity_m/h", "Q"="Q_m/h")

ggplot(springs, aes(x=site, y=CO2)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("CO2")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=DO)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("DO")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=pH)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("pH")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=FullRangeSpC)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("FullRangeSpC")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=K600)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("K600")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=u)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("velocity m/h")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=Q)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("Q m/h")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=ER)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("ER")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=GPPavg)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("GPP")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(springs, aes(x=site, y=NEP)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("NEP")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

names(springs)
