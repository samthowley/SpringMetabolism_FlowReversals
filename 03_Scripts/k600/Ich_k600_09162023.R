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
library(lubridate)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library('StreamMetabolism')

setwd("Z:/SpringsProject_Sam&Paul/Master")
Ichx<- read_excel("LittleFanning.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric","numeric",
                                  "numeric"))

x<-c('Date','Mouth_DO_sat')
Ichx<-Ichx[,x]

Ichx<- Ichx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


Ich <- read_excel("GD_k600.xlsx", sheet = "Sheet5", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric"))

Ichx<- Ichx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))

Ich<- Ich %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


Ich<-left_join(Ich, Ichx, by=c('day','Month','year'))

length<-1000
width <-2.25552
Ich$Froude<-Ich$u/((Ich$depth*9.81)^0.5)
(Ich$Q<-width*Ich$depth*Ich$u)

Ich$munch_gmd<-(((0.0752*(Ich$u*100))+0.1849)*24)/Ich$Mouth_DO_sat/Ich$depth

(Ich$owen_f<-(50.8*((Ich$u*100)^0.67)*(Ich$depth^-0.85))/(Ich$depth*100))*24
(Ich$owen_1h<-Ich$owen_f/(Ich$depth*100))
(Ich$owen_1d<-Ich$owen_1h*24)
(Ich$owen_md<-Ich$owen_1d*Ich$depth)


Ich <- Ich[complete.cases(Ich$Date.x), ]
ggplot()+
  geom_point(data=Ich, aes(x=uh, y=K600_avg),color='blue')+
  geom_point(data=Ich, aes(x=uh, y=munch_gmd),color='green')+
  geom_point(data=Ich, aes(x=uh, y=owen_1d),color='black')+
  ylab(k)+
  xlab('u/h')+
  ggtitle('Ich')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18, color = "black"),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

Ich <- Ich[complete.cases(Ich$Date.x), ]
ggplot(Ich, aes(x=depth, y=u)) + 
  geom_point()+
  ylab("velocity m/s")+
  xlab("stage")+
  stat_correlation(mapping = use_label(c('R')), size=5,
                   label.y = 0.9,label.x = 0.63)+
  stat_poly_line(formula = y ~ x, color='black') +
  stat_poly_eq(mapping = use_label(c("eq")),
               label.y = 1,
               label.x = 0.6,
               size=5)+
  ggtitle("Ich")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
