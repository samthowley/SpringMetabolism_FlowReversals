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
Otterx<- read_excel("Otter.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric"))

x<-c('Date','Mouth_DO_sat')
Otterx<-Otterx[,x]

Otterx<- Otterx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


Otter <- read_excel("GD_k600.xlsx", sheet = "Otter", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric"))

Otter<- Otter %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))

Otter<-left_join(Otter, Otterx, by=c('day','Month','year'))

length<-1000
width <-2.25552
(Otter$Q<-width*Otter$depth*Otter$u)

Otter$munch_gmd<-(((0.0752*(Otter$u*100))+0.1849)*24)/Otter$Mouth_DO_sat/Otter$depth

(Otter$owen_f<-(50.8*((Otter$u*100)^0.67)*(Otter$depth^-0.85))/(Otter$depth*100))*24
(Otter$owen_1h<-Otter$owen_f/(Otter$depth*100))
(Otter$owen_1d<-Otter$owen_1h*24)
(Otter$owen_md<-Otter$owen_1d*Otter$depth)


Otter <- Otter[complete.cases(Otter$Date.x), ]
ggplot()+
  geom_point(data=Otter, aes(x=uh, y=K600_avg),color='blue')+
  geom_point(data=Otter, aes(x=uh, y=munch_gmd),color='green')+
  geom_point(data=Otter, aes(x=uh, y=owen_1d),color='black')+
  ylab(k)+
  xlab('u/h')+
  ggtitle('Otter')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18, color = "black"),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

Otter <- Otter[complete.cases(Otter$Date.x), ]
ggplot(Otter, aes(x=depth, y=u)) + 
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
  ggtitle("Otter")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
