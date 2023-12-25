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
library(ggpmisc)

setwd("Z:/SpringsProject_Sam&Paul/Master")
GBx <- read_excel("LittleFanning.xlsx", 
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
GBx<-GBx[,x]

GBx<- GBx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


GB <- read_excel("GD_k600.xlsx", sheet = "GB", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric"))

GB<- GB %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))

GB<-left_join(GB, GBx, by=c('day','Month','year'))

length<-1000
width <-2.25552
(GB$Q<-width*GB$depth*GB$u)
k<-expression(paste('k600'~day[-1]))



GB$munch_gmd<-(((0.0752*(GB$u*100))+0.1849)*24)/GB$Mouth_DO_sat/GB$depth

(GB$owen_f<-(50.8*((GB$u*100)^0.67)*(GB$depth^-0.85))/(GB$depth*100))*24
(GB$owen_1h<-GB$owen_f/(GB$depth*100))
(GB$owen_1d<-GB$owen_1h*24)
(GB$owen_md<-GB$owen_1d*GB$depth)

GB <- GB[complete.cases(GB$Date.x), ]
ggplot()+
  geom_point(data=GB, aes(x=uh, y=K600_avg),color='blue')+
  geom_point(data=GB, aes(x=uh, y=munch_gmd),color='green')+
  geom_point(data=GB, aes(x=uh, y=owen_1d),color='black')+
  ylab(k)+
  xlab('u/h')+
  ggtitle('GB')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18, color = "black"),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

GB <- GB[complete.cases(GB$Date.x), ]
ggplot(GB, aes(x=depth, y=u)) + 
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
  ggtitle("GB")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
