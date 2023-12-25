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
AllenMill <- read_excel("AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
AllenMill<-filter(AllenMill, Date>='2022-07-20')

x<-c('Date','Mouth_DO_sat')
AllenMill<-AllenMill[,x]

AllenMill<- AllenMill %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


AM <- read_excel("GD_k600.xlsx", sheet = "AM", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric"))

AM<- AM %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))

AM<-left_join(AM, AllenMill, by=c('day','Month','year'))

length<-1000
width <-2.25552
slope<-0.0027
AM$Froude<-AM$u/((AM$depth*9.81)^0.5)
(AM$Q<-width*AM$depth*AM$u)
k<-expression(paste('k600'~day[-1]))


(AM$raymond7_1d<- (4725*(AM$u*slope)^0.86)*(AM$Q^-0.14)*(AM$depth^0.66))/AM$depth

(AM$raymond6_md<- (929*(AM$u*slope)^0.75)*(AM$Q^0.011))/AM$depth

(AM$raymond2_1d<- (5937*(1-2.54*AM$Froude^2)*(AM$u*slope)^0.89)*(AM$depth^0.58))

(AM$raymond1_1d<- ((AM$u*slope)^0.89)*(AM$depth^0.58)*5037)

AM$munch_gmd<-(((0.0752*(AM$u*100))+0.1849)*24)/AM$Mouth_DO_sat/AM$depth

(AM$owen_f<-(50.8*((AM$u*100)^0.67)*(AM$depth^-0.85))/(AM$depth*100))*24
(AM$owen_1h<-AM$owen_f/(AM$depth*100))
(AM$owen_1d<-AM$owen_1h*24)
(AM$owen_md<-AM$owen_1d*AM$depth)


AM <- AM[complete.cases(AM$Date.x), ]
ggplot()+
  geom_point(data=AM, aes(x=uh, y=K600_avg),color='blue')+
  geom_point(data=AM, aes(x=uh, y=munch_gmd),color='green')+
  geom_point(data=AM, aes(x=uh, y=owen_1d),color='black')+
  geom_point(data=AM, aes(x=uh, y=raymond1_1d),color='orange')+
  ylab(k)+
  xlab('u/h')+
  ggtitle('AM')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18, color = "black"),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

AM <- AM[complete.cases(AM$Date.x), ]
ggplot(AM, aes(x=depth, y=u)) + 
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
  ggtitle("AM")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
