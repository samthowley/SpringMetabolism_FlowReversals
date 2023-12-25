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
LFx<- read_excel("LittleFanning.xlsx", 
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
LFx<-LFx[,x]

LFx<- LFx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


LF <- read_excel("GD_k600.xlsx", sheet = "Sheet5", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric"))

LFx<- LFx %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))

LF<- LF %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year=year(Date))


LF<-left_join(LF, LFx, by=c('day','Month','year'))

length<-1000
width <-2.25552
slope<-0.0013
LF$Froude<-LF$u/((LF$depth*9.81)^0.5)
(LF$Q<-width*LF$depth*LF$u)
k<-expression(paste('k600'~day[-1]))



(LF$raymond7_1d<- (4725*(LF$u*slope)^0.86)*(LF$Q^-0.14)*(LF$depth^0.66))/LF$depth

(LF$raymond6_md<- (929*(LF$u*slope)^0.75)*(LF$Q^0.011))/LF$depth

(LF$raymond2_1d<- (5937*(1-2.54*LF$Froude^2)*(LF$u*slope)^0.89)*(LF$depth^0.58))

(LF$raymond1_1d<- ((LF$u*slope)^0.89)*(LF$depth^0.58)*5037)

LF$munch_gmd<-(((0.0752*(LF$u*100))+0.1849)*24)/LF$Mouth_DO_sat/LF$depth

(LF$owen_f<-(50.8*((LF$u*100)^0.67)*(LF$depth^-0.85))/(LF$depth*100))*24
(LF$owen_1h<-LF$owen_f/(LF$depth*100))
(LF$owen_1d<-LF$owen_1h*24)
(LF$owen_md<-LF$owen_1d*LF$depth)


LF <- LF[complete.cases(LF$Date.x), ]
ggplot()+
  geom_point(data=LF, aes(x=uh, y=K600_avg),color='blue')+
  geom_point(data=LF, aes(x=uh, y=munch_gmd),color='green')+
  geom_point(data=LF, aes(x=uh, y=owen_1d),color='black')+
  geom_point(data=LF, aes(x=uh, y=raymond1_1d),color='orange')+
  ylab(k)+
  xlab('u/h')+
  ggtitle('LF')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18, color = "black"),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

LF <- LF[complete.cases(LF$Date.x), ]
ggplot(LF, aes(x=depth, y=u)) + 
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
  ggtitle("LF")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
