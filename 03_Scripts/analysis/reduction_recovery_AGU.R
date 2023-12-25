rm(list=ls())

library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(measurements)
library(ggnewscale)
library(StreamMetabolism)
library(ggpmisc)

R_R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")
R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction_ammended.xlsx")
R_R$a<-'a'
R$a<-'a'
cols<-c(
  "h_high"="deepskyblue3",
  "h_brown"="burlywood4",
  "h_rev"="black")
R$IF <- factor(R$IF  , levels=c("h_high","h_brown","h_rev"))

h<-expression("Flood Stage Above Minimum")

(b<-ggplot(R_R, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=GPP_reduction_percent), size=6)+
    geom_smooth(aes(x=stage, y=GPP_reduction_percent, group=a), color='darkgreen', size=0.75,
                data=R, se = FALSE, method='lm')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on GPP")+
    xlab(h)+ylab("GPP Reduction (%)")+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 27, color="darkgreen"),
          axis.title.y.right =element_text(size = 27, color='darkred'),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 22, color="darkgreen"),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


(b1<-ggplot(R_R, aes(stage, shape=site, color= IF))+
    geom_point(aes(y=ER_reduction_percent), size=6)+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(" ")+ylab("|ER| Increase (%)")+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 27, color="darkred"),
          axis.title.y.right =element_text(size = 27, color='darkred'),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 22,color='darkred'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

#summary(lm(GPP_reduction ~ stage, data = R))
hs<-plot_grid(b, b1, nrow=1)

ggsave(filename="reduced.jpeg", 
       plot = hs, 
       width =12, 
       height = 5.5, 
       units = "in")


recovery <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/recovery.xlsx")
title<-expression(paste(recovery[stage]/recovery[metabolic]))
u<-"Velocity m/s"

recovery$a<-'a'
cols<-c(
  "h_high"="deepskyblue3",
  "h_brown"="burlywood4",
  "h_rev"="black")
recovery$IF <- factor(recovery$IF  , levels=c("h_high","h_brown","h_rev"))

(f<-ggplot(recovery, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=ratio_GPP), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("GPP Recovery")+
    xlab(" ")+
    ylab(title)+scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 25, color='black'),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 27, color='darkgreen'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(g<-ggplot(recovery, aes(stage, shape=site, color=IF))+
    geom_point(aes(y=ratio_ER), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols, 
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("ER Recovery")+
    xlab(" ")+
    ylab(" ")+scale_y_continuous(trans='log10')+
    theme(axis.text.x = element_text(size = 27, angle=0),
          axis.text.y = element_text(size = 27, angle=0),
          axis.title.y =element_text(size = 25, color='black'),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 27, color='darkred'),
          legend.position = "none",
          legend.text= element_text(size = 27),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

h<-plot_grid(f,g, ncol=2)
ggsave(filename="recovery.jpeg", 
       plot = h, 
       width =12, 
       height = 5, 
       units = "in")
