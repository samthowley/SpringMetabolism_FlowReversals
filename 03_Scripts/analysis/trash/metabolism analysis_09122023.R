rm(list=ls())
##packages######
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
library(zoo)
library(StreamMetabolism)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(rnoaa)
library(dataRetrieval)
library(ggpmisc)
#constants######

NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste('g'~O[2]/m^2/'day'))

GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
colors<-c("NEP"='blue', "GPP"='darkgreen', "ER"='darkred')
normNEPflux<-expression(paste('Normal NEP'~'(g'~O[2]/m^2/'day)'))
col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[actual]-h[minimum]))
u<-expression(paste('Velocity'~("m"~s^-1)))


#get data####
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-09-07')
Otter$stage_diff<-Otter$stage-min(Otter$stage, na.rm=T)
Otter$u<-(Otter$stage*-0.0868+0.1579)
Otter$day <- as.Date(Otter$Date)
Otter <- aggregate(Otter, by=list(Otter$day), FUN='mean')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-08-29')
GB$stage_diff<-GB$stage-min(GB$stage, na.rm=T)
GB$u<-(GB$stage*-0.483+0.35)
GB <- GB[!duplicated(GB[c('Date')]),]
GB$day <- as.Date(GB$Date)
GB <- aggregate(GB, by=list(GB$day), FUN='mean')


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')
LF$stage_diff<-LF$stage_avg-min(LF$stage_avg, na.rm=T)
LF$u<- (-0.115*LF$stage_avg + 0.169)
LF$day <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$day), FUN='mean')





AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-9-20')
AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)
AM$"u"<-(AM$stage*-0.255+0.477)
AM$day <- as.Date(AM$Date)
AM <- aggregate(AM, by=list(AM$day), FUN='mean')



Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
Ich<-filter(Ich, Date> '2022-07-20' & Date <='2023-08-21')
Ich$stage_diff<-Ich$stage_avg-min(Ich$stage_avg, na.rm=T)
Ich$u<-(Ich$stage_avg*-0.128+0.416)
Ich$day <- as.Date(Ich$Date)
Ich <- aggregate(Ich, by=list(Ich$day), FUN='mean')



US27bridge <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
US27bridge$day <- as.Date(US27bridge$Date)
US27bridge<- aggregate(US27bridge, by=list(US27bridge$day), FUN='mean')

US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$depth<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-1.5)
US27bridge$stage_diff<-US27bridge$depth-min(US27bridge$depth, na.rm=T)
US27bridge$u<-US27bridge$discharge/(US27bridge$depth*15)


###Scatter plots#######

col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')

(Ich_sc<-ggplot(data=Ich, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_colour_manual(name="", values = col)+
    
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=Ich, se = FALSE, method='lm')+

    xlab(u)+ggtitle("ID")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


(US27bridge_sc<-ggplot(data=US27bridge, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    xlab(u)+ggtitle("IU")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


  (AM_sc<-ggplot(data=AM, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=AM, se = FALSE, method='lm')+
    xlab(u)+ggtitle("AM")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = AM))


(LF_sc<-ggplot(data=LF, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=LF, se = FALSE, method='lm')+
    xlab(u)+ggtitle("LF")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = LF))



(GB_sc<-ggplot(data=GB, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    xlab(u)+ggtitle("GB")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = GB))



(Otter_sc<-ggplot(data=Otter, aes(x=u)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=u, y=GPPavg), color='darkgreen', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=ER*-1), color='darkred', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    geom_smooth(aes(x=u, y=NEP), color='blue', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    xlab(u)+ggtitle("OS")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = Otter))


plot_grid(US27bridge_sc, Ich_sc, LF_sc, GB_sc, Otter_sc, AM_sc)

OSNEP<-lm(NEP ~ u, data = Otter)
cf <- coef(OSNEP)
(SlopeOSNEP <- cf[2])

OSGPPavg<-lm(GPPavg ~ u, data = Otter)
cf <- coef(OSGPPavg)
(SlopeOSGPPavg <- cf[2])

OSER<-lm(ER ~ u, data = Otter)
cf <- coef(OSER)
(SlopeOSER <- cf[2])

NEP<-as.numeric(c(SlopeOSNEP))
GPP<-as.numeric(c(SlopeOSGPPavg))
ER<-as.numeric(c(SlopeOSER))

OS<- data.frame(NEP,GPP,ER)
OS$site<-"OS"
##
GBNEP<-lm(NEP ~ u, data = GB)
cf <- coef(GBNEP)
(SlopeGBNEP <- cf[2])

GBGPPavg<-lm(GPPavg ~ u, data = GB)
cf <- coef(GBGPPavg)
(SlopeGBGPPavg <- cf[2])

GBER<-lm(ER ~ u, data = GB)
cf <- coef(GBER)
(SlopeGBER <- cf[2])

NEP<-as.numeric(c(SlopeGBNEP))
GPP<-as.numeric(c(SlopeGBGPPavg))
ER<-as.numeric(c(SlopeGBER))

GB<- data.frame(NEP,GPP,ER)
GB$site<-"GB"
####
LFNEP<-lm(NEP ~ u, data = LF)
cf <- coef(LFNEP)
(SlopeLFNEP <- cf[2])

LFGPPavg<-lm(GPPavg ~ u, data = LF)
cf <- coef(LFGPPavg)
(SlopeLFGPPavg <- cf[2])

LFER<-lm(ER ~ u, data = LF)
cf <- coef(LFER)
(SlopeLFER <- cf[2])

NEP<-as.numeric(c(SlopeLFNEP))
GPP<-as.numeric(c(SlopeLFGPPavg))
ER<-as.numeric(c(SlopeLFER))

LF<- data.frame(NEP,GPP,ER)
LF$site<-"LF"
###
AMNEP<-lm(NEP ~ u, data = AM)
cf <- coef(AMNEP)
(SlopeAMNEP <- cf[2])

AMGPPavg<-lm(GPPavg ~ u, data = AM)
cf <- coef(AMGPPavg)
(SlopeAMGPPavg <- cf[2])

AMER<-lm(ER ~ u, data = AM)
cf <- coef(AMER)
(SlopeAMER <- cf[2])

NEP<-as.numeric(c(SlopeAMNEP))
GPP<-as.numeric(c(SlopeAMGPPavg))
ER<-as.numeric(c(SlopeAMER))

AM<- data.frame(NEP,GPP,ER)
AM$site<-"AM"
###
US27bridgeNEP<-lm(NEP ~ u, data = US27bridge)
cf <- coef(US27bridgeNEP)
(SlopeUS27bridgeNEP <- cf[2])

US27bridgeGPPavg<-lm(GPPavg ~ u, data = US27bridge)
cf <- coef(US27bridgeGPPavg)
(SlopeUS27bridgeGPPavg <- cf[2])

US27bridgeER<-lm(ER ~ u, data = US27bridge)
cf <- coef(US27bridgeER)
(SlopeUS27bridgeER <- cf[2])

NEP<-as.numeric(c(SlopeUS27bridgeNEP))
GPP<-as.numeric(c(SlopeUS27bridgeGPPavg))
ER<-as.numeric(c(SlopeUS27bridgeER))

US27bridge<- data.frame(NEP,GPP,ER)
US27bridge$site<-"US27bridge"
###
IchNEP<-lm(NEP ~ u, data = Otter)
cf <- coef(IchNEP)
(SlopeIchNEP <- cf[2])

IchGPPavg<-lm(GPPavg ~ u, data = Otter)
cf <- coef(IchGPPavg)
(SlopeIchGPPavg <- cf[2])

IchER<-lm(ER ~ u, data = Otter)
cf <- coef(IchER)
(SlopeIchER <- cf[2])

NEP<-as.numeric(c(SlopeIchNEP))
GPP<-as.numeric(c(SlopeIchGPPavg))
ER<-as.numeric(c(SlopeIchER))

Ich<- data.frame(NEP,GPP,ER)
Ich$site<-"Ich"

R_R<-rbind(Ich, LF, AM, OS, GB,US27bridge)

######velocity########



(Ich_u<-ggplot(data=Ich, aes(x=stage_diff)) + 
   geom_point(aes(y=u), size=1, color='darkgray')+
   geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
               data=Ich, se = FALSE, method='lm')+
   ylab('velocity m/s')+
   xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
   theme(axis.text.x = element_blank(),
         axis.text.y = element_text(size = 12, angle=0),
         axis.title.y =element_text(size = 12, color = "black"),
         axis.title.x =element_blank(),
         plot.title = element_blank(),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


(US27bridge_u<-ggplot(data=US27bridge, aes(x=stage_diff)) + 
    geom_point(aes(y=u), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    ylab('velocity m/s')+
    xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(AM_u<-ggplot(data=AM, aes(x=stage_diff)) + 
    geom_point(aes(y=u), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
                data=AM, se = FALSE, method='lm')+
    ylab('velocity m/s')+
    xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(LF_u<-ggplot(data=LF, aes(x=stage_diff)) + 
    geom_point(aes(y=u), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
                data=LF, se = FALSE, method='lm')+
    ylab('velocity m/s')+
    xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(Otter_u<-ggplot(data=Otter, aes(x=stage_diff)) + 
    geom_point(aes(y=u), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    ylab('velocity m/s')+
    xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(GB_u<-ggplot(data=GB, aes(x=stage_diff)) + 
    geom_point(aes(y=u), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u), color='darkgray', size=0.75,
                data=GB, se = FALSE, method='lm')+
    ylab('velocity m/s')+
    xlab("Actual Stage - Low Stage (m)")+ggtitle("ID")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


###plot grids#####

Ich_graph<-plot_grid(Ich_u,Ich_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))
US27bridge_graph<-plot_grid(US27bridge_u,US27bridge_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))
LF_graph<-plot_grid(LF_u,LF_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))
AM_graph<-plot_grid(AM_u,AM_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))
GB_graph<-plot_grid(GB_u,GB_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))
Otter_graph<-plot_grid(Otter_u,Otter_sc, align = "v", ncol = 1, rel_heights = c(1/4,3/4))

plot_grid(Ich_graph,US27bridge_graph, LF_graph, GB_graph, Otter_graph, AM_graph, ncol=3)

###Scatter plots 2#######

col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred', u='darkgray')

(Ich_sc<-ggplot(data=Ich, aes(x=stage_diff)) +
    geom_point(aes(y=u-20), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-20), color='darkgray', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+20, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=Ich, se = FALSE, method='lm')+
    xlab(h)+ggtitle("ID")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray", angle=-90),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


(US27bridge_sc<-ggplot(data=US27bridge, aes(x=stage_diff)) +
    geom_point(aes(y=u-20), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-20), color='darkgray', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+20, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER), color='darkred', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=US27bridge, se = FALSE, method='lm')+
    xlab(h)+ggtitle("IU")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ depth, data = US27bridge))

(AM_sc<-ggplot(data=AM, aes(x=stage_diff)) +
    geom_point(aes(y=u-20), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-20), color='darkgray', size=0.75,
                data=AM, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+20, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER), color='darkred', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=AM, se = FALSE, method='lm')+
    xlab(h)+ggtitle("AM")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = AM))

(LF_sc<-ggplot(data=LF, aes(x=stage_diff)) +
    geom_point(aes(y=u-20), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-20), color='darkgray', size=0.75,
                data=LF, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+20, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER), color='darkred', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=LF, se = FALSE, method='lm')+
    xlab(h)+ggtitle("LF")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = LF))



(GB_sc<-ggplot(data=GB, aes(x=stage_diff)) +
    geom_point(aes(y=u-30), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-30), color='darkgray', size=0.75,
                data=GB, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+30, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    xlab(h)+ggtitle("GB")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


(Otter_sc<-ggplot(data=Otter, aes(x=stage_diff)) +
    geom_point(aes(y=u-20), size=1, color='darkgray')+
    geom_smooth(aes(x=stage_diff, y=u-20), color='darkgray', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    scale_y_continuous( name = flux,
                        sec.axis = sec_axis( trans=~.+20, name="velocity (cm/s)"))+
    
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER), color='darkred', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=Otter, se = FALSE, method='lm')+
    xlab(h)+ggtitle("Otter")+
    
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18, color = "black"),
          axis.title.y.right =element_text(size = 18, color = "darkgray"),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


plot_grid(US27bridge_sc, Ich_sc, LF_sc, GB_sc, Otter_sc, AM_sc)
















