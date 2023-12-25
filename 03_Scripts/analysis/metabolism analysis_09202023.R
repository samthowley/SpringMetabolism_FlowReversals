rm(list=ls())
##packages######
library(ggpubr)
library(readxl)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(measurements)
library(StreamMetabolism)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
#constants######

NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste((g~O[2]/m^2/'day')))
GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
q<-c("NEP"='blue', "GPP"='darkgreen', "ER"='darkred')
normNEPflux<-expression(paste('Normal NEP'~'(g'~O[2]/m^2/'day)'))
col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
slopey<-expression(paste('g'~O[2]/m^3/'day'))

#get data####
OS<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
OS<-filter(OS, Date> '2022-07-20' & Date <='2023-09-07')
OS$stage_diff<-OS$stage-min(OS$stage, na.rm=T)
OS$u<-(OS$stage*-0.0868+0.1579)
OS$day <- as.Date(OS$Date)
OS <- aggregate(OS, by=list(OS$day), FUN='mean')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
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
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')
LF$stage_diff<-LF$stage-min(LF$stage, na.rm=T)
LF$u<- (-0.115*LF$stage + 0.169)
LF$day <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$day), FUN='mean')





AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-9-20')
AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)
AM$"u"<-(AM$stage*-0.24+0.46)
AM$day <- as.Date(AM$Date)
AM <- aggregate(AM, by=list(AM$day), FUN='mean')



ID <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
ID<-filter(ID, Date> '2022-07-20' & Date <='2023-08-24')
ID$stage_diff<-ID$stage-min(ID$stage, na.rm=T)
ID$u<-(ID$stage*-0.128+0.416)
ID$day <- as.Date(ID$Date)
ID <- aggregate(ID, by=list(ID$day), FUN='mean')



IU <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
IU$day <- as.Date(IU$Date)
IU<- aggregate(IU, by=list(IU$day), FUN='mean')

IU$depth<-conv_unit(IU$depth, "ft", "m")
IU$depth<-IU$depth-(max(IU$depth, na.rm=T)-1.5)
IU$stage_diff<-IU$depth-min(IU$depth, na.rm=T)
IU$u<-IU$discharge/(IU$depth*15)/10

mean(IU$DO.obs, na.rm=T)
###Scatter plots#######

col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')

(ID_sc<-ggplot(data=ID, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_colour_manual(name="", values = col)+
    
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=ID, se = FALSE, method='lm')+
    
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+
    
    xlab(h)+ggtitle("ID")+
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(ER~stage_diff, data = ID)
lm(GPPavg~stage_diff, data = ID)
lm(NEP~stage_diff, data = ID)


(IU_sc<-ggplot(data=IU, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=IU, se = FALSE, method='lm')+
    xlab(h)+ggtitle("IU")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+
    
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(NEP~stage_diff, data = IU)
lm(ER~stage_diff, data = IU)
lm(GPPavg~stage_diff, data = IU)


  (AM_sc<-ggplot(data=AM, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=AM, se = FALSE, method='lm')+
    xlab(h)+ggtitle("AM")+
      scale_x_continuous(n.breaks=4) +
      scale_y_continuous(n.breaks=3)+
      
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(ER~stage_diff, data = AM)
lm(NEP~stage_diff, data = AM)
lm(GPPavg~stage_diff, data = AM)


(LF_sc<-ggplot(data=LF, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=LF, se = FALSE, method='lm')+
    xlab(h)+ggtitle("LF")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+
    
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(ER~stage_diff, data = LF)
lm(NEP~stage_diff, data = LF)
lm(GPPavg~stage_diff, data = LF)



(GB_sc<-ggplot(data=GB, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    xlab(h)+ggtitle("GB")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+
    
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(NEP~stage_diff, 
   data = GB)
lm(GPPavg~stage_diff, data = GB)
lm(ER~stage_diff, data = GB)



(OS_sc<-ggplot(data=OS, aes(x=stage_diff)) + 
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=stage_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=ER*-1), color='darkred', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=stage_diff, y=NEP), color='blue', size=0.75,
                data=OS, se = FALSE, method='lm')+
    xlab(h)+ggtitle("OS")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+
    
    theme(axis.text.x = element_text(size = 24, angle=0),
          axis.text.y = element_text(size = 24, angle=0),
          axis.title.y =element_text(size = 24, color = "black"),
          axis.title.x =element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
lm(NEP~stage_diff, data = OS)
lm(GPPavg~stage_diff, data = OS)
lm(ER~stage_diff, data = OS)

mean(AM$GPPavg, na.rm=T)
plot_grid(IU_sc, GB_sc, AM_sc, nrow = 1)
#######slope######
(OSNEP<-lm(NEP ~ stage_diff, data = OS))
cf <- coef(OSNEP)
(SlopeOSNEP <- cf[2])

(OSGPPavg<-lm(GPPavg ~ stage_diff, data = OS))
cf <- coef(OSGPPavg)
(SlopeOSGPPavg <- cf[2])

(OSER<-lm(ER*-1~ stage_diff, data = OS))
cf <- coef(OSER)
(SlopeOSER <- cf[2])

NEP<-as.numeric(c(SlopeOSNEP))
GPP<-as.numeric(c(SlopeOSGPPavg))
ER<-as.numeric(c(SlopeOSER))

OS<- data.frame(NEP,GPP,ER)
OS$site<-"OS"
##
(GBNEP<-lm(NEP ~ stage_diff, data = GB))
cf <- coef(GBNEP)
(SlopeGBNEP <- cf[2])

(GBGPPavg<-lm(GPPavg ~ stage_diff, data = GB))
cf <- coef(GBGPPavg)
(SlopeGBGPPavg <- cf[2])

(GBER<-lm(ER*-1 ~ stage_diff, data = GB))
cf <- coef(GBER)
(SlopeGBER <- cf[2])

NEP<-as.numeric(c(SlopeGBNEP))
GPP<-as.numeric(c(SlopeGBGPPavg))
ER<-as.numeric(c(SlopeGBER))

GB<- data.frame(NEP,GPP,ER)
GB$site<-"GB"
####
(LFNEP<-lm(NEP ~ stage_diff, data = LF))
cf <- coef(LFNEP)
(SlopeLFNEP <- cf[2])

(LFGPPavg<-lm(GPPavg ~ stage_diff, data = LF))
cf <- coef(LFGPPavg)
(SlopeLFGPPavg <- cf[2])

(LFER<-lm(ER*-1 ~ stage_diff, data = LF))
cf <- coef(LFER)
(SlopeLFER <- cf[2])

NEP<-as.numeric(c(SlopeLFNEP))
GPP<-as.numeric(c(SlopeLFGPPavg))
ER<-as.numeric(c(SlopeLFER))

LF<- data.frame(NEP,GPP,ER)
LF$site<-"LF"
###
(AMNEP<-lm(NEP ~ stage_diff, data = AM))
cf <- coef(AMNEP)
(SlopeAMNEP <- cf[2])

(AMGPPavg<-lm(GPPavg ~ stage_diff, data = AM))
cf <- coef(AMGPPavg)
(SlopeAMGPPavg <- cf[2])

(AMER<-lm(ER*-1 ~ stage_diff, data = AM))
cf <- coef(AMER)
(SlopeAMER <- cf[2])

NEP<-as.numeric(c(SlopeAMNEP))
GPP<-as.numeric(c(SlopeAMGPPavg))
ER<-as.numeric(c(SlopeAMER))

AM<- data.frame(NEP,GPP,ER)
AM$site<-"AM"
###
(IUNEP<-lm(NEP ~ stage_diff, data = IU))
cf <- coef(IUNEP)
(SlopeIUNEP <- cf[2])

(IUGPPavg<-lm(GPPavg ~ stage_diff, data = IU))
cf <- coef(IUGPPavg)
(SlopeIUGPPavg <- cf[2])

(IUER<-lm(ER*-1 ~ stage_diff, data = IU))
cf <- coef(IUER)
(SlopeIUER <- cf[2])

NEP<-as.numeric(c(SlopeIUNEP))
GPP<-as.numeric(c(SlopeIUGPPavg))
ER<-as.numeric(c(SlopeIUER))

IU<- data.frame(NEP,GPP,ER)
IU$site<-"IU"
###
(IDNEP<-lm(NEP ~ stage_diff, data = ID))
cf <- coef(IDNEP)
(SlopeIDNEP <- cf[2])

(IDGPPavg<-lm(GPPavg ~ stage_diff, data = ID))
cf <- coef(IDGPPavg)
(SlopeIDGPPavg <- cf[2])

(IDER<-lm(ER*-1~ stage_diff, data = ID))
cf <- coef(IDER)
(SlopeIDER <- cf[2])

NEP<-as.numeric(c(SlopeIDNEP))
GPP<-as.numeric(c(SlopeIDGPPavg))
ER<-as.numeric(c(SlopeIDER))

ID<- data.frame(NEP,GPP,ER)
ID$site<-"ID"

R_R<-rbind(ID, LF, AM, OS, GB,IU)
names(R_R)
GPP_R<-R_R[,c(2,4)]
GPP_R<-rename(GPP_R, "met"="GPP")
GPP_R$what<-"GPP"

ER_R<-R_R[,c(3,4)]
ER_R<-rename(ER_R, "met"="ER")
ER_R$what<-"ER"

NEP_R<-R_R[,c(1,4)]
NEP_R<-rename(NEP_R, "met"="NEP")
NEP_R$what<-"NEP"

R_R2<-rbind(GPP_R, ER_R, NEP_R)

q<-c("ER"='darkred', "GPP"='darkgreen', "NEP"='blue')

slope<-ggplot(R_R2,aes(x=what,y=met))+
  geom_boxplot(outlier.color="black", fill=q)+
  ggtitle("Slope Among Sites for GPP, ER and NEP")+
  ylab(slopey)+xlab("")+
  theme(axis.text.x = element_text(size = 24, angle=0),
        axis.text.y = element_text(size = 24, angle=0),
        axis.title.y =element_text(size = 24, color = "black"),
        axis.title.x =element_text(size = 24),
        plot.title = element_text(size = 24),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))











#get data####
OS<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                col_types = c("date", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric"))
OS<-filter(OS, Date> '2022-07-20' & Date <='2023-09-07')
OS$stage_diff<-OS$stage-min(OS$stage, na.rm=T)
OS$u<-(OS$stage*-0.0868+0.1579)
OS$day <- as.Date(OS$Date)
OS <- aggregate(OS, by=list(OS$day), FUN='mean')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
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
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')
LF$stage_diff<-LF$stage-min(LF$stage, na.rm=T)
LF$u<- (-0.115*LF$stage + 0.169)
LF$day <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$day), FUN='mean')





AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-9-20')
AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)
AM$"u"<-(AM$stage*-0.24+0.46)
AM$day <- as.Date(AM$Date)
AM <- aggregate(AM, by=list(AM$day), FUN='mean')



ID <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"))
ID<-filter(ID, Date> '2022-07-20' & Date <='2023-08-24')
ID$stage_diff<-ID$stage-min(ID$stage, na.rm=T)
ID$u<-(ID$stage*-0.128+0.416)
ID$day <- as.Date(ID$Date)
ID <- aggregate(ID, by=list(ID$day), FUN='mean')



IU <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
IU$day <- as.Date(IU$Date)
IU<- aggregate(IU, by=list(IU$day), FUN='mean')

IU$depth<-conv_unit(IU$depth, "ft", "m")
IU$depth<-IU$depth-(max(IU$depth, na.rm=T)-1.5)
IU$stage_diff<-IU$depth-min(IU$depth, na.rm=T)
IU$u<-IU$discharge/(IU$depth*15)/10


#####box plots#########

x<-c("stage_diff", "u", "GPPavg", "ER", "NEP", "site")

ID$site<-"ID"
OS$site<-"OS"
AM$site<-"AM"
LF$site<-"LF"
GB$site<-"GB"
IU$site<-"IU"

ID<-ID[,x]
IU<-IU[,x]
OS<-OS[,x]
AM<-AM[,x]
LF<-LF[,x]
GB<-GB[,x]

springs<-rbind(LF, AM, GB, ID, OS, IU)
springs$site <- factor(springs$site , levels=c("IU", "ID", "GB", "LF", "OS", "AM"))

ER<-ggplot(springs, aes(x=site, y=ER)) + 
  geom_boxplot(outlier.colour="black", outlier.size=1,fill='darkred')+
  ylab(flux)+
  ggtitle(ERflux)+
  ylim(0,-40)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point", 
               size=1, show.legend=FALSE) + 
  theme(axis.text.x = element_text(size = 24, angle=0),
        axis.text.y = element_text(size = 24, angle=0),
        axis.title.y =element_blank(),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 24),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

GPP<-ggplot(springs, aes(x=site, y=GPPavg)) + 
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="darkgreen")+
  ggtitle(GPPflux)+
  stat_summary(fun=mean, colour="white", geom="point", 
               size=1, show.legend=FALSE) + 
  scale_y_continuous(n.breaks=3)+
  theme(axis.text.x = element_text(size = 24, angle=0),
        axis.text.y = element_text(size = 24, angle=0),
        axis.title.y =element_blank(),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 24),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

NEP<-ggplot(springs, aes(x=site, y=NEP)) + 
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="blue")+
  ylab(flux)+
  ggtitle(NEPflux)+
  ylim(20,-20)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point", 
               size=1, show.legend=FALSE) + 
  theme(axis.text.x = element_text(size = 24, angle=0),
        axis.text.y = element_text(size = 24, angle=0),
        axis.title.y =element_blank(),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 24),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

(box<-plot_grid(GPP, NEP, ER, ncol=1))

####together#####
(scatter<-plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,nrow=2))
(boxplots<-plot_grid(box,slope,  ncol=2))
together<-plot_grid(scatter, boxplots, nrow=2, rel_heights = c(3/5,1.7/5))

ggsave(filename="metabolism.jpeg", 
       plot = together, 
       width =12, 
       height = 14.5, 
       units = "in")


ggsave(filename="poster metabolism.jpeg", 
       plot = together, 
       width =17, 
       height = 15.5, 
       units = "in")
