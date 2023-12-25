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
library(zoo)
library(StreamMetabolism)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(rnoaa)
library(dataRetrieval)

NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste('g'~O[2]/m^2/'day'))

GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
colors<-c("NEP"='blue', "GPP"='darkgreen', "ER"='darkred')
normNEPflux<-expression(paste('Normal NEP'~'(g'~O[2]/m^2/'day)'))

#get data####
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-08-24')



GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-09-07')


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric","numeric", "numeric",
                               "numeric", "numeric", "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-08-21')




AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-09-07')





Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
Ich<-filter(Ich, Date> '2022-07-20' & Date <='2023-09-07')



US27 <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")



##NEP Graphs####
IchRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2022-11-06','2023-02-10','2023-05-07','2023-06-25')),
  xmax=as.POSIXct(c('2022-10-04','2023-01-24','2023-04-08','2023-06-14', '2023-09-07')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange","lightblue","darkorange",'lightblue',"darkorange"))

(h<-ggplot(Ich, aes(x=Date)) + 
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    geom_rect(data=IchRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=IchRECT$fill,alpha=0.6)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))

(NEP<-ggplot() + 
    geom_line(data=Ich, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=Ich, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=Ich, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    scale_colour_manual(values = colors)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("Ichetucknee")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=15),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(Ich_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))






US27RECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2022-11-06','2023-02-10','2023-05-07','2023-06-25')),
  xmax=as.POSIXct(c('2022-10-04','2023-01-24','2023-04-08','2023-06-14', '2023-09-07')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange","lightblue","darkorange",'lightblue',"darkorange"))

(h<-ggplot(US27, aes(x=Date)) + 
    geom_line(aes(y=depth, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    geom_rect(data=US27RECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=US27RECT$fill,alpha=0.6)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))

(NEP<-ggplot() + 
    geom_line(data=US27, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=US27, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=US27, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    scale_colour_manual(values = colors)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("US27etucknee")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=15),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(US27_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))







AMRECT = data.frame(
  xmin=as.POSIXct(c('2022-08-30','2022-11-06','2023-02-10','2023-05-07', '2023-06-20')),
  xmax=as.POSIXct(c('2022-10-01','2023-01-24','2023-04-08','2023-06-14', '2023-07-15')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkgreen","lightblue","brown",'lightblue', "darkgreen"))


(h<-ggplot(AM, aes(x=Date)) + 
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    geom_rect(data=AMRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=AMRECT$fill,alpha=0.6)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))


(NEP<-ggplot() + 
    geom_line(data=AM, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=AM, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=AM, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    
    scale_colour_manual(values = colors)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("Allen Mill Pond")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_blank(),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(AM_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))

DO_AMRECT = data.frame(
  xmin=as.POSIXct(c('2022-08-30','2023-02-10', '2023-06-20')),
  xmax=as.POSIXct(c('2022-10-01','2023-04-08', '2023-07-15')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkgreen","brown", "darkgreen"))


(AMDO<-ggplot() + 
    geom_rect(data=DO_AMRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=DO_AMRECT$fill,alpha=0.6)+
    geom_line(data=AM, aes(x=Date,y=DO),size=1) + #make transparent cloud
    scale_colour_manual(values = "black")+
    ylab("DO mg/L")+
    ggtitle("Allen Mill Pond")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(AMDO_graph<-plot_grid(hplain,DO, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))


FR<-filter(AM, Date>"2023-01-01" & Date<"2023-04-11")

FRRECT = data.frame(
  xmin=as.POSIXct(c('2023-02-12')),
  xmax=as.POSIXct(c('2023-02-27')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("brown"))

(FRh<-ggplot(FR, aes(x=Date)) + 
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 16),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))
(FRDO<-ggplot() + 
  geom_rect(data=FRRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill=FRRECT$fill,alpha=0.6)+
  ylab('DO mg/L')+
  ggtitle("Spike-and-fall: Allen Mill's Flow Reversal 02/2023")+
  geom_line(data=FR, aes(x=Date,y=DO, color="GPP"),size=1)+
  scale_color_manual(values='black')+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_blank(),
        plot.title =element_text(size=19),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none'))

(FRgraph<-plot_grid(FRh,FRDO, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))



GBRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2022-10-04','2022-07-06','2023-07-31')),
  xmax=as.POSIXct(c('2022-10-04','2023-07-30','2022-09-01','2023-08-17')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange","lightblue","lightblue", "darkorange"))


(h<-ggplot(GB, aes(x=Date)) + 
    geom_rect(data=GBRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=GBRECT$fill,alpha=0.6)+
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))

(NEP<-ggplot() + 
    geom_line(data=GB, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=GB, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=GB, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    
    scale_colour_manual(values = colors)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("Gilchrist Blue")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_blank(),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(GB_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))

DO_GBRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2023-07-31')),
  xmax=as.POSIXct(c('2022-10-04','2023-08-17')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange", "darkorange"))

(GBDO<-ggplot() + 
    geom_rect(data=DO_GBRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=DO_GBRECT$fill,alpha=0.6)+
    geom_line(data=GB, aes(x=Date,y=DO),size=1) + #make transparent cloud
    scale_colour_manual(values = "black")+
    ylab("DO mg/L")+
    ggtitle("Gilchrist Blue")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))




LFRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2022-11-06','2023-02-10','2023-05-07','2023-06-20')),
  xmax=as.POSIXct(c('2022-10-04','2023-01-27','2023-04-08','2023-06-09','2023-09-04')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange","lightblue","darkorange",'lightblue',"darkorange"))


(h<-ggplot(LF, aes(x=Date)) + 
    geom_rect(data=LFRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=LFRECT$fill,alpha=0.6)+
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))


(NEP<-ggplot() + 
    geom_line(data=LF, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=LF, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=LF, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_line(data=LF, aes(x=Date,y=DO, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    
    scale_colour_manual(values = colors)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("Little Fanning")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_blank(),
          axis.title.y =element_blank(),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(LF_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))

DO_LFRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-01','2023-02-10','2023-06-20')),
  xmax=as.POSIXct(c('2022-10-04','2023-04-08','2023-07-04')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkorange","darkorange","darkorange"))

(LFDO<-ggplot() + 
    geom_rect(data=DO_LFRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=DO_LFRECT$fill,alpha=0.6)+
    geom_line(data=LF, aes(x=Date,y=DO),size=1) + #make transparent cloud
    scale_colour_manual(values = "black")+
    ylab("DO mg/L")+
    ggtitle("Little Fanning")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))


OtRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-05','2022-11-06','2023-02-14','2023-05-07','2023-06-28')),
  xmax=as.POSIXct(c('2022-10-06','2023-01-24','2023-04-11','2023-06-14','2023-07-21')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkgreen","lightblue","darkgreen",'lightblue',"darkgreen"))


(h<-ggplot(Otter, aes(x=Date)) + 
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    geom_rect(data=OtRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=OtRECT$fill,alpha=0.6)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y =element_text(size = 12),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))

 (NEP<-ggplot() + 
    geom_line(data=Otter, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=Otter, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=Otter, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_hline(yintercept = 0)+
    
    scale_colour_manual(values = colstwo)+
    ylab(flux)+
    ylim(-37,35)+
    ggtitle("Otter")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 12, angle=0),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(Otter_graph<-plot_grid(h,NEP, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))


DO_OtRECT = data.frame(
  xmin=as.POSIXct(c('2022-09-05','2023-02-14','2023-06-28')),
  xmax=as.POSIXct(c('2022-10-06','2023-04-11','2023-07-21')),
  ymin=Inf,
  ymax=-Inf,
  fill=c("darkgreen","darkgreen","darkgreen"))

(hplain<-ggplot(Otter, aes(x=Date)) + 
    geom_line(aes(y=stage_avg, color="stage (m)"), size=1) + 
    scale_color_manual(values='black')+
    ylab("Stage (m)")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title.y = element_text(size = 15),
          axis.title.x =element_blank(),
          plot.title =element_blank(),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'))


(OtDO<-ggplot() + 
    geom_rect(data=DO_OtRECT,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill=DO_OtRECT$fill,alpha=0.6)+
    geom_line(data=Otter, aes(x=Date,y=DO),size=1) + #make transparent cloud
    scale_colour_manual(values = "black")+
    ylab("DO mg/L")+
    ggtitle("Otter")+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title.y =element_text(size = 15),
          axis.title.x =element_blank(),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 15, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'none',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))

(OtDO_graph<-plot_grid(hplain,DO, align = "v", ncol = 1, rel_heights = c(1/4,3/4)))

plot_grid(OtDO, AMDO, ncol=2)
plot_grid(LFDO, GBDO, ncol=2)


plot_grid(Ich_graph, LF_graph, GB_graph, ncol=3)

plot_grid(Ich_graph, LF_graph, GB_graph, Otter_graph, AM_graph, ncol=3)


(legend<-ggplot() + 
    geom_line(data=Otter, aes(x=Date,y=GPPavg, color="GPP"),size=3) + #make transparent cloud
    geom_line(data=Otter, aes(x=Date,y=ER, color="ER"),size=3) +
    geom_line(data=Otter, aes(x=Date,y=NEP, color="NEP"),size=3) +
    scale_colour_manual(values = colstwo)+
    ylab(flux)+
    ggtitle("Otter")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 12, angle=0),
          axis.title.y.right = element_text(),
          plot.title = element_text(size = 12, angle=0),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'bottom',
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(color=guide_legend(title="")))


#BoxPlots




####AM#####
startDate <- "2022-07-20"
endDate <- "2023-09-07"
parameterCd <- c('00065')
Dowling<-'02319800'

Dowling_15sec<-readNWISuv(Dowling,parameterCd,startDate,endDate)
Dowling_15sec<-Dowling_15sec[,-c(1,2,5,6)]
Dowling_15sec<-rename(Dowling_15sec, 'NGVD29_up'="X_00065_00000", 'Date'="dateTime")

Dowling_15sec<- Dowling_15sec %>% mutate(minute = minute(Date))
Dowling<-filter(Dowling_15sec, minute==0)
names(Dowling)
Dowling$NGVD29_up[is.na(Dowling$NGVD29_up)]<-mean(Dowling$NGVD29_up, na.rm = T)
Dowling<-Dowling[,-c(3)]


Luraville<-'02320000'

Luraville_15sec<-readNWISuv(Luraville,parameterCd,startDate,endDate)
Luraville_15sec<-Luraville_15sec[,-c(1,2,5,6)]
Luraville_15sec<-rename(Luraville_15sec, 'NGVD29_down'="X_00065_00000", 'Date'="dateTime")

Luraville_15sec<- Luraville_15sec %>% mutate(minute = minute(Date))
Luraville<-filter(Luraville_15sec, minute==0)
Luraville$NGVD29_down[is.na(Luraville$NGVD29_down)]<-mean(Luraville$NGVD29_down, na.rm = T)
Luraville<-Luraville[,-c(3)]

AllenMill <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))

AllenMill<-filter(AllenMill, Date> '2022-07-20' & Date <='2023-08-21')

AllenMill<-left_join(AllenMill, Dowling, by='Date')
AllenMill<-left_join(AllenMill, Luraville, by='Date')

AllenMill$river_elevation<-(AllenMill$NGVD29_up-AllenMill$NGVD29_down)*0.501

AllenMill$"u"<-(AllenMill$stage_avg*-0.2136+0.4426)

AllenMill$site<-'AM'

quantile(AllenMill$river_elevation, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
IQR(AllenMill$river_elevation, na.rm=T)
AllenMill<- AllenMill %>% mutate(riverIQR = case_when(
  river_elevation<2.19939   ~ "Q1",
  river_elevation<2.34468   ~ "Q2",
  river_elevation<=2.61522   ~ "Q3",
  river_elevation>2.61522 ~ 'Q4'))

AllenMill$day <- as.Date(AllenMill$Date)
AllenMill <- aggregate(AllenMill, by=list(AllenMill$day), FUN='mean')


####AM NEP vs river######
(AMr_g<-ggplot(AllenMill, aes(x=river_elevation, y=NEPnorm)) + 
   geom_point()+
   ylab(normNEPflux)+
   xlab("River Elevation")+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Allen Mill")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 0.1,
                label.x = 0.9,
                size=5)+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

###AM u and h vs river######

(AMu_g<-ggplot(AllenMill, aes(x=river_elevation, y=u)) + 
   geom_point(color='gray')+
   ylab("velocity m/s")+
   xlab("River Elevation")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   ylim(-0.05, 0.48)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Allen Mill")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

















#####LF######
startDate <- "2022-07-20"
endDate <- "2023-09-07"
parameterCd <- c('00065')
Wilcox<-'02323500'

Wilcox_15sec<-readNWISuv(Wilcox,parameterCd,startDate,endDate)
Wilcox_15sec<-Wilcox_15sec[,-c(1,2,5,6)]
Wilcox_15sec<-rename(Wilcox_15sec, 'NGVD29_up'="X_00065_00000", 'Date'="dateTime")

Wilcox_15sec<- Wilcox_15sec %>% mutate(minute = minute(Date))
Wilcox<-filter(Wilcox_15sec, minute==0)
Wilcox$NGVD29_up[is.na(Wilcox$NGVD29_up)]<-mean(Wilcox$NGVD29_up, na.rm = T)
Wilcox<-Wilcox[,-c(3)]

LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

LF<-left_join(LF, Wilcox, by='Date')

LF$u<- -0.12*LF$stage_avg + 0.18

LF$site<-"LF"
IQR(LF$NGVD29_up, na.rm=T)

LF<- LF %>% mutate(riverIQR = case_when(
  NGVD29_up<3.69   ~ "Q1",
  NGVD29_up<4.47   ~ "Q2",
  NGVD29_up<=5.04   ~ "Q3",
  NGVD29_up>5.04 ~ 'Q4'))

LF$day <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$day), FUN='mean')

###LF NEP######

(LFr_g<-ggplot(LF, aes(x=NGVD29_up, y=NEPnorm)) + 
   geom_point()+
   ylab(normNEPflux)+
   xlab("Wilcox NGVD29")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Little Fanning")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
###LF u and h vs river######

(LFu_g<-ggplot(LF, aes(x=NGVD29_up, y=u)) + 
   geom_point(color="gray")+
   ylab("velocity m/s")+
   xlab("Wilcox NGVD29")+
   ylim(-0.05, 0.48)+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Little Fanning")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))




























######Otter####
startDate <- "2022-07-20"
endDate <- "2023-09-07"
parameterCd <- c('00065')
Bell<-'02323000'

Bell_15sec<-readNWISuv(Bell,parameterCd,startDate,endDate)
Bell_15sec<-Bell_15sec[,-c(1,2,5,6)]
Bell_15sec<-rename(Bell_15sec, 'NGVD29_up'="X_00065_00000", 'Date'="dateTime")

Bell_15sec<- Bell_15sec %>% mutate(minute = minute(Date))
Bell<-filter(Bell_15sec, minute==0)
Bell$NGVD29_up[is.na(Bell$NGVD29_up)]<-mean(Bell$NGVD29_up, na.rm = T)
Bell<-Bell[,-c(3)]


Wilcox<-'02323500'

Wilcox_15sec<-readNWISuv(Wilcox,parameterCd,startDate,endDate)
Wilcox_15sec<-Wilcox_15sec[,-c(1,2,5,6)]
Wilcox_15sec<-rename(Wilcox_15sec, 'NGVD29_down'="X_00065_00000", 'Date'="dateTime")

Wilcox_15sec<- Wilcox_15sec %>% mutate(minute = minute(Date))
Wilcox<-filter(Wilcox_15sec, minute==0)
Wilcox$NGVD29_down[is.na(Wilcox$NGVD29_down)]<-mean(Wilcox$NGVD29_down, na.rm = T)
Wilcox<-Wilcox[,-c(3)]


Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"))
Otter<-left_join(Otter, Bell, by='Date')
Otter<-left_join(Otter, Wilcox, by='Date')


x<-26185/36510
Otter$river_elevation<-(Otter$NGVD29_up-Otter$NGVD29_down)*x
IQR(Otter$river_elevation, na.rm=T)

Otter$u<-Otter$stage_avg*-0.0868+0.1579
Otter$site<-"Otter"

quantile(Otter$river_elevation, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

Otter<- Otter %>% mutate(riverIQR = case_when(
  river_elevation<2.481515    ~ "Q1",
  river_elevation<3.119823    ~ "Q2",
  river_elevation<=3.650552    ~ "Q3",
  river_elevation>3.650552  ~ 'Q4'))

Otter$day <- as.Date(Otter$Date)
Otter <- aggregate(Otter, by=list(Otter$day), FUN='mean')


###Otter NEP######

(Otterr_g<-ggplot(Otter, aes(x=river_elevation, y=NEPnorm)) + 
   geom_point(color='black')+
   ylab(normNEPflux)+
   xlab("River Elevation")+
   ylim(0,1)+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Otter")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



###Otter u and h vs river######

(Otteru_g<-ggplot(Otter, aes(x=river_elevation, y=u)) + 
   geom_point(color='gray')+
   ylab("velocity m/s")+
   xlab("River Elevation")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   ylim(-0.05, 0.48)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Otter")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))










######Gilchrist Blue########
startDate <- "2022-07-20"
endDate <- "2023-9-07"
parameterCd <- c('00065')
rise<-'02321958'

rise_15sec<-readNWISuv(rise,parameterCd,startDate,endDate)
rise_15sec<-rise_15sec[,-c(1,2,5,6)]
rise_15sec<-rename(rise_15sec, 'NGVD29_up'="X_00065_00000", 'Date'="dateTime")

rise_15sec<- rise_15sec %>% mutate(minute = minute(Date))
rise<-filter(rise_15sec, minute==0)
rise$NGVD29_up[is.na(rise$NGVD29_up)]<-mean(rise$NGVD29_up, na.rm = T)
rise<-rise[,-c(3)]

FtWhite<-'02315500'

FtWhite_15sec<-readNWISuv(FtWhite,parameterCd,startDate,endDate)
FtWhite_15sec<-FtWhite_15sec[,-c(1,2,5,6)]
FtWhite_15sec<-rename(FtWhite_15sec, 'NGVD29_down'="X_00065_00000", 'Date'="dateTime")

FtWhite_15sec<- FtWhite_15sec %>% mutate(minute = minute(Date))
FtWhite<-filter(FtWhite_15sec, minute==0)
FtWhite$NGVD29_down[is.na(FtWhite$NGVD29_down)]<-mean(FtWhite$NGVD29_down, na.rm = T)
FtWhite<-FtWhite[,-c(3)]

GB <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric","numeric"))

GB<-left_join(GB, rise, by='Date')
GB<-left_join(GB, FtWhite, by='Date')


x<-16753/21307
GB$river_elevation<-(GB$NGVD29_down-GB$NGVD29_up)*x
IQR(GB$NGVD29_down, na.rm=T)

GB$u<-GB$stage_avg*-0.0868+0.1579
GB$site<'GB'

quantile(GB$river_elevation, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

GB<- GB %>% mutate(riverIQR = case_when(
  river_elevation<=38.81016       ~ "Q1",
  river_elevation<=39.57284      ~ "Q2",
  river_elevation<=41.42843      ~ "Q3",
  river_elevation>41.42843      ~ "Q4"))

GB$day <- as.Date(GB$Date)
GB <- aggregate(GB, by=list(GB$day), FUN='mean')


####GB NEP vs river######
(GBr_g<-ggplot(GB, aes(x=river_elevation, y=NEPnorm)) + 
  geom_point()+
  ylab(normNEPflux)+
  xlab("River Elevation")+
  scale_fill_manual(values=colors, name=' ')+
  ggtitle("Gilchrist Blue")+
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 1,label.x = 0.9)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 0.9,
                 label.x = 0.9,
                 size=5)+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 16),
        axis.title.x =element_text(size = 16),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

###GB u and h vs river######

(GBu_g<-ggplot(GB, aes(x=river_elevation, y=u)) + 
   geom_point(color="gray")+
   ylab("velocity m/s")+
   xlab("River Elevation")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue',
                  ) +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   ylim(-0.05, 0.48)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("Gilchrist Blue")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))











###Ichetuckne####

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))

Ich$u<-Ich$stage_avg*-0.0773+0.3319


r02322703_Level <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/r02322703_Level.xlsx", 
                              col_types = c("skip", "skip", "skip", 
                                            "date", "numeric", "skip"))

Ich<- Ich %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))

r02322703_Level<- r02322703_Level %>%
  mutate(day= day(Date),
         month= month(Date),
         year=year(Date))
r02322703_Level<-r02322703_Level[,-c(1)]

Ich<-left_join(Ich, r02322703_Level, by=c('day','month','year'))

Ich$day <- as.Date(Ich$Date)
Ich <- aggregate(Ich, by=list(Ich$day), FUN='mean')



US27bridge <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
US27bridge<-left_join(US27bridge, r02322703_Level, by=c('day','month','year'))

US27bridge$day <- as.Date(US27bridge$Date)
US27bridge <- aggregate(US27bridge, by=list(US27bridge$day), FUN='mean')

US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$US27bridge<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-2.5)

US27bridge$u<-US27bridge$discharge/(US27bridge$depth*15)/10


#####Ich vs NEP####
(Ichr_g<-ggplot(Ich, aes(x=NAVD88, y=NEPnorm)) + 
   geom_point()+
   ylab(normNEPflux)+
   xlab("Santa Fe NAVD88")+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("ICHE- down")+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.03,label.x = 0.05)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 0.1,
                label.x = 0.05,
                size=5)+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(US27r_g<-ggplot(US27bridge, aes(x=NAVD88, y=NEPnorm)) + 
    geom_point()+
    ylab(normNEPflux)+
    xlab("Santa Fe NAVD88")+
    scale_fill_manual(values=colors, name=' ')+
    ggtitle("ICHE- up")+
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.03,label.x =0.05)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 0.1,
                 label.x = 0.05,
                 size=5)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 16),
          axis.title.x =element_text(size = 16),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

###Ich u and h vs river######

(Ichu_g<-ggplot(Ich, aes(x=NAVD88, y=u)) + 
   geom_point(color='gray')+
   ylab("velocity m/s")+
   xlab("Santa Fe NAVD88")+
   ylim(-0.05, 0.48)+
   stat_correlation(mapping = use_label(c('P')), size=5,
                    label.y = 0.9,label.x = 0.9)+
   stat_poly_line(formula = y ~ x, color='blue') +
   stat_poly_eq(mapping = use_label(c("eq")),
                label.y = 1,
                label.x = 0.9,
                size=5)+
   scale_fill_manual(values=colors, name=' ')+
   ggtitle("ICHE- down")+
   theme(axis.text.x = element_text(size = 12, angle=0),
         axis.text.y = element_text(size = 16, angle=0),
         axis.title.y =element_text(size = 16),
         axis.title.x =element_text(size = 16),
         plot.title = element_text(size = 19),
         legend.position = "none",
         panel.background = element_rect(fill = 'white'),
         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(US27u_g<-ggplot(US27bridge, aes(x=NAVD88, y=u)) + 
    geom_point(color='gray')+
    ylab("velocity m/s")+
    xlab("Santa Fe NAVD88")+
    ylim(-0.05, 0.48)+
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.9,label.x = 0.9)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 1,
                 label.x = 0.9,
                 size=5)+
    scale_fill_manual(values=colors, name=' ')+
    ggtitle("ICHE- up")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 16),
          axis.title.x =element_text(size = 16),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

######

NEPgrid<-plot_grid(GBr_g, Otterr_g, AMr_g, ncol=1)
ugrid<-plot_grid(GBu_g, Otteru_g, AMu_g, ncol=1)

plot_grid(NEPgrid, ugrid, ncol=2, nrow=1)


NEPgrid<-plot_grid(US27r_g,Ichr_g, LFr_g, ncol=1)
ugrid<-plot_grid(US27u_g, Ichu_g, LFu_g,ncol=1)

plot_grid(NEPgrid, ugrid, ncol=2, nrow=1)






#get data####
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric","numeric",
                                 "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-08-24')



GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-08-21')


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-07-01')




AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-08-21')





Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric",  
                                "numeric",
                                "numeric", "numeric", "numeric"))
Ich<-filter(Ich, Date> '2022-07-20' & Date <='2023-08-21')



US27bridge <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
US27bridge$day <- as.Date(US27bridge$Date)
US27bridge<- aggregate(US27bridge, by=list(US27bridge$day), FUN='mean')


US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$depth<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-1.5)


###Scatter plots#######


library(ggpmisc)
(Ich_sc<-ggplot(data=Ich, aes(x=stage_avg,y=NEP)) + 
  geom_point(size=1)+
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.9,)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")), label.y = 3, size=5)+
    
  scale_color_manual(values='black')+
  xlab("Stage (m)")+
  ylab(NEPflux)+
    ylim(-23,25)+
    ggtitle("ICHE-down")+
  theme(axis.text.x = element_text(size = 16, angle=0),
        axis.text.y = element_text(size = 16, angle=0),
        axis.title.y =element_text(size = 18),
        axis.title.x =element_text(size = 18),
        plot.title = element_text(size = 19),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(US27_sc<-ggplot(data=US27bridge, aes(x=depth,y=NEP)) + 
    geom_point(size=1)+
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.9,)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")), label.y = 3, size=5)+
    ylim(-23,25)+
    scale_color_manual(values='black')+
    xlab("Stage (m)")+
    ylab(NEPflux)+
    ggtitle("ICHE-up")+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ depth, data = US27bridge))


(AM_sc<-ggplot(data=AM, aes(x=stage_avg,y=NEP)) + 
  geom_point(size=1)+
    
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.9,)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")), label.y = 3, size=5)+
    ylab(NEPflux)+
    xlab("Stage (m)")+
  ggtitle("Allen Mill Pond")+
  xlim(0,3.50)+
    ylim(-23,25)+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = AM))



(LF_sc<-ggplot(data=LF, aes(x=stage_avg,y=NEP, color="NEP")) + 
    geom_point(size=1)+
    
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.03,label.x = 1)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 0.1,
                 label.x = 1,
                 size=5)+
    scale_color_manual(values='black')+
    xlab("Stage (m)")+
    ylab(NEPflux)+
    ggtitle("Little Fanning")+
    xlim(0,3.50)+
    ylim(-23,25)+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = LF))


(GB_sc<-ggplot(data=GB, aes(x=stage_avg,y=NEP, color="NEP")) + 
    geom_point(size=1)+
    
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.03,label.x = 1)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 0.1,
                 label.x = 1,
                 size=5)+
    scale_color_manual(values='black')+
    xlab("Stage (m)")+
    ylab(NEPflux)+
    ggtitle("Gilchrist Blue")+
    xlim(0,3.50)+
    ylim(-23,25)+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = GB))



(Otter_sc<-ggplot(data=Otter, aes(x=stage_avg,y=NEP, color="NEP")) + 
    geom_point(size=1)+
    
    stat_correlation(mapping = use_label(c('P')), size=5,
                     label.y = 0.03,label.x = 1)+
    stat_poly_line(formula = y ~ x, color='blue') +
    stat_poly_eq(mapping = use_label(c("eq")),
                 label.y = 0.1,
                 label.x = 1,
                 size=5)+
    scale_color_manual(values='black')+
    xlab("Stage (m)")+
    ylab(NEPflux)+
    ggtitle("Otter")+
    xlim(0,3.50)+
    ylim(-23,25)+
    theme(axis.text.x = element_text(size = 16, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 18),
          axis.title.x =element_text(size = 18),
          plot.title = element_text(size = 19),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
summary(lm(NEP ~ stage_avg, data = Otter))


plot_grid(US27_sc, Ich_sc, LF_sc, GB_sc, Otter_sc, AM_sc)


range(Ich$NEP, na.rm=T)
dev.new()







