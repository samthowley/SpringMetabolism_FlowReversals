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
library(ggnewscale)
#############
######
cols<-c('moderate'='black','high'='red','low'='steelblue')

Otterx <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))
names(Otterx)
Otterx<-Otterx[,c(1,8,14)]

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Otter")

Otter<- read_excel("Otter_final.xlsx")
names(Otter)
Otter<-Otter[,-c(11,14)]
Otter<-left_join(Otterx, Otter, by='Date')


Otter$days <- as.Date(Otter$Date)

Otter <- Otter %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

ggplot()+
  geom_line(data=Otter, 
            aes(x=Date, y=stage_avg, color='h'))

Otter<- Otter %>% mutate(RI = case_when(
  stage_avg<0.82 ~ "low",
  stage_avg<1.12 ~ "moderate",
  stage_avg>=1.12 ~ "high"))

Otter_hi<-filter(Otter, RI== 'high')
Otter_mod<-filter(Otter, RI== 'moderate')
Otter_low<-filter(Otter, RI== 'low')

(d<-ggplot(Otter, aes(x=stage_avg, fill=RI))+ 
    geom_histogram(binwidth=0.05)+
    #geom_vline(xintercept=0.82, linetype='dotted', size=1)+
    geom_vline(xintercept=1.12, linetype='dotted', size=1)+
    xlab("meters")+
    scale_fill_manual(values=cols)+
    theme(axis.text.x = element_text(size = 10, angle=0),
          axis.text.y = element_text(size = 10, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_text(size=10),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="top"))

ggplot()+
  geom_line(data=Otter_hi, aes(x=Date, y=stage_avg), color='black', size=1.5)

Otter_hi_fall<-filter(Otter_hi, Date<='2023-01-01')
Otter_hi_winter<-filter(Otter_hi, Date>='2023-01-01 00:00:00' & Date<'2023-04-01 00:00:00')
Otter_hi_summer<-filter(Otter_hi, Date>='2023-04-01')

ggplot()+
  geom_line(data=Otter_low, aes(x=Date, y=stage_avg), color='black', size=1.5)

Otter_low_summer<-filter(Otter_low,Date>'2023-04-01')
Otter_low_winter<-filter(Otter_low, Date<'2023-04-01')



ggplot()+
    geom_point(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1, alpha=0.2, color='red')+
    stat_ellipse(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')

Otter_hi_fall<-filter(Otter_hi_fall, O2_mmol_m2_1d< -0.5)
Otter_hi_fall<-filter(Otter_hi_fall, CO2_mmol_m2_1d> 35)
Otter_hi_winter<-filter(Otter_hi_winter,CO2_mmol_m2_1d>2& CO2_mmol_m2_1d<8)

Otter_low_winter<-filter(Otter_low_winter, CO2_mmol_m2_1d< 35)



(x<-ggplot()+
    geom_point(data=Otter, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+

    geom_point(data=Otter_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Otter_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+

    stat_ellipse(data=Otter_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Otter_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+

    stat_ellipse(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Otter")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="none"))

(y<-ggplot()+
    geom_point(data=Otter, aes(x=cen.x, y=cen.y, color=days), size=3)+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=days, color=days), 
                size=0.75, data=Otter, se = FALSE, method='lm')+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+

    geom_point(data=Otter_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Otter_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=Otter_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Otter_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    
    stat_ellipse(data=Otter_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Ichetucknee")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom"))

z<-plot_grid(x,y,
             ncol=1,
             align = "v", axis="2")


(a<-ggplot()+
    geom_line(data=Otter, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    geom_line(data=Otter_hi_fall, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=Otter_hi_winter, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=Otter_low_winter, aes(x=Date, y=stage_avg), color='steelblue', size=1.5)+

    ylab("Stage (m)")+
    scale_color_manual(values=c('black'))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

(b<-ggplot()+
    geom_point(data=Otter, aes(x=Date, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

  
(c<-ggplot()+
    geom_point(data=Otter, aes(x=stage_avg, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    xlab("Stage (m)")+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

top<-plot_grid(d,a,b,c,
  ncol=1,
  align = "v", axis="2")

plot_grid(z,top, ncol=2, rel_widths = c(2/3,1/3))










GBx <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric","numeric", "numeric", "numeric", 
                                   "numeric"))
names(GBx)
GBx<-GBx[,c(1,8)]

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Gilchrist Blue")

GB_final <- read_excel("GB_final.xlsx")
names(GB_final)
GB<-GB_final[,-c(12)]
GB<-left_join(GBx, GB, by='Date')

GB$days <- as.Date(GB$Date)

GB <- GB %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

GB<- GB %>% mutate(RI = case_when(
  stage_avg<=0.37 ~ "low",
  stage_avg<=0.65 ~ "moderate",
  stage_avg>=0.65 ~ "high"))
GB$RI[is.na(GB$RI)] <- 'moderate'

(d<-ggplot(GB, aes(x=stage_avg, fill=RI))+
    scale_fill_manual(values=cols)+
    geom_histogram(binwidth=0.02)+
    #geom_vline(xintercept=0.37, linetype='dotted', size=1)+
    #geom_vline(xintercept=0.65, linetype='dotted', size=1)+
    xlab("meters")+
    theme(axis.text.x = element_text(size = 10, angle=0),
          axis.text.y = element_text(size = 10, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_text(size=10),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="bottom"))


GB_hi<-filter(GB, RI== 'high')
GB_mod<-filter(GB, RI== 'moderate')
GB_low<-filter(GB, RI== 'low')

ggplot()+
  geom_line(data=GB, 
            aes(x=Date, y=stage_avg, color='stage'))+
  geom_line(data=GB_hi, 
            aes(x=Date, y=stage_avg, color='hi'))+
  geom_line(data=GB_mod, 
            aes(x=Date, y=stage_avg, color='mod'))+
  geom_line(data=GB_low, 
            aes(x=Date, y=stage_avg, color='low'))

(x<-ggplot()+
    geom_point(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1, alpha=0.2, color='red')+
    stat_ellipse(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue'))
    
GB_low<-filter(GB_low, CO2_mmol_m2_1d>18)

(x<-ggplot()+
    geom_point(data=GB, aes(x=cen.x, y=cen.y, color=days), size=2)+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=days, color=days), 
                size=0.75, data=GB, se = FALSE, method='lm')+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=GB_low, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=GB_low, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Gilchrist Blue")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position='none'))

(y<-ggplot()+
    geom_point(data=GB, aes(x=cen.x, y=cen.y, color=days), size=2)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=GB_low, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=GB_hi, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=GB_low, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Gilchrist Blue")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position="bottom"))

z<-plot_grid(x,y,
               ncol=1,
               align = "v", axis="2")

(a<-ggplot()+
    geom_line(data=GB, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    geom_line(data=GB_hi, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=GB_low, aes(x=Date, y=stage_avg), color='steelblue', size=1.5)+

    ylab("Stage (m)")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(b<-ggplot()+
    geom_point(data=GB, aes(x=Date, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]~('mmol'/m^2/'day')/CO[2]~('mmol'/m^2/'day')))))+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(c<-ggplot()+
    geom_point(data=GB, aes(x=stage_avg, y=slope, color=RI), size=1.5)+
  ylab(expression(paste('Slope'~~(O[2]~('mmol'/m^2/'day')/CO[2]~('mmol'/m^2/'day')))))+
  xlab("Stage (m)")+
  scale_color_manual(values=cols)+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_text(size = 10),
        plot.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray')))


top<-plot_grid(d,a,b,c,
                ncol=1,
                align = "v", axis="2")

plot_grid(z,top, ncol=2, rel_widths = c(2/3,1/3))














dev.new()











LFx <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
names(LFx)
LFx<-LFx[,c(1,12)]

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Little Fanning")

LF_final <- read_excel("LF_final.xlsx")
names(LF_final)
LF<-LF_final[,-c(2)]
LF<-left_join(LFx, LF, by='Date')

names(LF)
LF$days <- as.Date(LF$Date)

LF <- LF %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()


LF<- LF %>% mutate(RI = case_when(
  stage_avg<0.33 ~ "low",
  stage_avg<0.61 ~ "moderate",
  stage_avg>=0.61 ~ "high"))

(d<-ggplot(LF, aes(x=stage_avg, fill=RI))+ 
    geom_histogram(binwidth=0.05)+
    #geom_vline(xintercept=0.775, linetype='dotted', size=1)+
    #geom_vline(xintercept=1.17, linetype='dotted', size=1)+
    xlab("meters")+
    scale_fill_manual(values=cols)+
    theme(axis.text.x = element_text(size = 10, angle=0),
          axis.text.y = element_text(size = 10, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_text(size=10),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="top"))



LF_hi<-filter(LF, RI== 'high')
LF_mod<-filter(LF, RI== 'moderate')
LF_low<-filter(LF, RI== 'low')

ggplot()+
  geom_line(data=LF, 
            aes(x=Date, y=stage_avg, color='stage'))+
  geom_line(data=LF_hi, 
            aes(x=Date, y=stage_avg, color='hi'))+
  geom_line(data=LF_mod, 
            aes(x=Date, y=stage_avg, color='mod'))+
  geom_line(data=LF_low, 
            aes(x=Date, y=stage_avg, color='low'))

LF_hi_fall<-filter(LF_hi, Date< '2023-01-01')
LF_hi_winter<-filter(LF_hi, Date>'2023-01-01')
LF_hi_summer<-filter(LF_hi_winter, Date>'2023-05-01')

LF_low_fall<-filter(LF_low, Date< '2023-04-01')
LF_low_winter<-filter(LF_low, Date>'2023-04-01')


(x<-ggplot()+
    geom_point(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1, alpha=0.2, color='red')+
    stat_ellipse(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue'))


(x<-ggplot()+
    geom_point(data=LF, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=LF_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=LF_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=LF_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=LF_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
     scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Little Fanning")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position="none"))

(y<-ggplot()+
    geom_point(data=LF, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=days, color=days), 
                size=0.75, data=LF, se = FALSE, method='lm')+
    new_scale_color() +
    
    geom_point(data=LF_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=LF_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=LF_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=LF_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=LF_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=LF_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Little Fanning")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position="bottom"))

z<-plot_grid(x,y,
             ncol=1,
             align = "v", axis="2")

LF_mod<-filter(LF, RI== 'moderate')
LF_mod1<-filter(LF_mod, Date<='2022-10-02')
LF_mod2<-filter(LF_mod, Date>='2023-05-01')
LF_mod3<-filter(LF_mod, Date>="2023-01-01"& Date<"2023-03-03")
LF_mod4<-filter(LF_mod, Date>"2023-03-03" & Date<"2023-05-01")


(a<-ggplot()+
    geom_line(data=LF_hi_fall, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='red', size=1.5)+
    geom_line(data=LF_hi_winter, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='red', size=1.5)+
    geom_line(data=LF_hi_summer, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='red', size=1.5)+
    geom_line(data=LF_low_fall, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='steelblue', size=1.5)+
    geom_line(data=LF_low_winter, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='steelblue', size=1.5)+
    geom_line(data=LF_mod1, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='black', size=1.5)+
    geom_line(data=LF_mod3, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='black', size=1.5)+
    geom_line(data=LF_mod4, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='black', size=1.5)+
    geom_line(data=LF_mod2, 
              aes(x=Date, y=stage_avg, color="Stage (m)"),color='black', size=1.5)+
    ylab("Stage (m)")+
    scale_color_manual(values=c('black'))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

(b<-ggplot()+
    geom_point(data=LF, aes(x=Date, y=slope,color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(c<-ggplot()+
    geom_point(data=LF, aes(x=stage_avg, y=slope,color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    xlab("Stage (m)")+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


top<-plot_grid(d,a,b,c,
                ncol=1,
                align = "v", axis="2")

plot_grid(z,top, ncol=2, rel_widths = c(2/3,1/3))
dev.new()


























AMx <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric",
                                "numeric"))
names(AMx)
AMx<-AMx[,c(1,7)]

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill")

AM_final <- read_excel("AM_final.xlsx")
names(AM_final)
AM<-AM_final[,-c(12)]
AM<-left_join(AMx, AM, by='Date')

AM$days <- as.Date(AM$Date)

AM <- AM %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

AM<- AM %>% mutate(RI = case_when(
  stage_avg<0.93 ~ "low",
  stage_avg<1.37 ~ "moderate",
  stage_avg>=1.37 ~ "high"))


(d<-ggplot(AM, aes(x=stage_avg, fill=RI))+ 
    geom_histogram(binwidth=0.05)+
    #geom_vline(xintercept=0.775, linetype='dotted', size=1)+
    #geom_vline(xintercept=1.17, linetype='dotted', size=1)+
    xlab("meters")+
    scale_fill_manual(values=cols)+
    theme(axis.text.x = element_text(size = 10, angle=0),
          axis.text.y = element_text(size = 10, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_text(size=10),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="top"))



AM_hi<-filter(AM, RI== 'high')
AM_mod<-filter(AM, RI== 'moderate')
AM_low<-filter(AM, RI== 'low')

ggplot()+
  geom_line(data=AM, 
            aes(x=Date, y=stage_avg, color='stage'))+
  geom_line(data=AM_hi, 
            aes(x=Date, y=stage_avg, color='hi'))+
  geom_line(data=AM_mod, 
            aes(x=Date, y=stage_avg, color='mod'))+
  geom_line(data=AM_low, 
            aes(x=Date, y=stage_avg, color='low'))


AM_hi_fall<-filter(AM_hi, Date< '2023-01-01')
AM_hi_winter<-filter(AM_hi, Date>'2023-01-01'&  Date<'2023-06-01')
AM_hi_summer<-filter(AM_hi, Date>'2023-06-01')

AM_low_fall<-filter(AM_low,Date>='2022-10-05'& Date< '2023-02-01')
AM_low_summer<-filter(AM_low,Date<='2022-09-05')
AM_low_winter<-filter(AM_low, Date>'2023-02-01')


(x<-ggplot()+
    geom_point(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1, alpha=0.2, color='red')+
    stat_ellipse(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue'))

(x<-ggplot()+
    geom_point(data=AM, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=AM_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_low_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    
    geom_point(data=AM_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=AM_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=AM_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    
    stat_ellipse(data=AM_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=AM_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=AM_low_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Allen Mill Pond")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="none"))

(y<-ggplot()+
    geom_point(data=AM, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="lightgray", trans='date')+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=days, color=days), 
                size=0.75, data=AM, se = FALSE, method='lm')+
    new_scale_color() +
    
    geom_point(data=AM_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_low_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    
    geom_point(data=AM_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=AM_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=AM_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=AM_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    
    stat_ellipse(data=AM_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=AM_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=AM_low_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Allen Mill Pond")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="bottom"))

z<-plot_grid(x,y,
             ncol=1,
             align = "v", axis="2")


AM_mod1<-filter(AM_mod, Date<='2022-08-05')
AM_mod7<-filter(AM_mod, Date>='2022-08-24'& Date<'2022-08-30')
AM_mod2<-filter(AM_mod, Date>='2022-09-05'& Date<='2022-11-05')
AM_mod3<-filter(AM_mod, Date>='2022-11-05'& Date< '2023-02-01')
AM_mod4<-filter(AM_mod, Date>= '2023-02-01' & Date<'2023-03-01')
AM_mod5<-filter(AM_mod, Date>='2023-04-01'&  Date<'2023-06-01')
AM_mod8<-filter(AM_mod, Date>='2023-05-01'&  Date<'2023-07-01')
AM_mod6<-filter(AM_mod, Date>='2023-07-01')

a<-ggplot()+
  geom_line(data=AM, aes(x=Date, y=stage_avg),color='black', size=1)+
  geom_line(data=AM_hi_fall, aes(x=Date, y=stage_avg),color='red', size=1)+
  geom_line(data=AM_hi_winter, aes(x=Date, y=stage_avg),color='red', size=1)+
  geom_line(data=AM_hi_summer, aes(x=Date, y=stage_avg),color='red', size=1)+
  geom_line(data=AM_low_fall, aes(x=Date, y=stage_avg),color='steelblue', size=1)+
  geom_line(data=AM_low_winter, aes(x=Date, y=stage_avg),color='steelblue', size=1)+
  geom_line(data=AM_low_summer, aes(x=Date, y=stage_avg),color='steelblue', size=1)+
  ylab("Stage (m)")+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))

(b<-ggplot()+
    geom_point(data=AM, aes(x=Date, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(c<-ggplot()+
    geom_point(data=AM, aes(x=stage_avg, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    xlab("Stage (m)")+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


top<-plot_grid(d,a,b,c,
                ncol=1,
                align = "v", axis="2")

plot_grid(z,top, ncol=2, rel_widths = c(2/3,1/3))























Ichx <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric",
                                "numeric"))
Ichx <- Ichx[!duplicated(Ichx[c('Date')]),]


names(Ichx)
Ichx<-Ichx[,c(1,12)]

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Ichetucknee")

Ich_final <- read_excel("Ich_final.xlsx")
names(Ich_final)
Ich<-Ich_final[,-c(9)]

Ich<-left_join(Ichx, Ich, by='Date')

Ich$days <- as.Date(Ich$Date)

Ich <- Ich %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

Ich<- Ich %>% mutate(RI = case_when(
  stage_avg<0.63 ~ "low",
  stage_avg<1.88 ~ "moderate",
  stage_avg>=1.88 ~ "high"))


(d<-ggplot(Ich, aes(x=stage_avg, fill=RI))+ 
    geom_histogram(binwidth=0.05)+
    #geom_vline(xintercept=0.775, linetype='dotted', size=1)+
    #geom_vline(xintercept=1.17, linetype='dotted', size=1)+
    xlab("meters")+
    scale_fill_manual(values=cols)+
    theme(axis.text.x = element_text(size = 10, angle=0),
          axis.text.y = element_text(size = 10, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_text(size=10),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'),
          legend.position="top"))




Ich_hi<-filter(Ich, RI== 'high')
Ich_mod<-filter(Ich, RI== 'moderate')
Ich_low<-filter(Ich, RI== 'low')

ggplot()+
  geom_line(data=Ich, 
            aes(x=Date, y=stage_avg, color='stage'))+
  geom_line(data=Ich_hi, 
            aes(x=Date, y=stage_avg, color='hi'))+
  geom_line(data=Ich_mod, 
            aes(x=Date, y=stage_avg, color='mod'))+
  geom_line(data=Ich_low, 
            aes(x=Date, y=stage_avg, color='low'))


Ich_hi_fall<-filter(Ich_hi,Date>'2022-08-01'&  Date< '2023-01-01')
Ich_hi_july22<-filter(Ich_hi, Date< '2022-07-11')
Ich_hi_winter<-filter(Ich_hi, Date>'2023-01-01' & Date<'2023-06-01')
Ich_hi_summer<-filter(Ich_hi, Date>'2023-04-01')


Ich_low_fall<-filter(Ich_low, Date< '2023-04-01')
Ich_low_winter<-filter(Ich_low, Date>'2023-04-01')

(x<-ggplot()+
    geom_point(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1, alpha=0.2, color='red')+
    stat_ellipse(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue'))


(x<-ggplot()+
    geom_point(data=Ich, aes(x=cen.x, y=cen.y, color=days), size=3)+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=Ich_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    
    geom_point(data=Ich_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_july22, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=Ich_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_july22, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    
    stat_ellipse(data=Ich_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=Ich_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Ichetucknee")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="none"))

(y<-ggplot()+
    geom_point(data=Ich, aes(x=cen.x, y=cen.y, color=days), size=3)+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=days, color=days), 
                size=0.75, data=Ich, se = FALSE, method='lm')+
    scale_color_gradient(low="black", high="light gray", trans='date')+
    new_scale_color() +
    
    geom_point(data=Ich_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='Low Stage'),size=1, alpha=0.2)+
    
    geom_point(data=Ich_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    geom_point(data=Ich_hi_july22, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color='High Stage'),size=1, alpha=0.2)+
    
    stat_ellipse(data=Ich_hi_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_summer, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    stat_ellipse(data=Ich_hi_july22, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='red')+
    
    stat_ellipse(data=Ich_low_fall, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    stat_ellipse(data=Ich_low_winter, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),size=1,type='norm',color='steelblue')+
    scale_colour_manual(name="Stage", values = c("High Stage"="red",
                                                 "Low Stage"="steelblue"))+
    
    xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
    ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
    ggtitle("Ichetucknee")+
    
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + 
    geom_vline(xintercept=0, linetype='dotted', size=1) +
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom"))

z<-plot_grid(x,y,
             ncol=1,
             align = "v", axis="2")


(a<-ggplot()+
    geom_line(data=Ich, aes(x=Date, y=stage_avg, color=RI), size=1.5)+
    ylab("Stage (m)")+
    scale_color_manual(values=c('moderate'='black',
                                'high'='red',
                                'low'='steelblue'))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

(b<-ggplot()+
    geom_point(data=Ich, aes(x=Date, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


(c<-ggplot()+
    geom_point(data=Ich, aes(x=stage_avg, y=slope, color=RI), size=1.5)+
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    xlab("Stage (m)")+
    scale_color_manual(values=cols)+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_text(size = 10),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))


top<-plot_grid(d,a,b,c,
                ncol=1,
                align = "v", axis="2")

plot_grid(z,top, ncol=2, rel_widths = c(2/3,1/3))
dev.new()


















Otter_hi<-filter(Otter, RI== 'high')
Otter_mod<-filter(Otter, RI== 'moderate')
Otter_low<-filter(Otter, RI== 'low')

Otter_hi_fall<-filter(Otter_hi, Date<='2023-01-01')
Otter_hi_winter<-filter(Otter_hi, Date>='2023-01-01' & Date<'2023-05-01')
Otter_hi_summer<-filter(Otter_hi, Date>='2023-05-01')

Otter_low_fall1<-filter(Otter_low,Date<='2022-10-01')
Otter_low_fall2<-filter(Otter_low,Date>='2022-10-01'& Date<='2023-04-01')
Otter_low_winter<-filter(Otter_low, Date>='2023-04-01')

Otter_mod_fall1<-filter(Otter_mod,Date<='2022-10-01')


Otter_mod_fall2<-filter(Otter_mod,Date>='2022-10-01'& Date<='2023-03-01')
Otter_mod_fall21<-filter(Otter_mod_fall2, Date<='2023-01-01')
Otter_mod_fall22<-filter(Otter_mod_fall2, Date>='2023-01-01')

Otter_mod_fall3<-filter(Otter_mod,Date>='2023-03-01'& Date<='2023-05-01')

Otter_mod_winter<-filter(Otter_mod, Date>='2023-05-01')



(a<-ggplot()+
    geom_line(data=Otter_hi_fall, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=Otter_hi_winter, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=Otter_hi_summer, aes(x=Date, y=stage_avg), color='red', size=1.5)+
    geom_line(data=Otter_low_winter, aes(x=Date, y=stage_avg), color='steelblue', size=1.5)+
    geom_line(data=Otter_low_fall1, aes(x=Date, y=stage_avg), color='steelblue', size=1.5)+
    geom_line(data=Otter_low_fall2, aes(x=Date, y=stage_avg), color='steelblue', size=1.5)+
    
    geom_line(data=Otter_mod_fall1, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    
    geom_line(data=Otter_mod_fall21, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    geom_line(data=Otter_mod_fall22, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    
    geom_line(data=Otter_mod_fall3, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    
    geom_line(data=Otter_mod_winter, aes(x=Date, y=stage_avg), color='black', size=1.5)+
    
    ylab("Stage (m)")+
    scale_color_manual(values=c('black'))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

