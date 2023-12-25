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
library(weathermetrics)
library(measurements)
library(ggnewscale)
#############
######

Otter<- read_excel("Z:/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))
Otter$days <- as.Date(Otter$Date)

OtFR<- Otter %>% mutate(RI = case_when(
  Date> "2023-01-26" & Date<"2023-5-10"~ 2))
OtFR<-filter(OtFR, RI== "2")

OtFR <- OtFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

max(OtFR$stage_avg, na.rm=T)
OtFR$disturb_count<-OtFR$consec-32


ggplot()+
    geom_line(data=OtFR, aes(x=Date, y=stage_avg, color=disturb_count), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="chartreuse3", high="purple")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days")


(Otter_CO2<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=CO2avg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))+
  theme(axis.text.x = element_text(size = 8, angle=0),
        axis.text.y = element_text(size = 8, angle=0),
        axis.title.y =element_text(size = 10),
        axis.title.x =element_blank(),
        plot.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+ 
  labs(color = "Days")


(Otter_DO<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=DOavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))

(Otter_FDOM<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=FDOM, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))


(Otter_GPP<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=GPPavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))


(Otter_ER<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=ER, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))

(Otter_NEP<-ggplot(OtFR, aes(x=stage_avg))+
    geom_point(aes(y=NEP, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))


OtFR$GPP_RR<- OtFR$GPPavg/max(Otter$GPPavg, na.rm=T)
OtFR$GPP_reduc<- (1-(OtFR$GPPavg/max(Otter$GPPavg, na.rm=T)))*100

OtFR$ER_RR<- OtFR$ER/max(Otter$ER, na.rm=T)
OtFR$ER_reduc<- 1-(OtFR$ER/mean(Otter$ER, na.rm=T))*100

OtFR$DO_RR<- OtFR$DOavg/max(Otter$DOavg, na.rm=T)
OtFR$DO_reduc<- (1-(OtFR$DOavg/max(Otter$DOavg, na.rm=T)))*100

OtFR$CO2_RR<- OtFR$CO2avg/max(Otter$CO2avg, na.rm=T)
OtFR$CO2_reduc<- 1-(OtFR$CO2avg/max(Otter$CO2avg, na.rm=T))*100


(Otter_NEP<-ggplot(OtFR, aes(ER))+
    geom_point(aes(y=GPPavg, fill=disturb_count), size=1.2, shape=21)+
    scale_fill_gradient(low="white", high="black"))




title <- ggdraw() + 
  draw_label(
    "Otter, 2023",
    x = 0.02,
    hjust = 0)

(a<-plot_grid(title,
          stage, pair, slope,
          rel_heights = c(0.1,0.4, 1, 1), 
          ncol=1))





######
setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill")
AMx <- read_excel("Z:/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric","numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
names(AMx)
AMx<-AMx[,c(1,7)]

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill")

AM_final <- read_excel("AM_final.xlsx")
names(AM_final)
AM<-AM_final[,-c(9)]
AM<-left_join(AMx, AM, by='Date')

AM<- AM_final %>% mutate(RI = case_when(
  Date> "2023-01-18" & Date<"2023-04-16"~ 2))

AM_springFR<-filter(AM, RI== "2")
AM_springFR$days <- as.Date(AM_springFR$Date)
AM_springFR <- AM_springFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()


AM_springFR$consec<-as.numeric(AM_springFR$consec)

(stage<-ggplot()+
    geom_line(data=AM_springFR, aes(x=Date, y=stage_avg, color=consec), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="chartreuse3", high="purple")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))


(pair<-ggplot()+
    geom_point(data=AM_springFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=AM_springFR, se = FALSE, method='lm', size=2) +
    geom_point(data=AM_springFR, aes(x=cen.x, y=cen.y, fill=consec), size=2, shape=21)+
    scale_fill_gradient(low="chartreuse3", high="purple")+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    scale_color_gradient(low="chartreuse3", high="purple")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))


(slope<-ggplot()+
    geom_point(data=AM_springFR, aes(x=stage_avg, y=slope, color=consec), size=2)+
    ggtitle("Otter")+
    xlab("Stage (m)") + ylab("slope")+
    scale_color_gradient(name="Days", low="chartreuse3", high="purple")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

title <- ggdraw() + 
  draw_label(
    "Allen Mill, 2023",
    x = 0.02,
    hjust = 0)

plot_grid(title,
             stage, pair, slope,
             rel_heights = c(0.1,0.4, 1, 1), 
             ncol=1)
dev.new()




setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Gilchrist Blue")
GBx <- read_excel("Z:/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric"))
names(GBx)
GBx<-GBx[,c(1,8,14)]

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Gilchrist Blue")
GB_final <- read_excel("GB_final.xlsx")
names(GB_final)
GB<-GB_final[,-c(8,9,10,12)]
GB<-left_join(GBx, GB, by='Date')

GB<- GB %>% mutate(RI = case_when(
  Date> "2022-08-17" & Date<"2022-11-21"~ 2))

GB_fallFR<-filter(GB, RI== "2")

GB_fallFR$days <- as.Date(GB_fallFR$Date)
GB_fallFR <- GB_fallFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

GB_fallFR$consec<-as.numeric(GB_fallFR$consec)

(stage<-ggplot()+
    geom_line(data=GB_fallFR, aes(x=Date, y=stage_avg, color=consec), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="blue", high="orange")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))


(pair<-ggplot()+
    geom_point(data=GB_fallFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=GB_fallFR, se = FALSE, method='lm', size=1) +
    scale_color_gradient(low="blue", high="orange")+
    new_scale_color() +
    
    geom_point(data=GB_fallFR, aes(x=cen.x, y=cen.y, fill=consec), size=4, shape=21)+
    scale_fill_gradient(low="blue", high="darkorange1")+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor =element_blank())+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))


(slope<-ggplot()+
    geom_point(data=GB_fallFR, aes(x=stage_avg, y=slope, color=consec), size=2)+
    xlab("Stage (m)") +
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_gradient(name="Days", low="blue", high="darkorange1")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

title <- ggdraw() + 
  draw_label(
    "Gilchrist Blue, 2022",
    x = 0.02,
    hjust = 0)

plot_grid(title,
          stage, pair, slope,
          rel_heights = c(0.1,0.4, 1, 1), 
          ncol=1)
dev.new()

######

LFx <- read_excel("Z:/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric"))
names(LFx)
LFx<-LFx[,c(1,12)]

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Little Fanning")

LF_final <- read_excel("LF_final.xlsx")
names(LF_final)
LF<-LF_final[,-c(2)]
LF<-left_join(LFx, LF, by='Date')


LFy<- LF %>% mutate(RI = case_when(
  Date> "2023-01-18" & Date<"2023-05-01"~ 2))
LF_winterFR<-filter(LFy, RI== "2")

ggplot()+
  geom_point(data=LF_winterFR, aes(x=Date, y=stage_avg), size=2)+
  xlab("Stage (m)")


LF_winterFR$days <- as.Date(LF_winterFR$Date)
LF_winterFR <- LF_winterFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

LF_winterFR$consec<-as.numeric(LF_winterFR$consec)


(stage<-ggplot()+
    geom_line(data=LF_winterFR, aes(x=Date, y=stage_avg, color=consec), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="blue", high="orange")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))


(pair<-ggplot()+
    geom_point(data=LF_winterFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=LF_winterFR, se = FALSE, method='lm', size=2) +
    scale_color_gradient(low="blue", high="orange")+

    geom_point(data=LF_winterFR, aes(x=cen.x, y=cen.y, fill=consec), size=4, shape=21)+
    scale_fill_gradient(low="blue", high="darkorange1")+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor =element_blank())+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))


(slope<-ggplot()+
    geom_point(data=LF_winterFR, aes(x=stage_avg, y=slope, color=consec), size=2)+
    xlab("Stage (m)") +
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_gradient(name="Days", low="blue", high="darkorange1")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

title <- ggdraw() + 
  draw_label(
    "Little Fanning, 2023",
    x = 0.02,
    hjust = 0)

(b<-plot_grid(title,
          stage, pair, slope,
          rel_heights = c(0.1,0.4, 1, 1), 
          ncol=1))

#######

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Ichetucknee")
Ichx <- read_excel("Z:/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
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

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Ichetucknee")

Ich_final <- read_excel("Ich_final.xlsx")
names(Ich_final)
Ich<-Ich_final[,-c(9)]
Ich<-left_join(Ichx, Ich, by='Date')

Ichy<- Ich %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-11-17"~ 2))
Ich_fallFR<-filter(Ichy, RI== "2")

ggplot()+
  geom_point(data=Ich_fallFR, aes(x=Date, y=stage_avg), size=2)+
  geom_point(data=Ich_fallFR, aes(x=Date, y=stage_avg), size=2)+
  xlab("Stage (m)")

Ich_fallFR$days <- as.Date(Ich_fallFR$Date)
Ich_fallFR <- Ich_fallFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

Ich_fallFR$consec<-as.numeric(Ich_fallFR$consec)


(stage<-ggplot()+
    geom_point(data=Ich_fallFR, aes(x=Date, y=stage_avg, color=consec), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="blue", high="orange")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))


(pair<-ggplot()+
    geom_point(data=Ich_fallFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=Ich_fallFR, se = FALSE, method='lm', size=2) +
    scale_color_gradient(low="blue", high="orange")+
    
    geom_point(data=Ich_fallFR, aes(x=cen.x, y=cen.y, fill=consec), size=4, shape=21)+
    scale_fill_gradient(low="blue", high="darkorange1")+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor =element_blank())+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))


(slope<-ggplot()+
    geom_point(data=Ich_fallFR, aes(x=stage_avg, y=slope, color=consec), size=2)+
    xlab("Stage (m)") +
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_gradient(name="Days", low="blue", high="darkorange1")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

title <- ggdraw() + 
  draw_label(
    "Ichetucknee, 2023",
    x = 0.02,
    hjust = 0)

(a<-plot_grid(title,
             stage, pair, slope,
             rel_heights = c(0.1,0.4, 1, 1), 
             ncol=1))


Ichy<- Ich %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-4-26"~ 2))
Ich_winterFR<-filter(Ichy, RI== "2")

ggplot()+
  geom_point(data=Ich_winterFR, aes(x=Date, y=stage_avg), size=2)+
  xlab("Stage (m)")

Ich_winterFR$days <- as.Date(Ich_winterFR$Date)
Ich_winterFR <- Ich_winterFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

Ich_winterFR$consec<-as.numeric(Ich_winterFR$consec)



(stage<-ggplot()+
    geom_line(data=Ich_winterFR, aes(x=Date, y=stage_avg, color=consec), size=1.5)+
    ylab("Stage (m)")+
    scale_color_gradient(low="blue", high="orange")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title.y =element_text(size = 10),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))


(pair<-ggplot()+
    geom_point(data=Ich_winterFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=Ich_fallFR, se = FALSE, method='lm', size=2) +
    scale_color_gradient(low="blue", high="orange")+
    
    geom_point(data=Ich_fallFR, aes(x=cen.x, y=cen.y, fill=consec), size=4, shape=21)+
    scale_fill_gradient(low="blue", high="darkorange1")+
    
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor =element_blank())+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))


(slope<-ggplot()+
    geom_point(data=Ich_winterFR, aes(x=stage_avg, y=slope, color=consec), size=1.5)+
    xlab("Stage (m)") +
    ylab(expression(paste('Slope'~~(O[2]/CO[2]))))+
    scale_color_gradient(name="Days", low="blue", high="darkorange1")+
    theme(axis.text.x = element_text(size = 8, angle=0),
          axis.text.y = element_text(size = 8, angle=0),
          axis.title =element_text(size = 10, angle=0),
          plot.title = element_blank(),
          legend.text=element_text(size=11),
          legend.title=element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.position="bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray')))

title <- ggdraw() + 
  draw_label(
    "Ichetucknee, 2022",
    x = 0.02,
    hjust = 0)

(b<-plot_grid(title,
             stage, pair, slope,
             rel_heights = c(0.1,0.4, 1, 1), 
             ncol=1))


dev.new()
plot_grid(a,b, ncol=2)

