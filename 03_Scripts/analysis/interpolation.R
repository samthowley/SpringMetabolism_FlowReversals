rm(list=ls())
####packages#####
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
library(mmand)
library(car)
library(imputeTS)
library(zoo)

########AM##########
flux<-expression(paste((g~O[2]/m^2/'day')))
GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))

AM_vent <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
AM_vent$Date <- as.Date(AM_vent$Date)
AM_vent<-AM_vent[,c(1, 9)]

AM_historic <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/UF_Continuous_Spring_Data.xlsx", 
                                        sheet = "02319915_AllenMillPond_Cont1", 
                                        col_types = c("date", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))
AM_historic<-AM_historic[,c(1,2)]
AM_historic<-rename(AM_historic, "vent"="NGVD29")
AM_historic$vent<-conv_unit(AM_historic$vent, "ft", "m")

AM_historic$stage<- AM_historic$vent - 6.29

AM_historic$Date <- as.Date(AM_historic$Date)
AM_historic<-AM_historic[,c(1,3)]
AM<-rbind(AM_historic, AM_vent)
AM<-filter(AM, stage>0)

AM$ER<- -0.93*AM$stage-16.6
AM$GPP<- -8.02*AM$stage+18.4
AM$GPP[AM$GPP<0] <- 0

MayoRain_11012023_09302023 <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/MayoRain_11012023_09302023.csv")
MayoRain_11012023_09302023<-MayoRain_11012023_09302023[,c(2,3)]
MayoRain_11012023_09302023<-rename(MayoRain_11012023_09302023, 'Date'='Period',
                                   "Rain_in"="2m Rain tot (in)")
MayoRain_11012023_09302023$Date <- mdy(MayoRain_11012023_09302023$Date)
AM<-left_join(MayoRain_11012023_09302023,AM)

AM <- aggregate(AM, by=list(AM$Date), FUN='mean')

AM<- AM %>%
  mutate(year = year(Date))
AM_met<-AM %>% group_by(year) %>% summarize(total_rain = sum(Rain_in, na.rm = TRUE),
                                                  avg_GPP = mean(GPP, na.rm = TRUE),
                                                  avg_ER = mean(ER, na.rm = TRUE))
AM_met<-na.omit(AM_met)
(bar<-ggplot(data = AM_met, aes(x = year,y = total_rain)) +
  geom_bar(stat = 'identity')+
    ggtitle("Allen Mill Pond")+
    ylab("Total Rain (in)")+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17, color = "black"),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(scatter<-ggplot(data=AM_met, aes(x=year)) + 
    geom_point(aes(y=avg_GPP), size=3, color='darkgreen')+
    geom_line(aes(y=avg_GPP), color='darkgreen')+
    geom_point(aes(y=avg_ER), size=3, color='darkred')+
    geom_line(aes(y=avg_ER), color='darkred')+
    geom_hline(yintercept = 0, size=2)+
    ylab(flux)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))   

(rain<-plot_grid(scatter, bar, ncol=1, align = 'v', rel_heights = c(2/3,1/3)))

ggsave(filename="rain.jpeg", 
       plot = rain,
       width =15, 
       height = 9, 
       units = "in")

AM<-left_join(AM,AM_met)
quantile(AM$total_rain, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
AM<- AM %>% mutate(RI = case_when(
  total_rain<=48.46   ~ "IQR1",
  total_rain<=57.910   ~ "IQR2",
  total_rain<=62.73    ~ "IQR3",
  total_rain<=63.010    ~ "IQR4"))
AM$RI[is.na(AM$RI)] <- "IQR4"

AM_met_GPP<-AM[,-c(1,5,10)]
AM_met_GPP$met<-'GPP'
AM_met_GPP<-rename(AM_met_GPP, 'flux'='GPP', "avg_flux"="avg_GPP")

AM_met_ER<-AM[,-c(1,6,9)]
AM_met_ER$met<-'ER'
AM_met_ER<-rename(AM_met_ER, 'flux'='ER', "avg_flux"="avg_ER")

AM_flux<-rbind(AM_met_GPP, AM_met_ER)


(barmet<-ggplot(AM_flux, aes(x=RI, y=flux, fill=met)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
    scale_fill_manual(values = c('darkred','darkgreen'))+
    ggtitle("Allen Mill Pond")+
    ylab(flux)+
    xlab("Total Rain (in) IQR")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(barh<-ggplot(AM_flux, aes(x=RI, y=stage)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
  ylab("Stage (m)")+
  xlab("Total Rain (in) IQR")+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17, color = "black"),
        axis.title.x =element_text(size = 17),
        plot.title = element_text(size = 17),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

AMbar<-plot_grid(barmet, barh, ncol=1, align = 'v')

ggsave(filename="AMbar.jpeg", 
       plot = AMbar,
       width =15, 
       height = 9, 
       units = "in")

######IU##########
startDate <- "2013-12-08"
endDate <- "2023-09-17"
parameterCd <- c('00065')
vent<-'02322700'

IU<-readNWISuv(vent,parameterCd,startDate,endDate)
IU<- IU %>% mutate(minute = minute(dateTime))
IU<-filter(IU, minute==0)

IU<-IU[,c(3,4)]
IU<-rename(IU, 'Date'="dateTime",
                   "stage"="X_00065_00000")

IU$Date <- as.Date(IU$Date)

IU$stage<-conv_unit(IU$stage, "ft", "m")
IU$stage<-IU$stage-(max(IU$stage, na.rm=T)-1.5)

IU$ER<- -2.52*IU$stage-9.2
IU$GPP<- -2.38*IU$stage+7.2
IU$GPP[IU$GPP<0] <- 0


AlachuaRain_11012023_09302023 <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AlachuaRain_11012023_09302023.csv")
AlachuaRain_11012023_09302023<-AlachuaRain_11012023_09302023[,c(2,3)]
AlachuaRain_11012023_09302023<-rename(AlachuaRain_11012023_09302023, 'Date'='Period',
                                   "Rain_in"="2m Rain tot (in)")
AlachuaRain_11012023_09302023$Date <- mdy(AlachuaRain_11012023_09302023$Date)

IU<-left_join(AlachuaRain_11012023_09302023,IU)

IU <- aggregate(IU, by=list(IU$Date), FUN='mean')

IU<- IU %>%
  mutate(year = year(Date))
IU_met<-IU %>% group_by(year) %>% summarize(total_rain = sum(Rain_in, na.rm = TRUE),
                                                avg_GPP = mean(GPP, na.rm = TRUE),
                                                avg_ER = mean(ER, na.rm = TRUE))
IU_met<-na.omit(IU_met)
(bar<-ggplot(data = IU_met, aes(x = year,y = total_rain)) +
    geom_bar(stat = 'identity')+
    ggtitle("Ichetucknee US27 Bridge")+
    ylab("Total Rain (in)")+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(scatter<-ggplot(data=IU_met, aes(x=year)) + 
    geom_point(aes(y=avg_GPP), size=3, color='darkgreen')+
    geom_line(aes(y=avg_GPP),color='darkgreen')+
    geom_point(aes(y=avg_ER), size=3, color='darkred')+
    geom_line(aes(y=avg_ER), color='darkred')+
    geom_hline(yintercept = 0, size=2)+
    ylab(flux)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))   

rainIU<- plot_grid(scatter, bar, ncol=1, align = 'v', rel_heights = c(2/3,1/3))

ggsave(filename="rainIU.jpeg", 
       plot = rainIU,
       width =15, 
       height = 9, 
       units = "in")


quantile(IU_met$total_rain, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
IU<-left_join(IU,IU_met)
quantile(IU$total_rain, probs = c(0,0.25,0.5,0.75,1), na.rm=T)
IU<- IU %>% mutate(RI = case_when(
  total_rain<=46.92   ~ "IQR1",
  total_rain<=51.03   ~ "IQR2",
  total_rain<=65.37    ~ "IQR3",
  total_rain<=70.32     ~ "IQR4"))
IU$RI[is.na(IU$RI)] <- "IQR4"

names(IU)
IU_met_GPP<-IU[,-c(1,5,10)]
IU_met_GPP$met<-'GPP'
IU_met_GPP<-rename(IU_met_GPP, 'flux'='GPP', "avg_flux"="avg_GPP")

IU_met_ER<-IU[,-c(1,6,9)]
IU_met_ER$met<-'ER'
IU_met_ER<-rename(IU_met_ER, 'flux'='ER', "avg_flux"="avg_ER")

IU_flux<-rbind(IU_met_GPP, IU_met_ER)

(barmet<-ggplot(IU_flux, aes(x=RI, y=flux, fill=met)) +
    geom_boxplot(outlier.colour="black", outlier.size=1)+
    scale_fill_manual(values = c('darkred','darkgreen'))+
    ggtitle("Allen Mill Pond")+
    ylab(flux)+
    xlab("Total Rain (in) IQR")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(barh<-ggplot(IU_flux, aes(x=RI, y=stage)) +
    geom_boxplot(outlier.colour="black", outlier.size=1)+
    ylab("Stage (m)")+
    xlab("Total Rain (in) IQR")+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.x =element_text(size = 17),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

IUbar<-plot_grid(barmet, barh, ncol=1, align = 'v')

ggsave(filename="IUbar.jpeg", 
       plot = IUbar,
       width =15, 
       height = 9, 
       units = "in")