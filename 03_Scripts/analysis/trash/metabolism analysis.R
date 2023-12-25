rm(list=ls())

library(ggplot2)
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

setwd("Z:/SpringsProject_Sam&Paul/Master")
AllenMill <- read_excel("AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric"))

GB<- read_excel("Z:/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
              col_types = c("date", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric"))

Ich <- read_excel("Ichetucknee.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", "numeric"))

Otter <- read_excel("Otter.xlsx", col_types = c("date", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric"))

LF <- read_excel("LittleFanning.xlsx", 
                            col_types = c("date", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric"))


OtterFR1<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-10" & Date<"2022-11-10"~ 2))
OtterFR1<-filter(OtterFR1,RI==2 )

(Otter_NEP1<-ggplot(OtterFR1, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Otter",subtitle = "08/10/2022- 11/10/2022")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(Otter_chem1<-ggplot(OtterFR1, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/2000, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*2000), 
                                            name="CO^2 ppm")))
(Otter_SpC1<-ggplot(OtterFR1, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=FullRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))

norm1<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-10" & Date<"2022-09-01"~ 2))
norm1<-filter(norm1,RI==2 )

OtterFR1$GPP_RR<- OtterFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)
OtterFR1$GPP_reduc<- (1-(OtterFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

OtterFR1$ER_RR<- OtterFR1$ER/mean(norm1$ER, na.rm=T)
OtterFR1$ER_reduc<- 1-(OtterFR1$ER/mean(norm1$ER, na.rm=T))*100

ggplot(OtterFR1, aes(x=Date))+
    geom_line(aes(y=GPP_reduc, color="FDOM"), size=1.2)

Otter_RR<-filter(OtterFR1, GPP_reduc> 36)

Otter_RR$day <- as.Date(Otter_RR$Date)
Otter_RR.day <- aggregate(Otter_RR, by=list(Otter_RR$day), FUN='mean')

ggplot(Otter_RR, aes(x=Date))+
    geom_line(aes(y=ER_reduc, color="FDOM"), size=1.2)
    
  


OtterFr2<- Otter %>% mutate(RI = case_when(
  Date> "2023-01-26" & Date<"2023-4-25"~ 2))
OtterFr2<-filter(OtterFr2,RI==2 )


(Otter_NEP2<-ggplot(OtterFr2, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    xlab("Date")+
    ggtitle("Otter",subtitle = "01/26/2023- 04/25/2023")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(1, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(Otter_chem2<-ggplot(OtterFr2, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/1000, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*1000), 
                                            name="CO^2 ppm")))
(Otter_SpC2<-ggplot(OtterFr2, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=FullRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))

norm2<- Otter %>% mutate(RI = case_when(
  Date> "2023-01-26" & Date<"2023-02-18"~ 2))
norm2<-filter(norm2,RI==2 )

OtterFr2$GPP_RR<- OtterFr2$GPPavg/mean(norm1$GPPavg, na.rm=T)
OtterFr2$GPP_reduc<- (1-(OtterFr2$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

OtterFr2$ER_RR<- OtterFr2$ER/mean(norm1$ER, na.rm=T)
OtterFr2$ER_reduc<- 1-(OtterFr2$ER/mean(norm1$ER, na.rm=T))*100

Otter_RR2<-filter(OtterFr2, GPP_reduc> 50)

Otter_RR2$day <- as.Date(Otter_RR2$Date)
Otter_RR2.day <- aggregate(Otter_RR2, by=list(Otter_RR2$day), FUN='mean')


OtterFr2$days <- as.Date(OtterFr2$Date)
OtterFr2 <- OtterFr2 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(a<-ggplot()+
    geom_point(data=OtterFr2,aes(x=ER,y=GPPavg, color=consec), size=5)+
    ggtitle("Otter", subtitle ="01/26/2023-04/25/2023")+
    xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
    ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
    scale_color_gradient(low="darkgreen", high="green")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Otter Days"))


left<-grid.arrange(
  Otter_NEP1 + rremove("xlab"), 
  Otter_chem1 + rremove("xlab"),
  Otter_SpC1 + rremove("xlab"), 
  ncol=1)


right<-grid.arrange(
  Otter_NEP2+ rremove("xlab"), 
  Otter_chem2 + rremove("xlab"),
  Otter_SpC2+ rremove("xlab"), 
  ncol=1)

grid.arrange(arrangeGrob(left, right, ncol = 2))









AMFR2<- AM %>% mutate(RI = case_when(
  Date> "2023-01-10" & Date<"2023-04-16"~ 2))
AMFR2<-filter(AMFR2,RI==2 )

(AM_NEP2<-ggplot(AMFR2, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Allen Mill",subtitle = "01/01/2023- 04/16/2023")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(AM_chem2<-ggplot(AMFR2, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/2000, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    ggtitle("AM 02/2023")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'white'),
          panel.grid.minor = element_line(color = 'white'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*2000), 
                                            name="CO^2 ppm")))
(AM_SpC2<-ggplot(AMFR2, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=FullRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))

norm2<- AllenMill %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-01-28"~ 2))
norm2<-filter(norm2,RI==2 )

AMFR2$GPP_RR<- AMFR2$GPPavg/mean(norm2$GPPavg, na.rm=T)
AMFR2$GPP_reduc<- (1-(AMFR2$GPPavg/mean(norm2$GPPavg, na.rm=T)))*100

AMFR2$ER_RR<- AMFR2$ER/mean(norm2$ER, na.rm=T)
AMFR2$ER_reduc<- 1-(AMFR2$ER/mean(norm2$ER, na.rm=T))*100

AM_RR<-filter(AMFR2, ER_reduc< -135)
AM_RR<-filter(AM_RR, Date< "2023-03-29")

ggplot(AM_RR, aes(x=Date))+
  geom_line(aes(y=ER_reduc, color="ER_reduc"), size=1.2)

AM_RR$day <- as.Date(AM_RR$Date)
AM_RR.day <- aggregate(AM_RR, by=list(AM_RR$day), FUN='mean')

AMFR2$days <- as.Date(AMFR2$Date)
AMFR2 <- AMFR2 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(b<-ggplot()+
  geom_point(data=AMFR2,aes(x=ER,y=GPPavg, color=consec), size=5)+
  ggtitle("Allen Mill", subtitle ="01/18/2023-04/16/2023")+
  xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
  ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
  scale_color_gradient(low="darkblue", high="violet")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.position = "bottom",
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Allen Mill Days"))


AMFR1<- AllenMill %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-11-16"~ 2))
AMFR1<-filter(AMFR1,RI==2 )

(AM_NEP1<-ggplot(AMFR1, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Allen Mill",subtitle = "08/01/2022- 11/16/2022")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(AM_chem1<-ggplot(AMFR1, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    xlab("Date")+
    scale_color_manual(values=c('darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*2000), 
                                            name="CO^2 ppm")))
(AM_SpC1<-ggplot(AMFR1, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=FullRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))

norm1<- AllenMill %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-08-29"~ 2))
norm1<-filter(norm1,RI==2 )

AMFR1$GPP_RR<- AMFR1$GPPavg/mean(norm2$GPPavg, na.rm=T)
AMFR1$GPP_reduc<- (1-(AMFR1$GPPavg/mean(norm2$GPPavg, na.rm=T)))*100

AMFR1$ER_RR<- AMFR1$ER/mean(norm2$ER, na.rm=T)
AMFR1$ER_reduc<- 1-(AMFR1$ER/mean(norm2$ER, na.rm=T))*100

ggplot(AMFR1, aes(x=Date))+
  geom_line(aes(y=ER_reduc, color="ER_reduc"), size=1.2)+
  geom_line(aes(y=DO*-25, color="DO"), size=1.2)

AM_RR1<-filter(AMFR1, Date< "2022-08-29")
AM_RR1<-filter(AM_RR1, Date< "2022-11-01")

AM_RR1$day <- as.Date(AM_RR1$Date)
AM_RR1.day <- aggregate(AM_RR1, by=list(AM_RR1$day), FUN='mean')

AMFR1$days <- as.Date(AMFR1$Date)
AMFR1 <- AMFR1 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(b<-ggplot()+
    geom_point(data=AMFR1,aes(x=ER,y=GPPavg, color=consec), size=5)+
    ggtitle("Allen Mill", subtitle ="01/18/2023-04/16/2023")+
    xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
    ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
    scale_color_gradient(low="darkblue", high="violet")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.position = "bottom",
          legend.title =element_text(size=15),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Allen Mill Days"))

left<-grid.arrange(
  AM_NEP1 + rremove("xlab"), 
  AM_chem1 + rremove("xlab"),
  AM_SpC1 + rremove("xlab"), 
  ncol=1)


right<-grid.arrange(
  AM_NEP2+ rremove("xlab"), 
  AM_chem2 + rremove("xlab"),
  AM_SpC2+ rremove("xlab"), 
  ncol=1)

grid.arrange(arrangeGrob(left, right, ncol = 2))








IchFR2<- Ich %>% mutate(RI = case_when(
  Date> "2023-01-25" & Date<"2023-04-01"~ 2))

IchFR2<-filter(IchFR2, RI==2)

(Ich_NEP2<-ggplot(IchFR2, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Ichetucknee; High stage",subtitle = "01/25/2023- 04/01/2023")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(Ich_chem2<-ggplot(IchFR2, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/500, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*500), 
                                            name="CO^2 ppm")))
(Ich_SpC2<-ggplot(IchFR2, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=LowRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))


norm1<- Ich %>% mutate(RI = case_when(
  Date< "2022-02-15"~ 2))
norm1<-filter(norm1,RI==2 )

IchFR2$GPP_RR<- IchFR2$GPPavg/mean(norm1$GPPavg, na.rm=T)
IchFR2$GPP_reduc<- (1-(IchFR2$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

IchFR2$ER_RR<- IchFR2$ER/mean(norm1$ER, na.rm=T)
IchFR2$ER_reduc<- 1-(IchFR2$ER/mean(norm1$ER, na.rm=T))*100

IchFR2$days <- as.Date(IchFR2$Date)
IchFR2 <- IchFR2 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(c<-ggplot()+
  geom_point(data=IchFR2,aes(x=ER,y=GPPavg, color=consec), size=5)+
  xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
  ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
  scale_color_gradient(low="darkblue", high="violet")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.position = "bottom",
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Ichetucknee Days"))




IchFR1<- Ich %>% mutate(RI = case_when(
  Date> "2022-08-02" & Date<"2022-11-01"~ 2))
IchFR1<-filter(IchFR1,RI==2 )

(Ich_NEP1<-ggplot(IchFR1, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Ichetucknee; High stage",subtitle = "08/02/2022- 11/01/2022")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(Ich_chem1<-ggplot(IchFR1, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/500, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*500), 
                                            name="CO^2 ppm")))
(Ich_SpC1<-ggplot(IchFR1, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=LowRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))

norm1<- Ich %>% mutate(RI = case_when(
  Date> "2022-08-02" & Date<"2022-08-28"~ 2))
norm1<-filter(norm1,RI==2 )

IchFR1$GPP_RR<- IchFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)
IchFR1$GPP_reduc<- (1-(IchFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

IchFR1$ER_RR<- IchFR1$ER/mean(norm1$ER, na.rm=T)
IchFR1$ER_reduc<- 1-(IchFR1$ER/mean(norm1$ER, na.rm=T))*100

IchFR1$days <- as.Date(IchFR1$Date)
ggplot()+
  geom_point(data=IchFR1,aes(x=ER,y=GPPavg, color=days), size=0.8)+
  xlab("Date")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_blank(),
        legend.key.size = unit(1.2, 'cm'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  scale_color_gradient(low="blue", high="red")

left<-grid.arrange(
  Ich_NEP1 + rremove("xlab"), 
  Ich_chem1 + rremove("xlab"),
  Ich_SpC1 + rremove("xlab"), 
  ncol=1)


right<-grid.arrange(
  Ich_NEP2+ rremove("xlab"), 
  Ich_chem2 + rremove("xlab"),
  Ich_SpC2+ rremove("xlab"), 
  ncol=1)

grid.arrange(arrangeGrob(left, right, ncol = 2))








LFFR1<- LF %>% mutate(RI = case_when(
  Date> "2022-08-02" & Date<"2022-10-31"~ 2))
LFFR1<-filter(LFFR1,RI==2 )

(LF_NEP1<-ggplot(LFFR1, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Little Fanning; High stage",subtitle = "08/02/2022- 10/31/2022")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(LF_chem1<-ggplot(LFFR1, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/2000, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*2000), 
                                            name="CO^2 ppm")))
(LF_SpC1<-ggplot(LFFR1, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=LowRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))


norm1<- LF %>% mutate(RI = case_when(
  Date> "2022-07-02" & Date<"2022-08-02"~ 2))
norm1<-filter(norm1,RI==2 )

ggplot(norm1, aes(x=Date))+
  geom_line(aes(y=GPPavg, color="ER"), size=1)


LFFR1$GPP_RR<- LFFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)
LFFR1$GPP_reduc<- (1-(LFFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

LFFR1$ER_RR<- LFFR1$ER/mean(norm1$ER, na.rm=T)
LFFR1$ER_reduc<- 1-(LFFR1$ER/mean(norm1$ER, na.rm=T))*100

LFFR1$days <- as.Date(LFFR1$Date)
ggplot()+
  geom_point(data=LFFR1,aes(x=ER,y=GPPavg, color=days), size=0.8)+
  xlab("Date")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_blank(),
        legend.key.size = unit(1.2, 'cm'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  scale_color_gradient(low="blue", high="red")



LFFR2<- LF %>% mutate(RI = case_when(
  Date> "2023-02-02" & Date<"2023-03-26"~ 2))
LFFR2<-filter(LFFR2,RI==2 )

(LF_NEP2<-ggplot(LFFR2, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ylab(expression(paste('g'~O[2]/m^2/'day')))+
    ggtitle("Little Fanning; High stage",subtitle = "02/02/2023- 03/26/2023")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(LF_chem2<-ggplot(LFFR2, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/500, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*500), 
                                            name="CO^2 ppm")))
(LF_SpC2<-ggplot(LFFR2, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=LowRangeSpC/5, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.7, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*5), 
                                            name="Conductivity")))



norm2<- LF %>% mutate(RI = case_when(
  Date> "2023-02-02" & Date<"2023-02-18"~ 2))
norm2<-filter(norm2,RI==2 )

LFFR2$GPP_RR<- LFFR2$GPPavg/mean(norm1$GPPavg, na.rm=T)
LFFR2$GPP_reduc<- (1-(LFFR2$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

LFFR2$ER_RR<- LFFR2$ER/mean(norm1$ER, na.rm=T)
LFFR2$ER_reduc<- 1-(LFFR2$ER/mean(norm1$ER, na.rm=T))*100

LFFR2$days <- as.Date(LFFR2$Date)
LFFR2 <- LFFR2 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(b<-ggplot()+
  geom_point(data=LFFR2,aes(x=ER,y=GPPavg, color=consec), size=5)+
  xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
  ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
  scale_color_gradient(low="darkgreen", high="green")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.position = "bottom",
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Little Fanning Days"))

left<-grid.arrange(
  LF_NEP1 + rremove("xlab"), 
  LF_chem1 + rremove("xlab"),
  LF_SpC1 + rremove("xlab"), 
  ncol=1)


right<-grid.arrange(
  LF_NEP2+ rremove("xlab"), 
  LF_chem2 + rremove("xlab"),
  LF_SpC2+ rremove("xlab"), 
  ncol=1)

grid.arrange(arrangeGrob(left, right, ncol = 2))










GBFR1<- GB %>% mutate(RI = case_when(
  Date> "2022-08-11" & Date<"2022-10-20"~ 2))
GBFR1<-filter(GBFR1,RI==2 )



(a<-ggplot(GBFR1, aes(x=Date))+
    geom_line(aes(y=ER, color="ER"), size=1.2)+
    geom_line(aes(y=GPPavg, color="GPP"), size=1.)+
    geom_line(aes(y=NEP, color="NEP"), size=1.2)+
    xlab("Date")+
    ggtitle("Gilchrist Blue; High stage",subtitle = "02/02/2023- 03/26/2023")+
    scale_color_manual(values=c('red','darkgreen','blue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(1.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  "))

(b<-ggplot(GBFR1, aes(x=Date))+
    geom_line(aes(y=DOavg, color="DO mg/L"), size=1.)+
    geom_line(aes(y=CO2avg/2000, color="CO^2 ppm"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('orange','darkblue'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(1.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="Daily DO (mg/L)",
                       sec.axis = sec_axis( trans=~.*(1*2000), 
                                            name="CO^2 ppm")))
GBFR1<-filter(GBFR1,FDOM<5)
(c<-ggplot(GBFR1, aes(x=Date))+
    geom_line(aes(y=FDOM, color="FDOM"), size=1.2)+
    geom_line(aes(y=LowRangeSpC/200, color="Conductivity"), size=1.2)+
    xlab("Date")+
    scale_color_manual(values=c('purple','brown'))+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(1.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "right",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "  ")+
    scale_y_continuous(name="FDOM",
                       sec.axis = sec_axis( trans=~.*(1*200), 
                                            name="Conductivity")))
plot_grid(a,b,c,ncol=1)

norm1<- GB %>% mutate(RI = case_when(
  Date> "2022-08-11" & Date<"2022-08-28"~ 2))
norm1<-filter(norm2,RI==2 )

ggplot(norm1, aes(x=Date))+
  geom_line(aes(y=ER, color="ER"), size=1)


GBFR1$GPP_RR<- GBFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)
GBFR1$GPP_reduc<- (1-(GBFR1$GPPavg/mean(norm1$GPPavg, na.rm=T)))*100

GBFR1$ER_RR<- GBFR1$ER/mean(norm1$ER, na.rm=T)
GBFR1$ER_reduc<- 1-(GBFR1$ER/mean(norm1$ER, na.rm=T))*100

GBFR1$days <- as.Date(GBFR1$Date)
GBFR1 <- GBFR1 %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

(a<-ggplot()+
    geom_point(data=GBFR1,aes(x=ER,y=GPPavg, color=consec), size=5)+
    xlab(expression(paste('ER'~'g'~O[2]/m^2/'day')))+
    ylab(expression(paste('GPP'~'g'~O[2]/m^2/'day')))+
    scale_color_gradient(low="red", high="orange")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.position = "bottom",
          legend.title =element_text(size=15),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Gilchrist Blue Days"))

