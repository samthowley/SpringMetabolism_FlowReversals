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

##### constants#####
flux<-expression(paste('Metabolic rate'~(g~O[2]/m^2/'day')))
GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
DO<-"DO mg/L"
hdiff<-expression(paste( h[i]-h[min]))
pCO2<-expression(paste(CO[2]~"ppm"))
h<-"Stage (m)"
depth<-"Measured Sensor Stage (m)"
u<-expression(paste('u'~("m"~s^-1)))
FR<-expression(paste("Flow Reversal"~ (h[reversal])))
BO<-expression(paste("Brownout"~ (h[brown])))
high<-expression(paste("High Stage Event"~ (h[high])))
cols=c(hh="blue", hb='chocolate4', hrev="darkgray")
yax<-expression(paste("Deviations from"~ h[n]~ "Mean"))


FR<-expression(paste("Flow Reversal"))
BO<-expression(paste("Brownout"))
high<-expression(paste("High Stage Event"))
hdiff<-('h'~Delta)

#get data####
OS<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx",
                col_types = c("date", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric"))
OS$stage_diff<-OS$stage-min(OS$stage, na.rm=T)
OS$u<-(OS$stage*-0.0868+0.1579)*100
OS<- OS %>% mutate(RI = case_when(
  stage<=0.82 ~ "hn",
  stage<1.3 ~ "hh",
  stage>=1.3 ~ "hb"))
OS_hn<-filter(OS, RI=="hn")
OS$RI[is.na(OS$RI)] <- 'hh'



GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx",
                col_types = c("date", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-08-29')
GB$stage_diff<-GB$stage-min(GB$stage, na.rm=T)
GB$u<-(GB$stage*-0.768+0.51)*100
GB<- GB %>% mutate(RI = case_when(
  stage<0.55 ~ "hn",
  stage>=0.55 ~ "hh"))
GB_hn<-filter(GB, RI=="hn")


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')
LF$stage_diff<-LF$stage-min(LF$stage, na.rm=T)
LF$u<- (-0.656*LF$stage + 0.44)*100
LF<- LF %>% mutate(RI = case_when(
  stage<0.42 ~ "hn",
  stage<0.65 ~ "hn",
  stage>=0.65 ~ "hh"))
LF_hn<-filter(LF, RI=="hn")


AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-08-29')
AM$stage_diff<-AM$stage-min(AM$stage, na.rm=T)
AM$"u"<-(AM$stage*-1.89+1.4)*100
AM<- AM %>% mutate(RI = case_when(
  stage<0.75 ~ "hn",
  stage<1.37 ~ "hh",
  stage<2.14 ~ "hb",
  stage>=2.14 ~"hrev"))
AM$RI[is.na(AM$RI)] <- 'hb'
AM_hn<-filter(AM, RI=="hn")



ID <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
ID<-filter(ID, Date> '2022-07-20' & Date <='2023-08-21')
ID$stage_diff<-ID$stage-min(ID$stage, na.rm=T)
ID$u<-(ID$stage*-4.1+2.33)*100
ID<- ID %>% mutate(RI = case_when(
  stage<=0.93 ~ "hn",
  stage<1.37 ~ "hh",
  stage>=1.37 ~ "hh"))
ID$RI[is.na(ID$RI)] <- 'hn'
ID_hn<-filter(ID, RI=="hn")

availableData <- whatNWISdata(siteNumber = "02322700")

startDate <- "2022-07-12"
endDate <- "2023-09-17"
parameterCd <- c('00010','00300','00065', '00060','00400', '00095')
vent<-'02322700'

IDetucknee<-readNWISuv(vent,parameterCd,startDate,endDate)
IDetucknee<- IDetucknee %>% mutate(minute = minute(dateTime))
IDetucknee<-filter(IDetucknee, minute==0)

names(IDetucknee)
bridge<-IDetucknee[,c(3,4,6,8,10,12,14)]
US27bridge<-rename(bridge, 'Date'="dateTime",
               'Mouth_Temp_C'="X_00010_00000",
               'Q'="X_00060_00000",
               "depth"="X_00065_00000",
               "DO.obs"="X_00300_00000",
               'SpC_avg'='X_00095_00000',
               'pH_avg'='X_00400_00000')

US27bridge$day <- as.Date(US27bridge$Date)
US27bridge<- aggregate(US27bridge, by=list(US27bridge$day), FUN='mean')

US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$depth<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-1.5)
US27bridge$stage_diff<-US27bridge$depth-min(US27bridge$depth, na.rm=T)
US27bridge$u<-US27bridge$Q/(US27bridge$depth*15)
US27bridge<- US27bridge %>% mutate(RI = case_when(
  depth<1.12 ~ "hn",
  depth>=1.12 ~ "hn"))
IU_hn<-filter(US27bridge, RI=="hn")

###carpentry#####
US27bridge$pH_deviatin<-mean(IU_hn$pH, na.rm = T)-US27bridge$pH
US27bridge$SpC_deviatin<-mean(IU_hn$SpC, na.rm = T)-US27bridge$SpC
US27bridge$temp_deviatin<-mean(IU_hn$Mouth_Temp_C, na.rm = T)-US27bridge$Mouth_Temp_C

AM$RI <- factor(AM$RI  , levels=c("hn","hh","hb", "hrev"))
OS$RI <- factor(OS$RI  , levels=c("hn","hh","hb"))
LF$RI <- factor(LF$RI  , levels=c("hn","hh"))
GB$RI <- factor(GB$RI  , levels=c("hn","hh"))
ID$RI <- factor(ID$RI  , levels=c("hn","hh"))
US27bridge$RI <- factor(US27bridge$RI  , levels=c("hn","hh"))

AM$site <- "AM"
OS$site <- "OS"
LF$site <- "LF"
GB$site <- "GB"
ID$site <- "ID"
US27bridge$site <- "IU"

OS$pH_deviatin<-OS$pH-mean(OS_hn$pH, na.rm = T)
OS$FDOM_deviatin<-OS$FDOM-mean(OS_hn$FDOM, na.rm = T)
OS$SpC_deviatin<-OS$SpC_avg-mean(OS_hn$SpC_avg, na.rm = T)
OS$temp_deviatin<-mean(OS_hn$Mouth_Temp_C, na.rm = T)-OS$Mouth_Temp_C

GB$pH_deviatin<-GB$pH-mean(GB_hn$pH, na.rm = T)
GB$FDOM_deviatin<-GB$FDOM-mean(GB_hn$FDOM, na.rm = T)
GB$SpC_deviatin<-GB$SpC_avg-mean(GB_hn$SpC_avg, na.rm = T)
GB$temp_deviatin<-mean(GB_hn$Mouth_Temp_C, na.rm = T)-GB$Mouth_Temp_C

LF$pH_deviatin<-LF$pH-mean(LF_hn$pH, na.rm = T)
LF$SpC_deviatin<-LF$SpC_avg-mean(LF_hn$SpC_avg, na.rm = T)
LF$temp_deviatin<-mean(LF_hn$Mouth_Temp_C, na.rm = T)-LF$Mouth_Temp_C

AM$pH_deviatin<-AM$pH-mean(AM_hn$pH, na.rm = T)
AM$FDOM_deviatin<-AM$FDOM-mean(AM_hn$FDOM, na.rm = T)
AM$SpC_deviatin<-AM$SpC_avg-mean(AM_hn$SpC_avg, na.rm = T)
AM$temp_deviatin<-mean(AM_hn$Mouth_Temp_C, na.rm = T)-AM$Mouth_Temp_C

ID$pH_deviatin<-ID$pH-mean(ID_hn$pH, na.rm = T)
ID$FDOM_deviatin<-ID$FDOM-mean(ID_hn$FDOM, na.rm = T)
ID$SpC_deviatin<-ID$SpC_avg-mean(ID_hn$SpC_avg, na.rm = T)
ID$temp_deviatin<-mean(ID_hn$Mouth_Temp_C, na.rm = T)-ID$Mouth_Temp_C

####boxplots#####
###SpC box####

names(US27bridge)
spc<-c("SpC_avg", "stage_diff", "RI", "SpC_deviatin", "site")
OS_spc<-OS[,spc]
ID_spc<-ID[,spc]
GB_spc<-GB[,spc]
LF_spc<-LF[,spc]
AM_spc<-AM[,spc]
IU_spc<-US27bridge[,spc]

SPC<-rbind(IU_spc, ID_spc, GB_spc, LF_spc,OS_spc, AM_spc)
SPC <- SPC[complete.cases(SPC), ]

SPC$site <- factor(SPC$site  , levels=c("IU","ID","GB","LF","AM","OS"))
SPC<-filter(SPC, RI== "hh"|RI== "hb"|RI== "hrev")

(SpC_box<-ggplot(SPC, aes(x=site, y=SpC_deviatin, fill=RI)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
  ylab(expression(paste(h[norm]~"SpC"-SpC[i])))+
  scale_fill_manual(values=cols)+
  ggtitle("")+
  xlab(yax)+
  geom_hline(yintercept =0, color="red")+
  scale_x_discrete(labels=c("hb" = expression(h[brown]),
                            "hrev" = expression(h[reversal]),
                            "hh" = expression(h[high]),
                            "hn" = expression(h[norm])))+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


###ph box####

names(ID)
ph<-c("pH_avg", "stage_diff", "RI", "pH_deviatin", "site")

OS_ph<-OS[,ph]
ID_ph<-ID[,ph]
GB_ph<-GB[,ph]
LF_ph<-LF[,ph]
AM_ph<-AM[,ph]
IU_ph<-US27bridge[,ph]
ph<-rbind(IU_ph, ID_ph, GB_ph, LF_ph,OS_ph, AM_ph)
ph <- ph[complete.cases(ph), ]

ph$site <- factor(ph$site  , levels=c("IU","ID","GB","LF","AM","OS"))
ph<-filter(ph, RI== "hh"|RI== "hb"|RI== "hrev")


(pH_box<-ggplot(ph, aes(x=site, y=pH_deviatin, fill=RI)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
  ylab(expression(paste(h[norm]~"pH"-pH[i])))+
  ggtitle("Chemical Response to Stage")+
  xlab(yax)+
  geom_hline(yintercept =0, color="red")+
    scale_x_discrete(labels=c("hb" = expression(h[brown]),
                              "hrev" = expression(h[reversal]),
                              "hh" = expression(h[high]),
                              "hn" = expression(h[norm])))+
    scale_fill_manual(values=cols)+
  theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 17),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



###Temp box####


temp<-c("Mouth_Temp_C", "stage_diff", "RI","temp_deviatin", "site")

OS_temp<-OS[,temp]
ID_temp<-ID[,c(temp)]
GB_temp<-GB[,temp]
LF_temp<-LF[,temp]
AM_temp<-AM[,temp]
IU_temp<-US27bridge[,temp]
temp<-rbind(IU_temp, ID_temp, GB_temp, LF_temp,OS_temp, AM_temp)
temp <- temp[complete.cases(temp), ]

temp$site <- factor(temp$site  , levels=c("IU","ID","GB","LF","AM","OS"))
temp<-filter(temp, RI== "hh"|RI== "hb"|RI== "hrev")

(temp_box<-ggplot(temp, aes(x=site, y=temp_deviatin, fill=RI)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
  ylab(expression(paste(h[norm]~"Temp."-"Temp."[i]~("C°"))))+  xlab(yax)+
  ggtitle("")+
  scale_fill_manual(values=cols)+
    scale_x_discrete(labels=c("hb" = expression(h[brown]),
                              "hrev" = expression(h[reversal]),
                              "hh" = expression(h[high]),
                              "hn" = expression(h[norm])))+
    geom_hline(yintercept =0, color="red")+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17),
          axis.title.x =element_text(size = 17),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))




###FDOM box####

names(US27bridge)
fdom<-c("FDOM", "stage_diff", "RI", "ER","FDOM_deviatin", "site")
OS_fdom<-OS[,fdom]
ID_fdom<-ID[,fdom]
GB_fdom<-GB[,fdom]
AM_fdom<-AM[,fdom]

FDOM<-rbind(ID_fdom, GB_fdom,OS_fdom, AM_fdom)
FDOM <- FDOM[complete.cases(FDOM), ]
FDOM$site <- factor(FDOM$site  , levels=c("ID","GB","AM","OS"))
FDOM<-filter(FDOM, RI== "hh"|RI== "hb"|RI== "hrev")
cols=c(hh="blue", hb='chocolate4', hrev="darkgray")


(FDOM_box<-ggplot(FDOM, aes(x=site, y=FDOM_deviatin, fill=RI)) +
  geom_boxplot(outlier.colour="black", outlier.size=1)+
  ylab(expression(paste(h[norm]~"FDOM"-"FDOM (ppb QS)"[i])))+  xlab(yax)+
  ggtitle("")+
  scale_fill_manual(name="",values=cols,
                    labels=c(expression("High-Stage Event"~(h[high])),
                                         expression("Brownout"~(h[brown])),
                                         expression("Flow Reversal"~(h[reversal]))))+
    geom_hline(yintercept =0, color="red")+
    theme(axis.text.x = element_text(size = 17, angle=0),
        axis.text.y = element_text(size = 17, angle=0),
        axis.title.y =element_text(size = 17),
        axis.title.x =element_text(size = 17),
        plot.title = element_text(size = 17),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=17),
        legend.title =element_text(size=17),
        legend.position = "bottom",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

#############
chembox<-plot_grid(pH_box, SpC_box,temp_box,FDOM_box, ncol=1, align='v')
ggsave(filename="chembox.jpeg", 
       plot = chembox, 
       width =12, 
       height = 14.5, 
       units = "in")

#####cherry pick######
library(mmand)
LFFR<-filter(LF,  Date> "2023-02-01" & Date<"2023-04-13")
LFFR$stage<-gaussianSmooth(LFFR$stage, 120)
LFFR$velocity<-0.158-0.087*LFFR$stage
LFFR$stage_diff<-gaussianSmooth(LFFR$stage_diff, 120)


(ch<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=stage_diff), color="black", linewidth=0.8)+
    ylab(hdiff)+xlab('Date')+
    ggtitle(high, subtitle = "LF")+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 13, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

u<-expression(atop('Velocity',
                    ("m"~s^-1)))

(cu<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=velocity), color="black", linewidth=0.8)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 14, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(c<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+
    theme(axis.text.x = element_blank(),
          axis.text.y.right = element_text(size = 17, angle=0, color="purple"),
          axis.text.y.left = element_text(size = 17, angle=0, color="black"),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_blank(),
          axis.title.x =element_blank(),
          legend.position = "none",
          plot.title = element_blank(),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(LF<-plot_grid(ch,cu,c, align = "v", ncol = 1, rel_heights = c(0.3,0.2,0.5)))



OtBO<-filter(OS,  Date> "2022-08-25" & Date<"2022-10-11")
OtBO$u<-gaussianSmooth(OtBO$u, 120)
OtBO$velocity<-0.158-0.087*OtBO$stage
OtBO$stage_diff<-gaussianSmooth(OtBO$stage_diff, 120)

(bu<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_blank(),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(bh<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=stage_diff), color="black", linewidth=0.8)+
    ylab(hdiff)+xlab('Date')+
    ggtitle(BO, subtitle = "OS")+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_blank(),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 17),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(b<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2)) +
    theme(axis.text.x = element_blank(),
          axis.text.y.right = element_text(size = 17, angle=0, color="purple"),
          axis.text.y.left = element_text(size = 17, angle=0, color="black"),
          axis.title.y =element_blank(),
          axis.title.y.right =element_blank(),
          axis.title.x =element_blank(),
          legend.position = "none",
          plot.title = element_blank(),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(OS<-plot_grid(bh,bu,b, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))




AMFR<-filter(AM,  Date> "2023-02-01" & Date<"2023-04-01")
AMFR$stage_diff<-gaussianSmooth(AMFR$stage_diff, 120)
AMFR$velocity<-0.5-0.255*AMFR$stage

(au<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=velocity), color="black", linewidth=0.8)+
    scale_y_continuous(n.breaks=3)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_blank(),
          axis.title.y.right =element_blank(),
          axis.title.x =element_blank(),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(ah<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=stage_diff), color="black", linewidth=0.8)+
    ylab(hdiff)+xlab('Date')+
    ggtitle(FR, subtitle = "AM")+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_blank(),
          axis.title.y.right =element_blank(),
          axis.title.x =element_blank(),
          plot.title = element_text(size = 15),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(a<-ggplot(AMFR, aes(x=Date))+
  geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
  geom_line(aes(y=DO), color="black", linewidth=0.8)+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_y_continuous(
    name = "DO mg/L",
    sec.axis = sec_axis( trans=~.*1000, name=pCO2)) +
  theme(axis.text.x = element_blank(),
        axis.text.y.right = element_text(size = 17, angle=0, color="purple"),
        axis.text.y.left = element_text(size = 17, angle=0, color="black"),
        axis.title.y =element_blank(),
        axis.title.y.right =element_text(size = 17, angle=270, color="purple"),
        axis.title.x =element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(AM<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))





cherrypick<-plot_grid(LF,OS,AM, nrow=1)

#poster
ggsave(filename="cherrypick.jpeg", 
       plot = cherrypick, 
       width =14, 
       height = 6, 
       units = "in")

ggsave(filename="cherrypick.jpeg", 
       plot = cherrypick, 
       width =16, 
       height = 9, 
       units = "in")

plot_grid(a, b, ncol=1)

#####cherry pick NEP######

AMFR<-filter(AM,  Date> "2023-01-26" & Date<"2023-04-01")
AMFR$ER <- na_interpolation(AMFR$ER, option='linear')
AMFR$GPPavg <- na_interpolation(AMFR$GPPavg, option='linear')
AMFR$days <- as.Date(AMFR$Date)

AMFR <- AMFR %>%
  arrange(days) %>%
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>%
  ungroup()

AMFR$disturb_count<-AMFR$consec-28


(au<-ggplot(AMFR, aes(x=disturb_count))+
    geom_line(aes(y=GPPavg, color="GPP"), size=2)+
    geom_line(aes(y=ER, color="ER"), size=2)+
    ylab(flux)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    ggtitle("AM", subtitle = "Bownout")+
    scale_color_manual(name= "", values = c('darkred','darkgreen'))+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



OtBO<-filter(OS,  Date> "2022-08-21" & Date<"2022-11-01")
OtBO$ER <- na_interpolation(OtBO$ER, option='linear')
OtBO$GPPavg <- na_interpolation(OtBO$GPPavg, option='linear')


(bu<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=GPPavg), color="darkgreen", linewidth=0.8)+
    geom_line(aes(y=ER), color="darkred", linewidth=0.8)+
    ylab(flux)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))




LFFR<-filter(LF,  Date> "2023-01-01" & Date<"2023-05-03")
LFFR$ER <- na_interpolation(LFFR$ER, option='linear')
LFFR$GPPavg <- na_interpolation(LFFR$GPPavg, option='linear')

(ch<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=GPPavg), color="darkgreen", linewidth=0.8)+
    geom_line(aes(y=ER), color="darkred", linewidth=0.8)+
    ylab(hdiff)+xlab('Date')+
    ggtitle(high, subtitle = "LF")+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))



plot_grid(au,bu,ch, nrow=1)


######recovery########

AMBO<- AllenMill %>% mutate(RI = case_when(
  Date> "2022-07-11" & Date<"2022-11-30"~ 2))
AMBO<-filter(AMBO, RI== "2")

AMBO <- AMBO %>%
  arrange(days) %>%
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>%
  ungroup()

AMBO$disturb_count<-AMBO$consec-65

AMBO$ER <- na_interpolation(AMBO$ER, option='linear')
AMBO$GPPavg <- na_interpolation(AMBO$GPPavg, option='linear')

ggplot(AMBO, aes(x=disturb_count))+
  geom_line(aes(y=GPPavg), size=0.8)+
  geom_vline(xintercept = 0)+geom_vline(xintercept = -30)

prior<-filter(AMBO, disturb_count< -30)
GPP_prior<-mean(prior$GPPavg, na.rm=T)
ER_prior<-mean(prior$ER, na.rm=T)
h_prior<-mean(prior$stage, na.rm=T)

AMBO$GPPavg<-AMBO$GPPavg/GPP_prior
AMBO$ER<-AMBO$ER/ER_prior
AMBO$stage<-AMBO$stage/h_prior

AMBO$GPPavg<-gaussianSmooth(AMBO$GPPavg, 2)
AMBO$ER<-gaussianSmooth(AMBO$ER, 2)


(dGPP<-ggplot(AMBO, aes(x=disturb_count))+
    geom_line(aes(y=GPPavg, color="GPP"), size=2)+
    ylab("GPP")+xlab('Count')+
    geom_hline(yintercept = 1, linetype='dashed')+
    ggtitle("AM", subtitle = "Bownout")+
    scale_color_manual(name= "", values = c('darkgreen'))+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(dER<-ggplot(AMBO, aes(x=disturb_count))+
    geom_line(aes(y=ER, color="ER"), size=2)+
    ylab("ER")+xlab('Count')+
    geom_hline(yintercept = 1, linetype='dashed')+
    ggtitle("")+
    scale_color_manual(name= "", values = c('darkred'))+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(dh<-ggplot(AMBO, aes(x=disturb_count))+
    geom_line(aes(y=stage, color="Stage"), size=2)+
    ylab(h)+xlab('Count')+
    geom_hline(yintercept = 1, linetype='dashed')+
    ggtitle(" ")+
    scale_color_manual(name= "", values = c('blue'))+
    theme(axis.text.x = element_text(size = 17, angle=0),
          axis.text.y = element_text(size = 17, angle=0),
          axis.title.y =element_text(size = 17, color = "black"),
          axis.title.y.right =element_text(size = 17, color = "blue"),
          axis.title.x =element_text(size = 17, angle=0),
          plot.title = element_text(size = 17, angle=0),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))
plot_grid(dGPP, dER, dh, nrow=1)

#####histogram#####


(stage_hist<-ggplot(LF, aes(x=stage)) + 
  geom_histogram(binwidth=0.01)+
  ylab('hours')+xlab("Stage (m)")+
  ggtitle("LF")+
  geom_vline(xintercept = 0.42, colour="darkred", size=2)+  
  geom_vline(xintercept = 0.65, colour="darkred", size=2)+
  theme(axis.text.x = element_text(size = 20, angle=0),
        axis.text.y = element_text(size = 20, angle=0),
        axis.title.y =element_text(size = 20),
        axis.title.x =element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=17),
        legend.title =element_text(size=17),
        legend.position = "bottom",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

ggsave(filename="stage_hist.jpeg", 
       plot = stage_hist, 
       width =15, 
       height = 9, 
       units = "in")
