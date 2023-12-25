rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(lubridate)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library(dataRetrieval)
library('StreamMetabolism')
library("hydroTSM")
library(rnoaa)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(zoo)
library(purrr)
library(ggExtra)

flux<-expression(paste('g'~O[2]/m^2/'day'))
NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
colstwo <- c("ER" = "red","NEP" = "blue","GPP" = "green3")
x<-c("GPPavg","ER","NEP","Date" )


#get data####
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx",
                   col_types = c("date", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-09-07')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx",
                col_types = c("date", "numeric", "numeric",
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
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')

AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-09-07')

Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx",
                  col_types = c("date", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric"))
Ich<-filter(Ich, Date> '2022-07-20' & Date <='2023-09-07')



US27bridge <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
US27bridge$GPPavg <- na_interpolation(US27bridge$GPPavg, option='linear')
US27bridge$ER <- na_interpolation(US27bridge$ER, option='linear')

#####CoV US27###########
US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$depth<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-1.5)


US27_cov<-US27bridge[,x]
US27_cov$day <- as.Date(US27_cov$Date)
US27_cov<- aggregate(US27_cov, by=list(US27_cov$day), FUN='mean')

US27_cov$day1<-US27_cov$day + 1
US27_cov$day10<-US27_cov$day + 10

US27_cov_day0<-US27_cov[,c(6,2,3,4)]
US27_cov_day0 <- US27_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

US27_cov_day1<-US27_cov[,c(7,2,3,4)]
US27_cov_day1<-rename(US27_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

US27_cov_day10<-US27_cov[,c(8,2,3,4)]
US27_cov_day10<-rename(US27_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')





US27_cov_1<-left_join(US27_cov_day0, US27_cov_day1, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day10, by='day')

US27_cov_1<-na.omit(US27_cov_1)

ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

US27_cov_1ER<-US27_cov_1[,ER]
US27_cov_1GPP<-US27_cov_1[,GPP]
US27_cov_1NEP<-US27_cov_1[,NEP]

corrplot(cor(x=US27_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

US27_covER<-as.data.frame(as.table(cor(x=US27_cov_1ER,use="complete.obs")))
US27_covGPP<-as.data.frame(as.table(cor(x=US27_cov_1GPP,use="complete.obs")))

US27_covGPP<-filter(US27_covGPP, Var1=="GPPavg")
US27_covER<-filter(US27_covER, Var1=="ER")

US27_covGPP$site<-'IU'
US27_covER$site<-'IU'

US27_covGPP$ID<-1
US27_covER$ID<-1

#####CoV Ich###########

Ich_cov<-Ich[,x]
Ich_cov$day <- as.Date(Ich_cov$Date)
Ich_cov<- aggregate(Ich_cov, by=list(Ich_cov$day), FUN='mean')

Ich_cov$day1<-Ich_cov$day + 1
Ich_cov$day10<-Ich_cov$day + 10


Ich_cov_day0<-US27_cov[,c(6,2,3,4)]
Ich_cov_day0 <- Ich_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

Ich_cov_day1<-US27_cov[,c(7,2,3,4)]
Ich_cov_day1<-rename(Ich_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

Ich_cov_day10<-US27_cov[,c(8,2,3,4)]
Ich_cov_day10<-rename(Ich_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')




Ich_cov_1<-left_join(Ich_cov_day0, Ich_cov_day1, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day10, by='day')

Ich_cov_1<-na.omit(Ich_cov_1)

ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

Ich_cov_1ER<-Ich_cov_1[,ER]
Ich_cov_1GPP<-Ich_cov_1[,GPP]
Ich_cov_1NEP<-Ich_cov_1[,NEP]

Ich_covER<-as.data.frame(as.table(cor(x=Ich_cov_1ER,use="complete.obs")))
Ich_covGPP<-as.data.frame(as.table(cor(x=Ich_cov_1GPP,use="complete.obs")))

Ich_covGPP<-filter(Ich_covGPP, Var1=="GPPavg")
Ich_covER<-filter(Ich_covER, Var1=="ER")

Ich_covGPP$site<-'Ich'
Ich_covER$site<-'Ich'

Ich_covGPP$ID<-2
Ich_covER$ID<-2


####LF CoV#####


LF_cov<-LF[,x]

LF_cov$day <- as.Date(LF_cov$Date)
LF_cov<- aggregate(LF_cov, by=list(LF_cov$day), FUN='mean')

LF_cov$day1<-LF_cov$day + 1
LF_cov$day10<-LF_cov$day + 10

LF_cov_day0<-LF_cov[,c(6,2,3,4)]
LF_cov_day0 <- LF_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

LF_cov_day1<-LF_cov[,c(7,2,3,4)]
LF_cov_day1<-rename(LF_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')


LF_cov_day10<-LF_cov[,c(8,2,3,4)]
LF_cov_day10<-rename(LF_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')




LF_cov_1<-left_join(LF_cov_day0, LF_cov_day1, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day10, by='day')


LF_cov_1<-na.omit(LF_cov_1)

ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

LF_cov_1ER<-LF_cov_1[,ER]
LF_cov_1GPP<-LF_cov_1[,GPP]
LF_cov_1NEP<-LF_cov_1[,NEP]

corrplot(cor(x=LF_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

LF_covER<-as.data.frame(as.table(cor(x=LF_cov_1ER,use="complete.obs")))
LF_covGPP<-as.data.frame(as.table(cor(x=LF_cov_1GPP,use="complete.obs")))

LF_covGPP<-filter(LF_covGPP, Var1=="GPPavg")
LF_covER<-filter(LF_covER, Var1=="ER")

LF_covGPP$site<-'LF'
LF_covER$site<-'LF'

LF_covGPP$ID<- 4
LF_covER$ID<- 4


####GB CoV#####


GB_cov<-GB[,x]

GB_cov$day <- as.Date(GB_cov$Date)
GB_cov<- aggregate(GB_cov, by=list(GB_cov$day), FUN='mean')


GB_cov$day1<-GB_cov$day + 1
GB_cov$day10<-GB_cov$day + 10


GB_cov_day0<-GB_cov[,c(6,2,3,4)]
GB_cov_day0 <- GB_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

GB_cov_day1<-GB_cov[,c(7,2,3,4)]
GB_cov_day1<-rename(GB_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')


GB_cov_day10<-GB_cov[,c(8,2,3,4)]
GB_cov_day10<-rename(GB_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')



GB_cov_1<-left_join(GB_cov_day0, GB_cov_day1, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day10, by='day')


GB_cov_1<-na.omit(GB_cov_1)

ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

GB_cov_1ER<-GB_cov_1[,ER]
GB_cov_1GPP<-GB_cov_1[,GPP]
GB_cov_1NEP<-GB_cov_1[,NEP]

c<-corrplot(cor(x=GB_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

GB_covER<-as.data.frame(as.table(cor(x=GB_cov_1ER,use="complete.obs")))
GB_covGPP<-as.data.frame(as.table(cor(x=GB_cov_1GPP,use="complete.obs")))

GB_covGPP<-filter(GB_covGPP, Var1=="GPPavg")
GB_covER<-filter(GB_covER, Var1=="ER")

GB_covGPP$site<-'GB'
GB_covER$site<-'GB'

GB_covGPP$ID<-3
GB_covER$ID<-3



####Otter CoV#####

Otter_cov<-Otter[,x]

Otter_cov$day <- as.Date(Otter_cov$Date)
Otter_cov<- aggregate(Otter_cov, by=list(Otter_cov$day), FUN='mean')


Otter_cov$day1<-Otter_cov$day + 1
Otter_cov$day10<-Otter_cov$day + 10

Otter_cov_day0<-Otter_cov[,c(6,2,3,4)]
Otter_cov_day0 <- Otter_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

Otter_cov_day1<-Otter_cov[,c(7,2,3,4)]
Otter_cov_day1<-rename(Otter_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')


Otter_cov_day10<-Otter_cov[,c(8,2,3,4)]
Otter_cov_day10<-rename(Otter_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')




Otter_cov_1<-left_join(Otter_cov_day0, Otter_cov_day1, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day10, by='day')





ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

Otter_cov_1ER<-Otter_cov_1[,ER]
Otter_cov_1GPP<-Otter_cov_1[,GPP]
Otter_cov_1NEP<-Otter_cov_1[,NEP]

d<-corrplot(cor(x=Otter_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

Otter_covER<-as.data.frame(as.table(cor(x=Otter_cov_1ER,use="complete.obs")))
Otter_covGPP<-as.data.frame(as.table(cor(x=Otter_cov_1GPP,use="complete.obs")))

Otter_covGPP<-filter(Otter_covGPP, Var1=="GPPavg")
Otter_covER<-filter(Otter_covER, Var1=="ER")

Otter_covGPP$site<-'Otter'
Otter_covER$site<-'Otter'

Otter_covGPP$ID<-5
Otter_covER$ID<-5


####AM CoV#####
AM_cov<-AM[,x]

AM_cov$day <- as.Date(AM_cov$Date)
AM_cov<- aggregate(AM_cov, by=list(AM_cov$day), FUN='mean')


AM_cov$day1<-AM_cov$day + 1
AM_cov$day10<-AM_cov$day + 10


AM_cov_day0<-AM_cov[,c(6,2,3,4)]
AM_cov_day0 <- AM_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

AM_cov_day1<-AM_cov[,c(7,2,3,4)]
AM_cov_day1<-rename(AM_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')


AM_cov_day10<-AM_cov[,c(8,2,3,4)]
AM_cov_day10<-rename(AM_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')



AM_cov_1<-left_join(AM_cov_day0, AM_cov_day1, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day10, by='day')
AM_cov_1<-na.omit(AM_cov_1)

ER<-c('ER','ER1','ER10')
GPP<-c('GPPavg','GPP1','GPP10')
NEP<-c('NEP','NEP1','NEP10')

AM_cov_1ER<-AM_cov_1[,ER]
AM_cov_1GPP<-AM_cov_1[,GPP]
AM_cov_1NEP<-AM_cov_1[,NEP]

e<-corrplot(cor(x=AM_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

AM_covER<-as.data.frame(as.table(cor(x=AM_cov_1ER,use="complete.obs")))
AM_covGPP<-as.data.frame(as.table(cor(x=AM_cov_1GPP,use="complete.obs")))

AM_covGPP<-filter(AM_covGPP, Var1=="GPPavg")
AM_covER<-filter(AM_covER, Var1=="ER")

AM_covGPP$site<-'AM'
AM_covER$site<-'AM'

AM_covGPP$ID<-6
AM_covER$ID<-6
#######

covGPP<-rbind(US27_covGPP, Ich_covGPP,LF_covGPP,GB_covGPP,Otter_covGPP,AM_covGPP)
covER<-rbind(US27_covER, Ich_covER,LF_covER,GB_covER,Otter_covER,AM_covER)

covER<- covER %>% mutate(future = case_when(
  Var2=='ER' ~ NA_real_,
  Var2=='ER1' ~ 1,
  Var2=='ER10' ~ 10))


covGPP<- covGPP %>% mutate(future = case_when(
  Var2=='GPPavg' ~ NA_real_,
  Var2=='GPP1' ~ 1,
  Var2=='GPP10' ~ 10))

colors<-c(AR1='blue', AR10='orange')

AR_GPP<-filter(covGPP, future==1 | future==10 )
AR_ER<-filter(covER, future==1 | future==10 )

AR_GPP<- AR_GPP %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_ER<- AR_ER %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_GPP$Freq<-abs(AR_GPP$Freq)
AR_ER$Freq<-abs(AR_ER$Freq)
#########Appling10######

ar_er <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/ar_er.csv")
ar_gpp <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/ar_gpp.csv")

ar_gpps1<-ar_gpp[,c(1,2,3)]
ar_gpps1$ar<-"AR1"
ar_gpps1<-rename(ar_gpps1, "ar_n"="ar1")

ar_gpps10<-ar_gpp[,c(1,2,12)]
ar_gpps10$ar<-"AR10"
ar_gpps10<-rename(ar_gpps10, "ar_n"="ar10")
ar_gpps10$ar0<-10

GPPar<-rbind(ar_gpps1,ar_gpps10)

ar_ers1<-ar_er[,c(1,2,3)]
ar_ers1$ar<-"AR1"
ar_ers1<-rename(ar_ers1, "ar_n"="ar1")

ar_ers10<-ar_er[,c(1,2,12)]
ar_ers10$ar<-"AR10"
ar_ers10<-rename(ar_ers10, "ar_n"="ar10")
ar_ers10$ar0<-10

ERar<-rbind(ar_ers1,ar_ers10)

View(GPPar)

#Graphs#########
library(cowplot)


(GPPden<-ggplot(GPPar, aes(x=ar_n,fill=ar)) +
    ggtitle("Appling et al. 2018")+
    geom_density(alpha=0.8) +
    xlab('AR')+ ylab('Density')+
    scale_fill_manual(values = c("blue", 'orange'))+ coord_flip()+
    theme(axis.text.x = element_text(size = 25, angle=0),
          axis.text.y = element_text(size = 25, angle=0),
          axis.title.y =element_text(size = 25),
          axis.title.x =element_text(size = 25),
          plot.title = element_text(size = 25),
          legend.position = "none",
          legend.text= element_text(size = 25),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(GPPAR<-ggplot(AR_GPP, aes(x=ID,y=Freq,color=AR))+
    ylab("AR")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+xlab(" ")+
    ggtitle("GPP Autocorrelation (AR)")+
    theme(axis.text.x = element_text(size = 25, angle=0),
          axis.text.y = element_text(size = 25, angle=0),
          axis.title.y =element_text(size = 25, color='darkgreen'),
          axis.title.x =element_text(size = 25),
          plot.title = element_text(size = 25, color='darkgreen'),
          legend.position = "none",
          legend.text= element_text(size = 25),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

combined_plot <- insert_yaxis_grob(GPPAR, GPPden, position = "right")
(GPP<-ggdraw(combined_plot))



(ERden<-ggplot(ERar, aes(x=ar_n, fill=ar)) +
  geom_density(alpha=0.8)+
    ggtitle("Appling et al. 2018")+
    xlab('AR')+ ylab('Density')+
    coord_flip()+
  scale_fill_manual(values = c("blue", 'orange'))+
    theme(axis.text.x = element_text(size = 25, angle=0),
          axis.text.y = element_text(size = 25, angle=0),
          axis.title.y =element_text(size = 25),
          axis.title.x =element_text(size = 25),
          plot.title = element_text(size = 25),
          legend.position = "bottom",
          legend.text= element_text(size = 25),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

(ERAR<-ggplot(AR_ER, aes(x=ID,y=Freq,color=AR))+
    ylab("AR")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+xlab(" ")+
    ggtitle("ER Autocorrelation (AR)")+
    theme(axis.text.x = element_text(size = 25, angle=0),
          axis.text.y = element_text(size = 25, angle=0),
          axis.title.y =element_text(size = 25,color='darkred'),
          axis.title.x =element_text(size = 25),
          plot.title = element_text(size = 25, color='darkred'),
          legend.position = "none",
          legend.text= element_text(size = 25),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

combined_plot <- insert_yaxis_grob(ERAR, ERden, position = "right")
(ER<-ggdraw(combined_plot))


AR<-plot_grid(GPP, ER, ncol=2)

ggsave(filename="AR.jpeg",
       plot = AR,
       width =12,
       height = 5,
       units = "in")

ggsave(filename="AR poster.jpeg", 
       plot = AR, 
       width =12, 
       height =6, 
       units = "in")
