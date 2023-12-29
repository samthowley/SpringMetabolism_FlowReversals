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
library(readxl)
library(weathermetrics)
library(measurements)
library('StreamMetabolism')
library("hydroTSM")
library(ggnewscale)
library(streamMetabolizer)
library(tidyverse)

GB <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Gilchrist Blue/DONE/GilchristBlue_one.xlsx")
samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv", 
                           col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
GB<-left_join(samplingperiod,GB)
GB<-filter(GB, Date> '')

x<-c("Date","DO","stage","Q_m/s","Mouth_Temp_C","K600_avg")
GB<-GB[,x]

GB<-rename(GB, 'Q'="Q_m/s")

quantile(GB$Q, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin1<-filter(GB, Q<=2.298168411  )
(Q1<-mean(bin1$Q))
(K1<-mean(bin1$K600_avg, na.rm=T))

bin2<-filter(GB, Q<=3.606377580  )
(Q2<-mean(bin2$Q))
(K2<-mean(bin2$K600_avg, na.rm=T))

bin3<-filter(GB, Q<=3.894516805  )
(Q3<-mean(bin3$Q))
(K3<-mean(bin3$K600_avg, na.rm=T))

bin4<-filter(GB, Q>=3.894516805   )
(Q4<-mean(bin4$Q))
(K4<-mean(bin4$K600_avg, na.rm=T))


GB<-rename(GB,'DO.obs'='DO',
                  'depth'='stage',
                  'temp.water'='Mouth_Temp_C',
                  'discharge'="Q")
GB$DO.sat<-Cs(GB$temp.water)

GB$solar.time <-as.POSIXct(GB$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
GB<-GB[,-c(1)]
GB$light<-calc_light(GB$solar.time,  29.8, -82.6)


GB <- GB[!duplicated(GB$solar.time),]
GB <- GB[complete.cases(GB$discharge),]

ggplot(GB, aes(x=solar.time))+
  geom_line(aes(y=depth, color="DO.obs"), linewidth=0.8)


#GB <- GB[c(1:5792),]



bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, 
                     K600_lnQ_nodes_centers = c(Q1,Q2,Q3, Q4),
                     K600_lnQ_nodes_meanlog= log(c(K1,K2,K3, K4)),
                     K600_lnQ_nodes_sdlog= 0.1,
                     K600_lnQ_nodediffs_sdlog = 0.05,
                     K600_daily_sigma_sigma= 0.24,
                     burnin_steps=1000, saved_steps=1000)

y<-c("DO.obs","depth","discharge","temp.water", "DO.sat","solar.time","light" )
GB<-GB[,y]

mm <- metab(bayes_specs, data=GB)

predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Gilchrist Blue/DONE/GBone_streamMetabol.xlsx")

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Gilchrist Blue/DONE")
GBone<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Gilchrist Blue/DONE/GBone_streamMetabol.xlsx")
GBone<-rename(GBone, 
              'Date'='date',
              'GPPavg'="GPP_daily_mean",
              'ER'='ER_daily_mean',
              'K600_avg'='K600_daily_mean')

GilchristBlue_one <- read_excel("GilchristBlue_one.xlsx")
x<-c("Date" ,        "stage"  ,  "Mouth_Temp_C" ,"Mouth_DO_sat")
GilchristBlue_one<-GilchristBlue_one[,x]

GilchristBlue_one$days <- as.Date(GilchristBlue_one$Date)
GilchristBlue_one.day <- aggregate(GilchristBlue_one, by=list(GilchristBlue_one$days), FUN='mean')
GilchristBlue_one.day<-GilchristBlue_one.day[,-c(2)]
GilchristBlue_one.day<-rename(GilchristBlue_one.day, 'Date'='Group.1')

GB_one<-left_join(GBone, GilchristBlue_one.day, by='Date')


GBtwo <- read_excel("GilchristBlue_two.xlsx")
range(GBtwo$Date, na.rm=T)
GBtwo$days <- as.Date(GBtwo$Date)
GBtwo.day <- aggregate(GBtwo, by=list(GBtwo$days), FUN='mean')
names(GBtwo.day)
GBtwo.day<-GBtwo.day[,c("Group.1","GPPavg","ER","K600_avg", 'stage',
                        'Mouth_Temp_C', 'Mouth_DO_sat','days')]
GBtwo.day<-rename(GBtwo.day, "Date"="Group.1")

GB_NEP<-rbind(GBtwo.day, GB_one)
GB_NEP <- GB_NEP[complete.cases(GB_NEP$GPPavg),]


GB_NEP$NEP<-GB_NEP$GPPavg+GB_NEP$ER


(NEP<-ggplot() + 
    geom_line(data=GB_NEP, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=GB_NEP, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=GB_NEP, aes(x=Date,y=NEP, color="NEP"),size=1) +
    ggtitle("NEP")+
    theme_minimal()+
    theme(axis.text.x = element_text(size = 15, angle=0),
          axis.text.y = element_text(size = 15, angle=0),
          axis.title =element_text(size = 15, angle=0),
          axis.title.y.right = element_text(),
          plot.title = element_blank(),
          legend.text=element_text(size=15),
          legend.title=element_text(size=15),
          legend.key.size = unit(0.5, "cm"),
          legend.position = 'bottom')+
    guides(color=guide_legend(title="")))

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry")

GB <- read_excel("GilchristBlue.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric"))

GB<- GB %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))

GB_NEP<- GB_NEP %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))
GB_NEP<-GB_NEP[,-c(1,5)]


master<-left_join(GB,GB_NEP, by=c('day','Month','year'))
master <- master[!duplicated(master[c('Date')]),]
x<-c("Date","CO2","pH","DO","SpC", "stage",
     "Mouth_Temp_C","Mouth_DO_sat","GPPavg","ER","NEP" )
master<-master[,x]

write_xlsx(GB, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx")

names(master)
ggplot() + 
  geom_line(data=master, aes(x=Date,y=stage, color="GPP"),size=1) 
