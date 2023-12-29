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

AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Allen Mill/DONE/AllenMill_not_one.xlsx")

x<-c("Date","DO","stage","Q_m/s","Mouth_Temp_C",'K600_avg')
AM<-AM[,x]
AM<-rename(AM, 'Q'="Q_m/s")

quantile(AM$Q, probs = c(0,0.25,0.5,0.75,1), na.rm=T)


bin<-filter(AM, Q<=3.873150979)
(Q<-mean(bin$Q))
(K<-mean(bin$K600_avg))

bin2<-filter(AM, Q<=4.814164929)
(Q2<-mean(bin2$Q))
(K2<-mean(bin2$K600_avg))

bin3<-filter(AM, Q<=5.020320957)
(Q3<-mean(bin3$Q))
(K3<-mean(bin3$K600_avg, na.rm=T))

bin4<-filter(AM, Q>=10.3    )
(Q4<-mean(bin4$Q))
(K4<-mean(bin4$K600_avg, na.rm=T))

bin5<-filter(AM, Q<=0.01    )
(Q5<-mean(bin5$Q))
(K5<-mean(bin5$K600_avg, na.rm=T))


samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv", 
                           col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
AM<-left_join(samplingperiod, AM, by='Date')

x<-c("Date", "DO","stage","Mouth_Temp_C","Q" )
AllenMill<-AM[,x]
AllenMill<-rename(AllenMill, 
                  'DO.obs'='DO',
                  'depth'='stage',
                  'temp.water'='Mouth_Temp_C',
                  'discharge'="Q")
AllenMill$DO.sat<-Cs(AllenMill$temp.water)

AllenMill$solar.time <-as.POSIXct(AllenMill$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
AllenMill<-AllenMill[,-c(1)]
AllenMill$light<-calc_light(AllenMill$solar.time,  30.155, -83.24)
AllenMill$depth<-na_interpolation(AllenMill$depth)
ggplot() + 
    geom_line(data=AllenMill, aes(x=solar.time,y=DO.obs, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=AllenMill, aes(x=solar.time,y=temp.water, color="NEP"),size=1)

bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)


bayes_specs <- specs(bayes_name, 
                     K600_lnQ_nodes_centers = c(Q,Q2,Q3,Q4),
                     K600_lnQ_nodes_meanlog= log(c(K,K2,K3,K4)),
                     K600_lnQ_nodes_sdlog= 0.1,
                     K600_lnQ_nodediffs_sdlog = 0.05,
                     K600_daily_sigma_sigma= 0.24,
                     burnin_steps=1000, saved_steps=1000)

y<-c("DO.obs","depth","discharge","temp.water", "DO.sat","solar.time","light" )
AllenMill<-AllenMill[,y]

mm <- metab(bayes_specs, data=AllenMill)

predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Allen Mill/DONE/AllenMillone_streamMetabol.xlsx")


setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Allen Mill/DONE")
AMone<- read_excel("AllenMillone_streamMetabol.xlsx")
AMone<-rename(AMone, 
              'Date'='date',
              'GPPavg'="GPP_daily_mean",
              'ER'='ER_daily_mean',
              'K600_avg'='K600_daily_mean')
AMone$GPPavg[AMone$GPPavg<0] <- 0

AM_one <- read_excel("AMone_NEP.xlsx")
names(AM_one)
AM_one<-AM_one[,c(1,10,11)]

AM_one$days <- as.Date(AM_one$Date)
AM_one.day <- aggregate(AM_one, by=list(AM_one$days), FUN='mean')
AM_one.day<-AM_one.day[,-c(2)]
AM_one.day<-rename(AM_one.day, 'Date'='Group.1')

AM_one<-left_join(AMone, AM_one.day, by='Date')
AMtwo <- read_excel("AllenMilltwo_NEP.xlsx")

AMtwo$days <- as.Date(AMtwo$Date)
AMtwo.day <- aggregate(AMtwo, by=list(AMtwo$days), FUN='mean')
names(AMtwo.day)
AMtwo.day<-AMtwo.day[,c("Group.1","GPPavg","ER","K600_avg",
                        'Mouth_Temp_C', 'Mouth_DO_sat','days')]
AMtwo.day<-rename(AMtwo.day, "Date"="Group.1")

AM_NEP<-rbind(AMtwo.day, AM_one)
AM_NEP <- AM_NEP[complete.cases(AM_NEP$GPPavg),]

AM_NEP$NEP<-AM_NEP$GPPavg+AM_NEP$ER


setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry")

AM <- read_excel("AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

AM<- AM %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))

AM_NEP<- AM_NEP %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))
AM_NEP<-AM_NEP[,-c(1)]


master<-left_join(AM, AM_NEP, by=c('day','Month','year'))
master <- master[!duplicated(master[c('Date')]),]
x<-c("Date","CO2","FDOM","pH","DO","SpC", "stage",
     "Mouth_Temp_C","Mouth_DO_sat","GPPavg","ER","NEP" )
master<-master[,x]

(NEP<-ggplot() + 
    geom_line(data=master, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=master, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=master, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_line(data=master, aes(x=Date,y=stage*4, color="h"),size=1) +
    
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


write_xlsx(master, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx")

