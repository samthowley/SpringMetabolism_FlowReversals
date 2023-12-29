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


LittleFanning<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/LittleFanning/DONE/LittleFanningone_NEP.xlsx")
samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv", 
                           col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
LittleFanning<-left_join(samplingperiod, LittleFanning)
LittleFanning<-rename(LittleFanning, 'Q'="Q_m/s")

quantile(LittleFanning$Q, probs = c(0,0.25,0.5,0.75,1), na.rm=T)


bin<-filter(LittleFanning, Q<=0.4182726      )
(Q<-mean(bin$Q))
(K<-mean(bin$K600_avg))

bin2<-filter(LittleFanning, Q<=0.4516319       )
(Q2<-mean(bin2$Q))
(K2<-mean(bin2$K600_avg))

bin3<-filter(LittleFanning, Q>=0.5197007      )
(Q3<-mean(bin3$Q))
(K3<-mean(bin3$K600_avg, na.rm=T))

bin4<-filter(LittleFanning, Q<=0.5936807       )
(Q4<-mean(bin$Q))
(K4<-mean(bin$K600_avg))

names(LittleFanning)
x<-c("Date", "DO","stage","Mouth_Temp_C","Q" )
LF<-LittleFanning[,x]
LF<-rename(LF, 
                  'DO.obs'='DO',
                  'depth'='stage',
                  'temp.water'='Mouth_Temp_C',
                  'discharge'="Q")
LF$DO.sat<-Cs(LF$temp.water)

LF$solar.time <-as.POSIXct(LF$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
LF<-LF[,-c(1)]
LF$light<-calc_light(LF$solar.time,  30.155, -83.24)


LF <- LF[!duplicated(LF$solar.time),]
LF <- LF[complete.cases(LF$discharge),]

ggplot(LF, aes(x=solar.time))+
  geom_line(aes(y=DO.obs, color="DO.obs"), linewidth=0.8)


#AllenMill <- AllenMill[c(1:5792),]



bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)


bayes_specs <- specs(bayes_name, 
                     K600_lnQ_nodes_centers = c(Q,Q2,Q3),
                     K600_lnQ_nodes_meanlog= log(c(K,K2,K3)),
                     K600_lnQ_nodes_sdlog= 0.1,
                     K600_lnQ_nodediffs_sdlog = 0.05,
                     K600_daily_sigma_sigma= 0.24,
                     burnin_steps=1000, saved_steps=1000)

mm <- metab(bayes_specs, data=LF)

predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/LittleFanning/DONE/LFone_streamMetabol.xlsx")




setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/LittleFanning/DONE")
LFone<- read_excel("LFone_streamMetabol.xlsx")
LFone<-rename(LFone, 
              'Date'='date',
              'GPPavg'="GPP_daily_mean",
              'ER'='ER_daily_mean',
              'K600_1d'='K600_daily_mean')
LFone$GPPavg[LFone$GPPavg<0] <- 0


LF_one <- read_excel("LittleFanningone_NEP.xlsx")
names(LF_one)
LF_one<-LF_one[,c(1,9,10)]

LF_one$days <- as.Date(LF_one$Date)
LF_one.day <- aggregate(LF_one, by=list(LF_one$days), FUN='mean')
LF_one.day<-LF_one.day[,-c(2)]
LF_one.day<-rename(LF_one.day, 'Date'='Group.1')

LF_one<-left_join(LFone, LF_one.day, by='Date')


LFtwo <- read_excel("LittleFanningtwo_NEP.xlsx")
LFtwo$days <- as.Date(LFtwo$Date)
LFtwo.day <- aggregate(LFtwo, by=list(LFtwo$days), FUN='mean')
names(LFtwo.day)
LFtwo.day<-LFtwo.day[,c("Group.1","GPPavg","ER","K600_1d",
                        'Mouth_Temp_C', 'Mouth_DO_sat','days')]
LFtwo.day<-rename(LFtwo.day, "Date"="Group.1")

LF_NEP<-rbind(LFtwo.day, LF_one)
LF_NEP <- LF_NEP[complete.cases(LF_NEP$GPPavg),]


LF_NEP$NEP<-LF_NEP$GPPavg+LF_NEP$ER


(NEP<-ggplot() + 
    geom_line(data=LF_NEP, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=LF_NEP, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=LF_NEP, aes(x=Date,y=NEP, color="NEP"),size=1) +
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

LF <- read_excel("LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

LF<- LF %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))

LF_NEP<- LF_NEP %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))
LF_NEP<-LF_NEP[,-c(1)]


master<-left_join(LF,LF_NEP, by=c('day','Month','year'))
master <- master[!duplicated(master[c('Date')]),]

x<-c("Date","CO2","FDOM","pH","DO","SpC", "stage",
     "Mouth_Temp_C","Mouth_DO_sat","GPPavg","ER","NEP" )
master<-master[,x]

write_xlsx(master, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx")

dev.new()
