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
library(dataRetrieval)

width <-16.86

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Otter/DONE")
Otter <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/Otter.xlsx")
samplingperiod <- read_csv("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/samplingperiod.csv", 
                           col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
Otter<-left_join(samplingperiod, Otter, by='Date')

x<-c("Date", "DO","stage","Temp")

Otter<-Otter[,x]
Otter["Temp"][is.na(Otter["Temp"])] <- mean(Otter$Temp, na.rm=T)
(Otter$temp.water<- fahrenheit.to.celsius(Otter$Temp))

Otter$'velocity m/s'<-Otter$stage*-0.087+ 0.16
Otter$discharge<-width*Otter$stage*Otter$'velocity m/s'
Otter$uh<-Otter$'velocity m/s'/Otter$stage
Otter$K600_avg<-Otter$uh*33.55+1.8

quantile(Otter$discharge, probs = c(0,0.25,0.5,0.75,1), na.rm=T)

bin<-filter(Otter, discharge>=0.3901374      )
(Q<-mean(bin$discharge))
(K<-mean(bin$K600_avg))

bin1<-filter(Otter, discharge<=1.1765475      )
(Q1<-mean(bin$discharge))
(K1<-mean(bin$K600_avg))

bin2<-filter(Otter, discharge<=1.1987266    )
(Q2<-mean(bin2$discharge))
(K2<-mean(bin2$K600_avg))

bin3<-filter(Otter, discharge<=1.2339980    )
(Q3<-mean(bin3$discharge))
(K3<-mean(bin3$K600_avg, na.rm=T))

bin4<-filter(Otter, discharge>=1.24)
(Q4<-mean(bin4$discharge))
(K4<-mean(bin4$K600_avg, na.rm=T))

Otter<-rename(Otter, 
                  'DO.obs'='DO',
                  'depth'='stage')
Otter$DO.sat<-Cs(Otter$temp.water)

Otter$solar.time <-as.POSIXct(Otter$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
Otter$light<-calc_light(Otter$solar.time,  29.6, -83)
y<-c("DO.obs","depth","discharge","temp.water", "DO.sat","solar.time","light" )
Otter<-Otter[,y]


Otter <- Otter[complete.cases(Otter$discharge),]


bayes_name <- mm_name(type='bayes', pool_K600="binned", err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name, 
                     K600_lnQ_nodes_centers = c(Q,Q2,Q3,Q4),
                     K600_lnQ_nodes_meanlog= log(c(K,K2,K3,K4)),
                     K600_lnQ_nodes_sdlog= 0.1,
                     K600_lnQ_nodediffs_sdlog = 0.05,
                     K600_daily_sigma_sigma= 0.24,
                     GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)

mm <- metab(bayes_specs, data=Otter)

predict_metab(mm)
plot_metab_preds(mm)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean)
plot(prediction2$K600_daily_mean)

write_xlsx(prediction2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Otter/DONE/Otter_streamMetabol.xlsx")




Otter_NEP<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/metabolism/Otter/DONE/Otter_streamMetabol.xlsx")

Otter_NEP<-rename(Otter_NEP, 
              'Date'='date',
              'GPPavg'="GPP_daily_mean",
              'ER'='ER_daily_mean',
              'K600_avg'='K600_daily_mean')

Otter_NEP$NEP<-Otter_NEP$GPPavg+Otter_NEP$ER

(NEP<-ggplot() + 
    geom_line(data=Otter_NEP, aes(x=Date,y=GPPavg, color="GPP"),size=1) + #make transparent cloud
    geom_line(data=Otter_NEP, aes(x=Date,y=ER, color="ER"),size=1) +
    geom_line(data=Otter_NEP, aes(x=Date,y=NEP, color="NEP"),size=1) +
    geom_line(data=Otter_NEP, aes(x=Date,y=K600_avg, color="K"),size=1) +
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

Otter <- read_excel("Otter.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric"))
(Otter$Mouth_Temp_C<- fahrenheit.to.celsius(Otter$Temp))
Otter$Mouth_DO_sat<-Cs(Otter$Mouth_Temp_C)

Otter<- Otter %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))

Otter_NEP<- Otter_NEP %>%
  mutate(day = day(Date), 
         Month = month(Date),
         year = year(Date))
Otter_NEP<-Otter_NEP[,-c(1)]


master<-left_join(Otter,Otter_NEP, by=c('day','Month','year'))
master <- master[!duplicated(master[c('Date')]),]
x<-c("Date","CO2","FDOM","pH","DO","SpC", "stage",
     "Mouth_Temp_C","Mouth_DO_sat","GPPavg","ER","K600_avg","NEP" )
Otter<-master[,x]

write_xlsx(Otter, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx")

