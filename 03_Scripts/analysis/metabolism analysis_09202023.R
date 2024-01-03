rm(list=ls())
##packages######
library(ggpubr)
library(readxl)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(measurements)
library(StreamMetabolism)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
#constants######

NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste((g~O[2]/m^2/'day')))
GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
slopey<-expression(paste('g'~O[2]/m^3/'day'))
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_text(size = 24, color = "black"),
                             axis.title.x =element_text(size = 24),
                             plot.title = element_text(size = 24),
                             legend.position = "none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
extract_slope <- function(site) {
  (siteNEP<-lm(NEP ~ depth_diff, data = site))
  cf <- coef(siteNEP)
  (SlopesiteNEP <- cf[2])

  (siteGPPavg<-lm(GPPavg ~ depth_diff, data = site))
  cf <- coef(siteGPPavg)
  (SlopesiteGPPavg <- cf[2])

  (siteER<-lm(ER*-1~ depth_diff, data = site))
  cf <- coef(siteER)
  (SlopesiteER <- cf[2])

  NEP<-as.numeric(c(SlopesiteNEP))
  GPP<-as.numeric(c(SlopesiteGPPavg))
  ER<-as.numeric(c(SlopesiteER))

  site<- data.frame(NEP,GPP,ER)
  return(site)}

#get data####

master_chem <- read_csv("02_Clean_data/master.csv")
master_met <- read_csv("02_Clean_data/master_metabolism.csv")
master<-left_join(master_chem, master_met, by=c('ID','Date'))

for(i in 1:nrow(master)){if(master$ID=='OS') {
  master$u[i]<-(master$depth[i]*-0.0868+0.1579)*100
  master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

}
  else if (master$ID=='ID'){
    master$u[i]<-(master$depth[i]*-4.1+2.33)*100
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='GB'){
    master$u[i]<-(master[i]$depth*-0.768+0.51)*100
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='LF'){
    master$u[i]<- (-0.656*master$depth[i] + 0.44)*100
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='AM'){
    master$u[i]<-(master$depth[i]*-1.89+1.4)*100
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)
  }
  else {master$u[i]<- NULL
  master$RI <- NULL
  master$depth_diff[i] <- NULL}}

###Scatter plots#######


(ID_sc<-ggplot(data=ID, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_colour_manual(name="", values = col)+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=ID, se = FALSE, method='lm')+
    scale_x_continuous(n.breaks=4) +scale_y_continuous(n.breaks=3)+theme_sam)

    xlab(h)+ggtitle("ID")

lm(ER~depth_diff, data = ID)
lm(GPPavg~depth_diff, data = ID)
lm(NEP~depth_diff, data = ID)


(IU_sc<-ggplot(data=IU, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=IU, se = FALSE, method='lm')+
    xlab(h)+ggtitle("IU")+
    scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+theme_sam)

lm(NEP~depth_diff, data = IU)
lm(ER~depth_diff, data = IU)
lm(GPPavg~depth_diff, data = IU)


  (AM_sc<-ggplot(data=AM, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=AM, se = FALSE, method='lm')+
    xlab(h)+ggtitle("AM")+
      scale_x_continuous(n.breaks=4) +
      scale_y_continuous(n.breaks=3)+ theme_sam)
lm(ER~depth_diff, data = AM)
lm(NEP~depth_diff, data = AM)
lm(GPPavg~depth_diff, data = AM)


(LF_sc<-ggplot(data=LF, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=LF, se = FALSE, method='lm')+
    xlab(h)+ggtitle("LF")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)
lm(ER~depth_diff, data = LF)
lm(NEP~depth_diff, data = LF)
lm(GPPavg~depth_diff, data = LF)



(GB_sc<-ggplot(data=GB, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    xlab(h)+ggtitle("GB")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)

lm(NEP~depth_diff,
   data = GB)
lm(GPPavg~depth_diff, data = GB)
lm(ER~depth_diff, data = GB)



(OS_sc<-ggplot(data=OS, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=OS, se = FALSE, method='lm')+
    xlab(h)+ggtitle("OS")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)
lm(NEP~depth_diff, data = OS)
lm(GPPavg~depth_diff, data = OS)
lm(ER~depth_diff, data = OS)

plot_grid(IU_sc, GB_sc, AM_sc, nrow = 1)

#######slope######

for(i in 1:nrow(master)){if(master$ID=='OS') {
  extract_slope(master)
}
  else if (master$ID=='ID'){
    extract_slope(master)
  }
  else if(master$ID=='GB'){
    extract_slope(master)
  }
  else if(master$ID=='LF'){
    extract_slope(master)
  }
  else if(master$ID=='AM'){
    extract_slope(master)
  }
  else {NEP<- NULL}}

master$ID<-"OS"



names(R_R)
GPP_R<-R_R[,c(2,4)]
GPP_R<-rename(GPP_R, "met"="GPP")
GPP_R$what<-"GPP"

ER_R<-R_R[,c(3,4)]
ER_R<-rename(ER_R, "met"="ER")
ER_R$what<-"ER"

NEP_R<-R_R[,c(1,4)]
NEP_R<-rename(NEP_R, "met"="NEP")
NEP_R$what<-"NEP"

R_R2<-rbind(GPP_R, ER_R, NEP_R)

q<-c("ER"='darkred', "GPP"='darkgreen', "NEP"='blue')

slope<-ggplot(R_R2,aes(x=what,y=met))+
  geom_boxplot(outlier.color="black", fill=q)+
  ggtitle("Slope Among Sites for GPP, ER and NEP")+
  ylab(slopey)+xlab("")+
  theme(axis.text.x = element_text(size = 24, angle=0),
        axis.text.y = element_text(size = 24, angle=0),
        axis.title.y =element_text(size = 24, color = "black"),
        axis.title.x =element_text(size = 24),
        plot.title = element_text(size = 24),
        legend.position = "none",
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))











#get data####
OS<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx",
                col_types = c("date", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric"))
OS<-filter(OS, Date> '2022-07-20' & Date <='2023-09-07')
OS$depth_diff<-OS$depth-min(OS$depth, na.rm=T)
OS$u<-(OS$depth*-0.0868+0.1579)
OS$day <- as.Date(OS$Date)
OS <- aggregate(OS, by=list(OS$day), FUN='mean')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx",
                col_types = c("date", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric",
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-08-29')
GB$depth_diff<-GB$depth-min(GB$depth, na.rm=T)
GB$u<-(GB$depth*-0.483+0.35)
GB <- GB[!duplicated(GB[c('Date')]),]
GB$day <- as.Date(GB$Date)
GB <- aggregate(GB, by=list(GB$day), FUN='mean')


LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')
LF$depth_diff<-LF$depth-min(LF$depth, na.rm=T)
LF$u<- (-0.115*LF$depth + 0.169)
LF$day <- as.Date(LF$Date)
LF <- aggregate(LF, by=list(LF$day), FUN='mean')





AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-9-20')
AM$depth_diff<-AM$depth-min(AM$depth, na.rm=T)
AM$"u"<-(AM$depth*-0.24+0.46)
AM$day <- as.Date(AM$Date)
AM <- aggregate(AM, by=list(AM$day), FUN='mean')



ID <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx",
                 col_types = c("date", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
ID<-filter(ID, Date> '2022-07-20' & Date <='2023-08-24')
ID$depth_diff<-ID$depth-min(ID$depth, na.rm=T)
ID$u<-(ID$depth*-0.128+0.416)
ID$day <- as.Date(ID$Date)
ID <- aggregate(ID, by=list(ID$day), FUN='mean')



IU <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
IU$day <- as.Date(IU$Date)
IU<- aggregate(IU, by=list(IU$day), FUN='mean')

IU$depth<-conv_unit(IU$depth, "ft", "m")
IU$depth<-IU$depth-(max(IU$depth, na.rm=T)-1.5)
IU$depth_diff<-IU$depth-min(IU$depth, na.rm=T)
IU$u<-IU$discharge/(IU$depth*15)/10


#####box plots#########

x<-c("depth_diff", "u", "GPPavg", "ER", "NEP", "site")
GB<-GB[,x]

springs$site <- factor(springs$site , levels=c("ID", "GB", "LF", "OS", "AM"))

ER<-ggplot(springs, aes(x=site, y=ER)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill='darkred')+
  ylab(flux)+
  ggtitle(ERflux)+
  ylim(0,-40)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam

GPP<-ggplot(springs, aes(x=site, y=GPPavg)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="darkgreen")+
  ggtitle(GPPflux)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) +
  scale_y_continuous(n.breaks=3)+theme_sam

NEP<-ggplot(springs, aes(x=site, y=NEP)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="blue")+
  ylab(flux)+
  ggtitle(NEPflux)+
  ylim(20,-20)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam

(box<-plot_grid(GPP, NEP, ER, ncol=1))

####together#####
(scatter<-plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,nrow=2))
(boxplots<-plot_grid(box,slope,  ncol=2))
together<-plot_grid(scatter, boxplots, nrow=2, rel_heights = c(3/5,1.7/5))

ggsave(filename="metabolism.jpeg",
       plot = together,
       width =12,
       height = 14.5,
       units = "in")


ggsave(filename="poster metabolism.jpeg",
       plot = together,
       width =17,
       height = 15.5,
       units = "in")
