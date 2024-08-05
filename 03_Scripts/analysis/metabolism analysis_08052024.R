rm(list=ls())
##packages######
library(grid)
library(cowplot)
library(ggpmisc)
library(tidyverse)
library(readxl)

library(mgcv)

#constants######
flux<-expression(paste((g~O[2]/m^2/'day')))
col<-c(NEP ='blue', GPP='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
poster_x<-'Depth Above Minimum'
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_text(size = 24, color = "black"),
                             axis.title.x =element_text(size = 24),
                             plot.title = element_text(size = 24),
                             legend.position = "none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

theme_sam_insideplots<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                                         axis.text.y = element_text(size = 24, angle=0),
                                         axis.title.y =element_blank(),
                                         axis.title.x =element_text(size = 24),
                                         plot.title = element_text(size = 24),
                                         legend.position = "none",
                                         panel.background = element_rect(fill = 'white'),
                                         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                                         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

theme_poster<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                                axis.text.y = element_text(size = 24, angle=0),
                                axis.title.y =element_blank(),
                                axis.title.x =element_blank(),
                                plot.title = element_text(size = 24),
                                legend.position = "none",
                                panel.background = element_rect(fill = 'white'),
                                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
#get data####
metabolism<-read_csv('02_Clean_data/master_metabolism4.csv')
metabolism<-metabolism[,c('ER', 'ER_1','ER_2','GPP','GPP_1','GPP_2','NEP', 'Date', 'ID')]
metabolism<-metabolism %>%rename('day'='Date') %>% mutate(day=as.Date(day))

depth<-read_csv('02_Clean_data/master_depth2.csv')
depth$day<-as.Date(depth$Date)
master<-left_join(depth,metabolism, by=c('ID','day'))

master<- master[!duplicated(master[c('ID','Date')]),]
master<-master%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min, day=as.Date(day))
master <- master[!duplicated(master[c('day','ID')]),]

sites<-split(master,master$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]
#Scatter data#######

GPP<-master[,c('Date','depth','depth_diff','GPP','ID')]
GPP<-GPP %>% rename('prod'='GPP') %>% mutate(type='GPP', day=as.Date(Date))
GPP <- GPP[!duplicated(GPP[c('day','ID')]),]


ER<-master[,c("Date",'depth','depth_diff','ER','ID')]
ER<-ER %>% rename('prod'='ER') %>% mutate(type='ER', day=as.Date(Date))
ER <- ER[!duplicated(ER[c('day','ID')]),]

NEP<-master[,c("Date",'depth','depth_diff','NEP','ID')]
NEP<-NEP %>% rename('prod'='NEP') %>% mutate(type='NEP', day=as.Date(Date))
NEP <- NEP[!duplicated(NEP[c('day','ID')]),]

master_scatter<-rbind(GPP, ER, NEP)
master_scatter<-master_scatter %>% filter(type != 'NEP')

sites<-split(master_scatter,master_scatter$ID)
AM_scatter<-sites[[1]]
GB_scatter<-sites[[2]]
ID_scatter<-sites[[3]]
IU_scatter<-sites[[4]]
LF_scatter<-sites[[5]]
OS_scatter<-sites[[6]]

cols<-c(
  "GPP"="darkgreen",
  "ER"="darkred",
  "NEP"="blue")

(AM_sc<-ggplot(data=AM_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("AM")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

model <- gam(GPP ~ s(x)+s(x), data = AM_scatter)






(ID_sc<-ggplot(data=ID_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("ID")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

(IU_sc<-ggplot(data=IU_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(LF_sc<-ggplot(data=LF_scatter, aes(x=depth_diff, y=prod, color=type)) +ggtitle("LF")+
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(GB_sc<-ggplot(data=GB_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots+ggtitle("GB"))


(OS_sc<-ggplot(data=OS_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("OS")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

