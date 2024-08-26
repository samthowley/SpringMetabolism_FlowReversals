rm(list=ls())

library(ggpubr)
library(grid)
library(cowplot)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
library(readxl)
library(stats)
library(minpack.lm)

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
#functions####
extract_slope <- function(site) {
  (siteNEP<-lm(NEP ~ depth_diff, data = site))
  cf <- coef(siteNEP)
  (SlopesiteNEP <- cf[2])
  (SlopeInterNEP <- cf[1])
  
  
  (siteGPP<-lm(GPP ~ depth_diff, data = site))
  cf <- coef(siteGPP)
  (SlopesiteGPP <- cf[2])
  (SlopeInterGPP <- cf[1])
  
  (siteER<-lm(ER*-1~ depth_diff, data = site))
  cf <- coef(siteER)
  (SlopesiteER <- cf[2])
  (SlopeInterER <- cf[1])
  
  NEP<-as.numeric(c(SlopesiteNEP))
  GPP<-as.numeric(c(SlopesiteGPP))
  ER<-as.numeric(c(SlopesiteER))
  
  NEPInter<-as.numeric(c(SlopeInterNEP))
  GPPInter<-as.numeric(c(SlopeInterGPP))
  ERInter<-as.numeric(c(SlopeInterER))
  
  return(list(NEP,GPP,ER,SlopeInterNEP,SlopeInterGPP,SlopeInterER))}
slope_df <- function(site) {
  slopes<-extract_slope(site)
  df<- data.frame(slopes[[1]],slopes[[2]],slopes[[3]],slopes[[4]],slopes[[5]],slopes[[6]])
  df<-pivot_longer(df, cols = 1:3, values_to = 'met') #wide to long
  df$name[df$name == 'slopes..1..'] <- 'NEP'
  df$name[df$name == 'slopes..2..'] <- 'GPP'
  df$name[df$name == 'slopes..3..'] <- 'ER'
  df$name[df$name == 'slopes..4..'] <- 'NEPInter'
  df$name[df$name == 'slopes..5..'] <- 'GPPInter'
  df$name[df$name == 'slopes..6..'] <- 'ERInter'
  
  return(df)}

#get data####
metabolism<-read_csv('02_Clean_data/master_metabolism4.csv')
metabolism<-metabolism[,c('ER', 'ER_1','ER_2','GPP','GPP_1','GPP_2', 'Date', 'ID')]
metabolism<-metabolism %>%rename('day'='Date') %>% mutate(day=as.Date(day))
depth<-read_csv('02_Clean_data/master_depth2.csv')
depth$day<-as.Date(depth$Date)
master<-left_join(depth,metabolism, by=c('ID','day'))

master<- master[!duplicated(master[c('ID','Date')]),]
master<-master%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min, day=as.Date(day))
master <- master[!duplicated(master[c('day','ID')]),]

sites<-split(master,master$ID)
#names(sites)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]
#Scatter data#######
pre_thesis<-filter(master, Date<'2023-09-20')

GPP<-pre_thesis[,c('Date','depth','depth_diff','GPP','ID')]
GPP<-GPP %>% rename('prod'='GPP') %>% mutate(type='GPP', day=as.Date(Date))
GPP <- GPP[!duplicated(GPP[c('day','ID')]),]

ER<-pre_thesis[,c("Date",'depth','depth_diff','ER','ID')]
ER<-ER %>% rename('prod'='ER') %>% mutate(type='ER', day=as.Date(Date))
ER <- ER[!duplicated(ER[c('day','ID')]),]

pre_thesis_scatter<-rbind(GPP, ER)

sites<-split(pre_thesis_scatter,pre_thesis_scatter$ID)
#names(sites)
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

#scatter plots####
(ID_sc<-ggplot(data=ID_scatter, aes(x=depth_diff, y=prod, color=type)) +
   geom_point(size=1)+ggtitle("ID")+
   scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
   ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
   theme_sam_insideplots 
 #stat_poly_line()+ stat_poly_eq(use_label(c("R2","P")),size=9, vjust = 12, hjust= -.5)
)

(IU_sc<-ggplot(data=IU_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    #   stat_poly_line()+
    ggtitle("IU")+
    # stat_poly_eq(use_label(c("R2","P")),size=9, vjust = 12, hjust= 0)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(AM_sc<-ggplot(data=AM_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    #   stat_poly_line()+
    # stat_poly_eq(use_label(c("R2","P")),size=9, vjust = 1, hjust= -0.5)+
    ggtitle("AM")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

(LF_sc<-ggplot(data=LF_scatter, aes(x=depth_diff, y=prod, color=type)) +ggtitle("LF")+
    geom_point(size=1)+
    #   stat_poly_line()+
    # stat_poly_eq(use_label(c("R2","P")), size=9,vjust = 12, hjust= -0.3)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(GB_sc<-ggplot(data=GB_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    #   stat_poly_line()+
    # stat_poly_eq(use_label(c("R2","P")),size=9, vjust = 1, hjust= -0.4)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots+ggtitle("GB"))


(OS_sc<-ggplot(data=OS_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("OS")+
    #   stat_poly_line()+
    # stat_poly_eq(use_label(c("R2","P")), label.x = 1, size=9)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,ncol=3)

