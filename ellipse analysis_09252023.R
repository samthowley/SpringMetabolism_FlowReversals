rm(list=ls())
###packages####
library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(cowplot)
library(ggnewscale)
####constants#####
h<-expression(paste( h[actual]-h[minimum]))
slope<-expression(paste( "Slope"~(CO[2]/O[2])))
x<-expression(CO[2]~'departure'~('mmol'~L^-1))
y<-expression(O[2]~'departure'~('mmol'~L^-1))
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 17, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

cols<-c(
  "hn"="lightblue",
  "hh"="blue",
  "hb"="burlywood4",
  "hrev"="black")
vachon_labels<-c("hb" = expression(h[brown]),
                 "hh" = expression(h[high]),
                 "hn" = expression(h[norm]))
###########
master_chem <- read_csv("02_Clean_data/master.csv")
master_pair <- read_csv("02_Clean_data/master_CO2-O2.csv")
master<-left_join(master_chem, master_pair, by=c('ID','Date'))

for(i in 1:nrow(master)){if(master$ID=='OS') {
  master$u[i]<-(master$depth[i]*-0.0868+0.1579)*100
  master<- master %>% mutate(RI = case_when(
    depth <=0.82 ~ "hn",
    depth <1.3 ~ "hh",
    depth >=1.3 ~ "hb"))
  master$RI [is.na(master$RI )] <- 'hh'
  master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

}
  else if (master$ID=='ID'){
    master$u[i]<-(master$depth[i]*-4.1+2.33)*100
    master<- master %>% mutate(RI = case_when(
      depth<=0.93 ~ "hn",
      depth<1.37 ~ "hh",
      depth>=1.37 ~ "hh"))
    master$RI[is.na(master$RI )] <- 'hn'
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='GB'){
    master$u[i]<-(master[i]$depth*-0.768+0.51)*100
    master<- master %>% mutate(RI = case_when(
      depth <0.55 ~ "hn",
      depth >=0.55 ~ "hh"))
    master$RI [is.na(master$RI )] <- 'hn'
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='LF'){
    master$u[i]<- (-0.656*master$depth[i] + 0.44)*100
    master<- master %>% mutate(RI = case_when(
      depth <0.42 ~ "hn",
      depth <0.65 ~ "hn",
      depth >=0.65 ~ "hh"))
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)

  }
  else if(master$ID=='AM'){
    master$u[i]<-(master$depth[i]*-1.89+1.4)*100
    master<- master %>% mutate(RI = case_when(
      depth <0.75 ~ "hn",
      depth <1.37 ~ "hh",
      depth <2.14 ~ "hb",
      depth >=2.14 ~"hrev"))
    master$RI [is.na(master$RI )] <- 'hb'
    master$depth_diff[i]<-master$depth[i]-min(master$depth, na.rm=T)
  }
  else {master$u[i]<- NULL
  master$RI <- NULL
  master$depth_diff[i] <- NULL}}

master$EQ<-1/abs(master$slope)


#####Otter#######
Otter<-filter(master, ID=='OS')
Otter_low<-filter(master, RI== 'hn' & ID== 'OS')
Otter_mod<-filter(master, RI== 'hh'& ID== 'OS')
Otter_hi<-filter(master, RI== 'hb' & ID== 'OS')

(Ot_va<-ggplot()+
  scale_colour_manual(name="", values = cols,labels=vachon_labels)+
    ggtitle("OS")+
  geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_point(data=Otter, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    geom_point(aes(x=mean(Otter_low$CO2_mmol_L, na.rm=T),
                    y=mean(Otter_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_low$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+

    geom_point(aes(x=mean(Otter_mod$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_mod$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_mod$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_mod$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(Otter_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "burlywood4",color = "red", size=10)+
    geom_point(aes(x=mean(Otter_hi$CO2_mmol_L, na.rm=T),
                   y=mean(Otter_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+theme_sam)
#####GB#######

GB<-filter(master, ID=='GB')
GB_low<-filter(master, RI== 'hn' & ID== 'GB')
GB_hi<-filter(master, RI== 'hh' & ID== 'GB')

(GB_va<-ggplot()+
    geom_point(data=GB, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols,
                        labels=vachon_labels)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    ggtitle("GB")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+

    geom_point(aes(x=mean(GB_low$CO2_mmol_L, na.rm=T),
                   y=mean(GB_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(GB_low$CO2_mmol_L, na.rm=T),
                   y=mean(GB_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+

    geom_point(aes(x=mean(GB_hi$CO2_mmol_L, na.rm=T),
                   y=mean(GB_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(GB_hi$CO2_mmol_L, na.rm=T),
                   y=mean(GB_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+theme_sam)

####LF######
LF<-filter(master, ID=='LF')
LF_low<-filter(master, RI== 'hn' & ID== 'LF')
LF_hi<-filter(master, RI== 'hb' & ID== 'LF')

(LF_va<-ggplot()+
    geom_point(data=LF, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols,
                        labels=vachon_labels)+
    geom_point(aes(x=mean(LF_low$CO2_mmol_L, na.rm=T),
                   y=mean(LF_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(LF_low$CO2_mmol_L, na.rm=T),
                   y=mean(LF_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_point(aes(x=mean(LF_hi$CO2_mmol_L, na.rm=T),
                   y=mean(LF_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(LF_hi$CO2_mmol_L, na.rm=T),
                   y=mean(LF_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("LF")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+theme_sam)



#####AM#######
AM<-filter(master, ID=='AM')
AM_hn<-filter(master, RI== 'hn' & ID=='AM')
AM_hh<-filter(master, RI== 'hh'& ID=='AM')
AM_hb<-filter(master, RI== 'hb'& ID=='AM')
AM_hrev<-filter(master, RI== 'hrev'& ID=='AM')


(AM_va<-ggplot()+
    geom_point(data=AM, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols,
                        labels=vachon_labels)+
    geom_point(aes(x=mean(AM_hn$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hn$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hn$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hn$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(AM_hh$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hh$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hh$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hh$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(AM_hb$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hb$O2_mmol_L, na.rm=T)),
               shape=21, fill = "burlywood4",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hb$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hb$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    geom_point(aes(x=mean(AM_hrev$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hrev$O2_mmol_L, na.rm=T)),
               shape=21, fill = "black",color = "red", size=10)+
    geom_point(aes(x=mean(AM_hrev$CO2_mmol_L, na.rm=T),
                   y=mean(AM_hrev$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("AM")+
    geom_abline(slope=0, intercept=0, size=0.5)+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    xlim(0,0.02)+
    xlab(x) + ylab(y)+
    scale_x_continuous(n.breaks=4)+theme_sam)

####ID######
ID<-filter(master, ID=='ID')
ID_low<-filter(master, RI== 'hn' & ID== 'ID')
ID_hi<-filter(master, RI== 'hb' & ID== 'ID')

(ID_va<-ggplot()+
    geom_point(data=ID, aes(x=CO2_mmol_L, y=O2_mmol_L, color=RI), shape=21)+
    scale_colour_manual(name="", values = cols,
                        labels=vachon_labels)+
    geom_point(aes(x=mean(ID_low$CO2_mmol_L, na.rm=T),
                   y=mean(ID_low$O2_mmol_L, na.rm=T)),
               shape=21, fill = "lightblue",color = "red", size=10)+
    geom_point(aes(x=mean(ID_low$CO2_mmol_L, na.rm=T),
                   y=mean(ID_low$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+

    geom_point(aes(x=mean(ID_hi$CO2_mmol_L, na.rm=T),
                   y=mean(ID_hi$O2_mmol_L, na.rm=T)),
               shape=21, fill = "blue",color = "red", size=10)+
    geom_point(aes(x=mean(ID_hi$CO2_mmol_L, na.rm=T),
                   y=mean(ID_hi$O2_mmol_L, na.rm=T)),
               shape=8,color = "red", size=10)+
    ggtitle("ID")+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_abline(slope=0, intercept=0, size=0.5)+
    xlab(x) + ylab(y)+
    scale_y_continuous(n.breaks=4)+theme_sam)


#####
ellipse<-plot_grid(ID_va, GB_va, LF_va, Ot_va, AM_va)
ggsave(filename="ellipse.jpeg",
       plot = ellipse,
       width =15,
       height = 9,
       units = "in")

##slope####
slope<-expression("Slope"~ (O[2]:CO[2]))

master$RI <- factor(master$RI  , levels=c("hn","hh","hb", "hrev"))

columns<-c("Date","depth","O2_mmol_L","CO2_mmol_L","ID",
           "RI", "slope","offset","u", "EQ")

master<-master[,columns]

ggplot(master, aes(x=RI, y=slope, group=RI))+ facet_wrap(~ ID, ncol=2)+
  geom_boxplot(outlier.color="black")+
  ylab(slope)+ggtitle("OS")+ylim(-2,0)+
  scale_x_discrete(labels=)+theme_sam


ggsave(filename="cloudbox.jpeg",
       plot = cloudbox,
       width =15,
       height = 9,
       units = "in")
