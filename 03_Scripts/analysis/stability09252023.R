rm(list=ls())

#packages#####
library(tidyverse)
library(gridExtra)
library(lubridate)
library(cowplot)
library("broom")
library(car)
library(imputeTS)
library(corrplot)

#constants#####
flux<-expression(paste('g'~O[2]/m^2/'day'))
colstwo <- c("ER" = "red","depth" = "blue","GPP" = "green3")
x<-c("GPP","ER","depth","Date" )
prelim <- function(spring,spring_rC) {
  
  rel_u <- lm(u ~ depth, data=spring_rC)
  (cf <- coef(rel_u))
  spring$velocity_m.s<-(spring$depth*cf[2]+cf[1])
  
  spring<-spring %>% mutate(Q_m.s= width*depth*velocity_m.s,
                            U.H.=velocity_m.s/depth,
                            u_mh=velocity_m.s*3600)
  
  rel_k <- lm(k600_1d ~ uh, data=spring_rC)
  (cf <- coef(rel_k))
  spring$K600_1d<- cf[2]*spring$U.H. + cf[1]
  

  spring<-spring %>%mutate(u_md=u_mh*24) %>%mutate(L_max=(u_md*3)/K600_1d) 
  
  keep<-spring %>%filter(L_max> length+length*0.5)
  
  return(keep)}
autocorrelation <- function(site) {
    site_cov<-site[,x]
    site_cov$day <- as.Date(site_cov$Date)
    site_cov<- aggregate(site_cov, by=list(site_cov$day), FUN='mean')
  
    site_cov$day1<-site_cov$day + 1
    site_cov$day10<-site_cov$day + 10
  
    site_cov_day0<-site_cov[,c(6,2,3,4)]
    site_cov_day0 <- site_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()
  
    site_cov_day1<-site_cov[,c(7,2,3,4)]
    site_cov_day1<-rename(site_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPP', "depth1"='depth')
  
    site_cov_day10<-site_cov[,c(8,2,3,4)]
    site_cov_day10<-rename(site_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPP', "depth10"='depth')
  
  
    site_cov_1<-left_join(site_cov_day0, site_cov_day1, by='day')
    site_cov_1<-left_join(site_cov_1, site_cov_day10, by='day')
  
    site_cov_1<-na.omit(site_cov_1)
  
    ER<-c('ER','ER1','ER10')
    GPP<-c('GPP','GPP1','GPP10')
    depth<-c('depth','depth1','depth10')
  
    site_cov_1ER<-site_cov_1[,ER]
    site_cov_1GPP<-site_cov_1[,GPP]
    site_cov_1depth<-site_cov_1[,depth]
  
    corrplot(cor(x=site_cov_1depth,use="complete.obs"),
             method = "number",
             type = "upper")
  
    site_covER<-as.data.frame(as.table(cor(x=site_cov_1ER,use="complete.obs")))
    site_covGPP<-as.data.frame(as.table(cor(x=site_cov_1GPP,use="complete.obs")))
    site_covdepth<-as.data.frame(as.table(cor(x=site_cov_1depth,use="complete.obs")))
  
    site_covGPP<-filter(site_covGPP, Var1=="GPP")
    site_covER<-filter(site_covER, Var1=="ER")
    site_covh<-filter(site_covdepth, Var1=="depth")
  
  
    return(list(site_covGPP,site_covER,site_covh))}
auto_theme<-    theme(axis.text.x = element_text(size = 25, angle=0),
                      axis.text.y = element_text(size = 25, angle=0),
                      axis.title.y =element_text(size = 25),
                      axis.title.x =element_text(size = 25),
                      plot.title = element_text(size = 25),
                      legend.position = "none",
                      legend.text= element_text(size = 25),
                      panel.background = element_rect(fill = 'white'),
                      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


#get data####
master<- read_csv("02_Clean_data/master_metabolism4.csv")

master<- master %>% mutate(width = case_when(
  ID=='ID'  ~ 50,
  ID=='GB'  ~ 23,
  ID=='LF'  ~ 20,
  ID=='AM'  ~ 24,
  ID=='OS'  ~ NA,
  ID=='IU'  ~ NA),
  length=case_when(ID=='ID'  ~ 2700,
                  ID=='GB'  ~ 350,
                  ID=='LF'  ~ 350,
                  ID=='AM'  ~ 520,
                  ID=='OS'  ~ NA,
                  ID=='IU'  ~ NA))

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min)

master$depth_diff<-master$depth-master$depth_min
master$day<-as.Date(master$Date)
master_met <- master[!duplicated(master[c('ID','day')]),]

sites<-split(master_met,master_met$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]

#IU#####
# startDate <- "2022-05-13"
# endDate <- "2024-05-05"
# parameterCd <- c('00060')
# ventID<-'02322700'
# 
# IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
# IU<-IU %>% rename('Date'='dateTime', 'Q_m.s'='X_00060_00000')%>%
#   mutate(min=minute(Date)) %>% filter(min==0) 

IU<-filter(IU, depth<2)

IU_results<-autocorrelation(IU)
IU_covGPP<-data.frame(IU_results[1]) #date column
IU_covER<-data.frame(IU_results[2]) #date column
IU_covh<-data.frame(IU_results[3]) #date column
IU_covER<- IU_covER %>% mutate(site="IU", ID=1)
IU_covGPP<- IU_covGPP %>% mutate(site="IU", ID=1)
IU_covh<- IU_covh %>% mutate(site="IU", ID=1)

#ID####
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")
ID_prep<-prelim(ID, ID_rC)

ID_results<-autocorrelation(ID_prep)
ID_covGPP<-data.frame(ID_results[1]) #date column
ID_covER<-data.frame(ID_results[2]) #date column
ID_covh<-data.frame(ID_results[3]) #date column
ID_covER<- ID_covER %>% mutate(site="ID", ID=2)
ID_covGPP<- ID_covGPP %>% mutate(site="ID", ID=2)
ID_covh<- ID_covh %>% mutate(site="ID", ID=2)

#LF####
LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
LF_prep<-prelim(LF, LF_rC)

LF_results<-autocorrelation(LF_prep)
LF_covGPP<-data.frame(LF_results[1]) #date column
LF_covER<-data.frame(LF_results[2]) #date column
LF_covh<-data.frame(LF_results[3]) #date column
LF_covER<- LF_covER %>% mutate(site="LF", ID=3)
LF_covGPP<- LF_covGPP %>% mutate(site="LF", ID=3)
LF_covh<- LF_covh %>% mutate(site="LF", ID=3)

#GB####
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
GB_prep<-prelim(GB, GB_rC)

GB_results<-autocorrelation(GB_prep)
GB_covGPP<-data.frame(GB_results[1]) #date column
GB_covER<-data.frame(GB_results[2]) #date column
GB_covh<-data.frame(GB_results[3]) #date column
GB_covER<- GB_covER %>% mutate(site="GB", ID=4)
GB_covGPP<- GB_covGPP %>% mutate(site="GB", ID=4)
GB_covh<- GB_covh%>% mutate(site="GB", ID=4)

#OS####
ggplot(OS, aes(x=Date)) +
  geom_line(aes(y=depth),size=1)+geom_hline(yintercept=1)
OS<-filter(OS, depth<1)
OS_results<-autocorrelation(OS)
OS_covGPP<-data.frame(OS_results[1]) #date column
OS_covER<-data.frame(OS_results[2]) #date column
OS_covh<-data.frame(OS_results[3]) #date column
OS_covER<- OS_covER %>% mutate(site="OS", ID=5)
OS_covGPP<- OS_covGPP %>% mutate(site="OS", ID=5)
OS_covh<- OS_covh %>% mutate(site="OS", ID=5)

#AM####
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
AM_prep<-prelim(AM, AM_rC)

AM_results<-autocorrelation(AM)
AM_covGPP<-data.frame(AM_results[1]) #date column
AM_covER<-data.frame(AM_results[2]) #date column
AM_covh<-data.frame(AM_results[3]) #date column
AM_covER<- AM_covER %>% mutate(site="AM", ID=6)
AM_covGPP<- AM_covGPP %>% mutate(site="AM", ID=6)
AM_covh<- AM_covh %>% mutate(site="AM", ID=6)

#compile#####

covGPP<-rbind(IU_covGPP, ID_covGPP,LF_covGPP,GB_covGPP,OS_covGPP,AM_covGPP)
covER<-rbind(IU_covER, ID_covER,LF_covER,GB_covER,OS_covER,AM_covER)
covh<-rbind(IU_covh, ID_covh,LF_covh,GB_covh,OS_covh,AM_covh)

covER<- covER %>% mutate(future = case_when(
  Var2=='ER' ~ NA_real_,
  Var2=='ER1' ~ 1,
  Var2=='ER10' ~ 10))


covGPP<- covGPP %>% mutate(future = case_when(
  Var2=='GPP' ~ NA_real_,
  Var2=='GPP1' ~ 1,
  Var2=='GPP10' ~ 10))

covh<- covh %>% mutate(future = case_when(
  Var2=='depthavg' ~ NA_real_,
  Var2=='depth1' ~ 1,
  Var2=='depth10' ~ 10))


colors<-c(AR1='blue', AR10='orange')

AR_GPP<-filter(covGPP, future==1 | future==10 )
AR_ER<-filter(covER, future==1 | future==10 )
AR_h<-filter(covh, future==1 | future==10 )

AR_GPP<- AR_GPP %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_ER<- AR_ER %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_h<- AR_h %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_GPP$Freq<-abs(AR_GPP$Freq)
AR_ER$Freq<-abs(AR_ER$Freq)
AR_h$Freq<-abs(AR_h$Freq)

#Appling10######

ar_er <- read_csv("02_Clean_data/Appling/ar_er.csv")
ar_gpp <- read_csv("02_Clean_data/Appling/ar_gpp.csv")

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

#Graphs#########

(GPPden<-ggplot(GPPar, aes(x=ar_n,fill=ar)) +
    ggtitle("Appling et al. 2018")+
    geom_density(alpha=0.8) +
    xlab('AR')+ ylab('Density')+
    scale_fill_manual(values = c("blue", 'orange'))+ coord_flip()+auto_theme)

(GPPAR<-ggplot(AR_GPP, aes(x=site,y=Freq,color=AR))+
    ylab("AR")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+xlab(" ")+scale_y_continuous(limits = c(0,1))+
    ggtitle("GPP Autocorrelation (AR)")+
    scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+auto_theme)

combined_plot <- insert_yaxis_grob(GPPAR, GPPden, position = "right")
(GPP<-ggdraw(combined_plot))

(ERden<-ggplot(ERar, aes(x=ar_n, fill=ar)) +
  geom_density(alpha=0.8)+
    ggtitle("Appling et al. 2018")+
    xlab('AR')+ ylab('Density')+
    coord_flip()+
  scale_fill_manual(values = c("blue", 'orange'))+auto_theme)


(ERAR<-ggplot(AR_ER, aes(x=site,y=Freq,color=AR))+
    ylab("AR")+
    xlab("RR Frequency Gradient")+scale_y_continuous(limits = c(0,1))+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+xlab(" ")+scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+
    ggtitle("ER Autocorrelation (AR)")+auto_theme)

combined_plot <- insert_yaxis_grob(ERAR, ERden, position = "right")
(ER<-ggdraw(combined_plot))


AR<-plot_grid(GPP, ER, ncol=2)

ggsave(filename="05_Figures/AR.jpeg",
       plot = AR,
       width =12,
       height = 5,
       units = "in")

ggsave(filename="05_Figures/AR poster.jpeg",
       plot = AR,
       width =12,
       height =6,
       units = "in")

(hAR<-ggplot(AR_h, aes(x=site,y=Freq,color=AR))+
    ylab("AR")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+xlab(" ")+
    ggtitle("Depth Autocorrelation (AR)")+
    scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+auto_theme)

names(AR_h)
