rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)
siteBO<-OSFR
check_ratios <- function(siteBO) {
  
  siteBO<-siteBO %>% select(Date, DO, depth, ER, GPP, depth_diff)

  u<-siteBO %>% mutate( ERmean= gaussianSmooth(ER,3))
  u<-u %>% select(Date, ERmean)
  siteBO<-left_join(siteBO,u,by=c('Date'))

  v<-siteBO %>%mutate(GPPmean= gaussianSmooth(GPP,3))
  v<-v %>% select(Date, GPPmean)
  siteBO<-left_join(siteBO,v,by=c('Date'))

  y<- siteBO %>% mutate(hmean= gaussianSmooth(depth,3))
  y<-y%>% select(Date, hmean)
  siteBO<-left_join(siteBO,y,by=c('Date'))
  
  siteBO <- siteBO %>%
    group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%
    ungroup()
  
  ind <- which.max(siteBO$hmean)
  siteBO_prior <- siteBO[seq_len(ind - 1), ]
  siteBO_prior<-filter(siteBO_prior, count<7)
  #siteBO_disturb <- siteBO[ind:nrow(siteBO), ]
  
  GPP_prior<-mean(siteBO_prior$GPPmean, na.rm=T)
  ER_prior<-mean(siteBO_prior$ERmean, na.rm=T)
  h_prior<-mean(siteBO_prior$hmean, na.rm=T)
  
  siteBO$GPP_ratio<-siteBO$GPP/GPP_prior
  siteBO$ER_ratio<-siteBO$ER/ER_prior
  siteBO$depth_ratio<-siteBO$depth/h_prior
  return(siteBO)}
recovery_calc <- function(siterecov) {

  GPPkeep<-c("count","GPP_ratio")
  GPPrecov<-siterecov[,GPPkeep]
  ind <- which.min(GPPrecov$GPP_ratio)
  GPPrecov<-GPPrecov[-c(0:ind),]
  #GPPrecov<-GPPrecov %>% filter(GPP_ratio<3)
  modGPP<-lm(GPP_ratio ~ count, data = GPPrecov)
  cf <- coef(modGPP)
  (GPPrecov_results<-(1-cf[1])/cf[2])

  ERkeep<-c("count","ER_ratio")
  ERrecov<-siterecov[,ERkeep]
  ind <- which.min(ERrecov$ER_ratio)
  ERrecov<-ERrecov[-c(0:ind),]
  #ERrecov<-ERrecov %>% filter(ER_ratio<1)
  modER<-lm(ER_ratio ~ count, data = ERrecov)
  cf <- coef(modER)
  (ERrecov_results<-(1-cf[1])/cf[2])

  hkeep<-c("count","depth_ratio")
  hrecov<-siterecov[,hkeep]
  ind <- which.max(hrecov$depth_ratio)
  hrecov<-hrecov[-c(0:ind),]
  #hrecov<-hrecov %>% filter(depth_ratio>1)
  modh<-lm(depth_ratio ~ count, data = hrecov)
  cf <- coef(modh)
  (hrecov_results<-(1-cf[1])/cf[2])
  
  h_diff<-max(siterecov$depth_diff, na.rm=T)
  
  IDFR_ls<-list(GPPrecov_results,ERrecov_results,hrecov_results,h_diff)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]])

  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'H_recov'
  colnames(df)[4]<-'h_diff'

  return(df)}
recovery_calc_FR <- function(siterecov) {
  
  GPPrecov<-siterecov%>% select(count, GPP_ratio, GPP)
  ind <- which.min(GPPrecov$GPP_ratio)
  GPPrecov<-GPPrecov[-c(0:ind),]
  modGPP<-lm(GPP_ratio ~ count, data = GPPrecov)
  cf <- coef(modGPP)
  (GPPrecov_results<-(1-cf[1])/cf[2])

  ERkeep<-c("count","ER_ratio")
  ERrecov<-siterecov[,ERkeep]
  ERrecov<-ERrecov %>% mutate(ER_ratio=ER_ratio*-1)
  ind <- which.min(ERrecov$ER_ratio)
  ERrecov<-ERrecov[-c(0:ind),]
  modER<-lm(ER_ratio ~ count, data = ERrecov)
  cf <- coef(modER)
  (ERrecov_results<-(1-cf[1])/cf[2])

  hkeep<-c("count","depth_ratio")
  hrecov<-siterecov[,hkeep]
  ind <- which.max(hrecov$depth_ratio)
  hrecov<-hrecov[-c(0:ind),]
  modh<-lm(depth_ratio ~ count, data = hrecov)
  cf <- coef(modh)
  (hrecov_results<-(1-cf[1])/cf[2])
  
  h_diff<-max(siterecov$depth_diff, na.rm=T)
  
  IDFR_ls<-list(GPPrecov_results,ERrecov_results,hrecov_results,h_diff)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]])

  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'H_recov'
  colnames(df)[4]<-'h_diff'

  return(df)}

#data####
master <- read_csv("02_Clean_data/master_metabolism4.csv")

master<-master %>%select(Date,DO,GPP,ER,depth,ID) %>%group_by(ID) %>% 
  mutate(depth_min=min(depth, na.rm=T))
master<-master %>%mutate(depth_diff=depth-depth_min, days=as.Date(Date))
master <- master[!duplicated(master[c('days', 'ID')]),]

IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
IU<-IDs[[4]]
LF<-IDs[[5]]
OS<-IDs[[6]]
#AM ####
AM<- AM %>% mutate(depthID = case_when(
  depth<0.9 ~ "low",
  depth>0.9 & depth<1.2 ~ "moderate",
  depth>=1.2 ~ "high"))

AMFR<- AM %>% mutate(RI = case_when(Date<"2022-11-20"~ 2)) %>% filter(RI==2)
AMFR_2022<-check_ratios(AMFR)
AMFR_0822<-AMFR_2022 %>% filter(count<100)%>% filter(count>75)
(AMFR_0822<-recovery_calc_FR(AMFR_0822))

AMFR<- AM %>% mutate(RI = case_when(Date> "2022-11-01" & Date<"2023-04-07"~ 2)) %>% filter(RI==2)
AMFR_0223<-check_ratios(AMFR)
(AMFR_0223<-recovery_calc(AMFR_0223))

AMFR<- AM %>% mutate(RI = case_when(Date> "2023-04-25" & Date<"2023-11-30"~ 2))%>% filter(RI==2)
AMFR_Sp2023<-check_ratios(AMFR)

AMFR_0623<-AMFR_Sp2023 %>% filter(count<100)%>% filter(count>25)
(AMFR_0623<-recovery_calc(AMFR_0623))

AMFR_0823<-AMFR_Sp2023 %>% filter(count>110)%>% filter(count<140)
(AMFR_0823<-recovery_calc(AMFR_0823))

AMFR<- AM %>% mutate(RI = case_when(Date>"2023-10-30"~2))%>% filter(RI==2) 
AMFR_2024<-check_ratios(AMFR)

AMFR_1223<-AMFR_2024 %>% filter(count>30 & count<120)
(AMFR_1223<-recovery_calc(AMFR_1223))

AMFR_0224<-AMFR_2024 %>% filter(count>210)
(AMFR_0224<-recovery_calc_FR(AMFR_0224))

AM_tbl<-rbind(AMFR_0822, AMFR_0223,AMFR_0623,AMFR_0823,AMFR_1223, AMFR_0224)
AM_tbl$ID<-'AM'
AM_tbl$num<-6
AM_tbl$IF <- c('bo','rev','bo','bo','rev','bo')

#OS####
OSFR<- OS %>% mutate(RI = case_when(Date> "2022-08-15" & Date<"2022-11-15"~ 2))
OSFR_08<-check_ratios(OSFR)
(OSFR_0822<-recovery_calc(OSFR_08))



OSFR<- OS %>% mutate(RI = case_when(Date> "2023-05-07" & Date<"2023-07-06"~ 2))%>%filter(RI==2)
OSFR_0723<-check_ratios(OSFR)
(OSFR_0723<-recovery_calc_FR(OSFR_0723))

OSFR<- OS %>% mutate(RI = case_when( Date> "2023-11-10" ~ 2)) %>% filter(RI==2)
OSFR_2024<-check_ratios(OSFR)

OSFR_1223<-OSFR_2024 %>% filter(count<120)%>% filter(count>25)
(OSFR_1223<-recovery_calc_FR(OSFR_1223))

OSFR_0324<-OSFR_2024 %>% filter(count>90 & count<163)
(OSFR_0324<-recovery_calc_FR(OSFR_0324))

ggplot(OSFR_0324, aes(count))+geom_line(aes(y=ER_ratio))


OS_tbl<-rbind(OSFR_0822, OSFR_0723,OSFR_1223)
OS_tbl$ID<-'OS'
OS_tbl$num<-5
OS_tbl$IF <- c('bo','bo', 'rev')

#LF######
LFFR<- LF %>% mutate(RI = case_when(Date> "2022-07-01" & Date<"2022-10-30"~ 2)) %>% filter(RI==2)
LFFR_0822<-check_ratios(LFFR)
(LFFR_0822<-recovery_calc(LFFR_0822))


LFFR<- LF %>% mutate(RI = case_when(Date> "2022-12-25" & Date<"2023-03-10"~ 2))%>% filter(RI==2)
LFFR_0323<-check_ratios(LFFR)
(LFFR_0323<-recovery_calc_FR(LFFR_0323))

LFFR<- LF %>% mutate(RI = case_when(Date> "2023-05-25" & Date<"2023-11-26"~ 2))%>% filter(RI==2)
LFFR_0723<-check_ratios(LFFR)
(LFFR_0723<-recovery_calc_FR(LFFR_0723))

LFFR<- LF %>% mutate(RI = case_when(Date> "2023-11-01"~ 2))%>% filter(RI==2)
(LFFR_1223<-recovery_calc_FR(LFFR_1223))

LFFR_0324<-check_ratios(LFFR) %>% filter(count>195)
(LFFR_0324<-recovery_calc_FR(LFFR_0324))

LF_tbl<-rbind(LFFR_0822,LFFR_0323, LFFR_0723,LFFR_1223, LFFR_0324)
LF_tbl$ID<-'LF'
LF_tbl$num<-3
LF_tbl$IF <- c("h","h","h","h","h")

#GB####

GBFR<- GB %>% mutate(RI = case_when(Date> "2022-08-01" & Date<"2022-12-22"~ 2))%>%filter(RI==2)
GBFR_0822<-check_ratios(GBFR)
(GBFR_0822<-recovery_calc(GBFR_0822))

GBFR<- GB %>% mutate(RI = case_when(Date> "2023-10-01" & Date<"2024-02-11"~ 2))%>%filter(RI==2)
GBFR_1224<-check_ratios(GBFR)
(GBFR_1224<-recovery_calc_FR(GBFR_1224))

GB_tbl<-rbind(GBFR_0822,GBFR_1224)
GB_tbl$ID<-'GB'
GB_tbl$num<-3
GB_tbl$IF <- c("h", 'rev')

#ID####

IDFR<- ID %>% mutate(RI = case_when(Date> "2022-12-15" & Date<"2023-04-30"~ 2))%>% filter(RI==2)
IDFR_0223<-check_ratios(IDFR)%>% filter(count<70)
(IDFR_0223<-recovery_calc(IDFR_0223))


IDFR<- ID %>% mutate(RI = case_when(Date> "2023-06-01" & Date<"2023-08-19"~ 2))%>% filter(RI==2)
IDFR_0723<-check_ratios(IDFR)
(IDFR_0723<-recovery_calc_FR(IDFR_0723))
IDFR_0723$GPP_recov<-0

IDFR<- ID %>% mutate(RI = case_when(Date> "2023-05-19" & Date<"2023-10-22"~ 2))%>% filter(RI==2)
#ggplot(IDFR, aes(Date))+ geom_line(aes(y=depth), size=1)
IDFR_0923<-check_ratios(IDFR)
#ggplot(IDFR_0923, aes(count))+ geom_point(aes(y=ERmean))
(IDFR_0923<-recovery_calc_FR(IDFR_0923))


IDFR<- ID %>% mutate(RI = case_when(Date> "2023-09-01" & Date<"2024-03-16"~ 2))
IDFR<-filter(IDFR, RI==2)
#ggplot(IDFR, aes(Date))+ geom_line(aes(y=depth), size=1)
IDFR_0124<-check_ratios(IDFR)
#ggplot(IDFR_0124, aes(count))+ geom_point(aes(y=ERmean))
(IDFR_0124<-recovery_calc_FR(IDFR_0124))


IDFR<- ID %>% mutate(RI = case_when(Date> "2024-03-16" & Date<"2024-06-16"~ 2))
IDFR<-filter(IDFR, RI==2)
#ggplot(IDFR, aes(Date))+ geom_line(aes(y=depth), size=1)
IDFR_0324<-check_ratios(IDFR)
ggplot(IDFR_0324, aes(count))+ geom_point(aes(y=ERmean))
(IDFR_0324<-recovery_calc_FR(IDFR_0324))


ID_tbl<-rbind(IDFR_0223,IDFR_0723,IDFR_0923,IDFR_0124,IDFR_0324)
ID_tbl$ID<-'ID'
ID_tbl$num<-2
ID_tbl$IF <- c("h","h","h","rev","h")


#IU####
#check
IUFR<- IU %>% mutate(RI = case_when(Date> "2023-11-20" & Date<"2024-02-10"~ 2))
IUFR<-filter(IUFR, RI==2)
#ggplot(IUFR, aes(Date))+geom_line(aes(y=depth), size=1)
IUFR_0223<-check_ratios(IUFR)
ggplot(IUFR_0223, aes(count))+geom_point(aes(y=ERmean))
(IUFR_0223<-recovery_calc_FR(IUFR_0223))


IUFR<- IU %>% mutate(RI = case_when(Date> "2024-03-04" & Date<"2024-05-10"~ 2))
IUFR<-filter(IUFR, RI==2)
#ggplot(IUFR, aes(x=Date))+geom_line(aes(y=GPP), size=1)
IUFR_0423<-check_ratios(IDFR)
IUFR_0423<-filter(IUFR_0423, count<82)
ggplot(IUFR_0423, aes(count))+geom_point(aes(y=ERmean))
(IUFR_0423<-recovery_calc_FR(IUFR_0423))


IU_tbl<-rbind(IUFR_0223,IUFR_0423)
IU_tbl$ID<-'IU'
IU_tbl$num<-1
IU_tbl$IF <- c("h","h")

###compile####
recov<-rbind(IU_tbl, ID_tbl, LF_tbl, GB_tbl, AM_tbl, OS_tbl)
recov<-recov %>% mutate(GPP_recov=abs(GPP_recov),
                        ER_recov=abs(ER_recov),H_recov=abs(H_recov))

recov$GPP_ratio<-recov$H_recov/recov$GPP_recov
recov$ER_ratio<-recov$H_recov/recov$ER_recov
# recov$GPP_ratio[recov$GPP_ratio>10] <- NA
# recov$ER_ratio[recov$ER_ratio>10] <- NA

write_csv(recov, "04_Outputs/recovery_analysis.csv")

###compile####
recov<-read_csv("04_Outputs/recovery_analysis.csv")
mean(recov$ER_ratio, na.rm = T)
recov$a<-'a'
cols<-c(
  "h"="deepskyblue3",
  "bo"="burlywood4",
  "rev"="black")

y<-expression(paste( (Stage[Recovery])/(Metabolic[Recovery])))
recov$IF <- factor(recov$IF  , levels=c("h","bo","rev"))
h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             axis.title.x = element_text(size = 27, angle=0),
                             legend.position = "none",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

(f<-ggplot(recov, aes(h_diff, shape=ID, color=IF))+
    geom_point(aes(y=GPP_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("GPP Recovery")+
    xlab(h)+
    ylab(y)+scale_y_continuous(trans='log10', limits = c(0.3,3))+
    theme_sam+theme(
      axis.title.y =element_text(size = 25, color='darkgreen'),
        plot.title = element_text(size = 27, color='darkgreen')))
summary(lm(log10(GPP_ratio) ~ h_diff, data=recov))
summary(lm(log10(ER_ratio) ~ h_diff, data=recov))



(g<-ggplot(recov, aes(h_diff, shape=ID, color=IF))+
    geom_point(aes(y=ER_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("ER Recovery")+
    xlab(hdiff)+
    ylab(y)+scale_y_continuous(trans='log10', limits = c(0.3,3))+theme_sam+
    theme(
      axis.title.y =element_text(size = 25, color='darkred'),
      plot.title = element_text(size = 27, color='darkred')))

both<-plot_grid(f,g, ncol=2)
ggsave(filename="05_Figures/recovery_magnitude.jpeg",
       plot = both,
       width =12,
       height = 6,
       units = "in")

(h<-ggplot(recov, aes(x=ID, color=IF, group=1))+
    geom_point(aes(y=GPP_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("GPP Recovery")+
    xlab('River Reversal Frequency')+
    scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+
    ylab(y)+scale_y_continuous(trans='log10', limits = c(0.3,3))+theme_sam+
    theme(
      axis.title.y =element_text(size = 25, color='darkgreen'),
      plot.title = element_text(size = 27, color='darkgreen'),
      axis.text.x = element_text(size=17)))

(i<-ggplot(recov_excluding_ID, aes(ID, color=IF))+
    geom_point(aes(y=ER_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("ER Recovery")+
    xlab("River Reversal Frequency")+
    scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+
    ylab(y)+scale_y_continuous(trans='log10', limits = c(0.3,3))+theme_sam+
    theme(
      axis.title.y =element_text(size = 25, color='darkred'),
      plot.title = element_text(size = 27, color='darkred'),
      axis.text.x = element_text(size=17)))
recov_excluding_ID<-filter(recov, ER_ratio<1.9)
summary(lm(log10(GPP_ratio) ~ ID, data=recov_excluding_ID))


both<-plot_grid(h,i, ncol=2)

ggsave(filename="05_Figures/recovery_site.jpeg",
       plot = both,
       width =12,
       height = 6,
       units = "in")

