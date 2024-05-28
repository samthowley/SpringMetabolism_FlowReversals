rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)

check_ratios <- function(siteBO) {
  x<-c("Date","DO","depth","ER","GPP",'depth_diff')
  siteBO<-siteBO[,x]

  siteBO <- siteBO %>%
    group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%
    ungroup()

  ind <- which.max(siteBO$depth)
  siteBO_prior <- siteBO[seq_len(ind - 1), ]
  siteBO_prior<-filter(siteBO_prior, count<10)
  siteBO_disturb <- siteBO[ind:nrow(siteBO), ]

  GPP_prior<-mean(siteBO_prior$GPP, na.rm=T)
  ER_prior<-mean(siteBO_prior$ER, na.rm=T)
  h_prior<-mean(siteBO_prior$depth, na.rm=T)

  siteBO_disturb$GPP_ratio<-siteBO_disturb$GPP/GPP_prior
  siteBO_disturb$ER_ratio<-siteBO_disturb$ER/ER_prior
  siteBO_disturb$depth_ratio<-siteBO_disturb$depth/h_prior

  u<-
    siteBO_disturb %>%
    mutate( ERmean= rollapply(ER_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  u<-u[,c(11,1)]
  siteBO_disturb<-left_join(siteBO_disturb,u,by=c('Date'))

  v<-
    siteBO_disturb %>%
    mutate( GPPmean= rollapply(GPP_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  v<-v[,c(12,1)]
  siteBO_disturb<-left_join(siteBO_disturb,v,by=c('Date'))

  y<-
    siteBO_disturb %>%
    mutate( hmean= rollapply(depth,4,mean, fill=NA, partial=TRUE, align='left'))
  y<-y[,c(13,1)]
  siterecov<-left_join(siteBO_disturb,y,by=c('Date'))
  return(siterecov)}
recovery_calc <- function(siterecov) {

  GPPkeep<-c("count","GPP_ratio")
  GPPrecov<-siterecov[,GPPkeep]
  ind <- which.min(GPPrecov$GPP_ratio)
  GPPrecov<-GPPrecov[-c(0:ind),]
  GPPrecov<-GPPrecov %>% filter(GPP_ratio<3)
  modGPP<-lm(GPP_ratio ~ count, data = GPPrecov)
  cf <- coef(modGPP)
  (GPPrecov_results<-(1-cf[1])/cf[2])

  ggplot(GPPrecov, aes(x=count,y=GPP_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  ERkeep<-c("count","ER_ratio")
  ERrecov<-siterecov[,ERkeep]
  ind <- which.min(ERrecov$ER_ratio)
  ERrecov<-ERrecov[-c(0:ind),]
  ERrecov<-ERrecov %>% filter(ER_ratio<1)
  modER<-lm(ER_ratio ~ count, data = ERrecov)
  cf <- coef(modER)
  (ERrecov_results<-(1-cf[1])/cf[2])

  ggplot(ERrecov, aes(x=count,y=ER_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  hkeep<-c("count","depth_ratio")
  hrecov<-siterecov[,hkeep]
  ind <- which.max(hrecov$depth_ratio)
  hrecov<-hrecov[-c(0:ind),]
  hrecov<-hrecov %>% filter(depth_ratio>1)
  modh<-lm(depth_ratio ~ count, data = hrecov)
  cf <- coef(modh)
  (hrecov_results<-(1-cf[1])/cf[2])
  
  (h_diff<-max(siterecov$depth_diff, na.rm=T))
  ggplot(hrecov, aes(x=count,y=depth_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  IDFR_ls<-list(GPPrecov_results,ERrecov_results,hrecov_results,h_diff)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]])

  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'H_recov'
  colnames(df)[4]<-'h_diff'

  return(df)}
recovery_calc_FR <- function(siterecov) {

  GPPkeep<-c("count","GPP_ratio")
  GPPrecov<-siterecov[,GPPkeep]
  ind <- which.min(GPPrecov$GPP_ratio)
  GPPrecov<-GPPrecov[-c(0:ind),]
  GPPrecov<-GPPrecov %>% filter(GPP_ratio<3)
  modGPP<-lm(GPP_ratio ~ count, data = GPPrecov)
  cf <- coef(modGPP)
  (GPPrecov_results<-(1-cf[1])/cf[2])

  ggplot(GPPrecov, aes(x=count,y=GPP_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  ERkeep<-c("count","ER_ratio")
  ERrecov<-siterecov[,ERkeep]
  ERrecov<-ERrecov %>% mutate(ER_ratio=ER_ratio*-1)
  ind <- which.min(ERrecov$ER_ratio)
  ERrecov<-ERrecov[-c(0:ind),]
  ERrecov<-ERrecov %>% filter(ER_ratio<2)
  modER<-lm(ER_ratio ~ count, data = ERrecov)
  cf <- coef(modER)
  (ERrecov_results<-(1-cf[1])/cf[2])

  ggplot(ERrecov, aes(x=count,y=ER_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  hkeep<-c("count","depth_ratio")
  hrecov<-siterecov[,hkeep]
  ind <- which.max(hrecov$depth_ratio)
  hrecov<-hrecov[-c(0:ind),]
  hrecov<-hrecov %>% filter(depth_ratio>1)
  modh<-lm(depth_ratio ~ count, data = hrecov)
  cf <- coef(modh)
  (hrecov_results<-(1-cf[1])/cf[2])
  
  (h_diff<-max(siterecov$depth_diff, na.rm=T))
  ggplot(hrecov, aes(x=count,y=depth_ratio))+
    geom_point(size=1)+geom_smooth(method = "lm")

  IDFR_ls<-list(GPPrecov_results,ERrecov_results,hrecov_results,h_diff)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]])

  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'H_recov'
  colnames(df)[4]<-'h_diff'

  return(df)}

##### data####
master <- read_csv("02_Clean_data/master_metabolism4.csv")
master<-master[,c("Date","DO" ,"GPP","ER","depth","ID")]

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master<-master %>%group_by(ID) %>% mutate(depth_diff=depth-depth_min)

master$days<-as.Date(master$Date)
master <- master[!duplicated(master[c('days', 'ID')]),]

IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
IU<-IDs[[4]]
LF<-IDs[[5]]
OS<-IDs[[6]]
####AM ####
AMFR<- AM %>% mutate(RI = case_when(Date> "2023-01-07" & Date<"2023-05-06"~ 2))
AMFR<-filter(AMFR, RI==2)
ggplot(AMFR, aes(Date))+geom_line(aes(y=depth_diff), size=1)

AMFR_0223<-check_ratios(AMFR)
ggplot(AMFR_0223, aes(x=count,y=hmean))+geom_point(size=1)+geom_smooth(method = "lm")
(AMFR_0223<-recovery_calc_FR(AMFR_0223))


AMFR<- AM %>% mutate(RI = case_when(Date> "2023-06-03" & Date<"2023-08-05"~ 2))
AMFR<-filter(AMFR, RI==2)
# ggplot(AMFR, aes(Date))+geom_line(aes(y=ER), size=1)

AMFR_0623<-check_ratios(AMFR)
AMFR_0623<-filter(AMFR_0623, count<50)
# ggplot(AMFR_0623, aes(x=count,y=hmean))+
#   geom_point(size=1)+geom_smooth(method = "lm")
(AMFR_0623<-recovery_calc(AMFR_0623))


AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-07-20" & Date<"2023-10-31"~ 2))
AMFR<-filter(AMFR, RI==2)
ggplot(AMFR, aes(Date))+geom_line(aes(y=depth), size=1)

AMFR_0823<-check_ratios(AMFR)
#ggplot(AMFR_0823, aes(x=count,y=hmean))+geom_point(size=1)+geom_smooth(method = "lm")
(AMFR_0823<-recovery_calc(AMFR_0823))


AMFR<- AM %>% mutate(RI = case_when(Date> "2023-11-20" & Date<"2024-01-15"~ 2))
AMFR<-filter(AMFR, RI==2)
ggplot(AMFR, aes(Date))+geom_line(aes(y=ER), size=1)
AMFR_1223<-check_ratios(AMFR)

ggplot(AMFR_1223, aes(x=count,y=ERmean))+
  geom_point(size=1)+geom_smooth(method = "lm")
(AMFR_1223<-recovery_calc_FR(AMFR_1223))


AM_tbl<-rbind(AMFR_0223,AMFR_0623,AMFR_0823,AMFR_1223)
AM_tbl$ID<-'AM'
AM_tbl$num<-6
AM_tbl$IF <- c("rev","bo",'bo','rev')

####OS####
OSFR<- OS %>% mutate(RI = case_when(Date> "2022-07-15" & Date<"2022-10-20"~ 2))
OSFR<-filter(OSFR, RI==2)
# ggplot(OSFR, aes(Date))+
#   geom_line(aes(y=GPP), size=1)
OSFR_08<-check_ratios(OSFR)
#ggplot(OSFR_08, aes(x=count,y=hmean))+geom_point(size=1)+geom_smooth(method = "lm")
(OSFR_0822<-recovery_calc(OSFR_08))


OSFR<- OS %>% mutate(RI = case_when(Date> "2023-05-07" & Date<"2023-07-23"~ 2))
OSFR<-filter(OSFR, RI==2)
ggplot(OSFR, aes(Date))+geom_line(aes(y=depth), size=1)
OSFR_0723<-check_ratios(OSFR)
ggplot(OSFR_0723, aes(x=count,y=GPPmean))+geom_point(size=1)+geom_smooth(method = "lm")
(OSFR_0723<-recovery_calc_FR(OSFR_0723))
OSFR_0723$GPP_recov<-NA

OSFR_0723$ER_recov<-NA

OSFR<- OS %>% mutate(RI = case_when( Date> "2023-11-18" & Date<"2024-04-11"~ 2))
OSFR<-filter(OSFR, RI==2)
OSFR_1223<-check_ratios(OSFR)
#ggplot(OSFR_1223, aes(count))+geom_point(aes(y=hmean))
(OSFR_1223<-recovery_calc_FR(OSFR_1223))


OS_tbl<-rbind(OSFR_0822, OSFR_0723,OSFR_1223)
OS_tbl$ID<-'OS'
OS_tbl$num<-5
OS_tbl$IF <- c('bo','bo', 'rev')

####LF######
LFFR<- LF %>% mutate(RI = case_when(Date> "2022-06-01" & Date<"2022-10-30"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+geom_point(aes(y=GPP))
LFFR_0822<-check_ratios(LFFR)
#LFFR_0822<-filter(LFFR_0822,count<110)
ggplot(LFFR_0822, aes(count))+geom_point(aes(y=GPPmean))
(LFFR_0822<-recovery_calc(LFFR_0822))


LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-05-01"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+geom_point(aes(y=depth_diff))
LFFR_0323<-check_ratios(LFFR)
LFFR_0323<-filter(LFFR_0323,count<110)
ggplot(LFFR_0323, aes(count))+geom_point(aes(y=GPPmean))
(LFFR_0323<-recovery_calc_FR(LFFR_0323))

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-05-25" & Date<"2023-08-26"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+geom_line(aes(y=depth_diff))
LFFR_0723<-check_ratios(LFFR)
LFFR_0723<-filter(LFFR_0723,count<68)
ggplot(LFFR_0723, aes(count))+geom_point(aes(y=hmean))
(LFFR_0723<-recovery_calc_FR(LFFR_0723))


# LFFR<- LF %>% mutate(RI = case_when(
#   Date> "2023-10-01" & Date<"2024-08-30"~ 2))
# LFFR<-filter(LFFR, RI==2)
# #ggplot(LFFR, aes(Date))+geom_line(aes(y=depth), size=1)
# LFFR_1223<-check_ratios(LFFR)
# #ggplot(LFFR_1223, aes(count))+  geom_point(aes(y=hmean))
# (LFFR_1223<-recovery_calc_FR(LFFR_1223))



LF_tbl<-rbind(LFFR_0822,LFFR_0323, LFFR_0723)
LF_tbl$ID<-'LF'
LF_tbl$num<-4
LF_tbl$IF <- c("h","h","h")

####GB####
GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-07-01" & Date<"2022-12-20"~ 2))
GBFR<-filter(GBFR, RI==2)
ggplot(GBFR, aes(Date))+ geom_line(aes(y=depth), size=1)

GBFR_0822<-check_ratios(GBFR)
GBFR_0822<-filter(GBFR_0822, count<120)
ggplot(GBFR_0822, aes(count))+geom_point(aes(y=hmean))
(GBFR_0822<-recovery_calc(GBFR_0822))


GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-11-10" & Date<"2024-01-27"~ 2))
GBFR<-filter(GBFR, RI==2)
ggplot(GBFR, aes(Date))+ geom_point(aes(y=depth), size=1)
GBFR_1224<-check_ratios(GBFR)
#GBFR_1224<-filter(GBFR_1224, count<60)
ggplot(GBFR_1224, aes(count))+ geom_point(aes(y=GPPmean))
(GBFR_1224<-recovery_calc_FR(GBFR_1224))


GB_tbl<-rbind(GBFR_0822,GBFR_1224)
GB_tbl$ID<-'GB'
GB_tbl$num<-3
GB_tbl$IF <- c("h", 'rev')


####ID####

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-01-10" & Date<"2023-04-30"~ 2))
IDFR<-filter(IDFR, RI==2)
#ggplot(IDFR, aes(Date))+geom_line(aes(y=depth), size=1)
IDFR_0223<-check_ratios(IDFR)
# ggplot(IDFR_0223, aes(count))+
#   geom_point(aes(y=hmean))
(IDFR_0223<-recovery_calc_FR(IDFR_0223))


IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-08-01"~ 2))
IDFR<-filter(IDFR, RI==2)
# ggplot(IDFR, aes(Date))+
#   geom_line(aes(y=depth), size=1)
IDFR_0723<-check_ratios(IDFR)
IDFR_0723<-filter(IDFR_0723,count>35)
# ggplot(IDFR_0723, aes(count))+
#   geom_point(aes(y=ERmean))
IDFR_0723<-recovery_calc_FR(IDFR_0723)


IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-08-16" & Date<"2023-10-30"~ 2))
IDFR<-filter(IDFR, RI==2)
# ggplot(IDFR, aes(Date))+
#   geom_line(aes(y=depth), size=1)
IDFR_0923<-check_ratios(IDFR)
IDFR_0923<-filter(IDFR_0923, count<45)
# ggplot(IDFR_0923, aes(count))+
#   geom_point(aes(y=hmean))
(IDFR_0923<-recovery_calc_FR(IDFR_0923))


ID_tbl<-rbind(IDFR_0223,IDFR_0723,IDFR_0923)
ID_tbl$ID<-'ID'
ID_tbl$num<-2
ID_tbl$IF <- c("h","h","h")


####IU####
#check
IUFR<- IU %>% mutate(RI = case_when(Date> "2023-11-20" & Date<"2024-02-10"~ 2))
IUFR<-filter(IUFR, RI==2)
#ggplot(IUFR, aes(Date))+geom_line(aes(y=depth), size=1)
IUFR_0223<-check_ratios(IUFR)
#ggplot(IUFR_0223, aes(count))+geom_point(aes(y=GPPmean))
(IUFR_0223<-recovery_calc_FR(IUFR_0223))


IUFR<- IU %>% mutate(RI = case_when(Date> "2024-03-04" & Date<"2024-05-28"~ 2))
IUFR<-filter(IUFR, RI==2)
#ggplot(IUFR, aes(x=Date))+geom_line(aes(y=GPP), size=1)
IUFR_0423<-check_ratios(IDFR)
IUFR_0423<-filter(IUFR_0423, count<45)
#ggplot(IUFR_0423, aes(count))+geom_point(aes(y=GPPmean))
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
ggsave(filename="recovery_magnitude.jpeg",
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

(i<-ggplot(recov, aes(ID, color=IF))+
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
both<-plot_grid(h,i, ncol=2)

ggsave(filename="recovery_site.jpeg",
       plot = both,
       width =12,
       height = 6,
       units = "in")

