rm(list=ls())

##packages######
library(ggpubr)
library(readxl)
library(grid)
library(StreamMetabolism)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)

extract_reduce <- function(site) {
  site<-site %>% filter(RI==2)%>%select(Date, DO, depth, ER, GPP, depth_diff, depthID, ID)
  
  site <- site %>% group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%ungroup()
  
  site_prior<- site %>% filter(count<=15)
  
  (GPP_prior<-mean(site_prior$GPP, na.rm=T))
  (ER_prior<-mean(site_prior$ER, na.rm=T))
  (h_prior<-mean(site_prior$depth, na.rm=T))
  
  
  h<-max(site$depth_diff, na.rm = T)
  ER<-min(site$ER, na.rm = T)
  GPP<-min(site$GPP, na.rm = T)
  date<-min(site$Date, na.rm = T)
  
  site_ls<-list(h_prior,ER_prior,GPP_prior,h,GPP,ER,date)
  df<- data.frame(site_ls[[1]],site_ls[[2]],site_ls[[3]],
                  site_ls[[4]],site_ls[[5]],site_ls[[6]],
                  site_ls[[7]])
  colnames(df)[1]<-'h0'
  colnames(df)[2]<-'ER0'
  colnames(df)[3]<-'GPP0'
  colnames(df)[4]<-'h'
  colnames(df)[5]<-'GPP'
  colnames(df)[6]<-'ER'
  colnames(df)[7]<-'date'
  
  return(df)}
extract_reduce_mod <- function(siteFR) {
  siteFR<-siteFR %>% filter(RI==2)%>%select(Date, DO, depth, ER, GPP, depth_diff, depthID, ID)
  
  siteFR_mod<- siteFR %>% filter(depthID=='moderate')

  siteFR <- siteFR %>% group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%ungroup()
  
  site_prior<- siteFR %>% filter(count<=15)
  
  (GPP0<-mean(site_prior$GPP, na.rm=T))
  (ER0<-mean(site_prior$ER, na.rm=T))
  (h0<-mean(site_prior$depth, na.rm=T))
  
  h<-mean(siteFR_mod$depth_diff, na.rm = T)
  ER<-mean(siteFR_mod$ER, na.rm = T)
  GPP<-mean(siteFR_mod$GPP, na.rm = T)
  date<-mean(siteFR_mod$Date, na.rm = T)
  
  sitemod_ls<-list(h0,ER0,GPP0,h,GPP,ER,date)
  df<- data.frame(sitemod_ls[[1]],sitemod_ls[[2]],sitemod_ls[[3]],
                  sitemod_ls[[4]],sitemod_ls[[5]],sitemod_ls[[6]],
                  sitemod_ls[[7]])
  colnames(df)[1]<-'h0'
  colnames(df)[2]<-'ER0'
  colnames(df)[3]<-'GPP0'
  colnames(df)[4]<-'h'
  colnames(df)[5]<-'GPP'
  colnames(df)[6]<-'ER'
  colnames(df)[7]<-'date'
  
  return(df)}
extract_reduce_dbl <- function(site, site_prior) {
  site<-site %>% filter(RI==2)%>%select(Date, DO, depth, ER, GPP, depth_diff, depthID, ID)
  
  site <- site %>% group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%ungroup()
  
  (GPP_prior<-mean(site_prior$GPP, na.rm=T))
  (ER_prior<-mean(site_prior$ER, na.rm=T))
  (h_prior<-mean(site_prior$depth, na.rm=T))
  
  
  h<-max(site$depth_diff, na.rm = T)
  ER<-min(site$ER, na.rm = T)
  GPP<-min(site$GPP, na.rm = T)
  date<-min(site$Date, na.rm = T)
  
  site_ls<-list(h_prior,ER_prior,GPP_prior,h,GPP,ER,date)
  df<- data.frame(site_ls[[1]],site_ls[[2]],site_ls[[3]],
                  site_ls[[4]],site_ls[[5]],site_ls[[6]],
                  site_ls[[7]])
  colnames(df)[1]<-'h0'
  colnames(df)[2]<-'ER0'
  colnames(df)[3]<-'GPP0'
  colnames(df)[4]<-'h'
  colnames(df)[5]<-'GPP'
  colnames(df)[6]<-'ER'
  colnames(df)[7]<-'date'
  
  return(df)}

master<-read_csv('02_Clean_data/master_metabolism4.csv')
master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master$depth_diff<-master$depth-master$depth_min

IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
IU<-IDs[[4]]
LF<-IDs[[5]]
OS<-IDs[[6]]

##########################
#GB Reduction#############
#########################

GB<- GB %>% mutate(depthID = case_when(
  depth<0.55  ~ "low",
  depth>0.55 & depth<=0.75 ~ "moderate",
  depth>=0.75~ "high"))

GBFR<- GB %>% mutate(RI = case_when(Date<"2023-01-30"~ 2))
GBFR_102023<-extract_reduce(GBFR)

GBFR<- GB %>% mutate(RI = case_when(Date> "2023-05-16" & Date<"2023-10-31"~ 2))
GBFR_082023<-extract_reduce(GBFR)

GBFR<- GB %>% mutate(RI = case_when(Date> "2023-09-16" & Date<"2024-05-11"~ 2))
GBFR_012024<-extract_reduce(GBFR)

# GBFRcheck<-filter(GBFR,RI==2 )
# ggplot(GBFRcheck, aes(Date)) + geom_line(aes(y=GPP, color=depthID))+geom_hline(yintercept = 0.75)


GB_tbl<-rbind(GBFR_102023,GBFR_082023,GBFR_012024)
GB_tbl$ID<-'GB'
GB_tbl$num<-4
GB_tbl$IF <- c("h",'h','rev')


##########################
#OS RI Reduction#############
#########################

OS<- OS %>% mutate(depthID = case_when(
  depth<0.8 ~ "low",
  depth>0.8 & depth<1 ~ "moderate",
  depth>=1 ~ "high"))

OtFR<- OS %>% mutate(RI = case_when(Date> "2022-08-10" & Date< "2023-01-01"~ 2))
OSFR_092023<-extract_reduce(OtFR)

OtFR<- OS %>% mutate(RI = case_when(Date> "2023-01-01" & Date<"2023-05-20"~ 2))
OSFR_032023<-extract_reduce(OtFR)

OtFR<- OS %>% mutate(RI = case_when(Date> "2023-05-20" & Date<"2023-12-10"~ 2))
OSFR_072023<-extract_reduce(OtFR)

OtFR<- OS %>% mutate(RI = case_when(Date> "2023-10-30" ~ 2))
OSFR_042024<-extract_reduce(OtFR)

# OSFRcheck<-filter(OtFR,RI==2 )
# ggplot(OSFRcheck, aes(Date)) +geom_line(aes(y=GPP, color=depthID))

OS_tbl<-rbind(OSFR_092023,OSFR_032023,OSFR_072023,OSFR_042024)
OS_tbl$ID<-'OS'
OS_tbl$num<-5
OS_tbl$IF <- c("rev",'bo','bo','rev')

##########################
#OS mod Reduction#############
#########################

OSmod<-OS %>% filter(depthID != "high")

OtFR<- OSmod %>% mutate(RI = case_when(Date> "2023-01-01" & Date<"2023-05-20"~ 2))
OSmod_032023<-extract_reduce_mod(OtFR)

OtFR<- OSmod %>% mutate(RI = case_when(Date> "2023-05-20" & Date<"2023-12-10"~ 2))
OSmod_092023<-extract_reduce_mod(OtFR)

# OSFRcheck<-filter(OtFR,RI==2 )
# ggplot(OSFRcheck, aes(Date)) +geom_point(aes(y=depth, color=depthID))

OSmod_tbl<-rbind(OSmod_032023,OSmod_092023)
OSmod_tbl$ID<-'OS'
OSmod_tbl$num<-5
OSmod_tbl$IF <- c("h",'h')




##########################
#AM RI Reduction#############
###########################

AM<- AM %>% mutate(depthID = case_when(
  depth<0.9 ~ "low",
  depth>0.9 & depth<1.2 ~ "moderate",
  depth>=1.2 ~ "high"))

AMFR<- AM %>% mutate(RI = case_when(Date< "2022-08-20"~ 2))
AMFR_072022<-extract_reduce(AMFR)

AMFR<- AM %>% mutate(RI = case_when(Date> "2022-08-17" & Date< "2022-11-20"~ 2))
AMFR_092022<-extract_reduce(AMFR)

AMFR<- AM %>% mutate(RI = case_when(Date> "2023-01-01" & Date< "2023-04-20"~ 2))
AMFR_022023<-extract_reduce(AMFR)

AMFR<- AM %>% mutate(RI = case_when(Date> "2023-05-20" & Date< "2023-09-01"~ 2))
AMFR_072023<-extract_reduce(AMFR)

AMFR<- AM %>% mutate(RI = case_when(Date> "2023-08-01" & Date< "2023-10-01"~ 2))
AMFR_092023<-extract_reduce(AMFR)

AMFR<- AM %>% mutate(RI = case_when(Date> "2023-11-01"~ 2))
AMFR_012024<-extract_reduce(AMFR)

# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AMFRcheck, aes(Date)) +geom_line(aes(y=GPP, color=depthID))

AM_tbl<-rbind(AMFR_072022,AMFR_092022,AMFR_022023,AMFR_072023,AMFR_092023,AMFR_012024)
AM_tbl$ID<-'AM'
AM_tbl$num<-6
AM_tbl$IF <- c('bo','bo','rev','bo','bo','rev')

##########################
#AM mod Reduction#############
#########################

AMmod<-AM %>% filter(depthID != "high")

AMFR<- AMmod %>% mutate(RI = case_when(Date< "2022-08-20"~ 2))
AMmod_072022<-extract_reduce_mod(AMFR)

AMFR<- AMmod %>% mutate(RI = case_when(Date> "2022-08-17" & Date< "2022-11-20"~ 2))
AMmod_092022<-extract_reduce_mod(AMFR)

AMFR<- AMmod %>% mutate(RI = case_when(Date> "2023-01-01" & Date< "2023-05-20"~ 2))
AMmod_022023<-extract_reduce_mod(AMFR)

AMFR<- AMmod %>% mutate(RI = case_when(Date> "2023-05-10" & Date< "2023-09-01"~ 2))
AMmod_072023<-extract_reduce_mod(AMFR)

AMFR<- AMmod %>% mutate(RI = case_when(Date> "2023-08-20" & Date< "2023-10-31"~ 2))
AMmod_092023<-extract_reduce_mod(AMFR)

AMFR<- AMmod %>% mutate(RI = case_when(Date> "2023-11-01"~ 2))
AMmod_012024<-extract_reduce_mod(AMFR)

# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AMFRcheck, aes(Date)) +geom_point(aes(y=GPP, color=depthID))

AMmod_tbl<-rbind(AMmod_072022, AMmod_092022, AMmod_022023, AMmod_072023, AMmod_092023, AMmod_012024)
AMmod_tbl$ID<-'AM'
AMmod_tbl$num<-6
AMmod_tbl$IF <- c("h",'h','h','h','h','h')

##########################
#LF Reduction#############
#########################

LF<- LF %>% mutate(depthID = case_when(
  depth<0.4 ~ "low",
  depth> 0.4 & depth<0.6~ "moderate",
  depth>=0.6 ~ "high"))

LFFR<- LF %>% mutate(RI = case_when(Date> "2022-08-01" & Date<"2022-10-01"~ 2))
LF_092023<-extract_reduce(LFFR)

LFFR<- LF %>% mutate(RI = case_when(Date> "2023-01-16" & Date<"2023-04-01"~ 2))
LF_032023<-extract_reduce(LFFR)

LFFR<- LF %>% mutate(RI = case_when(Date> "2023-06-10" & Date<"2023-07-30"~ 2))
LF_062023<-extract_reduce(LFFR)




LFFR_012024<- LF %>% mutate(RI = case_when(Date> "2023-11-01"~ 2)) %>% filter(RI==2)
LFFR_prior<-LFFR_012024 %>% filter(depthID=='low')
  
LFFR_012024<- LFFR_012024 %>% mutate(RI = case_when(Date> "2024-01-01" & Date< '2024-04-10'~ 2))
LFFR_012024_h<-extract_reduce_dbl(LFFR_012024,LFFR_prior)

LFFR_012024<- LFFR_012024 %>% mutate(RI = case_when(Date> "2024-04-10" & Date< '2024-05-10'~ 2))
LFFR_012024_FR1<-extract_reduce_dbl(LFFR_012024,LFFR_prior)

LFFR_012024<- LFFR_012024 %>% mutate(RI = case_when(Date> "2024-05-10"~ 2))
LFFR_012024_FR2<-extract_reduce_dbl(LFFR_012024,LFFR_prior)


LF_tbl<-rbind(LF_092023,LF_032023,LF_062023,LFFR_012024_h, LFFR_012024_FR1, LFFR_012024_FR2)
LF_tbl$ID<-'LF'
LF_tbl$num<-3
LF_tbl$IF <- c("h","h",'h','h','rev','rev')



##########################
#ID Reduction#############
##########################

ID<- ID %>% mutate(depthID = case_when(
  depth<1.1 ~ "low",
  depth>1.1 & depth<1.5 ~ "moderate",
  depth>=1.5 ~ "high"))

IDFR<- ID %>% mutate(RI = case_when(Date<"2023-04-30"~ 2))
IDFR_032023<-extract_reduce(IDFR)

IDFR_F23<- ID %>% mutate(RI = case_when(Date>"2023-04-30" & Date< "2023-11-05"~ 2))%>% filter(RI==2)
IDFR_F23_prior<-IDFR_F23 %>% filter(depthID=='low')

IDFR_F23t<-IDFR_F23 %>% mutate(RI = case_when(Date>'2023-06-17' & Date<'2023-08-01'~2))
IDFR_072023<-extract_reduce_dbl(IDFR_F23t,IDFR_F23_prior)

IDFR_F23t<-IDFR_F23 %>% mutate(RI = case_when(Date>'2023-08-01' & Date<'2023-08-28'~2))
IDFR_082023<-extract_reduce_dbl(IDFR_F23t,IDFR_F23_prior)

IDFR_F23t<-IDFR_F23 %>% mutate(RI = case_when(Date>'2023-08-28' & Date<'2023-10-05'~2))
IDFR_092023<-extract_reduce_dbl(IDFR_F23t,IDFR_F23_prior)




IDFR_W24<- ID %>% mutate(RI = case_when(Date>"2023-11-05" ~ 2))%>% filter(RI==2)
IDFR_W24_prior<-IDFR_W24 %>% filter(depthID=='low')

IDFR_W24t<-IDFR_W24 %>% mutate(RI = case_when(Date<'2024-02-28'~2))
IDFR_012024<-extract_reduce_dbl(IDFR_W24t,IDFR_W24_prior)

IDFR_W24t<-IDFR_W24 %>% mutate(RI = case_when(Date>'2024-03-28' & Date<'2024-5-17'~2))
IDFR_042024<-extract_reduce_dbl(IDFR_W24t,IDFR_W24_prior)

IDFR_W24t<-IDFR_W24 %>% mutate(RI = case_when(Date>'2024-5-17'~2))
IDFR_062024<-extract_reduce_dbl(IDFR_W24t,IDFR_W24_prior)

# IDFRcheck<-filter(IDFR_W24t,RI==2 )
# ggplot(IDFRcheck, aes(Date)) +geom_line(aes(y=depth, color=depthID))

ID_tbl<-rbind(IDFR_032023,IDFR_072023,IDFR_082023,IDFR_092023,IDFR_012024,
              IDFR_042024, IDFR_062024)
ID_tbl$ID<-'ID'
ID_tbl$num<-2
ID_tbl$IF <- c("h","h",'h','h','h','h','h')


##########################
#IU Reduction#############
##########################

IU<- IU %>% mutate(depthID = case_when(
  depth<1.7 ~ "low",
  depth>1.7& depth<3~ "moderate",
  depth>=3 ~ "high"))

IUFR<- IU %>% mutate(RI = case_when(Date> "2023-11-21"~ 2)) %>% filter(RI==2)
IUFR_prior<-IUFR%>%filter(depthID=='low')

IUFRt<- IUFR %>% mutate(RI = case_when(Date<"2024-03-16"~ 2))
IUFR_042024<-extract_reduce_dbl(IUFRt, IUFR_prior)

IUFRt<- IUFR %>% mutate(RI = case_when(Date>"2024-03-16" & Date<'2024-05-20'~ 2))
IUFR_052024<-extract_reduce_dbl(IUFRt, IUFR_prior)

IUFRt<- IUFR %>% mutate(RI = case_when(Date>'2024-05-20'~ 2))
IUFR_062024<-extract_reduce_dbl(IUFRt, IUFR_prior)

# IUFRcheck<-filter(IUFRt,RI==2 )
# ggplot(IUFRcheck, aes(Date)) +geom_line(aes(y=depth, color=depthID))

IU_tbl<-rbind(IUFR_042024, IUFR_052024,IUFR_062024)
IU_tbl$ID<-'IU'
IU_tbl$num<-1
IU_tbl$IF <- c('h', 'h', 'h')
#COMPILE###############
R_R<-rbind(IU_tbl, ID_tbl, LF_tbl, GB_tbl, AM_tbl, OS_tbl, OSmod_tbl,
           AMmod_tbl)

R_R$GPP_reduce<-(1-(R_R$GPP/R_R$GPP0))*100
R_R$ER_reduce<-(1-(R_R$ER0/R_R$ER))*100
R_R$GPP_reduce[R_R$GPP_reduce<0] <- 0
R_R$ER_reduce[R_R$ER_reduce<0] <- 0

write_csv(R_R, "04_Outputs/reduction_analysis.csv")

R_R<-read.csv("04_Outputs/reduction_analysis.csv")
mean(R_R$ER_reduce, na.rm = T)
R_R$a<-'a'
cols<-c(
  "h"="deepskyblue3",
  "bo"="burlywood4",
  "rev"="black")
R_R$IF <- factor(R_R$IF  , levels=c("h","bo","rev"))

h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             legend.position = "none",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



(a<-ggplot(R_R, aes(h, shape=ID, color=IF))+
    geom_point(aes(y=GPP_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on GPP")+
    xlab(h)+ylab("GPP Reduction (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkgreen"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkgreen"))+
    scale_y_continuous(limits = c(0,100)))


(b<-ggplot(R_R, aes(h, shape=ID, color= IF))+
    geom_point(aes(y=ER_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(hdiff)+ylab("|ER| Increase (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkred"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkred"))+
    scale_y_continuous(limits = c(0,100)))
summary(lm(ER_reduce ~ h, data=R_R))
summary(lm(GPP_reduce ~ h, data=R_R))

(flood_mag<-plot_grid(a, b, nrow=1))

ggsave(filename="05_Figures/reduced_mag.jpeg",
       plot = flood_mag,
       width =12,
       height = 5.5,
       units = "in")

(c<-ggplot(R_R, aes(ID, color= IF))+
    geom_point(aes(y=ER_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    scale_x_discrete(limits=c("IU","ID","LF","GB","OS","AM"))+
    xlab(hdiff)+ylab("|ER| Increase (%)")+xlab("River Reversal Frequency")+
    theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkred"),
      axis.title.x =element_text(size = 18),
      axis.text.x=element_text(size=18),
      plot.title = element_text(size = 22, color="darkred"))+
    scale_y_continuous(limits = c(0,100)))
# summary(lm(ER_reduce ~ h, data=R_R))
# summary(lm(GPP_reduce ~ h, data=RR_noID))

(flood_site<-plot_grid(a, c, nrow=1))

ggsave(filename="05_Figures/ERreduced_site.jpeg",
       plot = flood_site,
       width =12,
       height = 5.5,
       units = "in")

(all_reduced<-plot_grid(a,b,c, ncol=3, align = 'h'))

ggsave(filename="05_Figures/all reduced.jpeg",
       plot = all_reduced,
       width =14,
       height = 5.5,
       units = "in")


(b<-ggplot(R_R, aes(h, shape=ID, color= IF))+
    geom_point(aes(y=ER_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(" ")+ylab("|ER| Increase (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkred"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkred"),
      legend.position = 'bottom'))

ggsave(filename="05_Figures/reduced legend.jpeg",
       plot = b,
       width =12,
       height = 10,
       units = "in")
