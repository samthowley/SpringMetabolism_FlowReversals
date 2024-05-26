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

extract_reduce <- function(ID,IDFR) {

  ID_low<-filter(ID, depthID== 'low')

  NEP0<-mean(ID_low$NEP, na.rm=T)
  ER0<-mean(ID_low$ER, na.rm=T)
  GPP0<-mean(ID_low$GPP, na.rm=T)
  h0<-mean(ID_low$depth_diff, na.rm=T)

  IDFR<-filter(IDFR, RI== "2")

  h<-max(IDFR$depth_diff, na.rm = T)
  NEP<-min(IDFR$NEP, na.rm = T)
  ER<-min(IDFR$ER, na.rm = T)
  GPP<-min(IDFR$GPP, na.rm = T)
  date<-min(IDFR$Date, na.rm = T)

  IDFR_ls<-list(h0,ER0,GPP0,h,GPP,ER,date)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]],IDFR_ls[[5]],IDFR_ls[[6]],
                  IDFR_ls[[7]])
  colnames(df)[1]<-'h0'
  colnames(df)[2]<-'ER0'
  colnames(df)[3]<-'GPP0'
  colnames(df)[4]<-'h'
  colnames(df)[5]<-'GPP'
  colnames(df)[6]<-'ER'
  colnames(df)[7]<-'date'


  return(df)}
extract_reduce_mod <- function(ID,IDFR) {

  ID_low<-filter(ID, depthID== 'low')

  NEP0<-mean(ID_low$NEP, na.rm=T)
  ER0<-mean(ID_low$ER, na.rm=T)
  GPP0<-mean(ID_low$GPP, na.rm=T)
  h0<-mean(ID_low$depth_diff, na.rm=T)

  IDFR<-filter(IDFR, RI== "2")

  h<-mean(IDFR$depth_diff, na.rm = T)
  NEP<-mean(IDFR$NEP, na.rm = T)
  ER<-mean(IDFR$ER, na.rm = T)
  GPP<-mean(IDFR$GPP, na.rm = T)
  date<-mean(IDFR$Date, na.rm = T)

  IDFR_ls<-list(h0,ER0,GPP0,h,GPP,ER,date)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]],IDFR_ls[[5]],IDFR_ls[[6]],
                  IDFR_ls[[7]])
  colnames(df)[1]<-'h0'
  colnames(df)[2]<-'ER0'
  colnames(df)[3]<-'GPP0'
  colnames(df)[4]<-'h'
  colnames(df)[5]<-'GPP'
  colnames(df)[6]<-'ER'
  colnames(df)[7]<-'date'


  return(df)}

#get data####

master<-read_csv('02_Clean_data/master_metabolism4.csv')
names(master)
master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master$depth_diff<-master$depth-master$depth_min

IDs<-split(master,master$ID)
names(IDs)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
IU<-IDs[[4]]
LF<-IDs[[5]]
OS<-IDs[[6]]
####GB####

# GBFRcheck<-filter(GBFR,RI==2 )
# ggplot(GBFRcheck, aes(Date)) + geom_line(aes(y=ER))+
#   geom_line(aes(y=depth*10, color=depthID))+geom_hline(yintercept = 0.75)

GB<- GB %>% mutate(depthID = case_when(
  depth<0.55  ~ "low",
  depth>0.55 & depth<=0.75 ~ "moderate",
  depth>=0.75~ "high"))

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-11" & Date<"2022-11-15"~ 2))
GB_0622<-extract_reduce(GB, GBFR)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-11-14" & Date<"2024-02-11"~ 2))
GB_1223<-extract_reduce(GB, GBFR)

GB_tbl<-rbind(GB_0622,GB_1223)
GB_tbl$ID<-'GB'
GB_tbl$num<-2
GB_tbl$IF <- c("bo",'rev')

####Otter#####

# OSFRcheck<-filter(OtFR,RI==2 )
# ggplot(OSFRcheck, aes(Date)) +
#   geom_line(aes(y=depth, color=depthID))+
#   geom_line(aes(y=DO))

OS<- OS %>% mutate(depthID = case_when(
  depth<0.8 ~ "low",
  depth>0.8 & depth<1 ~ "moderate",
  depth>=1 ~ "high"))

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-20"~ 2))
OS_0722<-extract_reduce(OS, OtFR)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-06-15" & Date<"2023-07-20"~ 2))
OS_0623<-extract_reduce(OS, OtFR)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-11-18" & Date<"2024-04-11"~ 2))
OS_1223<-extract_reduce(OS, OtFR)

OS_tbl<-rbind(OS_0722,OS_0623,OS_1223)
OS_tbl$ID<-'OS'
OS_tbl$num<-3
OS_tbl$IF <- c("bo",'bo','rev')

Otter_mod<-filter(OS, depthID=='moderate')

Otmod<- Otter_mod %>% mutate(RI = case_when(Date<'2023-01-01'~ 2))
OSmod_0622<-extract_reduce_mod(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-01-01'& Date<'2023-06-01' ~ 2))
OSmod_0623<-extract_reduce_mod(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-06-01' & Date<'2023-11-21' ~ 2))
OSmod_0723<-extract_reduce_mod(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-12-01' ~ 2))
OSmod_1223<-extract_reduce_mod(OS, Otmod)

OSmod_tbl<-rbind(OSmod_0622,OSmod_0623,OSmod_0723, OSmod_1223)
OSmod_tbl$ID<-'OS'
OSmod_tbl$num<-4
OSmod_tbl$IF <- c("h","h",'h', "h")
####AllenMill##########

# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AMFRcheck, aes(Date)) +
#   geom_line(aes(y=ER))+
#   geom_line(aes(y=depth*10, color=depthID))

AM<- AM %>% mutate(depthID = case_when(
  depth<0.9 ~ "low",
  depth>0.9 & depth<1.2 ~ "moderate",
  depth>=1.2 ~ "high"))

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-01-20" & Date<"2023-04-15"~ 2))
AMFR_0223<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-08-01"~ 2))
AMFR_0623<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-08-22" & Date<"2023-10-01"~ 2))
AMFR_0823<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-11-17" & Date<"2024-01-11"~ 2))
AMFR_1224<-extract_reduce(AM, AMFR)

AM_tbl<-rbind(AMFR_0223,AMFR_0623,AMFR_0823,AMFR_1224)
AM_tbl$ID<-'AM'
AM_tbl$num<-5
AM_tbl$IF <- c("rev","bo",'bo','rev')
# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AM, aes(Date, depth, colour=RI)) + geom_line()+
#   geom_hline(yintercept = 0.8)+
#   geom_hline(yintercept = 1.37)


AM_mod<-filter(AM, depthID== 'moderate')
AMmod<- AM_mod %>% mutate(RI = case_when( Date<"2022-07-09"~ 2))
AMmod_0622<-extract_reduce_mod(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when( Date>"2022-07-09" & Date<"2023-02-09"~ 2))
AMmod_0823<-extract_reduce_mod(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-01-15" &  Date<"2023-05-01"~ 2))
AMmod_0223<-extract_reduce_mod(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-06-15" &  Date<"2023-08-01"~ 2))
AMmod_0623<-extract_reduce_mod(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-07-31" & Date<"2023-12-31"~ 2))
AMmod_1223<-extract_reduce_mod(AM, AMmod)

AMmod_tbl<-rbind(AMmod_0622,AMmod_0823,AMmod_0223,AMmod_0623,AMmod_1223)
AMmod_tbl$ID<-'AM'
AMmod_tbl$num<-5
AMmod_tbl$IF <- c("h","h",'h','h','h')

####LF#####

# LFFRcheck<-filter(LFFR,RI==2 )
# ggplot(LFFRcheck, aes(Date)) +
#     geom_line(aes(y=depth, color=depthID))+
#   geom_line(aes(y=ER))

LF<- LF %>% mutate(depthID = case_when(
  depth<0.4 ~ "low",
  depth> 0.4 & depth<0.6~ "moderate",
  depth>=0.6 ~ "high"))

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2022-08-19" & Date<"2022-10-01"~ 2))
LF_0922<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-02-06" & Date<"2023-04-01"~ 2))
LF_0223<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-15" & Date<"2023-07-15"~ 2))
LF_0723<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-11-30" & Date<"2024-08-01"~ 2))
LFFR<-filter(LFFR, ER>-30)
LF_0124<-extract_reduce(LF, LFFR)


LF_tbl<-rbind(LF_0922,LF_0223,LF_0723)
LF_tbl$ID<-'LF'
LF_tbl$num<-3
LF_tbl$IF <- c("h","h",'h')
######ID#######
# IDFRcheck<-filter(IDFR,RI==2 )
# ggplot(IDFRcheck, aes(Date)) +
#   geom_line(aes(y=depth*3, color=depthID))+
#   geom_line(aes(y=ER))+
#   geom_line(aes(y=GPP))

ID<- ID %>% mutate(depthID = case_when(
  depth<0.85 ~ "low",
  depth>0.85 & depth<1.5 ~ "moderate",
  depth>=1.5 ~ "high"))

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-01-21" & Date<"2023-04-30"~ 2))
ID_0123<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-08-01"~ 2))
ID_0823<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-08-01" & Date<"2023-09-30"~ 2))
ID_0923<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-11-01" & Date<"2024-01-31"~ 2))
ID_0124<-extract_reduce(ID, IDFR)

ID_tbl<-rbind(ID_0123,ID_0823,ID_0923,ID_0124)
ID_tbl$ID<-'ID'
ID_tbl$num<-1
ID_tbl$IF <- c("h","h",'h','h')

######IU#######
IU<-filter(IU, Date>'2022-05-02')

# IUFRcheck<-filter(IUFR,RI==2 )
# ggplot(IUFRcheck, aes(Date)) +
#   geom_line(aes(y=ER, color=depthID))

IU<- IU %>% mutate(depthID = case_when(
  depth<1.7 ~ "low",
  depth>1.7& depth<3~ "moderate",
  depth>=3 ~ "high"))

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-02-28"~ 2))
IU_0124<-extract_reduce(IU, IUFR)

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2024-02-28" & Date<"2024-05-28"~ 2))
IU_0324<-extract_reduce(IU, IUFR)

IU_tbl<-rbind(IU_0124, IU_0324)
IU_tbl$ID<-'IU'
IU_tbl$num<-1
IU_tbl$IF <- c('h', 'h')
###########
R_R<-rbind(IU_tbl, ID_tbl, LF_tbl, GB_tbl, AM_tbl, OS_tbl, OSmod_tbl,
           AMmod_tbl)

R_R$GPP_reduce<-(1-(R_R$GPP/R_R$GPP0))*100
R_R$ER_reduce<-(1-(R_R$ER0/R_R$ER))*100
R_R$GPP_reduce[R_R$GPP_reduce<0] <- NA
R_R$ER_reduce[R_R$ER_reduce<0] <- NA

write_csv(R_R, "04_Outputs/reduction_analysis.csv")

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
    xlab(hdiff)+ylab("GPP Reduction (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkgreen"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkgreen")))


(b<-ggplot(R_R, aes(h, shape=ID, color= IF))+
    geom_point(aes(y=ER_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(hdiff)+ylab("|ER| Increase (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkred"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkred")))
RR_noID<- R_R %>% filter(ID != 'ID')
summary(lm(ER_reduce ~ h, data=RR_noID))
summary(lm(GPP_reduce ~ h, data=RR_noID))

(hs<-plot_grid(a, b, nrow=1))

ggsave(filename="reduced.jpeg",
       plot = hs,
       width =12,
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

ggsave(filename="reduced legend.jpeg",
       plot = b,
       width =12,
       height = 10,
       units = "in")
