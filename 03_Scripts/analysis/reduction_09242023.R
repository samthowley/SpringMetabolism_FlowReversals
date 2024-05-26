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

  ID_low<-filter(ID, RI== 'low')

  NEP0<-mean(ID_low$NEP, na.rm=T)
  ER0<-mean(ID_low$ER, na.rm=T)
  GPP0<-mean(ID_low$GPPavg, na.rm=T)
  h0<-mean(ID_low$depth_diff, na.rm=T)

  IDFR<-filter(IDFR, RI== "2")

  h<-min(IDFR$depth_diff, na.rm = T)
  NEP<-min(IDFR$NEP, na.rm = T)
  ER<-min(IDFR$ER, na.rm = T)
  GPP<-min(IDFR$GPPavg, na.rm = T)
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

#get data####

master<- read_csv("02_Clean_data/master_met4.csv")

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master$depth_diff<-master$depth-master$depth_min

IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
LF<-IDs[[4]]
OS<-IDs[[5]]
IU<-IDs[[6]]

####GB####

# GBFRcheck<-filter(GBFR,RI==2 )
# ggplot(GBFRcheck, aes(Date, DO)) + geom_line()+
#   geom_hline(yintercept = 0.55)+
#   geom_hline(yintercept = 1)

GB<- GB %>% mutate(RI = case_when(
  depth<0.55 ~ "low",
  depth<0.55 ~ "moderate",
  depth>=1 ~ "high"))

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-10-20"~ 2))
GB_0622<-extract_reduce(GB, GBFR)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-31" & Date<"2023-8-21"~ 2))
GB_0723<-extract_reduce(GB, GBFR)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-12-18" & Date<"2024-02-11"~ 2))
GB_1223<-extract_reduce(GB, GBFR)


GB_tbl<-rbind(GB_0622,GB_0723,GB_1223)
GB_tbl$ID<-'GB'
GB_tbl$num<-3
GB_tbl$IF <- c("h","h",'rev')

####Otter#####

# OSFRcheck<-filter(OtFR,RI==2 )
# ggplot(OSFRcheck, aes(Date, DO, colour=RI)) + geom_line()+
#   geom_hline(yintercept = 0.84)+
#   geom_hline(yintercept = 1.27)

OS<- OS %>% mutate(RI = case_when(
  depth<0.84 ~ "low",
  depth<1.27 ~ "moderate",
  depth>=1.27 ~ "high"))

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-18"~ 2))
OS_0822<-extract_reduce(OS, OtFR)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-4-10"~ 2))
OS_0223<-extract_reduce(OS, OtFR)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-07-20"~ 2))
OS_0623<-extract_reduce(OS, OtFR)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-12-18" & Date<"2024-02-11"~ 2))
OS_1223<-extract_reduce(OS, OtFR)

OS_tbl<-rbind(OS_0822,OS_0223,OS_0623,OS_1223)
OS_tbl$ID<-'OS'
OS_tbl$num<-4
OS_tbl$IF <- c("bo","bo",'bo','rev')

####Otter mod######

# OSmodcheck<-filter(Otmod,RI==2 )
# ggplot(Otmod, aes(Date, DO)) + geom_line()+
#   geom_hline(yintercept = 0.84)+
#   geom_hline(yintercept = 1.27)

Otter_mod<-filter(OS, RI=='moderate')

Otmod<- Otter_mod %>% mutate(RI = case_when(Date<'2023-01-01'~ 2))
OSmod_0622<-extract_reduce(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-01-01'& Date<'2023-06-01' ~ 2))
OSmod_0623<-extract_reduce(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-06-01' & Date<'2023-11-21' ~ 2))
OSmod_0723<-extract_reduce(OS, Otmod)

Otmod<- Otter_mod %>% mutate(RI = case_when(Date>'2023-12-01' ~ 2))
OSmod_1223<-extract_reduce(OS, Otmod)

OSmod_tbl<-rbind(OSmod_0622,OSmod_0623,OSmod_0723)
OSmod_tbl$ID<-'OS'
OSmod_tbl$num<-4
OSmod_tbl$IF <- c("h","h",'h')
####AllenMill##########

# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AMFRcheck, aes(Date, DO)) + geom_line()+
#   geom_hline(yintercept = 0.8)+
#   geom_hline(yintercept = 1.37)

AM<- AM %>% mutate(RI = case_when(
  depth<0.8 ~ "low",
  depth<1.37 ~ "moderate",
  depth>=1.37 ~ "high"))

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-02-07" & Date<"2023-03-06"~ 2))
AMFR_0223<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-06-01" & Date<"2023-08-01"~ 2))
AMFR_0623<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-08-22" & Date<"2023-10-01"~ 2))
AMFR_0823<-extract_reduce(AM, AMFR)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-12-12" & Date<"2024-01-11"~ 2))
AMFR_1224<-extract_reduce(AM, AMFR)

AM_tbl<-rbind(AMFR_0223,AMFR_0623,AMFR_0823,AMFR_1224)
AM_tbl$ID<-'AM'
AM_tbl$num<-5
AM_tbl$IF <- c("rev","bo",'bo','rev')
######AM mod########
# AMFRcheck<-filter(AMFR,RI==2 )
# ggplot(AM, aes(Date, depth, colour=RI)) + geom_line()+
#   geom_hline(yintercept = 0.8)+
#   geom_hline(yintercept = 1.37)


AM_mod<-filter(AM, RI== 'moderate')
AMmod<- AM_mod %>% mutate(RI = case_when( Date<"2022-07-09"~ 2))
AMmod_0622<-extract_reduce(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when( Date>"2022-07-09" & Date<"2023-02-09"~ 2))
AMmod_0823<-extract_reduce(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-01-15" &  Date<"2023-05-01"~ 2))
AMmod_0223<-extract_reduce(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-06-15" &  Date<"2023-08-01"~ 2))
AMmod_0623<-extract_reduce(AM, AMmod)

AMmod<- AM_mod %>% mutate(RI = case_when(Date>"2023-07-31" & Date<"2023-12-31"~ 2))
AMmod_1223<-extract_reduce(AM, AMmod)

AMmod_tbl<-rbind(AMmod_0622,AMmod_0823,AMmod_0223,AMmod_0623,AMmod_1223)
AMmod_tbl$ID<-'AM'
AMmod_tbl$num<-5
AMmod_tbl$IF <- c("h","h",'h','h','h')

####LF############
# LFFRcheck<-filter(LFFR,RI==2 )
# ggplot(LFFRcheck, aes(Date, depth, colour=RI)) + geom_line()+
#   geom_hline(yintercept = 1)+
#   geom_hline(yintercept = 1.31)

LF<- LF %>% mutate(RI = case_when(
  depth<1 ~ "low",
  depth<1 ~ "moderate",
  depth>=1.31 ~ "high"))


LFFR<- LF %>%mutate(RI = case_when(
  Date> "2023-01-30" & Date<"2023-04-30"~ 2))
LF_0223<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-18" & Date<"2023-08-01"~ 2))
LF_0723<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-08-19" & Date<"2023-10-06"~ 2))
LF_0923<-extract_reduce(LF, LFFR)

LFFR<- LF %>% mutate(RI = case_when(
   Date> "2023-12-06" & Date<"2024-02-11"~ 2))
LF_0124<-extract_reduce(LF, LFFR)

LF_tbl<-rbind(LF_0223,LF_0723,LF_0923,LF_0124)
LF_tbl$ID<-'LF'
LF_tbl$num<-2
LF_tbl$IF <- c("h","h",'h','h')
######ID#######
# IDFRcheck<-filter(IDFR,RI==2 )
# ggplot(IDFRcheck, aes(Date, depth, colour=RI)) + geom_line()+
#   geom_hline(yintercept = 1)+
#   geom_hline(yintercept = 1.31)

ID<- ID %>% mutate(RI = case_when(
  depth<0.76 ~ "low",
  depth<1.17 ~ "moderate",
  depth>=1.17 ~ "high"))

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-01-21" & Date<"2023-04-30"~ 2))
ID_0223<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-05-30" & Date<"2023-08-01"~ 2))
ID_0823<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-08-30" & Date<"2023-10-31"~ 2))
ID_0923<-extract_reduce(ID, IDFR)

IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-01-31"~ 2))
ID_0124<-extract_reduce(ID, IDFR)

ID_tbl<-rbind(ID_0223,ID_0823,ID_0923,ID_0124)
ID_tbl$ID<-'ID'
ID_tbl$num<-1
ID_tbl$IF <- c("h","h",'h','h')

######IU#######
IU<-filter(IU, Date>'2022-05-02')

# IUFRcheck<-filter(IUFR,RI==2 )
# ggplot(IU, aes(Date, depth)) + geom_line()+
#   geom_hline(yintercept = 0.9)+
#   geom_hline(yintercept = 1.17)

IU<- IU %>% mutate(RI = case_when(
  depth<0.9 ~ "low",
  depth<1.17 ~ "moderate",
  depth>=1.17 ~ "high"))

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2022-08-20" & Date<"2022-10-20"~ 2))
IU_0822<-extract_reduce(IU, IUFR)

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-05-01"~ 2))
IU_0123<-extract_reduce(IU, IUFR)

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-06-30" & Date<"2023-10-31"~ 2))
IU_0623<-extract_reduce(IU, IUFR)

IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-01-31"~ 2))
IU_0124<-extract_reduce(IU, IUFR)

IU_tbl<-rbind(IU_0822,IU_0123,IU_0623,IU_0124)
IU_tbl$ID<-'IU'
IU_tbl$num<-1
IU_tbl$IF <- c("h","h",'h','h')
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
    xlab(" ")+ylab("|ER| Increase (%)")+theme_sam+theme(
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

ggplot(R_R, aes(ID, shape=ID))+
  geom_point(aes(y=ER_reduce), size=3,color="darkred")+
  geom_point(aes(y=GPP_reduce), size=3, color='darkgreen' )+
  ggtitle("Inverted Flood Impacts by ID")+
  xlab("RR Fequency")+theme_sam+
  scale_y_continuous(name = "GPP Reduction %",
    sec.axis = sec_axis( trans=~., name="|ER| Increase %"))

plot_grid(c,hs,ncol=1)

ggsave(filename="reduced.jpeg",
       plot = hs,
       width =12,
       height = 6,
       units = "in")
