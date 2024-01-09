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

extract_reduce <- function(site, siteFR) {

  site_low<-filter(site, RI== 'low')

  NEP0<-mean(site_low$NEP, na.rm=T)
  ER0<-mean(site_low$ER, na.rm=T)
  GPP0<-mean(site_low$GPPavg, na.rm=T)
  h0<-mean(site_low$depth_diff, na.rm=T)
  u0<-mean(site_low$u, na.rm=T)

  siteFR<-filter(siteFR, RI== "2")

  h<-min(siteFR$depth_diff, na.rm = T)
  NEP<-min(siteFR$NEP, na.rm = T)
  ER<-min(siteFR$ER, na.rm = T)
  GPP<-min(siteFR$GPPavg, na.rm = T)
  u<-max(siteFR$u, na.rm = T)
  date<-min(siteFR$Date, na.rm = T)

  siteFR_ls<-list(h,NEP,GPP,ER,u, date)
  df<- data.frame(siteFR_ls[[1]],siteFR_ls[[2]],siteFR_ls[[3]],
                  siteFR_ls[[4]],siteFR_ls[[5]],siteFR_ls[[6]])
  colnames(df)[1]<-'h'
  colnames(df)[2]<-'NEP'
  colnames(df)[3]<-'GPP'
  colnames(df)[4]<-'ER'
  colnames(df)[5]<-'u'
  colnames(df)[6]<-'date'

  return(df)}

#get data####

master_chem <- read_csv("02_Clean_data/master.csv")
master_met <- read_csv("02_Clean_data/master_metabolism.csv")
master<-left_join(master_chem, master_met, by=c('ID','Date'))

for(i in 1:nrow(master)) {if(master$ID[i]=='OS') {
  master$u[i]<-(master$depth[i]*-0.0868+0.1579)*100}
  else if (master$ID[i]=='ID'){
    master$u[i]<-(master$depth[i]*-4.1+2.33)*100}
  else if(master$ID[i]=='GB'){
    master$u[i]<-(master$depth[i]*-0.768+0.51)*100}
  else if(master$ID[i]=='LF'){
    master$u[i]<- (master$depth[i]*-0.656 + 0.44)*100}
  else if(master$ID[i]=='AM'){
    master$u[i]<-(master$depth[i]*-1.89+1.4)*100}
  else {master$u[i]<- NULL }}

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master$depth_diff<-master$depth-master$depth_min

sites<-split(master,master$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
LF<-sites[[4]]
OS<-sites[[5]]

####GB####
GB<- GB %>% mutate(RI = case_when(
  depth<0.38 ~ "low",
  depth>=0.38 ~ "high"))

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-06-01" & Date<"2022-9-20"~ 2))
GB_0622<-extract_reduce(GB, GBFR)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-25" & Date<"2023-8-25"~ 2))
GB_0723<-extract_reduce(GB, GBFR)

GB_tbl<-rbind(GB_0622,GB_0723)
GB_tbl$ID<-'GB'
GB_tbl$num<-3
GB_tbl$IF <- c("h","h")

####Otter#####
OS<- OS %>% mutate(RI = case_when(
  depth<0.84 ~ "low",
  depth<1.27 ~ "moderate",
  depth>=1.27 ~ "high"))

OtBO<- OS %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-18"~ 2))
OS_0822<-extract_reduce(OS, OtBO)

OtFR<- OS %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-5-10"~ 2))
OS_0223<-extract_reduce(OS, OtFR)

OtBO<- OS %>% mutate(RI = case_when(
  Date> "2023-06-25" & Date<"2023-08-10"~ 2))
OS_0623<-extract_reduce(OS, OtBO)

####Otter mod######

Otterx<- Otterx %>% mutate(RI = case_when(
  depth<0.84 ~ "low",
  depth<1.27 ~ "moderate",
  depth>=1.27 ~ "high"))

Otter_low<-filter(Otterx, RI== 'low')

Otter_mod<-filter(Otterx, depth>1 & depth< 1.2)

h0<-mean(Otter_low$depth_diff, na.rm=T)
NEP0<-mean(Otter_low$NEP, na.rm=T)
ER0<-mean(Otter_low$ER, na.rm=T)
GPP0<-mean(Otter_low$GPPavg, na.rm=T)
u0<-mean(Otter_low$u, na.rm=T)

ggplot(Otter_mod) + geom_line(aes(Date, depth))+geom_hline(yintercept = 1)+geom_hline(yintercept = 1.2)


OSBO<-filter(Otter_mod, Date<'2023-01-01')
h8<-mean(OSBO$depth_diff, na.rm = T)
NEP8<-mean(OSBO$NEP, na.rm = T)
ER8<-mean(OSBO$ER, na.rm = T)
GPP8<-mean(Otter_mod$GPPavg, na.rm = T)
u8<-mean(OSBO$u, na.rm = T)

OSBO<-filter(Otter_mod, Date>'2023-01-01'& Date<'2023-06-01' )
h6<-mean(OSBO$depth_diff, na.rm = T)
NEP6<-mean(OSBO$NEP, na.rm = T)
ER6<-mean(OSBO$ER, na.rm = T)
GPP6<-mean(Otter_mod$GPPavg, na.rm = T)
u6<-mean(OSBO$u, na.rm = T)

OSBO<-filter(Otter_mod, Date>'2023-06-01' )
h9<-mean(OSBO$depth_diff, na.rm = T)
NEP9<-mean(OSBO$NEP, na.rm = T)
ER9<-mean(OSBO$ER, na.rm = T)
GPP9<-mean(Otter_mod$GPPavg, na.rm = T)
u9<-mean(OSBO$u, na.rm = T)
Otter_mod$ID<-5
############

event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, NEP8, "", NEP6, NEP9))
GPP<-as.numeric(c(GPP0, GPP8, "", GPP6, GPP9))
ER<-as.numeric(c(ER0, ER8, "", ER6, ER9))
depth<-as.numeric(c(h0, h8, "", h6, h9))
u<-as.numeric(c(u0, u8, "", u6, u9))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("",GPP8/GPP0, "",GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("",ER8/ER0,"",ER6/ER0,ER9/ER0))
depth_reduction<-as.numeric(c("",h8/h0,"",h6/h0,h9/h0))

Otter_mod<- data.frame(event,NEP,GPP,ER, depth,u, NEP_reduction,GPP_reduction,ER_reduction,depth_reduction)
Otter_mod$IF<-'h_high'
Otter_mod$site<-"Otter"
Otter_mod$ID<-5


####AllenMill##########

AM<- AllenMill %>% mutate(RI = case_when(
  depth<0.8 ~ "low",
  depth<1.37 ~ "moderate",
  depth>=1.37 ~ "high"))

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-03-01"~ 2))


AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-05-01" & Date<"2023-08-01"~ 2))


AMBO<- AM %>% mutate(RI = case_when(
  Date> "2023-08-22" & Date<"2023-10-01"~ 2))

######AM mod########

AMx<- AllenMill %>% mutate(RI = case_when(
  depth<0.7 ~ "low",
  depth<1.37 ~ "moderate",
  depth>=1.37 ~ "high"))

AM_low<-filter(AMx, RI== 'low')
h0<-mean(AM_low$depth_diff, na.rm=T)
NEP0<-mean(AM_low$NEP, na.rm=T)
ER0<-mean(AM_low$ER, na.rm=T)
GPP0<-mean(AM_low$GPPavg, na.rm=T)
u0<-mean(AM_low$u, na.rm=T)

AM_mod<-filter(AMx, depth>=1.1 & depth<1.4)
ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=depth), size=1)


AMFR<- AM_mod %>% mutate(RI = case_when( Date<"2023-02-09"~ 2))
AMFR<-filter(AMFR, RI== "2")


ggplot(AMFR, aes(x=Date))+
  geom_line(aes(y=ER), size=1)
h2<-mean(AMFR$depth_diff, na.rm = T)
NEP2<-mean(AMFR$NEP, na.rm = T)
(ER2<-min(AMFR$ER, na.rm = T))
(GPP2<-mean(AMFR$GPPavg, na.rm = T))
(u2<-mean(AMFR$u, na.rm = T))


#ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=depth), size=1)


AMBO<- AM_mod %>% mutate(RI = case_when(
  Date>"2023-06-15" &  Date<"2023-08-01"~ 2))
AMBO<-filter(AMBO, RI== "2")

ggplot(AMBO, aes(x=Date))+geom_line(aes(y=ER), size=0.4)


h6<-mean(AMBO$depth_diff, na.rm = T)
NEP6<-mean(AMBO$NEP, na.rm = T)
(ER6<-mean(AMBO$ER, na.rm = T))
(GPP6<-mean(AMBO$GPPavg, na.rm = T) )
(u6<-mean(AMBO$u, na.rm = T))

#ggplot(AM_mod, aes(x=Date))+geom_line(aes(y=depth), size=1)


AMBO<- AM_mod %>% mutate(RI = case_when(
  Date>"2023-07-31"~ 2))
AMBO<-filter(AMBO, RI== "2")

ggplot(AMBO, aes(x=Date))+geom_line(aes(y=ER), size=0.4)

h9<-mean(AMBO$depth_diff, na.rm = T)
NEP9<-mean(AMBO$NEP, na.rm = T)
(ER9<-mean(AMBO$ER, na.rm = T))
(GPP9<-mean(AMBO$GPPavg, na.rm = T) )
(u9<-mean(AMBO$u, na.rm = T))


########
event<-c('0','08','02','06','09')
NEP<-as.numeric(c(NEP0, "", "", NEP6, NEP9))
GPP<-as.numeric(c(GPP0, "", "", GPP6, GPP9))
ER<-as.numeric(c(ER0, "", "", ER6, ER9))
depth<-as.numeric(c(h0, "", "", h6, h9))
u<-as.numeric(c(u0, "", "", u6, u9))
NEP_reduction<-as.numeric(c("",NEP0/NEP8, NEP0/NEP2, NEP0/NEP6,""))
GPP_reduction<-as.numeric(c("","",  "",GPP6/GPP0,GPP9/GPP0))
ER_reduction<-as.numeric(c("","","",ER6/ER0,ER9/ER0))
depth_reduction<-as.numeric(c("","","",h6/h0,h9/h0))

AM_mod<- data.frame(event,NEP,GPP,ER, depth,u, NEP_reduction,GPP_reduction,ER_reduction,depth_reduction)
AM_mod$IF<-'h_high'
AM_mod$site<-"AM"
AM_mod$ID<-6

####LF############
LF<- LF %>% mutate(RI = case_when(
  depth<0.4 ~ "low",
  depth<0.61 ~ "moderate",
  depth>=0.61 ~ "high"))


LFFR<- LF %>%mutate(RI = case_when(
  Date> "2023-01-30" & Date<"2023-04-21"~ 2))

LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-18" & Date<"2023-08-03"~ 2))
LFRR<-filter(LFRR, RI== "2")

LFRR<- LF %>% mutate(RI = case_when(
  Date> "2023-08-19" & Date<"2023-10-20"~ 2))


######IDetucknee#######

ID<- ID %>% mutate(RI = case_when(
  depth<0.76 ~ "low",
  depth<1.37 ~ "moderate",
  depth>=1.37 ~ "high"))

IDRR<- ID %>% mutate(RI = case_when(
  Date> "2022-07-01" & Date<"2022-10-10"~ 2))

IDRR<- ID %>% mutate(RI = case_when(
  Date> "2023-01-21" & Date<"2023-04-30"~ 2))

IDRR<- ID %>% mutate(RI = case_when(
  Date> "2023-05-30" & Date<"2023-08-01"~ 2))

###########
R_R<-rbind(US27, IDetucknee, LF, GB, AM, Otter, Otter_mod, AM_mod )
R<-rbind(LF, GB, AM, Otter, Otter_mod, AM_mod )

R_R$GPP_reduction_percent<- (1-R_R$GPP_reduction)*100
R_R$ER_reduction_percent<- (1-R_R$ER_reduction)*-100

R_R$GPP_reduction[R_R$GPP_reduction<1] <- NA
R_R$ER_reduction[R_R$ER_reduction<1] <- NA
R_R$GPP_reduction_percent[R_R$GPP_reduction_percent<0] <- NA


R$GPP_reduction_percent<- (1-R$GPP_reduction)*100
R$ER_reduction_percent<- (1-R$ER_reduction)*-100

R$GPP_reduction[R$GPP_reduction<1] <- NA
R$ER_reduction[R$ER_reduction<1] <- NA


write_xlsx(R, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction_ammended.xlsx")
write_xlsx(R_R, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")

R_R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction.xlsx")
R <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/reduction_ammended.xlsx")

R_R$a<-'a'
R$a<-'a'
cols<-c(
  "h_high"="deepskyblue3",
  "h_brown"="burlywood4",
  "h_rev"="black")
R$IF <- factor(R$IF  , levels=c("h_high","h_brown","h_rev"))

h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             axis.title.y =element_text(size = 27, color="darkgreen"),
                             axis.title.y.right =element_text(size = 27, color='darkred'),
                             axis.title.x =element_text(size = 27),
                             plot.title = element_text(size = 22, color="darkgreen"),
                             legend.position = "none",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



ggplot(R_R, aes(depth, shape=site, color=IF))+
    geom_point(aes(y=GPP_reduction_percent), size=6)+
    geom_smooth(aes(x=depth, y=GPP_reduction_percent, group=a), color='darkgreen', size=0.75,
                data=R, se = FALSE, method='lm')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High depth Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on GPP")+
    xlab(hdiff)+ylab("GPP Reduction (%)")+theme_sam


ggplot(R_R, aes(depth, shape=site, color= IF))+
    geom_point(aes(y=ER_reduction_percent), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High depth Event", "Brownout","Flow Reversal"))+
    ggtitle("Backwater Flood Impacts on ER")+
    xlab(" ")+ylab("|ER| Increase (%)")+theme_sam

hs<-plot_grid(b, b1, nrow=1)

ggsave(filename="reduced.jpeg",
       plot = hs,
       width =12,
       height = 5.5,
       units = "in")

c<-ggplot(R_R, aes(ID, shape=site))+
  geom_point(aes(y=ER_reduction_percent), size=3,color="darkred")+
  geom_point(aes(y=GPP_reduction_percent), size=3, color='darkgreen' )+
  ggtitle("Inverted Flood Impacts by Site")+
  xlab("RR Fequency")+theme_sam+
  scale_y_continuous(name = "GPP Reduction %",
    sec.axis = sec_axis( trans=~., name="|ER| Increase %"))

plot_grid(c,hs,ncol=1)

ggsave(filename="reduced.jpeg",
       plot = hs,
       width =12,
       height = 6,
       units = "in")
