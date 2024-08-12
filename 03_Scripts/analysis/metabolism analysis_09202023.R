rm(list=ls())
##packages######
library(ggpubr)
library(grid)
library(cowplot)
library(weathermetrics)
library(measurements)
library(streamMetabolizer)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
library(readxl)
library(gridGraphics)

#constants######

#NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste((g~O[2]/m^2/'day')))
#GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
#ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
col<-c(NEP ='blue', GPP='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
#slopey<-expression(paste('g'~O[2]/m^3/'day'))
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

GPP<-master[,c('Date','depth','depth_diff','GPP','ID')]
GPP<-GPP %>% rename('prod'='GPP') %>% mutate(type='GPP', day=as.Date(Date))
GPP <- GPP[!duplicated(GPP[c('day','ID')]),]


ER<-master[,c("Date",'depth','depth_diff','ER','ID')]
ER<-ER %>% rename('prod'='ER') %>% mutate(type='ER', day=as.Date(Date))
ER <- ER[!duplicated(ER[c('day','ID')]),]

master_scatter<-rbind(GPP, ER)

sites<-split(master_scatter,master_scatter$ID)
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
  #   stat_poly_line()+ggtitle("IU")+
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

#DO scatter plots####
master_chem<-read_csv('02_Clean_data/master_chem1.csv')
depth<-read_csv('02_Clean_data/master_depth2.csv')
depth<-depth[,c("Date", "depth", "ID")]
master_chem<-left_join(master_chem,depth, by=c('ID','Date'))

master_chem<-master_chem%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master_chem<-master_chem%>% mutate(depth_diff= depth-depth_min)

sites<-split(master_chem,master_chem$ID)
AM_chem<-sites[[1]]
GB_chem<-sites[[2]]
ID_chem<-sites[[3]]
IU_chem<-sites[[4]]
LF_chem<-sites[[5]]
OS_chem<-sites[[6]]


(ID_DO<-ggplot(data=ID_chem, aes(x=depth_diff, y=DO)) +
   geom_point(size=1)+theme_sam+xlab(h)+ggtitle("ID"))
(IU_DO<-ggplot(data=IU_chem, aes(x=depth_diff, y=DO)) +
    geom_point(size=1)+theme_sam+xlab(h)+ggtitle("IU"))
(LF_DO<-ggplot(data=LF_chem, aes(x=depth_diff, y=DO)) +
    geom_point(size=1)+theme_sam+xlab(h)+ggtitle("LF"))
(GB_DO<-ggplot(data=GB_chem, aes(x=depth_diff, y=DO)) +
    geom_point(size=1)+theme_sam+xlab(h)+ggtitle("GB"))
(AM_DO<-ggplot(data=AM_chem, aes(x=depth_diff, y=DO)) +
    geom_point(size=1)+theme_sam+xlab(h)+ggtitle("AM"))
(OS_DO<-ggplot(data=OS_chem, aes(x=depth_diff, y=DO))+ggtitle("OS")+
    geom_point(size=1)+theme_sam+xlab(h))

#Time Series####
DO.cols<-c(
  "Avg ER"="darkred",
  "Avg GPP"="darkgreen",
  "Two Station"="darkgray",
  "One Station"="black")

(ID_time<-ggplot(data=ID, aes(x=Date)) +
   geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
   geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
   geom_line(aes(y=GPP_1, color="One Station"),size=1)+
   geom_line(aes(y=ER_2, color="Two Station"),size=1)+
   geom_line(aes(y=ER, color="Avg ER"),size=1)+
   geom_line(aes(y=ER_1, color="One Station"),size=1)+
   theme_sam+ylab(flux)+ggtitle("ID")+
   scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))


(IU_time<-ggplot(data=IU, aes(x=Date)) +
    geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
    geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
    geom_line(aes(y=GPP_1, color="One Station"),size=1)+
    geom_line(aes(y=ER_2, color="Two Station"),size=1)+
    geom_line(aes(y=ER, color="Avg ER"),size=1)+
    geom_line(aes(y=ER_1, color="One Station"),size=1)+
    theme_sam+ylab(flux)+ggtitle("IU")+
    scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))

(LF_time<-ggplot(data=LF, aes(x=Date)) +
    geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
    geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
    geom_line(aes(y=GPP_1, color="One Station"),size=1)+
    geom_line(aes(y=ER_2, color="Two Station"),size=1)+
    geom_line(aes(y=ER, color="Avg ER"),size=1)+
    geom_line(aes(y=ER_1, color="One Station"),size=1)+
    theme_sam+ylab(flux)+ggtitle("LF")+  
    scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))

(AM_time<-ggplot(data=AM, aes(x=Date)) +
    geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
    geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
    geom_line(aes(y=GPP_1, color="One Station"),size=1)+
    geom_line(aes(y=ER_2, color="Two Station"),size=1)+
    geom_line(aes(y=ER, color="Avg ER"),size=1)+
    geom_line(aes(y=ER_1, color="One Station"),size=1)+
    theme_sam+ylab(flux)+ggtitle("AM")+ 
    scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))

(GB_time<-ggplot(data=GB, aes(x=Date)) +
    geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
    geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
    geom_line(aes(y=GPP_1, color="One Station"),size=1)+
    geom_line(aes(y=ER_2, color="Two Station"),size=1)+
    geom_line(aes(y=ER, color="Avg ER"),size=1)+
    geom_line(aes(y=ER_1, color="One Station"),size=1)+
    theme_sam+ylab(flux)+ggtitle("GB")+  
    scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))

(OS_time<-ggplot(data=OS, aes(x=Date)) +
    geom_line(aes(y=GPP_2, color="Two Station"),size=1)+
    geom_line(aes(y=GPP, color="Avg GPP"),size=1)+
    geom_line(aes(y=GPP_1, color="One Station"),size=1)+
    geom_line(aes(y=ER_2, color="Two Station"),size=1)+
    geom_line(aes(y=ER, color="Avg ER"),size=1)+
    geom_line(aes(y=ER_1, color="One Station"),size=1)+
    theme_sam+ylab(flux)+ggtitle("OS")+  
    scale_colour_manual(name="", values = DO.cols,labels=c("Avg GPP", "Avg ER","Two Station","One Station")))

timeseries<-plot_grid(ID_time,GB_time, LF_time, AM_time,nrow=1)

#slope######

OS_x<-slope_df(OS)
GB_x<-slope_df(GB)
ID_x<-slope_df(ID)
LF_x<-slope_df(LF)
AM_x<-slope_df(AM)
IU_x<-slope_df(IU)

slope_df<-rbind(OS_x, GB_x, LF_x, ID_x, AM_x,IU_x)

q<-c("ER"='darkred', "GPP"='darkgreen', "NEP"='blue')
expression(paste('slope'~'(g'~O[2]/m/'day)'))

(slope<-ggplot(slope_df,aes(x=name,y=met))+
  geom_boxplot(outlier.color="black", fill=q)+
  ggtitle("Metabolic Response to Rising Stage")+
  ylab(expression(paste('Slope'~'(g'~O[2]/m/'day)')))+xlab("")+theme_sam)

#box plots#########
master$ID <- factor(master$ID , levels=c("IU","ID", "GB", "LF", "OS", "AM"))

(ER<-ggplot(master, aes(x=ID, y=ER)) +
  geom_boxplot(outlier.colour="black", outlier.shape=NA,fill='darkred',coef=0)+
  ylab(flux)+ggtitle('ER')+ylim(0,-40)+
  scale_y_continuous(n.breaks=3, limits=c(-25,0))+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE)+theme_sam+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank()))

(GPP<-ggplot(master, aes(x=ID, y=GPP)) +
  geom_boxplot(outlier.colour="black", outlier.shape=NA,fill="darkgreen", coef=0)+
  ggtitle('GPP')+ylab(flux)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) +
  scale_y_continuous(n.breaks=3,  limits=c(0,21))+
    theme_sam+theme(axis.text.x=element_blank(),
          axis.title.x=element_blank()))

(NEP<-ggplot(master, aes(x=ID, y=NEP)) +
  geom_boxplot(outlier.colour="black", outlier.shape=NA,fill="blue", coef=0)+
  ylab(flux)+ggtitle('NEP')+ylim(20,-20)+
  scale_y_continuous(n.breaks=3, limits=c(-10, 5))+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam+
    theme(axis.title.x=element_blank()))

(box<-plot_grid(GPP, ER, NEP,ncol=1, align = 'v'))
#cherry pick#####
theme_chem<-theme()+theme(axis.text.x = element_blank(),
      axis.text.y.right = element_text(size = 17, angle=0, color="purple"),
      axis.text.y.left = element_text(size = 17, angle=0, color="black"),
      axis.title.y =element_text(size = 17, color="black", angle=90),
      axis.title.y.right =element_text(size = 17, color="purple", angle=270),
      axis.title.x =element_blank(),
      legend.position = "none",
      plot.title = element_blank(),
      panel.background = element_rect(fill = 'white'),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


theme_bland<-theme()+    theme(axis.text.x = element_blank(),
                               axis.text.y = element_text(size = 17, angle=0),
                               axis.title.y =element_text(size = 20, color = "black"),
                               axis.title.y.right =element_text(size = 17, color = "blue"),
                               axis.title.x =element_blank(),
                               plot.title = element_text(size = 17),
                               legend.position = "none",
                               panel.background = element_rect(fill = 'white'),
                               axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                               axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

high<-expression(paste("High Stage Event"~ (h[high])))
u<-expression(atop('Velocity',("m"~s^-1)))
SpC<-'Conductivity'
pCO2<-expression(paste(CO[2]~"ppm"))
FR<-expression(paste("Flow Reversal"~ (h[reversal])))
BO<-expression(paste("Brownout"~ (h[brown])))

####LF
library(mmand)
LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
rel_u <- lm(u ~ depth, data=LF_rC)
(cf <- coef(rel_u))
LF$u<-(LF$depth*cf[2]+cf[1])+0.1

LFFR<-filter(LF,  Date> "2022-12-06" & Date<"2023-05-20")
LFFR$u<-(LFFR$depth*cf[2]+cf[1])+0.01
LFFR$depth_diff<-gaussianSmooth(LFFR$depth_diff, 60)
LFFR$u<-gaussianSmooth(LFFR$u, 60)
LFFR$depth_diff<-gaussianSmooth(LFFR$depth_diff, 60)


(ch<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle("High Stage Event")+
    scale_y_continuous(n.breaks=3)+theme_bland)


(cu<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3, lim=c(-0.25,0.165))+theme_bland)

(c<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+theme_chem)

(LFg<-plot_grid(ch,cu,c, align = "v", ncol = 1, rel_heights = c(0.3,0.2,0.5)))


###Ot
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
rel_u <- lm(u ~ depth, data=OS_rC)
(cf <- coef(rel_u))
OS$u<-(OS$depth*cf[2]+cf[1])

OtBO<-filter(OS,  Date> "2022-08-18" & Date<"2022-10-12")
OtBO$u<-gaussianSmooth(OtBO$u, 70)
OtBO$depth_diff<-gaussianSmooth(OtBO$depth_diff, 70)

(bu<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3, lim=c(-0.365,0.165))+
    theme_bland)

(bh<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+ggtitle("Brown Out")+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3)+theme_bland)

OtBO<-filter(OtBO, CO2>2000)
(b<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+theme_chem)

(OSg<-plot_grid(bh,bu,b, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

###AM
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
rel_u <- lm(u ~ depth, data=AM_rC)
(cf <- coef(rel_u))
AM$u<-(AM$depth*cf[2]+cf[1])

AMFR<-filter(AM,  Date> "2023-12-15" & Date<"2024-01-11")
AMFR$depth_diff<-gaussianSmooth(AMFR$depth_diff, 6)
AMFR$u<-gaussianSmooth(AMFR$u, 6)

(au<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    scale_y_continuous(n.breaks=3, lim=c(-0.365,0.165))+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+theme_bland)

(ah<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle("Flow Reversal")+
    scale_y_continuous(n.breaks=3)+theme_bland)

(a<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+theme_chem)

(AMg<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

hypoxia<-plot_grid(LFg, OSg, AMg, ncol = 3, align='h')

#velocity example
AMFR<-filter(AM,  Date> "2023-11-15" & Date<"2023-12-21")
AMFR$depth_diff<-gaussianSmooth(AMFR$depth_diff, 6)
AMFR$u<-gaussianSmooth(AMFR$u, 6)

(au<-ggplot(AMFR, aes(x=depth))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    scale_y_continuous(n.breaks=3)+
    ylab(u)+xlab('Depth (m)')+
    geom_hline(yintercept = 0, linetype='dashed')+theme_bland+
    theme(axis.title.x = element_text(size=17)))

#plot grids and save figures#####

scatter<-plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,nrow=2)
boxplots<-plot_grid(box,slope,  ncol=2)
together<-plot_grid(boxplots,scatter, nrow=2, rel_heights = c(3/5,1.7/5))
scatter_ex<-plot_grid(box,cherrypick_scatter,  ncol=2)
scatterDO<-plot_grid(IU_DO, ID_DO,GB_DO, LF_DO, OS_DO, AM_DO,nrow=2)

ggsave(filename="05_Figures/flood types.jpeg",
       plot = hypoxia,
       width =20,
       height = 12,
       units = "in")

ggsave(filename="05_Figures/all metabolism plots.jpeg",
       plot = together,
       width =12,
       height = 14.5,
       units = "in")

ggsave(filename="05_Figures/metabolism boxplots.jpeg",
       plot = boxplots,
       width =17,
       height = 12,
       units = "in")
ggsave(filename="05_Figures/scatter.jpeg",
       plot = scatter,
       width =18,
       height = 14.5,
       units = "in")

# width =42,
# height = 8,

ggsave(filename="05_Figures/scatterDO.jpeg",
       plot = scatterDO,
       width =18,
       height = 14.5,
       units = "in")

ggsave(filename="05_Figures/metabolism_timeseries.jpeg",
       plot = timeseries,
       width =42,
       height = 16,
       units = "in")

#present interest####
OS_in<-filter(OS, Date>'2023-11-01')
LF_in<-filter(LF, Date>'2024-04-01')
AM_in<-filter(AM, Date>'2023-11-01')
GB_in<-filter(GB, Date>'2023-11-01')
IU_in<-filter(IU, Date>'2023-11-01')
ID_in<-filter(ID, Date>'2023-11-01')
names(LF)
a<-ggplot(data=OS_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("OS")
b<-ggplot(data=OS_in, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
c<-ggplot(data=OS_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
d<-ggplot(data=OS_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
OS_zoom<-plot_grid(a,b,c,d, ncol=1, align = 'v')

e<-ggplot(data=LF_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("LF")
f<-ggplot(data=LF, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
g<-ggplot(data=LF_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
ggplot(data=LF_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
(LF_zoom<-plot_grid(e,f,g,h, ncol=1, align = 'v'))

i_<-ggplot(data=AM_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("AM")
j<-ggplot(data=AM_in, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
k<-ggplot(data=AM_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
l<-ggplot(data=AM_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
AM_zoom<-plot_grid(i_,j,k,l, ncol=1, align = 'v')

m<-ggplot(data=GB_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("GB")
GB_in$SpC[GB_in$SpC>450]<-NA
n<-ggplot(data=GB_in, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
o<-ggplot(data=GB_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
p<-ggplot(data=GB_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
GB_zoom<-plot_grid(m,n,o,p, ncol=1, align = 'v')

u<-ggplot(data=IU_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("IU")
v<-ggplot(data=IU_in, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
w<-ggplot(data=IU_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
x<-ggplot(data=IU_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
IU_zoom<-plot_grid(u,v,w,x, ncol=1, align = 'v')

q<-ggplot(data=ID_in, aes(x=Date)) +geom_line(aes(y=depth),size=1)+theme_sam+ggtitle("ID")
r<-ggplot(data=ID_in, aes(x=Date)) +geom_line(aes(y=SpC),size=1)+theme_sam
s<-ggplot(data=ID_in, aes(x=Date)) +geom_line(aes(y=DO),size=1)+theme_sam
t<-ggplot(data=ID_in, aes(x=Date)) +
  geom_line(aes(y=GPP),size=1)+geom_line(aes(y=ER),size=1)+theme_sam+ylab(flux)
ID_zoom<-plot_grid(q,r,s,t, ncol=1)

interested1<-plot_grid(IU_zoom, ID_zoom, LF_zoom, ncol=3)
interested2<-plot_grid(GB_zoom, OS_zoom, AM_zoom, ncol=3)

ggsave(filename="05_Figures/interested1.jpeg",
       plot = interested1,
       width =35,
       height = 18,
       units = "in")


ggsave(filename="05_Figures/interested2.jpeg",
       plot = interested2,
       width =35,
       height = 18,
       units = "in")
