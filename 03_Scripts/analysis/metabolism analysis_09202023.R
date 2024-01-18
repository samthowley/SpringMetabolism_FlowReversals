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
#constants######

NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
flux<-expression(paste((g~O[2]/m^2/'day')))
GPPflux<-expression(paste('GPP'~'(g'~O[2]/m^2/'day)'))
ERflux<-expression(paste('ER'~'(g'~O[2]/m^2/'day)'))
col<-c(NEP ='blue', GPPavg='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
slopey<-expression(paste('g'~O[2]/m^3/'day'))
theme_sam<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_text(size = 24, color = "black"),
                             axis.title.x =element_text(size = 24),
                             plot.title = element_text(size = 24),
                             legend.position = "none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
extract_slope <- function(site) {
  (siteNEP<-lm(NEP ~ depth_diff, data = site))
  cf <- coef(siteNEP)
  (SlopesiteNEP <- cf[2])

  (siteGPPavg<-lm(GPPavg ~ depth_diff, data = site))
  cf <- coef(siteGPPavg)
  (SlopesiteGPPavg <- cf[2])

  (siteER<-lm(ER*-1~ depth_diff, data = site))
  cf <- coef(siteER)
  (SlopesiteER <- cf[2])

  NEP<-as.numeric(c(SlopesiteNEP))
  GPP<-as.numeric(c(SlopesiteGPPavg))
  ER<-as.numeric(c(SlopesiteER))

  return(list(NEP,GPP,ER))}
slope_df <- function(site) {
  slopes<-extract_slope(site)
  df<- data.frame(slopes[[1]],slopes[[2]],slopes[[3]])
  df<-pivot_longer(df, cols = 1:3, values_to = 'met') #wide to long
  df$name[df$name == 'slopes..1..'] <- 'NEP'
  df$name[df$name == 'slopes..2..'] <- 'GPP'
  df$name[df$name == 'slopes..3..'] <- 'ER'
  return(df)}

#get data####

master_chem <- read_csv("02_Clean_data/master.csv")
master_met <- read_csv("02_Clean_data/master_metabolism.csv")
master<-left_join(master_chem, master_met, by=c('ID','Date'))
master <- master[!duplicated(master[c('ID','Date')]),]


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

ggplot(master, aes(Date, u)) + geom_line() + facet_wrap(~ ID, ncol=2)

###Scatter plots#######

(ID_sc<-ggplot(data=ID, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_colour_manual(name="", values = col)+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=ID, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=ID, se = FALSE, method='lm')+
    scale_x_continuous(n.breaks=4) +scale_y_continuous(n.breaks=3)+theme_sam)

    xlab(h)+ggtitle("ID")

#
# (IU_sc<-ggplot(data=IU, aes(x=depth_diff)) +
#     geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
#     geom_point(aes(y=ER*-1), size=1, color='darkred')+
#     geom_point(aes(y=NEP), size=1, color='blue')+
#     ylab(flux)+scale_color_manual(values='black')+
#     geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
#                 data=IU, se = FALSE, method='lm')+
#     geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
#                 data=IU, se = FALSE, method='lm')+
#     geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
#                 data=IU, se = FALSE, method='lm')+
#     xlab(h)+ggtitle("IU")+
#     scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+theme_sam)
#
# lm(NEP~depth_diff, data = IU)
# lm(ER~depth_diff, data = IU)
# lm(GPPavg~depth_diff, data = IU)


  (AM_sc<-ggplot(data=AM, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=AM, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=AM, se = FALSE, method='lm')+
    xlab(h)+ggtitle("AM")+
      scale_x_continuous(n.breaks=4) +
      scale_y_continuous(n.breaks=3)+ theme_sam)

(LF_sc<-ggplot(data=LF, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=LF, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=LF, se = FALSE, method='lm')+
    xlab(h)+ggtitle("LF")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)

(GB_sc<-ggplot(data=GB, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    xlab(h)+ggtitle("GB")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)

(OS_sc<-ggplot(data=OS, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=OS, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=OS, se = FALSE, method='lm')+
    xlab(h)+ggtitle("OS")+
    scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_sam)
#######slope######

OS_x<-slope_df(OS)
GB_x<-slope_df(GB)
ID_x<-slope_df(ID)
LF_x<-slope_df(LF)
AM_x<-slope_df(AM)

slope<-rbind(OS_x, GB_x, LF_x, ID_x, AM_x)

q<-c("ER"='darkred', "GPP"='darkgreen', "NEP"='blue')

ggplot(slope,aes(x=name,y=met))+
  geom_boxplot(outlier.color="black", fill=q)+
  ggtitle("Slope Among Sites for GPP, ER and NEP")+
  ylab(slopey)+xlab("")+theme_sam
#####box plots#########
master$ID <- factor(master$ID , levels=c("ID", "GB", "LF", "OS", "AM"))

ER<-ggplot(master, aes(x=ID, y=ER)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill='darkred')+
  ylab(flux)+
  ggtitle(ERflux)+
  ylim(0,-40)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam

GPP<-ggplot(master, aes(x=ID, y=GPPavg)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="darkgreen")+
  ggtitle(GPPflux)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) +
  scale_y_continuous(n.breaks=3)+theme_sam

NEP<-ggplot(master, aes(x=ID, y=NEP)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill="blue")+
  ylab(flux)+
  ggtitle(NEPflux)+
  ylim(20,-20)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam

(box<-plot_grid(GPP, NEP, ER, ncol=1))
######cherry pick#####
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
                               axis.title.y =element_text(size = 13, color = "black"),
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
library(mmand)

####LF#####
LFFR<-filter(LF,  Date> "2023-02-01" & Date<"2023-04-13")
LFFR$depth<-gaussianSmooth(LFFR$depth, 120)
LFFR$depth_diff<-gaussianSmooth(LFFR$depth_diff, 120)


(ch<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle(high, subtitle = "LF")+
    scale_y_continuous(n.breaks=3)+theme_bland)


(cu<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=u), color="black", linewidth=0.8)+
    ylab(u)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3)+theme_bland)

(c<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+theme_chem)

(LF<-plot_grid(ch,cu,c, align = "v", ncol = 1, rel_heights = c(0.3,0.2,0.5)))


###Ot####
OtBO<-filter(OS,  Date> "2023-12-18" & Date<"2024-01-11")

(bu<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=SpC), color="black", linewidth=0.8)+
    ylab(SpC)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(n.breaks=3)+theme_bland)

(bh<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle(FR, subtitle = "OS, 12/20/2023")+
    scale_y_continuous(n.breaks=3)+theme_bland)

(b<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2))+theme_chem)

(OSg<-plot_grid(bh,bu,b, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

###AM#####
AMFR<-filter(AM,  Date> "2023-12-14" & Date<"2024-01-09")
AMFR$depth_diff<-gaussianSmooth(AMFR$depth_diff, 120)

(au<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=SpC), color="black", linewidth=0.8)+
    scale_y_continuous(n.breaks=3)+
    ylab(SpC)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+theme_bland)

(ah<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle(FR, subtitle = "AM, 12/14/2023")+
    scale_y_continuous(n.breaks=3)+theme_bland)

(a<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2)) +theme_chem)

(AMg<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

###GB#####
GBFR<-filter(GB,  Date> "2023-12-18" & Date<"2024-01-09")
GBFR$depth_diff<-gaussianSmooth(GBFR$depth_diff, 120)

(au<-ggplot(GBFR, aes(x=Date))+
    geom_line(aes(y=SpC), color="black", linewidth=0.8)+
    scale_y_continuous(n.breaks=3)+
    ylab(SpC)+xlab('Date')+
    geom_hline(yintercept = 0, linetype='dashed')+theme_bland)

(ah<-ggplot(GBFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle(FR, subtitle = "GB, 12/28/2023")+
    scale_y_continuous(n.breaks=3)+theme_bland)

(a<-ggplot(GBFR, aes(x=Date))+
    geom_line(aes(y=CO2/1000), color="purple", linewidth=0.8)+
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    geom_hline(yintercept = 0, linetype='dashed')+
    scale_y_continuous(
      name = "DO mg/L",
      sec.axis = sec_axis( trans=~.*1000, name=pCO2)) +theme_chem)

(GBg<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

plot_grid(GBg, AMg, OSg, ncol = 3)
####together#####
(scatter<-plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,nrow=2))
(boxplots<-plot_grid(box,slope,  ncol=2))
together<-plot_grid(scatter, boxplots, nrow=2, rel_heights = c(3/5,1.7/5))

ggsave(filename="metabolism.jpeg",
       plot = together,
       width =12,
       height = 14.5,
       units = "in")

ggsave(filename="poster metabolism.jpeg",
       plot = together,
       width =17,
       height = 15.5,
       units = "in")

ggplot(GB, aes(x=Date))+
    geom_line(aes(y=CO2), color="purple", linewidth=0.8)
