rm(list=ls())
##packages######
library(ggpubr)
library(grid)
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
theme_poster<-theme()+    theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_blank(),
                             axis.title.x =element_blank(),
                             plot.title = element_text(size = 24),
                             legend.position = "none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

extract_slope <- function(site) {
  (siteNEP<-lm(NEP ~ depth_diff, data = site))
  cf <- coef(siteNEP)
  (SlopesiteNEP <- cf[2])
  (SlopeInterNEP <- cf[1])


  (siteGPPavg<-lm(GPPavg ~ depth_diff, data = site))
  cf <- coef(siteGPPavg)
  (SlopesiteGPPavg <- cf[2])
  (SlopeInterGPPavg <- cf[1])

  (siteER<-lm(ER*-1~ depth_diff, data = site))
  cf <- coef(siteER)
  (SlopesiteER <- cf[2])
  (SlopeInterER <- cf[1])

  NEP<-as.numeric(c(SlopesiteNEP))
  GPP<-as.numeric(c(SlopesiteGPPavg))
  ER<-as.numeric(c(SlopesiteER))

  NEPInter<-as.numeric(c(SlopeInterNEP))
  GPPInter<-as.numeric(c(SlopeInterGPPavg))
  ERInter<-as.numeric(c(SlopeInterER))

  return(list(NEP,GPP,ER,SlopeInterNEP,SlopeInterGPPavg,SlopeInterER))}
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

master<- read_csv("02_Clean_data/master_met4.csv")
# master_parsed<-master %>% filter(ID!='GB', ID!='OS')
#
# master_met4_notparsed<- read_csv("02_Clean_data/master_met4_notparsed.csv")
# master_met4_notparsed<-master_met4_notparsed %>% filter(ID=='GB' | ID=='OS')
# master<-rbind(master_parsed, master_met4_notparsed)

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min)

master$depth_diff<-master$depth-master$depth_min
master$day<-as.Date(master$Date)
master_met <- master[!duplicated(master[c('ID','day')]),]

sites<-split(master_met,master_met$ID)
names(sites)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]
###Scatter plots#######

(ID_sc<-ggplot(data=ID, aes(x=depth_diff)) +
   geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
   geom_point(aes(y=ER*-1), size=1, color='darkred')+
   geom_point(aes(y=NEP), size=1, color='blue')+
   ylab(flux)+scale_color_manual(values='black')+
   geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
               data=ID, se = FALSE, method='lm')+
   geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
               data=ID, se = FALSE, method='lm')+
   geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
               data=ID, se = FALSE, method='lm')+
   xlab(poster_x)+ggtitle("ID")+
   scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+theme_poster)
lm(GPPavg ~ depth_diff, data = ID)

(IU_sc<-ggplot(data=IU, aes(x=depth_diff)) +
    geom_point(aes(y=GPPavg), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    ylab(flux)+scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPPavg), color='darkgreen', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=IU, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=IU, se = FALSE, method='lm')+
    xlab(poster_x)+ggtitle("IU")+
    scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+theme_sam)


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
      scale_y_continuous(n.breaks=3)+ theme_poster)

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
    xlab(h)+ggtitle("LF")+scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_poster)

# library(mmand)
# GB<-GB %>% mutate(ER_smooth=gaussianSmooth(ER,1.5),
#                   GPP_smooth=gaussianSmooth(GPPavg,1.5),
#                   NEP_smooth=gaussianSmooth(NEP,1.5))
# ggplot(data=GB, aes(x=Date)) +
#   geom_line(aes(y=depth*5), size=1)+
#   geom_line(aes(y=GPP_smooth), size=1, color='darkgreen')+
#   geom_line(aes(y=ER_smooth), size=1, color='darkred')

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
    xlab(h)+ggtitle("GB")+scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_poster)


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
    xlab(h)+ggtitle("OS")+scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+theme_poster)

#######slope######

OS_x<-slope_df(OS)
GB_x<-slope_df(GB)
ID_x<-slope_df(ID)
LF_x<-slope_df(LF)
AM_x<-slope_df(AM)
IU_x<-slope_df(IU)


slope<-rbind(OS_x, GB_x, LF_x, ID_x, AM_x,IU_x)

q<-c("ER"='darkred', "GPP"='darkgreen', "NEP"='blue')
expression(paste('slope'~'(g'~O[2]/m^2/'day)'/'h'))

slope<-ggplot(slope,aes(x=name,y=met))+
  geom_boxplot(outlier.color="black", fill=q)+
  ggtitle("Metabolic Response to Rising Stage")+
  ylab(expression(paste('Slope'~'(g'~O[2]/m^2/'day)'/'h')))+xlab("")+theme_sam
#####box plots#########
master$ID <- factor(master$ID , levels=c("IU","ID", "GB", "LF", "OS", "AM"))

(ER<-ggplot(master, aes(x=ID, y=ER)) +
  geom_boxplot(outlier.colour="black", outlier.size=1,fill='darkred')+
  ylab(flux)+
  ggtitle(ERflux)+
  ylim(0,-40)+
  scale_y_continuous(n.breaks=3)+
  stat_summary(fun=mean, colour="white", geom="point",
               size=1, show.legend=FALSE) + theme_sam)

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
library(mmand)

LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
rel_u <- lm(u ~ depth, data=LF_rC)
(cf <- coef(rel_u))
LF$u<-(LF$depth*cf[2]+cf[1])

LFFR<-filter(LF,  Date> "2022-12-06" & Date<"2023-05-20")
# LFFR$depth<-gaussianSmooth(LFFR$depth, 120)
# LFFR$depth_diff<-gaussianSmooth(LFFR$depth_diff, 120)


(ch<-ggplot(LFFR, aes(x=Date))+
    geom_line(aes(y=depth_diff), color="black", linewidth=0.8)+
    ylab(h)+xlab('Date')+
    ggtitle(high, subtitle = "LF")+
    scale_y_continuous(n.breaks=3)+theme_sam)


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
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    scale_y_continuous(
      name = "DO mg/L")+theme_sam)

(bm<-ggplot(OtBO, aes(x=Date))+
    geom_line(aes(y=GPPavg), color="black", linewidth=0.8)+
    theme_sam)


(OSg<-plot_grid(bh,bu,b, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

###AM#####
AMFR<-filter(AM,  Date> "2023-12-15" & Date<"2024-01-11")
# AMFR$depth_diff<-gaussianSmooth(AMFR$depth_diff, 120)

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
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    scale_y_continuous(
      name = "DO mg/L")+theme_sam)

(am<-ggplot(AMFR, aes(x=Date))+
    geom_line(aes(y=GPPavg), color="black", linewidth=0.8)+
    theme_sam)

(AMg<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

###GB#####
GBFR<-filter(GB,  Date> "2023-12-01" & Date<"2024-01-11")
# GBFR$depth_diff<-gaussianSmooth(GBFR$depth_diff, 120)

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
    geom_line(aes(y=DO), color="black", linewidth=0.8)+
    scale_y_continuous(
      name = "DO mg/L") +theme_sam)

(am<-ggplot(GBFR, aes(x=Date))+
    geom_line(aes(y=GPPavg), color="black", linewidth=0.8)+
    theme_sam)

(GBg<-plot_grid(ah,au,a, align = "v", ncol = 1, rel_heights = c(0.2,0.2,0.5)))

plot_grid(GBg, AMg, OSg, ncol = 3)
####together#####
scatter<-plot_grid(IU_sc, ID_sc,GB_sc, LF_sc, OS_sc, AM_sc,nrow=2)
boxplots<-plot_grid(box,slope,  ncol=2)
together<-plot_grid(boxplots,scatter, nrow=2, rel_heights = c(3/5,1.7/5))

ggsave(filename="metabolism.jpeg",
       plot = together,
       width =12,
       height = 14.5,
       units = "in")

ggsave(filename="poster metabolism.jpeg",
       plot = boxplots,
       width =17,
       height = 12,
       units = "in")

ggplot(GB, aes(x=Date))+
    geom_line(aes(y=CO2), color="purple", linewidth=0.8)
