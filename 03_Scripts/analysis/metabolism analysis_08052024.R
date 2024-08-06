
rm(list=ls())
##packages######
library(grid)
library(cowplot)
library(ggpmisc)
library(tidyverse)
library(readxl)
library(lme4)

library(nls2)

#constants######
flux<-expression(paste((g~O[2]/m^2/'day')))
col<-c(NEP ='blue', GPP='darkgreen',ER ='darkred')
DO<-"DO mg/L"
h<-expression(paste( h[i]-h[min]~(m)))
u<-expression(paste('Velocity'~("m"~s^-1)))
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
#get data####
metabolism<-read_csv('02_Clean_data/master_metabolism4.csv')
metabolism<-metabolism[,c('ER', 'ER_1','ER_2','GPP','GPP_1','GPP_2','NEP', 'Date', 'ID')]
metabolism<-metabolism %>%rename('day'='Date') %>% mutate(day=as.Date(day))

depth<-read_csv('02_Clean_data/master_depth2.csv')
depth$day<-as.Date(depth$Date)
master<-left_join(depth,metabolism, by=c('ID','day'))

master<- master[!duplicated(master[c('ID','Date')]),]
master<-master%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min, day=as.Date(day))
master <- master[!duplicated(master[c('day','ID')]),]


LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")

u<-rbind(LF_rC,AM_rC,GB_rC,OS_rC,ID_rC)

rC <- lmList(depth ~ u | ID, data=u)
(cf <- coef(rC))

master <- master %>%
  mutate(u= case_when(
    ID== 'AM'~ (10^cf[1,1]) *depth^(cf[1,2]),
    ID== 'GB'~ (10^cf[2,1]) *depth^(cf[2,2]),
    ID== 'ID'~ (10^cf[3,1]) *depth^(cf[3,2]),
    ID== 'LF'~ (10^cf[4,1]) *depth^(cf[4,2]),
    ID== 'OS'~ (10^cf[5,1]) *depth^(cf[5,2])))

sites<-split(master,master$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]

#Scatter data#######

GPP<-master %>%select(Date, depth, depth_diff, GPP, ID)%>% rename('prod'='GPP') %>% mutate(type='GPP', day=as.Date(Date))
GPP <- GPP[!duplicated(GPP[c('day','ID')]),]


ER<-master %>% select(Date, depth, depth_diff, ER, ID)%>%rename('prod'='ER') %>% mutate(type='ER', day=as.Date(Date))
ER <- ER[!duplicated(ER[c('day','ID')]),]

master_scatter<-rbind(GPP, ER)
master_scatter<-master_scatter %>% filter(type != 'NEP')

sites<-split(master_scatter,master_scatter$ID)
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

(AM_sc<-ggplot(data=AM_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("AM")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)
###############
AM <- AM[complete.cases(AM[ , c('GPP', 'depth','u')]), ]


start_vals<-expand.grid(a = seq(1, 10, length = 10), b = seq(0.5, 1.5, length = 10), c = seq(-1, 1, length = 10))

model_1var <- nls2(
  GPP ~ a * sin(b * depth + c),
  data = AM,
  start = expand.grid(a = seq(1, 10, length = 10), b = seq(0.5, 1.5, length = 10), c = seq(-1, 1, length = 10)),
  algorithm = "brute-force"
)

coef(model_1var)
AM$predicted_y <- predict(model_1var, newdata = AM)
plot(AM$depth, AM$GPP, main = "Non-linear Fit", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(AM$depth, AM$predicted_y, col = "red", lwd = 2)

model_exp <- nls2(
  GPP ~ a * exp(b * depth) * sin(c * u),
  data = AM,
  start = start_vals,
  algorithm = "brute-force"
)
AM$predicted_y <- predict(model_exp, newdata = AM)
plot(AM$depth, AM$GPP, main = "Non-linear Fit", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(AM$depth, AM$predicted_y, col = "red", lwd = 2)




AM_2<-AM %>% filter(u>0)
model <- nls2(
  GPP ~ a * -sin(depth + c) + d,
  data = AM_2,
  start = expand.grid(a = seq(1, 5, length = 5), c = seq(-1, 1, length = 3), 
                      d = seq(1, 3, length = 3)),
  algorithm = "brute-force"
)
AM_2$predicted_y <- predict(model_exp, newdata = AM_2)
plot(AM_2$depth, AM_2$GPP, main = "Non-linear Fit", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(AM_2$depth, AM_2$predicted_y, col = "red", lwd = 2)


# Plot the original data and the predictions
plot(AM$depth, AM$GPP, main = "GAM Fit", xlab = "x", ylab = "y")
lines(AM$depth, predictions, col = "blue", lwd = 2)



(ID_sc<-ggplot(data=ID_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("ID")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

(IU_sc<-ggplot(data=IU_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(LF_sc<-ggplot(data=LF_scatter, aes(x=depth_diff, y=prod, color=type)) +ggtitle("LF")+
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam)

(GB_sc<-ggplot(data=GB_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots+ggtitle("GB"))


(OS_sc<-ggplot(data=OS_scatter, aes(x=depth_diff, y=prod, color=type)) +
    geom_point(size=1)+ggtitle("OS")+
    scale_colour_manual(name="", values = cols,labels=c("GPP", "ER","NEP"))+
    ylab(flux)+xlab(h)+scale_x_continuous(n.breaks=4) + scale_y_continuous(n.breaks=3)+
    theme_sam_insideplots)

