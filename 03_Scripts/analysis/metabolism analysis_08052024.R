
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
metabolism<-metabolism%>%select(ER, ER_1, ER_2, GPP, GPP_1, GPP_2, Date, ID) %>%rename('day'='Date')  

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

###############
GB <- GB[complete.cases(GB[ , c('GPP', 'depth')]), ]

start_values <- expand.grid(
  a = seq(0, 15, length.out = 10),
  b = seq(0, 1, length.out = 10),
  c = seq(0, 5, length.out = 10))

#decaying
model_1var <- nls2(
  GPP ~ a * exp(b * depth) + c,
  data = GB,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10)),
  algorithm = "brute-force")

coef(model_1var)
GB$predicted_y <- predict(model_1var, newdata = GB)
ggplot(data=GB, aes(x=depth, y=GPP)) +
    geom_point(size=1)+ggtitle("GB")+geom_line(aes(y=predicted_y))
    

#quadractic
model_1var <- nls2(
  GPP ~ a * depth^2 + b * depth + c,
  data = GB,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10)),
  algorithm = "brute-force")

coef(model_1var)
GB$predicted_y <- predict(model_1var, newdata = GB)
ggplot(data=GB, aes(x=depth, y=GPP)) +
  geom_point(size=1)+ggtitle("GB")+geom_line(aes(y=predicted_y))



