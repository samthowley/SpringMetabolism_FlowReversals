rm(list=ls())

library(ggpubr)
library(grid)
library(cowplot)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
library(readxl)
library(stats)
library(minpack.lm)

#functions####
river_elevation <- function(site_id,parameterCd) {
  river <- readNWISuv(site_id,parameterCd)
  split<-split(river, river$site_no)
  
  up_gage <-split(river, river$site_no)[[2]]
  up_gage<-up_gage[,c(3,4)]
  up_gage<-rename(up_gage, 'Date'='dateTime', 'depth_up'='X_00065_00000')
  
  down_gage <-split(river, river$site_no)[[1]]
  down_gage<-down_gage[,c(3,4)]
  down_gage<-rename(down_gage, 'Date'='dateTime', 'depth_down'='X_00065_00000')
  
  river<-left_join(down_gage,up_gage, by='Date')
  
  river<- river %>% mutate(minute = minute(Date))
  river<-river %>% filter(minute==0) %>% 
    mutate(elevation=abs(depth_up-depth_down)*proportion)
  river<-river[,c(1,5)]
  return(river)}
for_wilcox <- function(site_id,parameterCd) {
  river <- readNWISuv(site_id,parameterCd)

  river<-river[,c(3,4)]
  river<-rename(river, 'Date'='dateTime', 'elevation'='X_00065_00000')
  
  river<- river %>% mutate(minute = minute(Date))
  river<-river %>% filter(minute==0)
  river<-river[,c(1,2)]
  return(river)}



interpolation_lm<- function(site, river, el_lowbound, el_upbound,
                            h_lowbound,h_upbound) {
  
  site_river<-left_join(river,site)
  
  site_river<- site_river %>% mutate(elevationID = case_when(
    elevation<el_lowbound  ~ "low",
    elevation>el_lowbound & elevation<=el_upbound ~ "moderate",
    elevation>=el_upbound~ "high"))
  site_river$elevationID[is.na(site_river$elevationID)]<-"high"
  
  
  mod<-lm(depth~elevation*elevationID, data=site_river)
  site_river$h_pred<-predict(mod, site_river)
  
  site_river<- site_river %>% mutate(depthID = case_when(
    h_pred<h_lowbound  ~ "low",
    h_pred>h_lowbound & h_pred<=h_upbound ~ "moderate",
    h_pred>=h_upbound~ "high"))
  site_river$depthID[is.na(site_river$depthID)]<-"high"
  
  mod<-lm(GPP~h_pred, data=site_river)
  site_river$GPP_lm<-predict(mod, site_river)
  
  mod<-lm(ER~h_pred, data=site_river)
  site_river$ER_lm<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio_lm=ER_lm/ER,
                                    GPP_ratio_lm=GPP_lm/GPP,
                                    h_ratio=h_pred/depth)
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_lm[site_river$ER_lm>-3]<- -3
  site_river$GPP_lm[site_river$GPP_lm<0]<- 0
  
  return(site_river)}

interpolation_lm_interaction <- function(site, river, el_lowbound, el_upbound,
                                         h_lowbound,h_upbound) {
  
  site_river<-left_join(river,site)
  
  site_river<- site_river %>% mutate(elevationID = case_when(
    elevation<el_lowbound  ~ "low",
    elevation>el_lowbound & elevation<=el_upbound ~ "moderate",
    elevation>=el_upbound~ "high"))
  site_river$elevationID[is.na(site_river$elevationID)]<-"high"
  
  
  mod<-lm(depth~elevation*elevationID, data=site_river)
  site_river$h_pred<-predict(mod, site_river)
  
  site_river<- site_river %>% mutate(depthID = case_when(
    h_pred<h_lowbound  ~ "low",
    h_pred>h_lowbound & h_pred<=h_upbound ~ "moderate",
    h_pred>=h_upbound~ "high"))
  site_river$depthID[is.na(site_river$depthID)]<-"high"
  
  mod<-lm(GPP~h_pred*depthID, data=site_river)
  site_river$GPP_lm_interaction<-predict(mod, site_river)
  
  mod<-lm(ER~h_pred*depthID, data=site_river)
  site_river$ER_lm_interaction<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio_lm_interaction=ER_lm_interaction/ER,
                                    GPP_ratio_lm_interaction=GPP_lm_interaction/GPP,
                                    h_ratio=h_pred/depth)
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_lm_interaction[site_river$ER_lm_interaction>-3]<- -3
  site_river$GPP_lm_interaction[site_river$GPP_lm_interaction<0]<- 0

  return(site_river)}

interpolation_poly <- function(site, river, el_lowbound, el_upbound,
                               h_lowbound,h_upbound) {
  
  site_river<-left_join(site, river)
  
  site_river<- site_river %>% mutate(elevationID = case_when(
    elevation<el_lowbound  ~ "low",
    elevation>el_lowbound & elevation<=el_upbound ~ "moderate",
    elevation>=el_upbound~ "high"))
  
  mod<-lm(depth~elevation*elevationID, data=site_river)
  site_river$h_pred<-predict(mod, site_river)
  
  site_river<- site_river %>% mutate(depthID = case_when(
    h_pred<h_lowbound  ~ "low",
    h_pred>h_lowbound & h_pred<=h_upbound ~ "moderate",
    h_pred>=h_upbound~ "high"))
  site_river$depthID[is.na(site_river$depthID)]<-"high"
  
  mod <- lm(GPP ~ poly(h_pred, 2, raw = TRUE), data = site_river)
  site_river$GPP_poly<-predict(mod, site_river)
  
  mod <- lm(ER ~ poly(h_pred, 2, raw = TRUE), data = site_river)
  site_river$ER_poly<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio_poly=ER_poly/ER,
                                    GPP_ratio_poly=GPP_poly/GPP,
                                    h_ratio=h_pred/depth)
  
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_poly[site_river$ER_poly>-3]<- -3
  site_river$GPP_poly[site_river$GPP_poly<0]<- 0
  
  return(site_river)}

interpolation_poly_interaction <- function(site, river, el_lowbound, el_upbound,
                                           h_lowbound,h_upbound) {
  
  site_river<-left_join(site, river)
  
  site_river<- site_river %>% mutate(elevationID = case_when(
    elevation<el_lowbound  ~ "low",
    elevation>el_lowbound & elevation<=el_upbound ~ "moderate",
    elevation>=el_upbound~ "high"))
  
  mod<-lm(depth~elevation*elevationID, data=site_river)
  site_river$h_pred<-predict(mod, site_river)
  
  site_river<- site_river %>% mutate(depthID = case_when(
    h_pred<h_lowbound  ~ "low",
    h_pred>h_lowbound & h_pred<=h_upbound ~ "moderate",
    h_pred>=h_upbound~ "high"))
  site_river$depthID[is.na(site_river$depthID)]<-"high"
  
  mod <- lm(GPP ~ poly(h_pred, 2, raw = TRUE) + depthID, data = site_river)
  site_river$GPP_poly_interaction<-predict(mod, site_river)
  
  mod <- lm(ER ~ poly(h_pred, 2, raw = TRUE) + depthID, data = site_river)
  site_river$ER_poly_interaction<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio_poly_interaction=ER_poly_interaction/ER,
                                    GPP_ratio_poly_interaction=GPP_poly_interaction/GPP,
                                    h_ratio=h_pred/depth)
  
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_poly_interaction[site_river$ER_poly_interaction>-3]<- -3
  site_river$GPP_poly_interaction[site_river$GPP_poly_interaction<0]<- 0
  
  return(site_river)}

all_at_once<-function(site, river, el_lowbound, el_upbound,
                      h_lowbound,h_upbound){
  site_lm<-interpolation_lm(site, river, el_lowbound, el_upbound,
                   h_lowbound,h_upbound)
  
  site_lm_interaction<-interpolation_lm_interaction(site_lm, river, el_lowbound, el_upbound,
                               h_lowbound,h_upbound)
  
  site_poly<-interpolation_poly(site_lm_interaction, river, el_lowbound, el_upbound,
                     h_lowbound,h_upbound)
  
  site_poly_interaction<-interpolation_poly_interaction(site_poly, river, el_lowbound, el_upbound,
                                 h_lowbound,h_upbound)
  
return(site_poly_interaction)}



checkGPP_lm <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_lm, color="est"), size=1)+ggtitle("Linear Model")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_lm, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=GPP,y=GPP_lm)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio_lm), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d, ncol = 1)
  
  return(GPPcheck)}
checkER_lm <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_lm, color="est"), size=1)+ggtitle("Linear Model")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_lm, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=ER,y=ER_lm)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio_lm), size=1)
  
  ERcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(ERcheck)}

checkGPP_lm_interaction<- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_lm_interaction, color="est"), size=1)+ggtitle("Lineaer Model with interaction")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_lm_interaction, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=GPP,y=GPP_lm_interaction)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio_lm_interaction), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(GPPcheck)}
checkER_lm_interaction <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_lm_interaction, color="est"), size=1)+ggtitle("Lineaer Model with interaction")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_lm_interaction, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=ER,y=ER_lm_interaction)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio_lm_interaction), size=1)
  
  ERcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(ERcheck)}

checkGPP_poly <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_poly, color="est"), size=1)+ggtitle("Polynomial Model")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_poly, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=GPP,y=GPP_poly)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio_poly), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(GPPcheck)}
checkER_poly <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_poly, color="est"), size=1)+ggtitle("Polynomial Model")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_poly, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=ER,y=ER_poly)) +
    geom_point(size=1)+
  stat_poly_line()+
  stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)

  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio_poly), size=1)
  
  ERcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(ERcheck)}

checkGPP_poly_interaction <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_poly_interaction, color="est"), size=1)+ggtitle("Polynomial Model with Interaction")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_poly_interaction, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=GPP,y=GPP_poly_interaction)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio_poly_interaction), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(GPPcheck)}
checkER_poly_interaction <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_poly_interaction, color="est"), size=1)+ggtitle("Polynomial Model with Interaction")
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_poly_interaction, color="est"), size=1)+theme(legend.position = "none")
  
  c<-ggplot(data=zoom, aes(x=ER,y=ER_poly_interaction)) +
    geom_point(size=1)+
    stat_poly_line()+
    stat_poly_eq(use_label(c("R2")),size=5, label.x = 1,label.y = 0)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio_poly_interaction), size=1)
  
  ERcheck<-plot_grid(a,b,c,d, ncol=1)
  
  return(ERcheck)}

all_graphs_GPP<-function(site){
  a<-checkGPP_lm(site)
  b<-checkGPP_lm_interaction(site)
  c<-checkGPP_poly(site)
  d<-checkGPP_poly_interaction(site)
  
  graphs<-plot_grid(a,b,c,d,ncol=4)
  return(graphs)}

all_graphs_ER<-function(site){
  a<-checkER_lm(site)
  b<-checkER_lm_interaction(site)
  c<-checkER_poly(site)
  d<-checkER_poly_interaction(site)
  
  graphs<-plot_grid(a,b,c,d,ncol=4)
  return(graphs)}
#get data####
master<-read_csv('02_Clean_data/master_metabolism4.csv')
master<-master%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min)

sites<-split(master,master$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]

#river elevation####
proportion<-0.79
site_id <- c('02322500','02321958')
parameterCd <- c('00065')
ftwhite <- river_elevation(site_id,parameterCd)
#write_csv(ftwhite, "04_Outputs/ftwhite.csv")

site_id <- '02323500'
parameterCd <- c('00065')
proportion<-1
wilcox <- for_wilcox(site_id,parameterCd)
#write_csv(wilcox, "04_Outputs/wilcox.csv")

site_id <- c('02320000','02319800')
parameterCd <- c('00065')
proportion<-0.501
dowling <- river_elevation(site_id,parameterCd)
#write_csv(dowling, "04_Outputs/dowling.csv")

site_id <- c('02323000','02323500')
parameterCd <- c('00065')
proportion<-0.72
OS_wilcox <- river_elevation(site_id,parameterCd)
#write_csv(OS_river, "04_Outputs/OS_river.csv")

site_id<-'02322700'
parameterCd <- c('00065')
IU_hist <- for_wilcox(site_id,parameterCd)
#write_csv(IU, "04_Outputs/IU_historical.csv")

#GB run####
ggplot(GB, aes(depth)) +
  geom_point(aes(y=ER,color='ER'))+geom_vline(xintercept = 0.73)


GB_inter<-all_at_once(GB,ftwhite,
                             el_lowbound=1.7,
                             el_upbound=2.5,
                             h_lowbound=0.62,
                             h_upbound=0.73)

zoom<-filter(GB_inter, Date>"2022-05-01")
a<-ggplot(zoom, aes(depth)) +
  geom_point(aes(y=GPP,color='GPP'), color="darkgreen")+
  geom_smooth(data=GB_inter, aes(x=depth, y=GPP, group=depthID, color=depthID),
              method='lm', se=FALSE)+
  geom_point(aes(y=ER,color='ER'), color="darkred")+
  geom_smooth(data=GB_inter, aes(x=depth, y=ER, group=depthID, color=depthID), method='lm', se=FALSE)+
  ggtitle("GB, linear model")+ theme(legend.position = "none")

b<-ggplot(zoom, aes(depth)) +
  geom_point(aes(y=GPP,color='GPP'), color="darkgreen")+
  geom_smooth(data=GB_inter, aes(x=depth, y=GPP), method='lm', se=FALSE)+
  geom_point(aes(y=ER,color='ER'), color="darkred")+
  geom_smooth(data=GB_inter, aes(x=depth, y=ER), method='lm', se=FALSE)+
  ggtitle("GB, linear model, interaction")+ theme(legend.position = "none")

c<-ggplot(zoom, aes(depth)) +
  geom_point(aes(y=GPP,color='GPP'), color="darkgreen")+
  geom_smooth(data=GB_inter, aes(x=depth, y=GPP), method='lm', 
              formula = y ~ poly(x, 2), se=FALSE)+
  geom_point(aes(y=ER,color='ER'), color="darkred")+
  geom_smooth(data=GB_inter, aes(x=depth, y=ER), method='lm', 
              formula = y ~ poly(x, 2), se=FALSE)+ggtitle("Polynomial")+ theme(legend.position = "none")

d<-ggplot(zoom, aes(depth)) +
  geom_point(aes(y=GPP,color='GPP'), color="darkgreen")+
  geom_smooth(data=GB_inter, aes(x=depth, y=GPP, group=depthID, color=depthID), method='lm', 
              formula = y ~ poly(x, 2), se=FALSE)+
  geom_point(aes(y=ER,color='ER'), color="darkred")+
  geom_smooth(data=GB_inter, aes(x=depth, y=ER, group=depthID, color=depthID), method='lm', 
              formula = y ~ poly(x, 2), se=FALSE)+ggtitle("Polynomial, interaction")+ theme(legend.position = "none")

plot_grid(a,b,c,d, ncol=2)

(GB_gpp<-all_graphs_GPP(GB_inter))
(GB_er<-all_graphs_ER(GB_inter))

#AM run####                           
AM_inter<-all_at_once(AM,dowling,
                      el_lowbound=2.45,
                      el_upbound=2.8,
                      h_lowbound=0.89,
                      h_upbound=1.2)


(AM_gpp<-all_graphs_GPP(AM_inter))
(AM_er<-all_graphs_ER(AM_inter))

#LF run####

LF_inter<-all_at_once(LF,wilcox,
                      el_lowbound=6,
                      el_upbound=8.4,
                      h_lowbound=0.55,
                      h_upbound=0.93)


(LF_gpp<-all_graphs_GPP(LF_inter))
(LF_er<-all_graphs_ER(LF_inter))



#OS run####

OS_inter<-all_at_once(OS,OS_wilcox,
                       el_lowbound=4,
                       el_upbound=4.6,
                       h_lowbound=1.25,
                       h_upbound=1.6)


(OS_gpp<-all_graphs_GPP(OS_inter))
(OS_er<-all_graphs_ER(OS_inter))
#IU run####
IU_inter<-all_at_once(IU,IU_hist,
                      el_lowbound=16,
                      el_upbound=17.5,
                      h_lowbound=1.7,
                      h_upbound=3)


(IU_gpp<-all_graphs_GPP(IU_inter))
(IU_er<-all_graphs_ER(IU_inter))

#ID run####
ID_inter<-all_at_once(ID,IU_hist,
                      el_lowbound=15.8,
                      el_upbound=18.3,
                      h_lowbound=1.2,
                      h_upbound=2.2)


(ID_gpp<-all_graphs_GPP(ID_inter))
(ID_er<-all_graphs_ER(ID_inter))


all_Interpolations<-rbind(ID_inter, IU_inter, GB_inter, LF_inter, AM_inter, OS_inter)
all_Interpolations<-filter(all_Interpolations, Date>"2022-05-01")
all_Interpolations<-all_Interpolations %>% filter(Date>"2022-05-01") %>% filter(ID !=NA |ID !='IU')


ggplot(all_Interpolations, aes(Date)) +
  geom_line(aes(y=h_pred,color='predicted depth'), size=1)+
  geom_line(aes(y=depth,color='depth'), size=1)+
  facet_wrap(~ ID, ncol=2)


#work flow####

#display r2 on graphs


#display lm
#display lm with interplation
#dispaly poly
#display w interpolation 

# ID_river<-left_join(ID, IU_hist)
# ggplot(ID_river, aes(x=depth)) + 
#   geom_histogram(binwidth=0.1)+
#   geom_vline(xintercept = 1.2)+
#   geom_vline(xintercept = 2.2)
# 
# ID_inter<-filter(ID_inter, Date>'2022-05-15')
# 
# ggplot(ID_inter, aes(Date)) +
#   geom_line(aes(y=elevation, color=elevationID))+
#   geom_line(aes(y=depth, color=depthID))


