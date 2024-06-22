rm(list=ls())

library(ggpubr)
library(grid)
library(cowplot)
library(corrplot)
library(dataRetrieval)
library(ggpmisc)
library(tidyverse)
library(readxl)

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
checkGPP <- function(site_river) {

  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_pred, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_pred, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=GPP)) +
    geom_point(aes(y=GPP_pred), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d)
  
  return(GPPcheck)}
checkER <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_pred, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_pred, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=ER)) +
    geom_point(aes(y=ER_pred), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio), size=1)
  
  ERcheck<-plot_grid(a,b,c,d)
  
  return(ERcheck)}

interpolation <- function(site, river, el_lowbound, el_upbound,
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
  site_river$GPP_pred<-predict(mod, site_river)
  
  mod<-lm(ER~h_pred*depthID, data=site_river)
  site_river$ER_pred<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio=ER_pred/ER,
                                GPP_ratio=GPP_pred/GPP,
                                h_ratio=h_pred/depth)
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_pred[site_river$ER_pred>-3]<- -3
  site_river$GPP_pred[site_river$GPP_pred<0]<- 0
  
  return(site_river)}


#get data####
master<-read_csv('02_Clean_data/master_metabolism4.csv')
master<-master%>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))%>%
  mutate(depth_diff= depth-depth_min)

sites<-split(master,master$ID)
AM<-sites[[1]]
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
OS_river <- river_elevation(site_id,parameterCd)
#write_csv(OS_river, "04_Outputs/OS_river.csv")

site_id<-'02322700'
parameterCd <- c('00065')
IU <- for_wilcox(site_id,parameterCd)
#write_csv(IU, "04_Outputs/IU_historical.csv")

#GB run####
GB<-sites[[2]]

GB_inter<-interpolation(GB,ftwhite,
                        el_lowbound=1.7,
                        el_upbound=2.5,
                        h_lowbound=0.56,
                        h_upbound=0.73)
GB_GPPcheck<-checkGPP(GB_inter)
GB_ERcheck<-checkER(GB_inter)

#AM run####
AM<-sites[[1]]

AM<- AM %>% mutate(depthID = case_when(
  depth<0.9 ~ "low",
  depth>0.9 & depth<1.2 ~ "moderate",
  depth>=1.2 ~ "high"))

dowling<- dowling %>% mutate(elevationID = case_when(
  depth<0.55  ~ "low",
  depth>0.55 & depth<=0.75 ~ "moderate",
  depth>=0.75~ "high"))

ggplot(AM, aes(x=weight)) + 
  geom_histogram(binwidth=0.01)+
  geom_vline(xintercept=)+geom_vline(xintercept=)

AM_inter<-interpolation(AM,ftwhite,
                        el_lowbound=1.7,
                        el_upbound=2.5,
                        h_lowbound=0.56,
                        h_upbound=0.73)
AM_GPPcheck<-checkGPP(AM_inter)
AM_ERcheck<-checkER(AM_inter)


