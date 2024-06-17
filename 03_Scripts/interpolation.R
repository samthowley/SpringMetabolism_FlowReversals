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
  up_gage<-rename(up_gage, 'Date'='dateTime', 'stage_up'='X_00065_00000')
  
  down_gage <-split(river, river$site_no)[[1]]
  down_gage<-down_gage[,c(3,4)]
  down_gage<-rename(down_gage, 'Date'='dateTime', 'stage_down'='X_00065_00000')
  
  river<-left_join(down_gage,up_gage, by='Date')
  
  river<- river %>% mutate(minute = minute(Date))
  river<-river %>% filter(minute==0) %>% 
    mutate(elevation=abs(stage_up-stage_down)*proportion)
  river<-river[,c(1,5)]
  return(river)}
for_wilcox <- function(site_id,parameterCd) {
  river <- readNWISuv(site_id,parameterCd)

  river<-river[,c(3,4)]
  river<-rename(river, 'Date'='dateTime', 'stage_up'='X_00065_00000')
  
  river<- river %>% mutate(minute = minute(Date))
  river<-river %>% filter(minute==0)
  river<-river[,c(1,5)]
  return(river)}

interpolation <- function(site) {
  GPP_bound<-min(site$GPP, na.rm=T)
  ER_bound<-min(site$ER, na.rm=T)
  
  mod_h<-lm(formula =  stage ~ elevation, data = site)
  cf <- coef(mod_h)
  (site$h_inter<-(cf[2]*site$elevation)+cf[1])
  
  
  modGPP<-lm(GPP ~ stage, data = site)
  cf <- coef(modGPP)
  (site$GPP_est<- cf[2]*site$h_inter+cf[1])
  
  
  modER<-lm(ER ~ stage, data = site)
  cf <- coef(modER)
  (site$ER_est<- cf[2]*site$h_inter+cf[1])
  
  
  site$GPP[site$GPP< GPP_bound] <- GPP_bound
  site$ER[site$ER< ER_bound] <- ER_bound
  
  site<- site %>% mutate(ratio_GPP=GPP/GPP_est, ratio_ER=ER/ER_est,
                                 ratio_h=depth/h_inter)
  
  return(site)}

#get data####
metabolism<-read_csv('02_Clean_data/master_metabolism4.csv')
metabolism<-metabolism[,c('ER','GPP','NEP', 'Date', 'ID')]
metabolism<-metabolism %>%rename('day'='Date') %>% mutate(day=as.Date(day))
depth<-read_csv('02_Clean_data/master_depth2.csv')
depth$day<-as.Date(depth$Date)
master<-left_join(depth,metabolism, by=c('ID','day'))

master<- master[!duplicated(master[c('ID','Date')]),]
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

site_id <- '02323500'
parameterCd <- c('00065')
proportion<-1
wilcox <- for_wilcox(site_id,parameterCd)

site_id <- c('02320000','02319800')
parameterCd <- c('00065')
proportion<-0.501
dowling <- river_elevation(site_id,parameterCd)

site_id <- c('02323000','02323500')
parameterCd <- c('00065')
proportion<-0.72
OS_river <- river_elevation(site_id,parameterCd)


###GB run####
GB<-left_join(GB, ftwhite)
GB_edit<-filter(GB, depth_diff<0.6)


ggplot(data=GB, aes(x=depth_diff)) +
    geom_point(aes(y=GPP), size=1, color='darkgreen')+
    geom_point(aes(y=ER*-1), size=1, color='darkred')+
    geom_point(aes(y=NEP), size=1, color='blue')+
    scale_color_manual(values='black')+
    geom_smooth(aes(x=depth_diff, y=GPP), color='darkgreen', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=ER*-1), color='darkred', size=0.75,
                data=GB, se = FALSE, method='lm')+
    geom_smooth(aes(x=depth_diff, y=NEP), color='blue', size=0.75,
                data=GB, se = FALSE, method='lm')+
    ggtitle("GB")+scale_x_continuous(n.breaks=4) +
    scale_y_continuous(n.breaks=3)+geom_vline(xintercept = 0.6)

