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
checkGPP_lm <- function(site_river) {

  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_lm, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_lm, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=GPP)) +
    geom_point(aes(y=GPP_lm), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d)
  
  return(GPPcheck)}
checkER_lm <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_lm, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_lm, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=ER)) +
    geom_point(aes(y=ER_lm), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio), size=1)
  
  ERcheck<-plot_grid(a,b,c,d)
  
  return(ERcheck)}

interpolation_lm <- function(site, river, el_lowbound, el_upbound,
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
  site_river$GPP_lm<-predict(mod, site_river)
  
  mod<-lm(ER~h_pred*depthID, data=site_river)
  site_river$ER_lm<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio=ER_lm/ER,
                                GPP_ratio=GPP_lm/GPP,
                                h_ratio=h_pred/depth)
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_lm[site_river$ER_lm>-3]<- -3
  site_river$GPP_lm[site_river$GPP_lm<0]<- 0
  
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
  
  mod <- lm(GPP ~ poly(h_pred, 2, raw = TRUE) + depthID, data = site_river)
  site_river$GPP_poly<-predict(mod, site_river)
  
  mod <- lm(ER ~ poly(h_pred, 2, raw = TRUE) + depthID, data = site_river)
  site_river$ER_poly<-predict(mod, site_river)
  
  site_river<-site_river %>% mutate(ER_ratio=ER_poly/ER,
                                    GPP_ratio=GPP_poly/GPP,
                                    h_ratio=h_pred/depth)
  
  
  site_river<-site_river %>% mutate(day=as.Date(Date))
  site_river <- site_river[!duplicated(site_river[c('day')]),]
  site_river$ER_poly[site_river$ER_poly>-3]<- -3
  site_river$GPP_poly[site_river$GPP_poly<0]<- 0
  
  return(site_river)}
checkGPP_poly <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP, color="OG"), size=1)+
    geom_point(aes(y=GPP_poly, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=GPP, color="OG"), size=1)+
    geom_line(aes(y=GPP_poly, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=GPP)) +
    geom_point(aes(y=GPP_poly), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=GPP_ratio), size=1)
  
  GPPcheck<-plot_grid(a,b,c,d)
  
  return(GPPcheck)}
checkER_poly <- function(site_river) {
  
  zoom<-filter(site_river, Date>"2022-05-03")
  a<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER, color="OG"), size=1)+
    geom_point(aes(y=ER_poly, color="est"), size=1)
  
  b<-ggplot(data=zoom, aes(x=Date)) +
    geom_line(aes(y=ER, color="OG"), size=1)+
    geom_line(aes(y=ER_poly, color="est"), size=1)
  
  c<-ggplot(data=zoom, aes(x=ER)) +
    geom_point(aes(y=ER_poly), size=1)
  
  d<-ggplot(data=zoom, aes(x=depth)) +
    geom_point(aes(y=ER_ratio), size=1)
  
  ERcheck<-plot_grid(a,b,c,d)
  
  return(ERcheck)}


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

GB_inter<-interpolation_lm(GB,ftwhite,
                        el_lowbound=1.7,
                        el_upbound=2.5,
                        h_lowbound=0.56,
                        h_upbound=0.73)
GB_GPPcheck_lm<-checkGPP_lm(GB_inter)
GB_ERcheck_lm<-checkER_lm(GB_inter)

GB_inter<-interpolation_poly(GB,ftwhite,
                           el_lowbound=1.7,
                           el_upbound=2.5,
                           h_lowbound=0.56,
                           h_upbound=0.73)
GB_GPPcheck_poly<-checkGPP_poly(GB_inter)
GB_ERcheck_poly<-checkER_poly(GB_inter)

#AM run####

AM_inter<-interpolation_lm(AM,dowling,
                           el_lowbound=2.45,
                           el_upbound=2.8,
                           h_lowbound=0.89,
                           h_upbound=1.2
                           )
AM_GPPcheck_lm<-checkGPP_lm(AM_inter)
AM_ERcheck_lm<-checkER_lm(AM_inter)

AM_inter<-interpolation_poly(AM,ftwhite,
                             el_lowbound=2.45,
                             el_upbound=2.8,
                             h_lowbound=0.89,
                             h_upbound=1.2
                             )
AM_GPPcheck_poly<-checkGPP_poly(AM_inter)
AM_ERcheck_poly<-checkER_poly(AM_inter)

#LF run####

LF_inter<-interpolation_lm(LF,wilcox,
                           el_lowbound=6,
                           el_upbound=8.4,
                           h_lowbound=0.55,
                           h_upbound=0.93)
LF_GPPcheck_lm<-checkGPP_lm(LF_inter)
LF_ERcheck_lm<-checkER_lm(LF_inter)

LF_inter<-interpolation_poly(LF,wilcox,
                             el_lowbound=1.7,
                             el_upbound=2.5,
                             h_lowbound=0.56,
                             h_upbound=0.73)
LF_GPPcheck_poly<-checkGPP_poly(LF_inter)
LF_ERcheck_poly<-checkER_poly(LF_inter)



#OS run####
OS_river<-left_join(OS, OS_wilcox)
names(OS_river)
ggplot(OS_river, aes(x=depth)) + 
  geom_histogram(binwidth=0.05)+
  geom_vline(xintercept = 1.6)+
  geom_vline(xintercept = 1.25)

OS_inter<-filter(OS_inter, Date>'2022-05-15')

ggplot(OS_inter, aes(Date)) +
  geom_line(aes(y=elevation, color=elevationID))+
  geom_line(aes(y=depth, color=depthID))




OS_inter<-interpolation_lm(OS,OS_wilcox,
                           el_lowbound=4,
                           el_upbound=4.6,
                           h_lowbound=1.25,
                           h_upbound=1.6
                           )
OS_GPPcheck_lm<-checkGPP_lm(OS_inter)
OS_ERcheck_lm<-checkER_lm(OS_inter)

OS_inter<-interpolation_poly(OS,OS_wilcox,
                             el_lowbound=4,
                             el_upbound=4.6,
                             h_lowbound=0.79,
                             h_upbound=1.25
)

OS_GPPcheck_poly<-checkGPP_poly(OS_inter)
OS_ERcheck_poly<-checkER_poly(OS_inter)
#IU run####
IU_inter<-interpolation_lm(IU,IU_hist,
                           el_lowbound=16,
                           el_upbound=17.5,
                           h_lowbound=1.7,
                           h_upbound=3)

IU_GPPcheck_lm<-checkGPP_lm(IU_inter)
IU_ERcheck_lm<-checkER_lm(IU_inter)

IU_inter<-interpolation_poly(IU,IU_hist,
                             el_lowbound=16,
                             el_upbound=17.5,
                             h_lowbound=1.7,
                             h_upbound=3)
IU_GPPcheck_poly<-checkGPP_poly(IU_inter)
IU_ERcheck_poly<-checkER_poly(IU_inter)
#ID run####
ID_river<-left_join(ID, IU_hist)
ggplot(ID_river, aes(x=depth)) + 
  geom_histogram(binwidth=0.1)+
  geom_vline(xintercept = 1.2)+
  geom_vline(xintercept = 2.2)

ID_inter<-filter(ID_inter, Date>'2022-05-15')

ggplot(ID_inter, aes(Date)) +
  geom_line(aes(y=elevation, color=elevationID))+
  geom_line(aes(y=depth, color=depthID))


ID_inter<-interpolation_lm(ID,IU_hist,
                           el_lowbound=15.8,
                           el_upbound=18.3,
                           h_lowbound=1.2,
                           h_upbound=2.2)
ID_GPPcheck_lm<-checkGPP_lm(ID_inter)
ID_ERcheck_lm<-checkER_lm(ID_inter)

ID_inter<-interpolation_poly(ID,IU_hist,
                             el_lowbound=15.8,
                             el_upbound=18.3,
                             h_lowbound=1.2,
                             h_upbound=2.2)
ID_GPPcheck_poly<-checkGPP_poly(ID_inter)
ID_ERcheck_poly<-checkER_poly(ID_inter)


