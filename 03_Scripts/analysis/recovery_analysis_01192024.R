####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)

recovery_calc <- function(site) {
  x<-c("Date","DO","depth","ER","GPPavg")
  site<-site[,x]

  siteBO <- siteBO %>%
    group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%
    ungroup()

  ind <- which.max(siteBO$depth)
  siteBO_prior <- siteBO[seq_len(ind - 1), ]
  siteBO_prior<-filter(siteBO_prior, count<20)
  siteBO_disturb <- siteBO[ind:nrow(siteBO), ]

  GPP_prior<-mean(siteBO_prior$GPPavg, na.rm=T)
  ER_prior<-mean(siteBO_prior$ER, na.rm=T)
  h_prior<-mean(siteBO_prior$depth, na.rm=T)
  DO_prior<-mean(siteBO_prior$DO, na.rm=T)

  siteBO_disturb$GPPavg_ratio<-siteBO_disturb$GPPavg/GPP_prior
  siteBO_disturb$ER_ratio<-siteBO_disturb$ER/ER_prior
  siteBO_disturb$DO_ratio<-siteBO_disturb$DO/DO_prior
  siteBO_disturb$depth_ratio<-siteBO_disturb$depth/h_prior

  u<-
    siteBO_disturb %>%
    mutate( ERmean= rollapply(ER_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  u<-u[,c(12,1)]
  siteBO_disturb<-left_join(siteBO_disturb,u,by=c('Date'))

  v<-
    siteBO_disturb %>%
    mutate( GPPmean= rollapply(GPPavg_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  v<-v[,c(13,1)]
  siteBO_disturb<-left_join(siteBO_disturb,v,by=c('Date'))

  w<-
    siteBO_disturb %>%
    mutate( DOmean= rollapply(DO,4,mean, fill=NA, partial=TRUE, align='left'))
  w<-w[,c(14,1)]
  siteBO_disturb<-left_join(siteBO_disturb,w,by=c('Date'))

  y<-
    siteBO_disturb %>%
    mutate( hmean= rollapply(depth,4,mean, fill=NA, partial=TRUE, align='left'))
  y<-y[,c(15,1)]
  siteBO_disturb<-left_join(siteBO_disturb,y,by=c('Date'))

  return(siteBO_disturb)}
extract_recovery <- function(site) {
  modER<-lm(ERmean ~ disturb_count, data = siterecov)
  cf <- coef(modER)
  (InterceptER<- cf[1])
  (SlopeER <- cf[2])
  (ER_recov<-(1-InterceptER)/SlopeER)

  modGPP<-lm(GPPmean ~ disturb_count, data = siterecov)
  cf <- coef(modGPP)
  (InterceptGPP<- cf[1])
  (SlopeGPP <- cf[2])
  (GPP_recov<-(1-InterceptGPP)/SlopeGPP)

  modDO<-lm(DOmean ~ disturb_count, data = siterecov)
  cf <- coef(modDO)
  (InterceptDO<- cf[1])
  (SlopeDO <- cf[2])
  (DO_recov<-(1-InterceptDO)/SlopeDO)

  modH<-lm(depth_avg ~ disturb_count, data = siterecov)
  cf <- coef(modH)
  (InterceptH<- cf[1])
  (SlopeH <- cf[2])
  (H_recov<-(1-InterceptH)/SlopeH)

  IDFR_ls<-list(GPP_recov,ER_recov,DO_recov,H_recov)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]])
  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'DO_recov'
  colnames(df)[4]<-'H_recov'

  return(df)}
#####

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
master$days<-as.Date(master$Date)
master <- master[!duplicated(master[c('days', 'ID')]),]


IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
LF<-IDs[[4]]
OS<-IDs[[5]]

