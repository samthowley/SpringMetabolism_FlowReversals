rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library(dataRetrieval)
library('StreamMetabolism')
library("hydroTSM")
library(rnoaa)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library("devtools")
library(lubridate)
library(ggplot2)


####functions#######
CO2_inter_func<-function(fil){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  return(CO2)}
CO2_CS<-function(fil){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  return(CO2)}
CO2_Sh2<-function(fil){
  CO2 <- read_excel(fil, sheet='CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  return(CO2)}
CO2_dat<-function(fil){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  return(CO2)}
pH_csv<-function(fil){
    pH <- read_csv(fil, skip=3)
    pH<-pH[,c(1,5)]
    colnames(pH)[1] <- "Date"
    colnames(pH)[2] <- "pH"
    pH$pH<-as.numeric(pH$pH)
    return(pH)}
pH_xl<-function(fil){
  pH <- read_xlsx(fil, skip=3)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  return(pH)}
formatted <- data.frame()
DO_format_func<-function(fil){
  DO <- read_csv(fil)
  DO<-DO[,c(1,2,3)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  return(DO)}
DO_unformat_func<-function(fil){
  DO <- read_csv(fil)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  return(DO)}
SpC_func<-function(fil){
  SpC <- read_csv(fil)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  return(SpC)}
PT_func<-function(fil){
  PT <- read_csv(fil)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  return(PT)}
FAWN_func<-function(fil){
  FAWN <- read_csv(fil, col_types = cols(`FAWN Station` = col_skip(),
                                         `N (# obs)` = col_skip()))
  colnames(FAWN)[1] <- "Date"
  FAWN$Date <- mdy_hm(FAWN$Date)
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  return(FAWN)}
####CO2####


file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything/interpolated", pattern=".xlsx", full.names=TRUE)
AM_CO2_inter<-data.frame()
for(fil in file.names){CO2<-CO2_inter_func(fil)
AM_CO2_inter<-rbind(AM_CO2_inter, CO2)
CO2$CO2<- ((CO2$CO2/5.0614)-328.16)
AM_CO2_inter <- AM_CO2_inter[!duplicated(AM_CO2_inter[c('Date')]),]}
AM_CO2_inter$ID<-"AM"

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything", pattern=".xlsx", full.names=TRUE)
AM_CO2_all<-data.frame()
for(fil in file.names){CO2<-CO2_inter_func(fil)
AM_CO2_all<-rbind(AM_CO2_all, CO2)
AM_CO2_all <- AM_CO2_all[!duplicated(AM_CO2_all[c('Date')]),]}
AM_CO2_all$ID<-"AM"

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 CS", pattern=".xlsx", full.names=TRUE)
AM_CO2_CS <- data.frame()
for(fil in file.names){
  CO2<-CO2_CS(fil)
  AM_CO2_CS<-rbind(AM_CO2_CS, CO2)
  AM_CO2_CS <- AM_CO2_CS[!duplicated(AM_CO2_CS[c('Date')]),]}
AM_CO2_CS$ID<-"AM"

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
AM_CO2_Sh2 <- data.frame()
for(fil in file.names){
  CO2<-CO2_Sh2(fil)
  AM_CO2_Sh2<-rbind(AM_CO2_Sh2, CO2)
  AM_CO2_Sh2 <- AM_CO2_Sh2[!duplicated(AM_CO2_Sh2[c('Date')]),]}
AM_CO2_Sh2$ID<-"AM"

AM_CO2<-rbind(AM_CO2_inter, AM_CO2_all, AM_CO2_CS, AM_CO2_Sh2)


