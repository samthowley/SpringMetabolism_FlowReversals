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
