rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)

#call in data#######
chem <- read_csv("02_Clean_data/master_chem1.csv")
chem<-chem %>%mutate(Date=as.Date(Date))%>% group_by(Date, ID)%>%
  mutate(pH=mean(pH, na.rm=T), SpC=mean(SpC, na.rm=T), depth=mean(depth, na.rm=T),
         DO=mean(DO, na.rm=T), Temp=mean(Temp, na.rm=T))%>%
  distinct(ID, Date, .keep_all = T)%>% select(-CO2)

master_metabolism4 <- read_csv("02_Clean_data/master_metabolism4.csv")
master_metabolism4<-master_metabolism4 %>%select(Date, ID, GPP, ER)

chem<-left_join(chem, master_metabolism4, by=c('Date', 'ID'))

chem<-chem %>%group_by(ID) %>% 
  mutate(depth_min=min(depth, na.rm=T))%>% 
  mutate(depth_diff=depth-depth_min)%>% distinct(ID, Date, .keep_all = TRUE) 

#formulate data#####
#partition flow into low, moderate, high stage 
variableID<-chem %>% mutate(depthID = case_when(
  ID=='AM' & depth<0.9 ~ "low",
  ID=='AM' &depth>0.9 & depth<1.2 ~ "moderate",
  ID=='AM' &depth>=1.2 ~ "high",
  ID=='GB' &depth<0.55  ~ "low",
  ID=='GB' &depth>0.55 & depth<=0.74 ~ "moderate",
  ID=='GB' &depth>=0.74~ "high",
  ID=='OS' &depth<1.07 ~ "low",
  ID=='OS' &depth>1.07 & depth<1 ~ "moderate",
  ID=='OS' &depth>=1 ~ "high",
  ID=='LF' &depth<0.65 ~ "low",
  ID=='LF' &depth> 0.65 & depth<0.6~ "moderate",
  ID=='LF' &depth>=0.6 ~ "high",
  ID=='ID' &depth<1.9 ~ "low",
  ID=='ID'& depth>1.9 & depth<1.5 ~ "moderate",
  ID=='ID' &depth>=1.5 ~ "high",
  ID=='IU' &depth<1.7 ~ "low",
  ID=='IU'& depth>1.7 & depth<3 ~ "moderate",
  ID=='IU' &depth>=3 ~ "high"))%>%
  mutate(SpC_disturb=case_when(
    ID=='IU'~'0',
    SpC<=300~'1',
    ID=='AM' & SpC<=340~'1',
    ID=='LF' & SpC<=400 & Date>'2024-01-20'~'1',
    ID=='GB' & SpC<=350~'1'))%>%
  mutate(pH_disturb=case_when(
    ID=='LF' & pH<7.55 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7.2 & Date<'2024-01-01'~'1',
    ID=='OS' & pH<7.4 & Date>'2023-01-01'~'1'))

variableID <- variableID %>%
  mutate(pH_disturb = ifelse(is.na(pH_disturb), '0', pH_disturb),
         SpC_disturb = ifelse(is.na(SpC_disturb), '0', SpC_disturb))

#ID disturbance#
stageID <- variableID %>%
  mutate(stageID = case_when(
    depthID == 'high' ~ "high",
    TRUE ~ "baseline"))

depthID <- stageID %>%
  mutate(floodtype = case_when(
    depthID == 'high' & SpC_disturb == 1 ~ 3,
    depthID == 'high' & pH_disturb == 1 ~ 3,
    depthID == 'high' & pH_disturb == 0 & SpC_disturb == 0 ~ 2,
    TRUE ~ 1))

# Assign unique flood numbers
IDs <- depthID %>%
  mutate(
    flood_group = cumsum(stageID == "high" & lag(stageID, default = "baseline") != "high"),
    flood_ID = ifelse(stageID == "high", flood_group, NA)) %>%
  mutate(baseline_ID = ifelse(stageID == "baseline", 
                              cumsum(stageID == "baseline" & lag(stageID, default = "high") != "baseline"), NA))

#baseline
baseline_stats<-IDs%>%arrange(ID, Date)%>%
  group_by(ID, flood_group) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    h_count = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))%>% ungroup() %>%
  group_by(ID, baseline_ID)%>%
  mutate(GPP_baseline = mean(GPP[depthID == "low"], na.rm = TRUE),
                  ER_baseline = mean(ER[depthID == "low"], na.rm = TRUE),
                  h_baseline = mean(depth[depthID == "low"], na.rm = TRUE))%>%ungroup%>%
  group_by(ID)%>%
  fill(GPP_baseline,ER_baseline,h_baseline, .direction = "downup")
  
recovery <- baseline_stats %>%
  mutate(
    GPP_ratio = GPP / GPP_baseline, 
    ER_ratio = ER / ER_baseline, 
    h_ratio = depth / h_baseline
  ) %>%
  group_by(ID) %>%
  mutate(group = cumsum(h_count == 1 & lag(h_count, default = 0) == 0)) %>%
  ungroup() %>%
  group_by(group, ID) %>%
  mutate(
    GPP_recover = if_else(h_count > 0, cumsum(GPP_ratio <= 0.8), NA_real_),
    ER_recover  = if_else(h_count > 0, cumsum(ER_ratio <= 0.8), NA_real_),
  ) %>%
  ungroup()%>% group_by(ID, flood_group)%>% mutate(
    h_recover = if_else(h_count > 0, cumsum(h_ratio <= 0.8), NA_real_)) 
  


ggplot(data=recovery%>%filter(ID=='AM'), aes(x=Date, color=h_recover))+
  geom_point(aes(y=depth))+facet_wrap(~ID, scales='free')+geom_hline(yintercept = 1)

range(recovery$h_ratio, na.rm = T)
  