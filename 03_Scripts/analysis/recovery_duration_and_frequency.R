rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)

#data####
chem <- read_csv("02_Clean_data/master_chem1.csv")

chem<-chem %>%group_by(ID) %>% 
  mutate(depth_min=min(depth, na.rm=T))%>% 
  mutate(depth_diff=depth-depth_min)



chem<-chem %>% mutate(depthID = case_when(
  ID=='AM' & depth<0.9 ~ "low",
  ID=='AM' &depth>0.9 & depth<1.2 ~ "moderate",
  ID=='AM' &depth>=1.2 ~ "high",
  ID=='GB' &depth<0.55  ~ "low",
  ID=='GB' &depth>0.55 & depth<=0.75 ~ "moderate",
  ID=='GB' &depth>=0.75~ "high",
  ID=='OS' &depth<0.8 ~ "low",
  ID=='OS' &depth>0.8 & depth<1 ~ "moderate",
  ID=='OS' &depth>=1 ~ "high",
  ID=='LF' &depth<0.4 ~ "low",
  ID=='LF' &depth> 0.4 & depth<0.6~ "moderate",
  ID=='LF' &depth>=0.6 ~ "high",
  ID=='ID' &depth<1.1 ~ "low",
  ID=='ID'& depth>1.1 & depth<1.5 ~ "moderate",
  ID=='ID' &depth>=1.5 ~ "high"
  ))%>%
  mutate(SpC_disturb=case_when(
    SpC<=300~'1',
    ID=='AM' & SpC<=340~'1',
    ID=='LF' & SpC<=400~'1',
    ID=='GB' & SpC<=350~'1'
  ))%>%
  mutate(pH_disturb=case_when(
    ID=='LF' & pH<7.55 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7.2 & Date<'2024-01-01'~'1',
    ID=='OS' & pH<7.4 & Date>'2023-01-01'~'1',
  ))

chem <- chem %>%
  mutate(pH_disturb = ifelse(is.na(pH_disturb), '0', pH_disturb),
         SpC_disturb = ifelse(is.na(SpC_disturb), '0', SpC_disturb))
         

ggplot(data=chem%>% filter(ID=='GB'), aes(x=Date, color=SpC_disturb)) +
  geom_point(aes(y=SpC))+
  facet_wrap(~ID)






