rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)

#data####
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


ggplot(data=variableID%>%filter(ID=='LF'), aes(x=Date, color=depthID)) +
  geom_point(aes(y=depth))+geom_hline(yintercept = 0.65)


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

#duration
duration <- depthID %>% 
  arrange(ID, Date) %>% 
  group_by(ID) %>%
  mutate(
    group = cumsum(stageID == "baseline"),  # Create a grouping variable that increments at each "baseline"
    flood_count = unlist(ave(stageID, group, FUN = function(x) {
      cumsum(x %in% c("high"))
    }))) %>%ungroup()  %>% mutate(flood_count=as.numeric(flood_count))%>%select(-group)

#time between
time_btwn <- duration %>%
  mutate(group = cumsum(stageID %in% c("high")),  # Create a new group for each "high" or "backwater"
         time_btwn = if_else(stageID == "baseline", row_number(), NA_integer_)) %>%
  group_by(group) %>%
  mutate(time_btwn = if_else(stageID == "baseline", row_number(), 0)) %>%
  ungroup()%>% select(-group)


# Assign unique flood numbers
IDs <- time_btwn %>%
  mutate(
    flood_group = cumsum(stageID == "high" & lag(stageID, default = "baseline") != "high"),
    flood_ID = ifelse(stageID == "high", flood_group, NA)  # Assign unique numbers only to "high"
  ) %>%
  select(-flood_group)%>%
  mutate(flood_count = ifelse(is.na(flood_count), 0, flood_count))%>%
  
  mutate(baseline_ID = ifelse(stageID == "baseline", 
                          cumsum(stageID == "baseline" & lag(stageID, default = "high") != "baseline"), NA))



#seperate baseline df from flood df

baseline <- IDs %>% filter(baseline_ID != is.na(baseline_ID))%>%
  select(Date, ID, GPP, ER, time_btwn, baseline_ID, depth, depthID)

#baseline, hours to days
baseline_stats<-baseline%>%  
  group_by(ID, baseline_ID) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    h_count = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))%>%
  mutate(GPP_baseline = mean(GPP[depthID == "low"], na.rm = TRUE),
         ER_baseline = mean(ER[depthID == "low"], na.rm = TRUE),
         h_baseline = mean(depth[depthID == "low"], na.rm = TRUE))%>%
  select(Date, ID, time_btwn, baseline_ID, GPP_baseline, ER_baseline, h_baseline)

#baseline summary
baseline_tbl <- baseline_stats %>%
  group_by(ID, baseline_ID) %>%
  summarise(time_btwn=max(time_btwn),
    GPP_baseline = mean(GPP_baseline, na.rm = TRUE),
    ER_baseline = mean(ER_baseline, na.rm = TRUE),
    h_baseline = mean(h_baseline, na.rm = TRUE),
    .groups = 'keep')%>%
  rename('connectID'='baseline_ID')


floods <- IDs %>% filter(flood_ID != is.na(flood_ID))%>%
  select(Date, ID, GPP, ER, flood_count, flood_ID, depth, floodtype)

floods_stats<-floods%>%  
  group_by(ID, flood_ID) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    h_count = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))

flood_tbl <- floods_stats %>%
  group_by(ID, flood_ID) %>%
  summarise(
    floodtype=max(floodtype),
    duration=max(flood_count),
    Date = mean(Date, na.rm = TRUE),
    
    GPP_disturb = ifelse(all(is.na(GPP[flood_count > 0 & flood_count < 5])), NA, mean(GPP[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    ER_disturb = ifelse(all(is.na(ER[flood_count > 0 & flood_count < 5])), NA, mean(ER[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    .groups = 'keep') %>%ungroup()%>%
  rename('connectID'='flood_ID')%>%select(-ID)

table <-left_join(baseline_tbl, flood_tbl, by=c('connectID'))

clean_table<-table %>%mutate()%>% mutate(GPP_reduce=(1-(GPP_disturb/GPP_baseline))*100, 
                                         ER_reduce=(1-(ER_disturb/ER_baseline))*100)

write_csv(clean_table, "04_Outputs/duration_recovery.csv")
#analysis#####


cols<-c( "2"="deepskyblue3","3"="burlywood4")
h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             legend.position = "bottom",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

ggplot(clean_table, aes(duration, shape=ID, color=as.factor(floodtype)))+
    geom_point(aes(y=GPP_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Backwater Flood"))+
    ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
    ylab("GPP Reduction (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkgreen"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkgreen"))

ggplot(clean_table, aes(duration, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=ER_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam+theme(
    #axis.title.y =element_text(size = 27, color="darkgreen"),
    axis.title.x =element_text(size = 27))
    #plot.title = element_text(size = 22, color="darkgreen"))

names(disturbance_table)



ggplot(clean_table, aes(time_btwn, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=GPP_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
  ylab("GPP Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkgreen"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkgreen"))

ggplot(clean_table, aes(time_btwn, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=ER_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam+theme(
    #axis.title.y =element_text(size = 27, color="darkgreen"),
    axis.title.x =element_text(size = 27))
#plot.title = element_text(size = 22, color="darkgreen"))

