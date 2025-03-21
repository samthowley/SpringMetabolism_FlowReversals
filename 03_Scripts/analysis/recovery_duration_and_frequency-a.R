rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)

#data####
chem <- read_csv("02_Clean_data/master_chem1.csv")%>%mutate(Date=as.Date(Date))%>% group_by(Date, ID)%>%
  mutate(pH=mean(pH, na.rm=T), SpC=mean(SpC, na.rm=T), depth=mean(depth, na.rm=T),
                                                  DO=mean(DO, na.rm=T), Temp=mean(Temp, na.rm=T))%>%
  distinct(ID, Date, .keep_all = T)%>% select(-CO2)

master_metabolism4 <- read_csv("02_Clean_data/master_metabolism4.csv")%>%select(Date, ID, GPP, ER)

chem<-left_join(chem, master_metabolism4, by=c('Date', 'ID'))%>%group_by(ID) %>% 
  mutate(depth_min=min(depth, na.rm=T))%>% 
  mutate(depth_diff=depth-depth_min)%>% distinct(ID, Date, .keep_all = TRUE) 


smooth <- chem %>%
  mutate(across(c(pH, SpC, depth, DO), ~rollmean(.x, k = 3, fill = NA, align = "center"), .names = "{.col}"))

# ggplot(velocity %>% filter(ID=='AM'), aes(x=Date, color=velocity))+
#   geom_point(aes(y=depth))+geom_hline(yintercept = 0.8)+
#   geom_hline(yintercept = 1.2)

#partitioning data set in everyway possible
variableID<-smooth %>% mutate(depthID = case_when(
  ID=='AM' & depth<0.8 ~ "low",
  ID=='AM' &depth>0.8 & depth<1.2 ~ "moderate",
  ID=='AM' &depth>=1.2 ~ "high",
  
  ID=='GB' &depth<0.55  ~ "low",
  ID=='GB' &depth>0.55 & depth<=0.7 ~ "moderate",
  ID=='GB' &depth>=0.7~ "high",
  
  ID=='OS' &depth<0.8 ~ "low",
  ID=='OS' & depth>0.8 & depth<1.07 ~ "moderate",
  ID=='OS' &depth>=1.07 ~ "high",
  
  ID=='LF' &depth<0.35 ~ "low",
  ID=='LF' &depth> 0.35 & depth<0.75~ "moderate",
  ID=='LF' &depth>=0.75 ~ "high",
  
  ID=='ID' &depth<1.3 ~ "low",
  ID=='ID'& depth>1.3 & depth<2.1 ~ "moderate",
  ID=='ID' &depth>=2.1 ~ "high",
  
  ID=='IU' &depth<1.7 ~ "low",
  ID=='IU'& depth>1.7 & depth<3 ~ "moderate",
  ID=='IU' &depth>=3 ~ "high")) %>%
  mutate(SpC_disturb=case_when(
    ID=='AM' & SpC<=350~'1',
    ID=='OS' & SpC<=410~'1',
    ID=='LF' & SpC<=540 & Date>'2024-01-20'~'1',
    ID=='GB' & SpC<=360~'1'))%>%
  select(-pH)


u<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "velocity")

rC <- lmList(u ~ depth | ID, data=u)
(cf <- coef(rC))

velocity <- variableID %>%
  mutate(velocity= case_when(
    ID== 'AM'~ cf[1,1]+(depth*cf[1,2]),
    ID== 'GB'~ cf[2,1]+(depth*cf[2,2]),
    ID== 'ID'~ cf[3,1]+(depth*cf[3,2]),
    ID== 'LF'~ cf[4,1]+(depth*cf[4,2]),
    ID== 'OS'~ cf[5,1]+(depth*cf[5,2])))%>%
  mutate(u_disturb=case_when(velocity<=0~"RR",
                             velocity>0~"normal"))%>%
  mutate(DO_hypoxia=case_when(DO<=3~'hypoxic',
                              DO>3~'normal'))


#ID periods based off their chemical signature#####

stageID <- velocity %>%
  mutate(stageID = case_when(
    depthID == 'low' ~ "baseline",
    TRUE ~ "high"))

floodID <- stageID %>%
  mutate(floodtype = case_when(
    depthID == 'high' & SpC_disturb==1 & velocity>=0 ~ 'BO',
    depthID == 'high'& SpC_disturb==1 & velocity<0 ~ 'RR',
    depthID == 'high'~ 'high'
))


ggplot(floodID, aes(Date, color=DO_hypoxia))+
  geom_point(aes(y=depth), size=1)+facet_wrap(~ID, scales='free')








#Create tables#####
#duration
#count accumulative and sequential occurrences of "high" for each group
duration <- floodID %>% 
  arrange(ID, Date) %>% 
  group_by(ID) %>%
  mutate(
    group = cumsum(stageID == "baseline"),  # Create a grouping variable that increments at each "baseline"
    flood_count = unlist(ave(stageID, group, FUN = function(x) {
      cumsum(x %in% c("high"))
    }))) %>%ungroup()  %>% mutate(flood_count=as.numeric(flood_count))%>%select(-group)

#time between
time_btwn <- duration %>% 
  arrange(ID, Date) %>% group_by(ID) %>%
  mutate(
    group = cumsum(stageID == "high"),  # Reset count when "high" appears
    time_bwtn = ave(stageID, group, FUN = function(x) cumsum(x == "baseline"))
  ) %>% ungroup() %>%select(-group) 

# Assign unique flood numbers
#increments whenever whenever a "high" is encountered. lags checks if previous row is "high"
#if stage is not high, row will be NA
IDs <- time_btwn %>%
  mutate(
    flood_group = cumsum(stageID == "high" & lag(stageID, default = "baseline") != "high"), 
    flood_ID = ifelse(stageID == "high", flood_group, NA)) %>%
  select(-flood_group)%>%
  mutate(flood_count = ifelse(is.na(flood_count), 0, flood_count))%>%
  
  mutate(baseline_ID = ifelse(stageID == "baseline", 
                          cumsum(stageID == "baseline" & lag(stageID, default = "high") != "baseline"), NA))

#seperate baseline df from flood df
baseline <- IDs %>% filter(baseline_ID != is.na(baseline_ID))%>%
  mutate(time_bwtn=as.numeric(time_bwtn))

#baseline, hours to days
baseline_stats<-baseline%>%  mutate(time_bwtn=as.numeric(time_bwtn))%>%
  group_by(ID, baseline_ID) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    h_count = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))%>%
  
  mutate(GPP_baseline = mean(GPP[depthID == "low"], na.rm = TRUE),
         ER_baseline = mean(ER[depthID == "low"], na.rm = TRUE),
         h_baseline = mean(depth[depthID == "low"], na.rm = TRUE),
         
         time_btwn= max(time_bwtn, na.rm = T))%>%
  select(Date, ID, depth, h_count, time_btwn, baseline_ID, GPP_baseline, ER_baseline, h_baseline)


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
    Date = min(Date[h_count == 0], order_by = Date),
    h_flood=max(depth, na.rm=T),
    GPP_disturb = ifelse(all(is.na(GPP[flood_count > 0 & flood_count < 5])), NA, mean(GPP[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    ER_disturb = ifelse(all(is.na(ER[flood_count > 0 & flood_count < 5])), NA, mean(ER[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    .groups = 'keep') %>%ungroup()%>%
  rename('connectID'='flood_ID')%>%select(-ID)

table <-left_join(baseline_tbl, flood_tbl, by=c('connectID'))

clean_table<-table %>%mutate()%>% mutate(GPP_reduce=(1-(GPP_disturb/GPP_baseline))*100, 
                                         ER_reduce=(1-(ER_disturb/ER_baseline))*100,
                                         h_diff=h_flood-h_baseline,
                                         Date=as.Date(Date))

write_csv(clean_table, "04_Outputs/duration_recovery_2025-a.csv")

recovery <- read_csv("04_Outputs/recovery_analysis.csv")
recovery<- recovery %>% arrange(ID, Date) %>% 
  select(Date, ID, GPP_recov, ER_recov, H_recov)%>%
  mutate(Date=as.Date(Date))

check<-left_join(clean_table, recovery, by=c('ID', 'Date'))
#analysis#####


cols<-c( "high"="deepskyblue3","BO"="burlywood4", 'RR'='black')
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

a<-ggplot(clean_table %>% filter(GPP_reduce> -100), aes(duration, shape=ID, color=as.factor(floodtype)))+
    geom_point(aes(y=GPP_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Backwater Flood"))+
    ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
    ylab("GPP Reduction (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkgreen"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkgreen"))

b<-ggplot(clean_table, aes(duration, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=ER_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkred"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkred"))

plot_grid(a,b)



a<-ggplot(clean_table, aes(time_btwn, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=GPP_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
  ylab("GPP Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkgreen"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkgreen"))

b<-ggplot(clean_table, aes(time_btwn, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=ER_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkred"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkred"))


a<-ggplot(clean_table, aes(h_diff, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=GPP_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
  ylab("GPP Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkgreen"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkgreen"))

b<-ggplot(clean_table, aes(h_diff, shape=ID, color=as.factor(floodtype)))+
  geom_point(aes(y=ER_reduce), size=6)+
  scale_colour_manual(name="", values = cols,
                      labels=c("High Stage Event", "Backwater Flood"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam+theme(
    axis.title.y =element_text(size = 27, color="darkred"),
    axis.title.x =element_text(size = 27),
    plot.title = element_text(size = 22, color="darkred"))
