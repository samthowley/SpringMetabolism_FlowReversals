rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(zoo)
library(lme4)
library(plotly)
library(broom)
#data####
chem <- read_csv("02_Clean_data/master_chem1.csv")%>%
  arrange(ID, Date) %>% group_by(ID)%>%
  mutate(across(c(SpC, depth, DO, pH), ~rollmean(.x, k = 3, fill = NA, align = "center"), .names = "{.col}"))%>%
  mutate(Date=as.Date(Date))%>% 
  ungroup() %>% group_by(ID, Date)%>%
  mutate(pH=mean(pH, na.rm=T), SpC=mean(SpC, na.rm=T), 
         depth=mean(depth, na.rm=T),
         DO=mean(DO, na.rm=T,), Temp=mean(Temp, na.rm=T), 
         CO2=mean(CO2, na.rm=T))%>%ungroup()%>%
  distinct(ID, Date, .keep_all = T)
master_metabolism4 <- read_csv("02_Clean_data/master_metabolism4.csv")%>%
  select(Date, ID, GPP, ER) %>%
  filter(GPP != is.nan(GPP)| ER!=is.nan(ER))

chem<-left_join(master_metabolism4, chem, by=c('Date', 'ID'))%>%
  filter(!is.na(depth)) %>%         
  group_by(ID) %>% 
  distinct(ID, Date, .keep_all = TRUE) 

variableID<-chem %>% mutate(depthID = case_when(
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
    ID== 'OS'~ cf[5,1]+(depth*cf[5,2])))

stageID <- velocity %>% 
  mutate(stageID = case_when(
    depthID == 'low' ~ "baseline",
    TRUE ~ "high"))

floodID <- stageID %>% 
  mutate(floodtype_num = case_when(
    depthID == 'high' & DO<=2.5 ~ 2,
    depthID == 'high' & SpC_disturb=='1'  ~ 3,
    depthID == 'high'~ 1),
    
    floodtype = case_when(
      depthID == 'high' & DO<=2.5 ~ 'BO',
      depthID == 'high' & SpC_disturb=='1' ~ 'FR',
      depthID == 'high'~ 'HS'))#%>%
  #filter(ID=='GB')

ggplot(floodID, aes(Date, color=floodtype))+
  geom_point(aes(y=depth), size=1)+facet_wrap(~ID, scales='free', ncol=2)


###################################################
#count time between floods, and duration of floods#
###################################################
duration <- floodID %>% 
  arrange(ID, Date) %>% 
  group_by(ID) %>%
  mutate(
    group = cumsum(stageID == "baseline"),  # Create a grouping variable that increments at each "baseline"
    flood_count = unlist(ave(stageID, group, FUN = function(x) {
      cumsum(x %in% c("high"))
    }))) %>%ungroup()  %>% mutate(flood_count=as.numeric(flood_count))%>%select(-group)


time_btwn <- duration %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(
    group = cumsum(stageID == "high"),             # New group ID whenever "high" appears
    time_bwtn = ave(stageID, ID, group,            # Cumulative count of "baseline" per group
                    FUN = function(x) cumsum(x == "baseline")) %>% as.numeric()) %>%
  ungroup() %>%
  select(-group)

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

#############################
#Prepare baseline table######
############################

baseline <- IDs %>% 
  filter(!is.na(baseline_ID)) %>%  
  mutate(time_bwtn = suppressWarnings(as.numeric(time_bwtn))) %>%  
  group_by(ID, baseline_ID) %>%
  arrange(depth) %>%
  slice_head(n = 14) %>%  
  summarize(
    CO2_baseline = if_else(all(is.na(CO2)), NA_real_, mean(CO2, na.rm = TRUE)),
    GPP_baseline = if_else(all(is.na(GPP)), NA_real_, mean(GPP, na.rm = TRUE)),
    ER_baseline = if_else(all(is.na(ER)), NA_real_, mean(ER, na.rm = TRUE)),
    h_baseline = if_else(all(is.na(depth)), NA_real_, mean(depth, na.rm = TRUE)),
    DO_baseline = if_else(all(is.na(DO)), NA_real_, mean(DO, na.rm = TRUE)),
    baseline_ID = max(baseline_ID, na.rm = TRUE),
    baseline_Date = mean(Date, na.rm = TRUE)
  )

  
timebtwn_tbl<-IDs%>%  
  group_by(ID, baseline_ID) %>%
  summarize(
    time_btwn= max(time_bwtn, na.rm = T),
    Date=mean(Date, na.rm=T))

baseline_tbl<- full_join(baseline, timebtwn_tbl, by=c('baseline_ID', 'ID'))

##################################
#join flood and baseline periods#############
#################################
floods<-floodID %>% 
  select(Date,ID, GPP, ER, depth, CO2, DO, SpC, floodtype, stageID, depthID)%>%
  mutate(floodtype=if_else(is.na(floodtype), "norm", floodtype))

daily_floods<- full_join(floods, baseline_tbl, by=c('Date', 'ID'))%>% 
  arrange(ID, Date)%>% 
  fill(baseline_ID, CO2_baseline,GPP_baseline,ER_baseline,h_baseline,DO_baseline,time_btwn,.direction='down')%>%
  mutate(CO2_reduc=1-(CO2/CO2_baseline),
         GPP_reduc=1-(GPP/GPP_baseline),
         ER_reduc=1-(ER/ER_baseline),
         h_reduc=1-(depth/h_baseline),
         h_diff=depth-h_baseline)

unique(floods$floodtype)
##################################
#prepare average tables#############
#################################
floods <- IDs %>% filter(depthID=='high')%>%
  select(Date, ID, GPP, ER,DO, flood_count, flood_ID, depth, floodtype,floodtype_num)

disturbed_averages <- floods %>%
  group_by(flood_ID,ID, floodtype) %>%
  arrange(desc(depth)) %>% #arrange depths from highest to lowest
  slice_head(n = 4) %>% #select the top 4 columns
  summarise(
    disturb_GPP = mean(GPP, na.rm = TRUE),
    disturb_ER = mean(ER, na.rm = TRUE),
    disturb_h = mean(depth, na.rm=T),
    disturb_DO = mean(DO, na.rm=T),
    flood_Date= mean(Date, na.rm=T),
    .groups = 'keep')%>%ungroup()

floodtype_duration<-floods%>%
  group_by(flood_ID,ID, floodtype) %>%
  summarise(
    floodtype_num=max(floodtype_num, na.rm=T), 
    duration=max(flood_count, na.rm=T),
    .groups = 'keep') %>%ungroup() 

flood_tble <-full_join(disturbed_averages, floodtype_duration, by=c('ID', 'flood_ID', 'floodtype'))

baseline_tbl_edit<-baseline_tbl %>%
  rename(Date=baseline_Date)%>% 
  group_by(ID)%>%
  mutate(GPP_baseline=if_else(is.na(GPP_baseline), mean(GPP_baseline, na.rm=T), GPP_baseline),
         ER_baseline=if_else(is.na(ER_baseline), mean(ER_baseline, na.rm=T), ER_baseline))%>%
  filter(time_btwn>6)

flood_tble_edit<-flood_tble %>% 
  rename(Date=flood_Date)%>% filter(duration>6)


joined_tbl<-full_join(baseline_tbl_edit,flood_tble_edit, by=c('ID', 'Date'))%>% 
  arrange(ID, Date)%>%
  fill(time_btwn,GPP_baseline,ER_baseline,h_baseline, .direction = 'down')%>%
  filter(!is.na(duration))%>%filter(!is.na(floodtype))%>%
  select(-baseline_ID)%>%
  mutate(GPP_reduce=(1-(disturb_GPP/GPP_baseline))*100,
         ER_reduce=(1-(disturb_ER/ER_baseline))*100,
         h_diff=disturb_h-h_baseline)

HS_events<-joined_tbl%>%filter(floodtype=='HS')
brwn_events<-joined_tbl%>%filter(floodtype!='HS')

#################################
#Recovery#######################
##############################

recovery_df<-left_join(IDs, baseline_tbl, by=c('ID', 'baseline_ID'))%>%
  fill(CO2_baseline, DO_baseline, GPP_baseline, ER_baseline, h_baseline, .direction = 'downup')%>%
  mutate(across(c(GPP, depth, DO, ER, CO2), ~rollmean(.x, k = 5, fill = NA, align = "center"), .names = "{.col}"))

perc_change<-recovery_df%>% mutate(h_change=(depth-lag(depth))/lag(depth),
                                   DO_change=(DO-lag(DO))/lag(DO), CO2_change=(CO2-lag(CO2))/lag(CO2), 
                                   GPP_change=(GPP-lag(GPP))/lag(GPP),
                                   ER_change=(ER-lag(ER))/lag(ER))

split_change <- perc_change %>%
  arrange(ID, Date) %>% group_by(ID) %>%
  mutate(flood_ssn = cumsum(replace_na(h_change > 0.1, FALSE))) %>%
  ungroup()

h_count<-split_change%>%
  arrange(ID, Date) %>%  # Make sure it's sorted by Date (not group)
  group_by(ID, flood_ssn) %>%
  mutate(max_h = which.max(replace(depth, is.na(depth), -Inf)),
    h_count = case_when(
      row_number() < max_h ~ row_number() - max_h,
      row_number() == max_h ~ 0,
      row_number() > max_h ~ row_number() - max_h))

for_h_lm <- h_count %>%
  group_by(ID, flood_ssn) %>%
  filter(h_count>0)%>%
    mutate(
      days_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
      flood_day = cumsum(replace_na(days_diff, 0)))%>%
  filter(flood_day<90)%>%
  filter(flood_day>=5)

lm_h <- for_h_lm %>%
  group_by(flood_ssn, ID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(depth ~ h_count, data = .x)),
    coef_info = map(model, tidy),
    glance_info = map(model, glance)) %>%
  mutate(slope = map_dbl(coef_info, ~ .x$estimate[.x$term == "h_count"]),
    intercept = map_dbl(coef_info, ~ .x$estimate[.x$term == "(Intercept)"]),
    r_squared = map_dbl(glance_info, ~ .x$r.squared),
    p_value = map_dbl(coef_info, ~ .x$p.value[.x$term == "h_count"])) %>%
  select(ID, flood_ssn, slope, intercept, r_squared, p_value)%>%
  rename(slope_h=slope, intercept_h=intercept, r2_h=r_squared, p_h=p_value)
  
GPP_count <- h_count %>%
  filter(depthID != 'low')%>%
  group_by(ID, flood_ssn) %>%
  mutate(GPP = if_else(GPP < 0.000005, 0, GPP)) %>%
  mutate(GPP_fixed = replace(GPP, is.na(GPP), Inf),
    min_GPP_index = max(which(GPP_fixed == min(GPP_fixed, na.rm = TRUE))),
    GPP_count = row_number() - min_GPP_index) %>%
  select(-GPP_fixed)

for_GPP_lm<-GPP_count %>%
  filter(depthID != 'low')%>%
  filter(GPP_count>0)%>%
    filter(n() > 5) %>% 
  mutate(GPP= if_else(GPP>GPP_baseline, NA, GPP))%>%
  mutate(GPP=if_else(ID=='AM'& flood_ssn==24 & GPP_count>6, NA, GPP))


ggplot(for_GPP_lm, aes(Date,y=ER, color=as.factor(flood_ssn)))+
  geom_point(size=2)+
  facet_wrap(~ID, scales='free', ncol=2)+
  geom_smooth(method = 'lm', se=F)+ggtitle('LF')


GPP_lm <- for_GPP_lm %>%
  group_by(flood_ssn, ID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(depth ~ GPP_count, data = .x)),
    coef_info = map(model, tidy),
    glance_info = map(model, glance)) %>%
  mutate(slope = map_dbl(coef_info, ~ .x$estimate[.x$term == "GPP_count"]),
    intercept = map_dbl(coef_info, ~ .x$estimate[.x$term == "(Intercept)"]),
    r_squared = map_dbl(glance_info, ~ .x$r.squared),
    p_value = map_dbl(coef_info, ~ .x$p.value[.x$term == "GPP_count"])) %>%
  select(ID, flood_ssn, slope, intercept, r_squared, p_value)%>%
  rename(slope_GPP=slope, intercept_GPP=intercept, r2_GPP=r_squared, p_GPP=p_value)

ER_count<-GPP_count %>%group_by(ID, flood_ssn)%>%
  mutate(min_ER = which.min(replace(ER, is.na(ER), -Inf)),
         ER_count = case_when(
           row_number() < min_ER ~ row_number() - min_ER,
           row_number() == min_ER ~ 0,
           row_number() > min_ER ~ row_number() - min_ER))%>%
  mutate(days_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
    flood_day = cumsum(replace_na(days_diff, 0)))

for_ER_lm <- ER_count %>%
  filter(depthID != 'low', ER_count > 0) %>%
  arrange(ID, flood_ssn, Date) %>%
  group_by(ID, flood_ssn) %>%
  mutate(
    days_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
    flood_day = cumsum(replace_na(days_diff, 0)),
    ER = if_else(ER > ER_baseline, NA_real_, ER)
  ) %>%  ungroup()

ggplot(for_ER_lm, aes(Date,y=ER, color=as.factor(flood_ssn)))+
  geom_point(size=2)+
  facet_wrap(~ID, scales='free', ncol=2)+
  geom_smooth(method = 'lm', se=F)+ggtitle('LF')

ER_lm <- ER_count %>%
  group_by(flood_ssn, ID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(depth ~ ER_count, data = .x)),
    coef_info = map(model, tidy),
    glance_info = map(model, glance)) %>%
  mutate(slope = map_dbl(coef_info, ~ .x$estimate[.x$term == "ER_count"]),
    intercept = map_dbl(coef_info, ~ .x$estimate[.x$term == "(Intercept)"]),
    r_squared = map_dbl(glance_info, ~ .x$r.squared),
    p_value = map_dbl(coef_info, ~ .x$p.value[.x$term == "ER_count"])) %>%
  select(ID, flood_ssn, slope, intercept, r_squared, p_value)%>%
  rename(slope_ER=slope, intercept_ER=intercept, r2_ER=r_squared, p_ER=p_value)

DO_count<-ER_count %>%group_by(ID, flood_ssn)%>%
  mutate(min_DO = which.min(replace(DO, is.na(DO), -Inf)),
         DO_count = case_when(
           row_number() < min_DO ~ row_number() - min_DO,
           row_number() == min_DO ~ 0,
           row_number() > min_DO ~ row_number() - min_DO))%>%
  mutate(days_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
         flood_day = cumsum(replace_na(days_diff, 0)))

for_DO_lm <- DO_count %>%
  filter(depthID != 'low', DO_count > 0) %>%
  arrange(ID, flood_ssn, Date) %>%
  group_by(ID, flood_ssn) %>%
  fill(DO_baseline, .direction='down')%>%
  mutate(
    days_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
    flood_day = cumsum(replace_na(days_diff, 0)), #counting days after flood
    DO = if_else(DO > DO_baseline, NA_real_, DO)
  ) %>%  ungroup()

ggplot(for_DO_lm, aes(Date,y=DO, color=as.factor(flood_ssn)))+
  geom_point(size=2)+
  facet_wrap(~ID, scales='free', ncol=2)

#############################
##Recovery for DO/GPP/ER####
############################

ggplot(h_count%>%filter(ID=='AM'), aes(Date,color=as.factor(higher_event)))+
  geom_point(aes(y=GPP), size=1)+
  facet_wrap(~ID, scales='free', ncol=2)+
  geom_hline(yintercept = 1)+ 
  geom_smooth(aes(y = GPP), method = 'lm')










lm_GPP<-h_count %>%
  group_by(event_ID, ID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(GPP_frac ~ h_count, data = .x)),
         model_summary = map(model, summary))%>%
  mutate(tidy_model = map(model, broom::tidy)) %>%
  unnest(tidy_model) %>%
  select(event_ID, ID, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)%>%
  mutate(GPP_recovery=(1-`(Intercept)`)/h_count)%>%
  select(event_ID, ID, GPP_recovery)

lm_ER<-h_count %>%
  group_by(event_ID, ID) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(ER_frac ~ h_count, data = .x)),
         model_summary = map(model, summary))%>%
  mutate(tidy_model = map(model, broom::tidy)) %>%
  unnest(tidy_model) %>%
  select(event_ID, ID, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)%>%
  mutate(ER_recovery=(1-`(Intercept)`)/h_count)%>%
  select(event_ID, ID, ER_recovery)




recovery<-left_join(lm_GPP, lm_ER, by=c('ID', 'event_ID'))
recovery<-left_join(recovery, lm_h, by=c('ID', 'event_ID'))%>%
  mutate(ER_ratio= ER_recovery/h_recovery, GPP_ratio= GPP_recovery/h_recovery)

recovery_summary<-recovery_df %>%
  group_by(event_ID, ID)%>%
  summarize(floodtype=mean(floodtype_num, na.rm=T))
  

####################
#Figures###########
###################

cols<-c("HS"="deepskyblue3","BO"="burlywood4", 'FR'='black')
h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

theme_sam<-theme()+theme(axis.text.x = element_text(size = 17, angle=0),
                         plot.title = element_text(size = 17),
                             axis.text.y = element_text(size = 17, angle=0),
                             legend.position = "bottom",
                             legend.text= element_text(size = 17),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


(a<-ggplot(daily_floods%>% filter(!floodtype %in% c(NA), ID=='AM') , aes(h_diff, y=GPP_reduc, shape=ID, color=floodtype))+
  geom_point(size=3)+
  geom_smooth(aes(group=1),method = lm, se=F, linetype='dotted', color='black')+
  scale_colour_manual(name="", values = cols,labels=c("BO","FR"))+
  ggtitle("Backwater Flood Impacts on GPP")+
  scale_x_log10()+
  ylab("GPP Reduction (%)")+theme_sam)

(b<-ggplot(daily_floods%>%filter(stageID=='high'), aes(h_diff,y=ER_reduc, shape=ID, color=as.factor(floodtype)))+
  geom_point(size=3)+
    geom_smooth(aes(group=1),method = lm, se=F, linetype='dotted', color='black')+
  scale_colour_manual(name="", values = cols,labels=c("BO","FR"))+
  ggtitle("Backwater Flood Impacts on ER")+
  scale_x_log10()+
  ylab("ER Reduction (%)")+theme_sam)

plot_grid(a,b)


ggplot(daily_floods%>%filter(floodtype!='HS'), aes(h_reduc,y=ER_reduc, shape=ID, color=as.factor(floodtype)))+
  geom_point(size=3)+
  geom_smooth(aes(group=1),method = lm, se=F, linetype='dotted', color='black')+
  scale_colour_manual(name="", values = cols,labels=c("BO","FR"))+
  ggtitle("Backwater Flood Impacts on ER")+
  ylab("ER Reduction (%)")+theme_sam


for_hist_GPP<-daily_floods%>%select(ID, Date, baseline_ID, floodtype, GPP)%>%
  rename(met=GPP)%>% mutate(met_type='GPP')
for_hist_ER<-daily_floods%>%select(ID, Date, baseline_ID, floodtype, ER)%>%
  rename(met=ER)%>% mutate(met_type='ER')


hist_met<-rbind(for_hist_ER, for_hist_GPP)%>%filter(complete.cases(met))#%>%
  # group_by(baseline_ID) %>%
  # filter(n_distinct(floodtype) == 4) %>%
  # ungroup()

hist_met$floodtype <- factor(hist_met$floodtype , levels=c('norm', 'HS', 'BO', 'FR'))

ggplot(hist_met,aes(x=floodtype, y=met, fill=met_type)) +
  geom_boxplot()+
  scale_fill_manual(name="", values = c('darkred', 'darkgreen'))+
  facet_wrap(~ID, scales='free')


dev.new()
ggplotly(a)

unique(hist_met$floodtype)
