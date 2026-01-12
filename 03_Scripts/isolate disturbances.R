rm(list=ls())

library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)
library(weathermetrics)

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

#master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%select(-min)

chem <- master%>%
  mutate(Temp_C = fahrenheit.to.celsius(Temp),
         Temp_K=Temp_C+273.15,
         exp=2400*((1/Temp_K)-(1/298.15)),
         KH=0.034*2.178^(exp),
         CO2.mg.L=CO2/10^6*KH*44.01*10^3)

floods <- read_csv("01_Raw_data/flood periods.csv") %>%
  mutate(
    start = mdy(start), 
    end   = mdy(end))%>%
  select(ID, start, end, flood.event)

stage_flagged <- chem %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  mutate(
    flooded = !is.na(start), 
    day=as.Date(Date),
    depthID = case_when(
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
      ID=='IU' &depth>=3 ~ "high")
    ) %>%  # TRUE if matched an interval
  select(-start, -end)%>%
  group_by(day, ID)%>%
  arrange(ID, Date)

# stage_flagged_long <- stage_flagged %>%
#   mutate(depth=depth*20)%>%
#   pivot_longer(
#     cols =c('CO2.mg.L', 'DO', 'depth'),          # Pivot all columns except 'country'
#     names_to = "variable",        # Name the new column with original column names 'year'
#     values_to = "value"       # Name the new column with values 'value'
  #)

ggplot(stage_flagged%>% filter(!is.na(flood.event), ID=='OS', depthID!='low'), 
       aes(x = Date, y=depth*4)) +
  geom_point(aes(y=DO), color='pink')+
  #geom_point(aes(y=CO2/10^3), color='lightgreen')+
  geom_point(aes(y=depth*4), color='black')+
  geom_smooth(method='loess', color='black')+
  facet_wrap(~flood.event, scales='free')



plot_grid(
ggplot(stage_flagged_long%>% filter(!is.na(flood.event), ID=='AM'), 
       aes(x = Date, y=value, color=variable, group=interaction(variable))) +
  geom_point()+
  geom_smooth(method='loess', aes(group=interaction(variable)), color='black')+
  scale_y_continuous(
    name = "mg/L",
    sec.axis = sec_axis(~ . /20, name = "stage (m)"))+
  facet_wrap(~flood.event, scales='free'),
# ,
# 
# ggplot(stage_flagged_long%>% filter(ID=='LF',!is.na(flood.event)), 
#        aes(x = Date, y=SpC)) +
#   geom_point()+
#   facet_wrap(~flood.event, scales='free')
# ,
nrow=1)

undisturbed <- stage_flagged %>%
  fill(flood.event, .direction='updown')%>%
  filter(flooded == FALSE) %>%
  group_by(ID, flood.event)%>%
  summarize(
    depth.base=mean(depth, na.rm=T),
    DO.base=max(DO, na.rm=T),
    CO2.base=min(CO2, na.rm=T)
  )

#loess smoothing###########

loess.prep.1 <- stage_flagged %>%
  filter(flooded) %>%
  arrange(flood.event, Date) %>%
  group_by(flood.event, ID) %>%
  mutate(
    t = as.numeric(Date - min(Date)),
    group_ID=paste(ID, flood.event, sep = "_")) %>%   # numeric time for loess
  ungroup()%>%
  filter(!is.na(CO2))

loess.prep.2<-left_join(loess.prep.1, undisturbed)%>%
  arrange(ID, Date)%>%
  fill(depth.base, DO.base, CO2.base, .direction = 'down')

fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.3, min_rows = 5) {
  y_name <- rlang::as_name(rlang::enquo(y_var))
  x_name <- rlang::as_name(rlang::enquo(x_var))
  g_name <- rlang::as_name(rlang::enquo(group_var))
  
  split_list <- split(df, df[[g_name]])
  
  lapply(split_list, function(.x) {
    # Remove NAs pairwise for this group/var
    complete_cases <- complete.cases(.x[[y_name]], .x[[x_name]])
    .x_clean <- .x[complete_cases, ]
    
    if (nrow(.x_clean) < min_rows) {
      message("Skip group with only ", nrow(.x_clean), " complete cases (min: ", min_rows, ")")
      return(NULL)
    }
    
    fit <- loess(.x_clean[[y_name]] ~ .x_clean[[x_name]], span = span)
    
    # Predict on full original rows (fills NA with NA)
    .x %>%
      mutate(!!paste0(y_name, "_loess") := predict(fit, newdata = .x[[x_name]]))
  }) %>%
    compact() %>%
    bind_rows()
}

CO2<-fit_loess_by_group(loess.prep.2%>%filter(!is.na(CO2)), CO2, t, group_ID)
depth<-fit_loess_by_group(CO2%>%filter(!is.na(depth)), depth, t, group_ID)
all<-fit_loess_by_group(depth%>%filter(!is.na(DO)), DO, t, group_ID)

#t to baseline and peak#########

#return
count.hours<-all%>%
  group_by(ID, flood.event) %>%
  mutate(
    max_height = which.max(replace(depth_loess, is.na(depth_loess), -Inf)), 
    h_count.depth = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))

locate.peak<-count.hours%>%
  filter(h_count.depth>0)
  
library(lme4)
rC <- lmList(depth ~ h_count.depth | group_ID, data = locate.peak)
(cf <- coef(rC))

cf_df <- as_tibble(cf) %>%
  mutate(ID = names(rC))%>%
  rename('Intercept'="(Intercept)" , slope="h_count.depth")%>%
  separate(ID, into=c('ID', 'flood.event'), sep="_")%>%
  mutate(flood.event=as.numeric(flood.event))


solve.for.t.return<-left_join(cf_df, undisturbed)%>%
  mutate(hours.return=((depth.base-Intercept)/slope)/24)

#peak
locate.rise<-count.hours%>%
  filter(h_count.depth<0)

rC <- lmList(depth ~ h_count.depth | group_ID, data = locate.rise)
(cf <- coef(rC))

cf_df <- as_tibble(cf) %>%
  mutate(ID = names(rC))%>%
  rename('Intercept'="(Intercept)" , slope="h_count.depth")%>%
  separate(ID, into=c('ID', 'flood.event'), sep="_")%>%
  mutate(flood.event=as.numeric(flood.event))


solve.for.t.peak<-left_join(cf_df, undisturbed)%>%
  mutate(hours.return=(abs(depth.base-Intercept)/slope)/24)

#check
regress.chk<-locate.peak%>%mutate(regression=1)%>%
  select(Date, ID, flood.event, regression)

check.regress<-left_join(stage_flagged, regress.chk)

ggplot(check.regress, 
       aes(x = Date, color=regression)) +
  geom_line(aes(y=depth), size=1)+
  facet_wrap(~ID, scales='free')

#flood water quality############
ggplot(flood.peak%>%filter(ID=='AM'), 
       aes(x = Date, color=peak.flood)) +
  geom_point(aes(y=depth), size=1)+
  facet_wrap(~ID, scales='free')

flood.peak<-count.hours%>%
  mutate(peak.flood=case_when(h_count.depth > -30 & h_count.depth< 30 ~ "peak"))

#time btwn events#########

prep.time.btwn<-stage_flagged%>%
  mutate(condition=case_when(
    flooded==TRUE~"flooded",
    flooded==FALSE~"base"
  ))

time.btwn <- prep.time.btwn %>% 
  arrange(ID, Date) %>% 
  group_by(ID) %>%
  mutate(
    group = cumsum(condition == "flooded"),  # Create a grouping variable that increments at each "baseline"
    time.btwn = unlist(ave(condition, group, FUN = function(x) {
      cumsum(x %in% c("base"))
    }))) %>%ungroup()  %>%
  fill(flood.event, .direction = "updown")%>%
  group_by(ID, condition,flood.event)%>%
  summarize(
    time.btwn=max(as.numeric(time.btwn), na.rm = T)/24
  )%>%filter(condition=='base')
 

