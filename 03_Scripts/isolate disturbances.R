rm(list=ls())

library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)

chem <- read_csv("02_Clean_data/master_chem1.csv")


floods <- read_csv("01_Raw_data/flood periods.csv") %>%
  mutate(
    start = mdy(start), 
    end   = mdy(end))

stage_flagged <- variableID %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  mutate(flooded = !is.na(start), day=as.Date(Date)) %>%  # TRUE if matched an interval
  select(-start, -end)%>%
  group_by(day, ID)%>%
  mutate(CO2.daily=mean(CO2, na.rm=T), DO.daily=mean(DO, na.rm=T))

stage_flagged_long <- stage_flagged %>%
  pivot_longer(
    cols =c('CO2', 'DO', 'depth'),          # Pivot all columns except 'country'
    names_to = "variable",        # Name the new column with original column names 'year'
    values_to = "value"       # Name the new column with values 'value'
  )



ggplot(stage_flagged_long%>% filter(ID=='AM'), aes(x = Date, y=value, color=variable, group=interaction(flood.event, variable))) +
  geom_point()+
  geom_smooth(method='loess', aes(group=interaction(flood.event,variable)))+
  facet_wrap(~ID, scales='free')

ggplot(stage_flagged%>% filter(ID=='AM', flooded), aes(x = Date, y=DO.daily)) +
  geom_point()+
  geom_line(alpha=0.2)+
  
  geom_smooth(method='loess', aes(group=flood.event))+
  facet_wrap(~ID, scales='free')
#t to return to baseline###########

loess.prep <- stage_flagged %>%
  filter(flooded) %>%
  arrange(flood.event, Date) %>%
  group_by(flood.event, ID) %>%
  mutate(
    t = as.numeric(Date - min(Date)),
    group_ID=paste(ID, flood.event, sep = "_")) %>%   # numeric time for loess
  ungroup()%>%
  filter(!is.na(CO2))

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

CO2<-fit_loess_by_group(loess.prep%>%filter(!is.na(CO2)), CO2, t, group_ID)
depth<-fit_loess_by_group(loess.prep%>%filter(!is.na(depth)), depth, t, group_ID)
DO<-fit_loess_by_group(loess.prep%>%filter(!is.na(DO)), DO, t, group_ID)
SpC<-fit_loess_by_group(loess.prep%>%filter(!is.na(SpC)), SpC, t, group_ID)


ggplot(test, aes(x = Date)) +
  geom_line(aes(y = CO2))+
  geom_line(aes(y = CO2_loess), color='red')+
  # geom_line(aes(y = ER), color='red')+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')




