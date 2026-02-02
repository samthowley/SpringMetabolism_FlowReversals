
library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(weathermetrics)
library(lme4)
library(zoo)

smooth <- function(flagged, variable) {
  prep <- flagged %>%
    filter(!is.na({{variable}})) %>%
    arrange(flood, Date) %>%
    group_by(flood, ID) %>%
    mutate(
      t = as.numeric(Date - min(Date)),
      group_ID=paste(ID, flood, sep = "_")) %>%   # numeric time for loess
    ungroup()
  
  smooth<-fit_loess_by_group(prep, {{variable}}, t, group_ID)
  
}

baseline <- function(flagged, variable) {
  
  flood.IDs<-flagged%>%select(ID, flood)
  
  base_tbl <- flagged %>%
    mutate(
      flooded = case_when(
        is.na(flood) ~ "n",
        TRUE ~ "y"
      )
    ) %>%
    group_by(ID) %>%
    fill(flood, .direction = "updown") %>%
    mutate(group_ID = paste0(ID, "_", flood)) %>%
    filter(flooded == "n") %>%
    group_by(flood, ID) %>%
    summarise(
      base = mean({{ variable }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ID, flood)
  
  base_tbl<-left_join(flood.IDs, base_tbl)%>%
    filter(!is.na(flood))%>%distinct(ID, flood, .keep_all = T)%>%
    fill(base, .direction = 'down')
}

trim.declines<-function(df,  variable){
  
  prep <- df%>%
    group_by(ID, flood)%>%
    mutate(
      normalized={{variable}}/max({{variable}}, na.rm = T),
      day        = as.Date(Date),
      trim       = case_when(
        normalized >= 0.95~'remove',
        TRUE~ NA
      ),
      min_idx = which.min({{variable}}),
      min_date = Date[min_idx],    
      stage=case_when(
        Date<min_date ~"before",
        TRUE ~'after'),
    )%>%select(-min_idx, -min_date)
  head<-prep %>%
    arrange(ID, Date)%>%
    group_by(ID, flood)%>%
    filter(stage=='before')%>%
    fill(trim, .direction='up')%>%
    mutate(
      flood=if_else(!is.na(trim), NA, flood)
    )
  
  tail<-prep %>%
    arrange(ID, Date)%>%
    group_by(ID, flood)%>%
    filter(stage=='after')%>%
    fill(trim, .direction='down')%>%
    mutate(
      flood=if_else(!is.na(trim), NA, flood)
    )
  
  trimmed<-rbind(head, tail)%>%arrange(ID, Date)
  
  remove.flukes <- trimmed %>%
    group_by(ID, flood) %>%
    mutate(
      remove=n_distinct(day),
      flood=if_else(remove<7, NA, flood)
    ) %>%
    ungroup()
  
  
}
trim.increases<-function(df,  variable){
  
  prep <- df%>%
    group_by(ID, flood)%>%
    mutate(
      normalized={{variable}}/min({{variable}}, na.rm = T),
      day        = as.Date(Date),
      trim       = case_when(
        normalized <= 1.05~'remove',
        TRUE~ NA
      ),
      min_idx = which.max({{variable}}),
      min_date = Date[min_idx],    
      stage=case_when(
        Date<min_date ~"before",
        TRUE ~'after'),
    )%>%select(-min_idx, -min_date)
  
  head<-prep %>%
    arrange(ID, Date)%>%
    group_by(ID, flood)%>%
    filter(stage=='before')%>%
    fill(trim, .direction='up')%>%
    mutate(
      flood=if_else(!is.na(trim), NA, flood)
    )

  tail<-prep %>%
    arrange(ID, Date)%>%
    group_by(ID, flood)%>%
    filter(stage=='after')%>%
    fill(trim, .direction='down')%>%
    mutate(
      flood=if_else(!is.na(trim), NA, flood)
    )

  trimmed<-rbind(head, tail)%>%arrange(ID, Date)

  remove.flukes <- trimmed %>%
    group_by(ID, flood) %>%
    mutate(
      remove=n_distinct(day),
      flood=if_else(remove<7, NA, flood)
    ) %>%
    ungroup()
  
  
}

 
minimum<-function(df, variable){
  minimum<-df%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.min(replace({{variable}}, is.na({{variable}}), -Inf)), 
      minimum = case_when(
        row_number() == max_height ~ 0))%>%
    filter(minimum==0)%>%
    select(Date, ID, flood, {{variable}})%>%
    rename(minimum={{variable}})
  
}
maximum<-function(df, variable){
  maximum<-df%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.max(replace({{variable}}, is.na({{variable}}), -Inf)), 
      maximum = case_when(
        row_number() == max_height ~ 0))%>%
    filter(maximum==0)%>%
    select(Date, ID, flood, {{variable}})%>%
    rename(maximum={{variable}})
  
}

duration<-function(df){
  duration<- df %>%
    filter(!is.na(flood))%>%
    group_by(ID, flood)%>%
    mutate(
      duration=n_distinct(day)
    )%>% 
    summarise(
      duration=max(duration)
    )
}

count.min<-function(trim, variable) {
  
  count.hours<-trim%>%
    group_by(ID, flood) %>%
    arrange(Date) %>%  # Chronological rows
    mutate(
      var_clean = replace({{ variable }}, is.na({{ variable }}), Inf),
      min_val = min(var_clean, na.rm = TRUE),
      
      # LAST position where var == minimum
      max_height = max(which(var_clean == min_val)),
      
      count = case_when(
        row_number() < max_height ~ row_number() - max_height,
        row_number() == max_height ~ 0,
        TRUE ~ row_number() - max_height
      )
    ) %>%
    select(-var_clean, -min_val) %>%
    ungroup()
}
count.max<-function(trim, variable) {
  
  count.hours<-trim%>%
    group_by(ID, flood) %>%
    arrange(Date) %>%  # Chronological rows
    mutate(
      var_clean = replace({{ variable }}, is.na({{ variable }}), Inf),
      max_val = max(var_clean, na.rm = TRUE),
      
      # LAST position where var == minimum
      max_height = max(which(var_clean == max_val)),
      
      count = case_when(
        row_number() < max_height ~ row_number() - max_height,
        row_number() == max_height ~ 0,
        TRUE ~ row_number() - max_height
      )
    ) %>%
    select(-var_clean, -max_val) %>%
    ungroup()
}

fit_recessions <- function(trim, base, variable, base.var) {
  
  prep <- trim %>%
    filter(!is.na(flood), count>0)%>%
    mutate(group_ID = paste0(ID, "_", flood))

  # Build formula dynamically
  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)
  
  recess.lm <- coef(rC) %>%
    as_tibble() %>%
    mutate(ID = names(rC)) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood"))%>%
    rename(recess.intercept=Intercept, recess.slope=slope)%>%
    select(-base)

}
fit_rise <- function(trim, base, variable, base.var) {
  
  prep <- trim %>%
    filter(!is.na(flood), count<0)%>%
    mutate(group_ID = paste0(ID, "_", flood))
  
  # Build formula dynamically
  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)
  
  recess.lm <- coef(rC) %>%
    as_tibble() %>%
    mutate(ID = names(rC)) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood"))%>%
    rename(rise.intercept=Intercept, rise.slope=slope)%>%
    select(-base)
  
}

flood.base_compare <- function(peak_df, base_df, variable) {

  left_join(peak_df, base_df, by=c('flood', 'ID')) %>%
    mutate(
      percent.change=({{variable}}-base)/base
    ) %>%
    group_by(ID)%>%
    select(
      ID,
      flood,
      percent.change

    )
}

