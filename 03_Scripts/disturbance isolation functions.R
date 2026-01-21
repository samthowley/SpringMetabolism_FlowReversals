
library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(weathermetrics)
library(lme4)

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
  
  
  var_name  <- as_label(enquo(variable))
  base_name <- paste0("base.", var_name)
  
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
    rename(!!base_name := base)%>%
    arrange(ID, flood)
  
  base_tbl<-left_join(flood.IDs, base_tbl)%>%
    filter(!is.na(flood))%>%distinct(ID, flood, .keep_all = T)%>%
    fill(base_name,.direction = 'down')
    
    
  
}

trim.greater.than1<-function(flagged, base.df, base.variable, variable){
  
  prep <- flagged %>% 
    left_join(base.df) %>%
    fill({{base.variable}}, .direction = "down") %>%
    group_by(ID, day)%>%
    mutate(
      edited.flood=flood,
      daily=mean({{variable}}, na.rm=T)
    )%>% ungroup()%>%
    group_by(flood, ID)%>%
    mutate(
      normalized = daily/{{base.variable}},
      day        = as.Date(Date),
      trim       = normalized >= 0.95,
      min_idx = which.min(daily),
      min_date = Date[min_idx],    
      stage=case_when(
        Date<min_date ~"before",
        TRUE ~'after'),
    )%>%select(-min_idx, -min_date)
  
  
  bounds<-prep %>%
    filter(trim==TRUE)%>%
    group_by(ID, flood, stage)%>%
    summarise(
      first.day=min(Date, na.rm=T),
      last.day=max(Date, na.rm=T)
    )
  
  
  head<-bounds%>%
    filter(stage=='before')%>%
    select(-first.day)
  
  
  tail<-bounds%>%
    filter(stage=='after')%>%
    select(-last.day)
  
  
  remove.head<-
    left_join(prep, head, by=c('flood', 'ID'))%>%
    mutate(
      remove=
        case_when(
          Date>last.day | is.na(last.day) ~"keep"),
      flood=if_else(remove!='keep', NA_real_, flood),
    )%>%
    select(-last.day, -remove, -stage.x, -stage.y)
  
  
  remove.tail<-
    left_join(remove.head, tail, by=c('flood', 'ID'))%>%
    mutate(
      remove=
        case_when(
          Date<first.day | is.na(first.day) ~"keep"),
      flood=if_else(remove!='keep', NA, flood),
    )%>%
    select(-first.day, -remove)%>%
    group_by(ID, flood, day)
  
  
  remove.flukes <- remove.tail %>%
    group_by(ID, flood) %>%
    mutate(
      remove=n_distinct(day),
      flood=if_else(remove<7, NA, flood)
    ) %>%
    ungroup()
  #
}
trim.less.than1<-function(flagged, base.df, base.variable, variable){
  
  prep <- flagged %>% 
    left_join(base.df) %>%
    fill({{base.variable}}, .direction = "down") %>%
    mutate(day=as.Date(Date))%>%
    group_by(ID, day)%>%
    mutate(
      daily=mean({{variable}}, na.rm=T)
    )%>% ungroup()%>%
    group_by(flood, ID)%>%
    mutate(
      normalized = daily/{{base.variable}},
      day        = as.Date(Date),
      trim       = normalized <= 0.95,
      max_idx = which.max(daily),
      max_date = Date[max_idx],    
      stage=case_when(
        Date<max_date ~"before",
        TRUE ~'after'),
    )%>%select(-max_idx, -max_date)
  
  
  bounds<-prep %>%
    filter(trim==TRUE)%>%
    group_by(ID, flood, stage)%>%
    summarise(
      first.day=min(Date, na.rm=T),
      last.day=max(Date, na.rm=T)
    )
  
  
  head<-bounds%>%
    filter(stage=='before')%>%
    select(-first.day)
  
  
  tail<-bounds%>%
    filter(stage=='after')%>%
    select(-last.day)
  
  
  remove.head<-
    left_join(prep, head, by=c('flood', 'ID'))%>%
    mutate(
      remove=
        case_when(
          Date>last.day | is.na(last.day) ~"keep"),
      flood=if_else(remove!='keep', NA_real_, flood),
    )%>%
    select(-last.day, -remove, -stage.x, -stage.y)
  
  
  remove.tail<-
    left_join(remove.head, tail, by=c('flood', 'ID'))%>%
    mutate(
      remove=
        case_when(
          Date<first.day | is.na(first.day) ~"keep"),
      flood=if_else(remove!='keep', NA, flood),
    )%>%
    select(-first.day, -remove)%>%
    group_by(ID, flood, day)
  
  
  remove.flukes <- remove.tail %>%
    group_by(ID, flood) %>%
    mutate(
      remove=n_distinct(day),
      flood=if_else(remove<7, NA, flood)
    ) %>%
    ungroup()
  
}

find.peak<-function(smooth, variable) {
  
  count.hours<-smooth%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.max(replace({{variable}}, is.na({{variable}}), -Inf)), 
      count = case_when(
        row_number() < max_height ~ row_number() - max_height,
        row_number() == max_height ~ 0,
        row_number() > max_height ~ row_number() - max_height))
  
  find.peak<-count.hours%>%
    group_by(ID, flood) %>%
    filter(count==0)
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

time.btwn.and.duration<-function(df){
  
  prep<- edit %>% 
    mutate(
      flooded=case_when(
        !is.na(flood)~'flooded',
        TRUE~'norm')
    )%>%
    fill(flood, .direction = 'down')
  
  
  time.btwn<- prep %>%
    filter(flooded=='norm')%>%
    group_by(ID, flood)%>%
    mutate(
      time.btwn=n_distinct(day)
    )%>% 
    summarise(
      time.btwn=max(time.btwn)
    )
  
  
  duration<- prep %>%
    filter(flooded=='flooded')%>%
    group_by(ID, flood)%>%
    mutate(
      duration=n_distinct(day)
    )%>% 
    summarise(
      duration=max(duration)
    )
  
  final<-full_join(time.btwn, duration, by=c('ID', 'flood'))
}

fit_recessions.greater1 <- function(trim, base, variable, base.var) {
  
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
    rename(recess.intercept=Intercept, recess.slope=slope)

}
fit_recessions.less1 <- function(trim, base, variable, base.var) {
  
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
    rename(rise.intercept=Intercept, rise.slope=slope)
  
}

flood.base_compare <- function(peak_df, base_df, variable) {
  var_name   <- as_label(enquo(variable))
  base_name  <- paste0("base.",   var_name)
  pct_name   <- paste0("percent.change.", var_name)
  
  left_join(peak_df, base_df) %>%
    mutate(
      !!pct_name := (
        {{ variable }} - .data[[base_name]]
      ) / .data[[base_name]]
    ) %>%
    group_by(ID)%>%
    fill(base_name, .direction = "down")%>%
    select(
      Date,
      ID,
      flood,
      {{ variable }},
      .data[[base_name]],
      .data[[pct_name]]
    )
}

