
library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(weathermetrics)
library(lme4)
library(zoo)
library(strucchange)

floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(start=as.Date(start), end=as.Date(end))


fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.4, min_rows = 5) {
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
      base_1 = mean({{ variable }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ID, flood)
  
  depth_i <- flagged %>%
    fill(flood, .direction = "downup")%>%
    group_by(ID) %>%
    mutate(
      depth25 = quantile(depth, 0.25, na.rm = TRUE),
      depth75 = quantile(depth, 0.75, na.rm = TRUE),
      IQR_val = depth75 - depth25,
      depth_i = case_when(
        depth < depth25  ~ "low",
        depth > depth75  ~ "high",
        TRUE ~ "normal"
      ))%>%
    group_by(ID, depth_i, flood)%>% 
    summarise(
      base_i=mean({{ variable }}, na.rm=T)
    )%>%
    filter(depth_i=="low")
  
  base<-full_join(base_tbl, depth_i, by=c('ID', 'flood'))%>% 
    mutate(base=(base_1+base_i)/2)%>%
    select(flood, ID, base)%>%
    fill(base, .direction = '{{variable}}wnup')%>%
    group_by(flood, ID)%>%
    mutate(base=if_else(is.na(base), mean(base, na.rm=T), base))
  

}


prep.for.slope.min<-function(df.smooth, variable, variable_loess){
  
  df.recover<-df.smooth%>%    
    group_by(ID, flood) %>%
    mutate(
      date = as.Date(Date),
      within_baseline = ({{variable}}/ base),
      threshold = if_else(any(within_baseline < 0.8, na.rm = TRUE), 0.8, 1.0),
      recovered = if_else(within_baseline >= threshold, "recovered", NA_character_)
          )
  
  df.count <- count.min(df.recover, {{variable_loess}}) %>%
    filter({{variable}}<base)%>%
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > 1.5,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0
    ) %>%
    filter(!(after_first_gap & stage.flood == 'post')) %>%
    ungroup()
  
}
prep.for.slope.max<-function(df.smooth, variable, variable_loess){

  df.recover<-df.smooth%>%
    group_by(ID, flood) %>%
    mutate(
      date = as.Date(Date),
      within_baseline = ({{variable}}/ base),
      threshold = if_else(any(within_baseline > 1.2, na.rm = TRUE), 1.2, 1.0),
      recovered = if_else(within_baseline <= threshold, "recovered", NA_character_)
    )

  df.count <- count.max(df.recover, {{variable_loess}}) %>%
    filter({{variable}}>base)%>%
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > 1.5,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0
    ) %>%
    filter(!(after_first_gap & stage.flood == 'post')) %>%
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
      duration=n_distinct(date)
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
    filter(!is.na(flood), count > 0) %>%
    mutate(group_ID = paste0(ID, "_", flood))
  
  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)
  
  coef(rC) %>%
    as_tibble() %>%
    mutate(
      ID    = names(rC),
      r2    = sapply(rC, function(m) if (!is.null(m)) summary(m)$r.squared else NA_real_)
    ) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood")) %>%
    rename(recess.intercept = Intercept, recess.slope = slope, r2.recess=r2) %>%
    select(-base)
}
fit_rise <- function(trim, base, variable, base.var) {
  prep <- trim %>%
    filter(!is.na(flood), count < 0) %>%
    mutate(group_ID = paste0(ID, "_", flood))
  
  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)
  
  coef(rC) %>%
    as_tibble() %>%
    mutate(
      ID = names(rC),
      r2 = sapply(rC, function(m) if (!is.null(m)) summary(m)$r.squared else NA_real_)
    ) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood")) %>%
    rename(rise.intercept = Intercept, rise.slope = slope, r2.rise=r2) %>%
    select(-base)
}
