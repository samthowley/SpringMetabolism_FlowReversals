
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

recovery_time <- function(flagged, count_df, base_df, variable) {
  
  first_recovery<-count.min(flagged%>%
                              arrange(ID, Date) %>%
                              fill(flood, .direction = "down"), 
                            {{variable}}) %>%
    left_join(base_df, by = c("ID", "flood")) %>%
    group_by(ID, flood) %>%
    mutate(
      date = as.Date(Date),
      within_baseline = ({{variable}} / base),
      recovered = if_else(within_baseline >= 0.7, "recovered", NA),
      first_recovery = min(date[recovered == "recovered"], na.rm = TRUE),
    )%>%
    distinct(ID, flood, first_recovery)
  
  floodpeak<-count_df%>% filter(count==0)%>%
    mutate(date=as.Date(date))%>%
    select(ID, flood, date)%>%
    rename(floodpeak=date)
  
  recovery_time<-left_join(first_recovery, floodpeak)%>%
    mutate(recovey_period=floodpeak-first_recovery)%>%
    
    recovered<-left_join(first_recovery, recovery_time)
}


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

remove.data.gaps <- function(df, date, gap) {
  df1<-df%>%
    arrange(ID, Date) %>%
    group_by(ID, flood) %>%
    mutate(days_since_last = as.numeric(difftime({{date}}, lag(), units = "days"))) %>%
    filter(is.na(days_since_last) | cumsum(coalesce(days_since_last > {{gap}}, FALSE)) == 0) %>%
    ungroup()
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
    fill(base, .direction = 'downup')%>%
    group_by(flood, ID)%>%
    mutate(base=if_else(is.na(base), mean(base, na.rm=T), base))
  

}

remove_gaps <- function(trim_counted, gap_days) {
  trim_counted %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      days_since_last = as.numeric(difftime(Date, lag(Date), units = "days")),
      row = row_number(),
      peak_row = first(row[count == 0])
    ) %>%
    mutate(
      pre_gap_row = {
        pre <- which(row < peak_row & days_since_last >= gap_days)
        if (length(pre) > 0) max(pre) else 0
      },
      post_gap_row = {
        post <- which(row > peak_row & days_since_last >= gap_days)
        if (length(post) > 0) min(post) else (max(row) + 1)
      }
    ) %>%
    filter(row > pre_gap_row & row < post_gap_row) %>%
    select(-days_since_last, -row, -peak_row, -pre_gap_row, -post_gap_row) %>%
    ungroup()
}

prep.count.min<-function(df.smooth, variable){
  
  df.recover<-df.smooth%>%    
    group_by(ID, flood) %>%
    mutate(
      date = as.Date(Date),
      within_baseline = ({{variable}}/ base),
      recovered = if_else(within_baseline >= 0.8, "recovered", NA)
    )
  
  df.count<-count.min(df.recover, {{variable}})%>%
    arrange(ID, Date)%>%
    group_by(ID, flood) %>%
    mutate(
      flood.stage=case_when(
        count<0~ 'pre',
        count>=0~ 'post'),
      last_recovery  = max(Date[recovered == "recovered" & flood.stage == 'pre'],  na.rm = TRUE),
      first_recovery = min(Date[recovered == "recovered" & flood.stage == 'post'], na.rm = TRUE))%>%
    filter(
      case_when(
        !is.infinite(first_recovery) ~ Date >= last_recovery & Date <= first_recovery,
        TRUE ~ Date >= last_recovery
      )
    )
  
}
prep.count.max<-function(df.smooth, variable){
  
  df.recover<-df.smooth%>%    
    group_by(ID, flood) %>%
    mutate(
      date = as.Date(Date),
      within_baseline = ({{variable}}/ base),
      recovered = if_else(within_baseline >= 0.8, "recovered", NA)
    )
  
  df.count<-count.max(df.recover, {{variable}})%>%
    arrange(ID, Date)%>%
    group_by(ID, flood) %>%
    mutate(
      flood.stage=case_when(
        count<0~ 'pre',
        count>=0~ 'post'),
      last_recovery  = max(Date[recovered == "recovered" & flood.stage == 'pre'],  na.rm = TRUE),
      first_recovery = min(Date[recovered == "recovered" & flood.stage == 'post'], na.rm = TRUE))%>%
    filter(
      case_when(
        !is.infinite(first_recovery) ~ Date >= last_recovery & Date <= first_recovery,
        TRUE ~ Date >= last_recovery
      )
    )
  
}

prep.by.slope_increases<-function(df, variable){
  slopes <- df %>%
    filter(!is.na({{variable}}), !is.na(Date))%>%
    arrange(ID, Date) %>%
    group_by(ID)%>%
    mutate(
      norm={{variable}}, #/mean(depth_smooth, na.rm=T),
      date      = as.Date(Date),
      # 3-day index starting at the first date in your data
      day_index = as.integer(date - min(date)),
      block3    = day_index %/% 4) %>%ungroup()%>%
    group_by(block3, ID) %>%
    summarise(
      start_date = min(date),
      end_date   = max(date),
      slope      = {
        x <- as.numeric(Date)      # seconds since origin
        y <- norm
        coef(lm(y ~ x))[2] * 86400     # convert to units per day
      },
      .groups = "drop"
    )%>% group_by(ID) %>%
    ungroup()
  
  out_filtered <- 
    df %>%
    filter(!is.na(flood))%>%
    mutate(day = as.Date(Date)) %>%
    left_join(slopes, by = join_by(ID, between(day, start_date, end_date))) %>%
    group_by(ID, flood) %>%
    mutate(
      abs.slope = abs(slope),
      remove = case_when(
        count < 0  & slope < 0  ~ "remove",
        count >= 0 & slope >= 0 ~ "remove",
        TRUE ~ "keep"),
      stage = if_else(count < 0, "pre", "post")
    )
}
prep.by.slope_decreases<-function(df, variable){
  slopes <- df %>%
    filter(!is.na({{variable}}), !is.na(Date))%>%
    arrange(ID, Date) %>%
    group_by(ID)%>%
    mutate(
      norm={{variable}}, #/mean(depth_smooth, na.rm=T),
      date      = as.Date(Date),
      # 3-day index starting at the first date in your data
      day_index = as.integer(date - min(date)),
      block3    = day_index %/% 4) %>%ungroup()%>%
    group_by(block3, ID) %>%
    summarise(
      start_date = min(date),
      end_date   = max(date),
      slope      = {
        x <- as.numeric(Date)      # seconds since origin
        y <- norm
        coef(lm(y ~ x))[2] * 86400     # convert to units per day
      },
      .groups = "drop"
    )%>% group_by(ID) %>%
    ungroup()
  
  out_filtered <- 
    df %>%
    filter(!is.na(flood))%>%
    mutate(day = as.Date(Date)) %>%
    left_join(slopes, by = join_by(ID, between(day, start_date, end_date))) %>%
    group_by(ID, flood) %>%
    mutate(
      abs.slope = abs(slope),
      remove = case_when(
        count < 0  & slope > 0  ~ "remove",
        count >= 0 & slope <= 0 ~ "remove",
        TRUE ~ "keep"),
      stage = if_else(count < 0, "pre", "post")
    )
}

trim<-function(prep){
  
  trim<-prep %>%
    group_by(ID, flood, stage) %>%
    mutate(
      stage = if_else(count < 0, "pre", "post"),
      last_remove_day_pre  = if (stage[1] == "pre")  max(day[remove == "remove"], na.rm = TRUE) else as.Date(NA),
      first_remove_day_post = if (stage[1] == "post") min(day[remove == "remove"], na.rm = TRUE) else as.Date(NA),
      last_remove_day_pre  = if_else(is.infinite(last_remove_day_pre),  as.Date(NA), last_remove_day_pre),
      first_remove_day_post = if_else(is.infinite(first_remove_day_post), as.Date(NA), first_remove_day_post),
      keep_window = case_when(
        stage == "pre"  & !is.na(last_remove_day_pre)   ~ day >= last_remove_day_pre,
        stage == "post" & !is.na(first_remove_day_post) ~ day <= first_remove_day_post,
        TRUE ~ TRUE
      )
    ) %>%
    filter(keep_window) %>%
    ungroup() %>%
    select(-start_date, -end_date, -day,
           -last_remove_day_pre, -first_remove_day_post, -keep_window)
  
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

get_bp_slopes <- function(data, y, x, breaks = 2) {
  y <- enquo(y)
  x <- enquo(x)
  y_name <- quo_name(y)
  x_name <- quo_name(x)
  
  df <- data %>%
    select(!!y, !!x) %>%
    filter(!is.na(!!y), !is.na(!!x)) %>%
    arrange(!!x)
  
  if (nrow(df) < 5) {
    return(list(
      breakpoints = tibble(breakpoint_number = integer(), breakpoint_x = numeric()),
      segments = tibble(seg_id=integer(), segment=character(), seg_start=numeric(),
                        seg_end=numeric(), n=integer(), slope=numeric(), intercept=numeric()),
      data_segmented = df,
      bp_fit = NULL
    ))
  }
  
  fml <- as.formula(paste0(y_name, " ~ ", x_name))
  bp  <- breakpoints(fml, data = df, breaks = breaks)
  
  idx <- bp$breakpoints
  idx <- idx[!is.na(idx)]
  x_bp <- sort(as.numeric(df[[x_name]][idx]))
  
  cuts <- c(-Inf, x_bp, Inf)
  df <- df %>%
    mutate(
      segment = cut(.data[[x_name]], breaks = cuts, include.lowest = TRUE),
      seg_id  = as.integer(segment)
    )
  
  seg_tbl <- df %>%
    filter(!is.na(segment)) %>%
    group_by(seg_id, segment) %>%
    summarise(
      seg_start = min(.data[[x_name]], na.rm = TRUE),
      seg_end   = max(.data[[x_name]], na.rm = TRUE),
      n         = n(),
      slope     = coef(lm(fml, data = cur_data()))[[x_name]],
      intercept = coef(lm(fml, data = cur_data()))[["(Intercept)"]],
      .groups = "drop"
    ) %>%
    arrange(seg_id)
  
  bp_tbl <- tibble(
    breakpoint_number = seq_along(x_bp),
    breakpoint_x      = x_bp
  )
  
  list(
    bp_fit = bp,
    breakpoints = bp_tbl,
    segments = seg_tbl,
    data_segmented = df
  )
}

run_bp_all_sites <- function(data, site_col = ID, y = ER, x = depth, breaks = 2) {
  site_col <- enquo(site_col)
  
  results <- data %>%
    group_by(!!site_col) %>%
    group_nest() %>%
    mutate(res = map(data, ~ get_bp_slopes(.x, y = {{ y }}, x = {{ x }}, breaks = breaks)))
  
  # tidy outputs you can immediately use
  breakpoints_tbl <- results %>%
    transmute(!!site_col, breakpoints = map(res, "breakpoints")) %>%
    unnest(breakpoints)
  
  segments_tbl <- results %>%
    transmute(!!site_col, segments = map(res, "segments")) %>%
    unnest(segments)
  
  # keep raw objects (bp_fit etc.) if you want to plot per site later
  list(
    results = results,                 # nested list-column per site
    breakpoints = breakpoints_tbl,     # long table: site + breakpoint depths
    segments = segments_tbl            # long table: site + segment slopes + interval
  )
}
