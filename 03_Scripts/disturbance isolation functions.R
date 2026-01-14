smooth <- function(flagged, variable) {
  prep <- flagged %>%
    filter(!is.na(flood),!is.na({{variable}})) %>%
    arrange(flood, Date) %>%
    group_by(flood, ID) %>%
    mutate(
      t = as.numeric(Date - min(Date)),
      group_ID=paste(ID, flood, sep = "_")) %>%   # numeric time for loess
    ungroup()
  
  smooth<-fit_loess_by_group(prep, {{variable}}, t, group_ID)
  
}
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

count.peak<-function(trim, variable) {
  
  count.hours<-trim%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.max(replace({{variable}}, is.na({{variable}}), -Inf)), 
      count = case_when(
        row_number() < max_height ~ row_number() - max_height,
        row_number() == max_height ~ 0,
        row_number() > max_height ~ row_number() - max_height))%>%
    filter(count>0)
  
}
count.min<-function(trim, variable) {
  
  count.hours<-trim%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.min(replace({{variable}}, is.na({{variable}}), -Inf)), 
      count = case_when(
        row_number() < max_height ~ row_number() - max_height,
        row_number() == max_height ~ 0,
        row_number() > max_height ~ row_number() - max_height))
  
}
#right now I only use this for stage
trim <- function(recession, base, variable_loess, base.variable) {
  
  normalized<-left_join(recession, base)%>%
    fill({{base.variable}}, .direction = "down")%>%
    mutate(normalized={{variable_loess}}/{{base.variable}})
}

fit_recessions <- function(trim, base, variable, base.var) {
  
  prep <- trim %>%
    mutate(group_ID = paste0(ID, "_", flood))
  
  # Build formula dynamically
  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)
  
  recess.lm <- coef(rC) %>%
    as_tibble() %>%
    mutate(ID = names(rC)) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood"))
  
  # Calculate recovery days using joined base values
  var_name <- as_label(enquo(base.var))
  recess.lm %>%
    mutate(
      recovery.days = (.data[[var_name]] - Intercept) / slope
    )
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

time.btwn<-function(stage_flagged){
  prep.time.btwn<-stage_flagged%>%
    mutate(condition=case_when(
      is.na(flood)~"base",
      TRUE~'flooded'
    ))
  
  time.btwn <- prep.time.btwn %>% 
    arrange(ID, Date) %>% 
    group_by(ID) %>%
    mutate(
      group = cumsum(condition == "flooded"),  # Create a grouping variable that increments at each "baseline"
      time.btwn = unlist(ave(condition, group, FUN = function(x) {
        cumsum(x %in% c("base"))
      }))) %>%ungroup()  %>%
    fill(flood, .direction = "updown")%>%
    group_by(ID, condition,flood)%>%
    summarize(
      time.btwn=max(as.numeric(time.btwn), na.rm = T)/24
    )%>%filter(condition=='base')
  
  
}
