source("03_Scripts/disturbance isolation functions.R")

GPP <- read_csv("04_Outputs/master.metabolism.csv")%>%
  select(Date, ID, GPP)%>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv")%>%
      mutate(Date=as.Date(Date))%>%
      group_by(ID, Date)%>%
      summarise(depth=mean(depth, na.rm=T))
  )%>%left_join(read_csv("02_Clean_data/Chem/DO.csv"))


floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(start=as.Date(start), end=as.Date(end))

GPP_flagged <- GPP %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  filter(!is.na(GPP))%>%
  mutate(date=Date,
         GPP=if_else(ID=='GB' & flood=='1' & Date>'2022-10-01' & Date<'2022-10-06', NA, GPP)
  )


GPP.base<-baseline(GPP_flagged, GPP)


fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.5, min_rows = 5) {
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

GPP.count<-count.min(GPP_flagged, GPP)

GPP.remove.gaps<-remove_gaps(GPP.count, gap_days=7)

GPP.smooth<-smooth(GPP.remove.gaps, GPP)


GPP.prep<-prep.by.slope_decreases(GPP.smooth, GPP_loess)%>%
  mutate(
    remove=if_else(count>=-10 & count<=10, "keep", remove),
    remove=if_else(ID=='LF'& count>=-20 & count<=10, "keep", remove),
    )

GPP%>% 
  filter(ID=='GB', !is.na(flood))%>%
  ggplot(aes(x=Date, y=GPP))+
  geom_line(aes(y=GPP_loess), alpha=0.2)+
  geom_point(aes(color=remove))+
  facet_wrap(~flood, scales='free')+
  theme_minimal()


GPP.trim<-trim(GPP.prep)


GPP.trim%>% 
  filter(ID=='AM'#, flood==2
         )%>%
  ggplot(aes(x=count, y=GPP))+
  geom_point(aes(color=remove))+
  geom_point(aes(y=GPP_loess), alpha=0.3, color='blue')+
  geom_smooth(method = lm, aes(group=stage, y=GPP))+
  facet_wrap(~flood, scales='free')+
  theme(legend.position = "bottom")


GPP.min<-minimum(GPP.trim,  GPP)

GPP.duration<-duration(GPP.trim)

recession.lm<-fit_recessions(GPP.trim, GPP.base, GPP, base.GPP) 
rise.lm<-fit_rise(GPP.trim, GPP.base, GPP, base.GPP) 


GPP.recover.count1<-GPP.count %>%
  arrange(ID, Date) %>%
  fill(flood, .direction = "down") %>%
  left_join(GPP.base, by = c("ID", "flood")) %>%
  fill(base, .direction="downup")

GPP.recover.count2<-smooth(GPP.recover.count1, GPP)



GPP.recover.count2%>%
  group_by(ID, flood) %>%
  filter(count >= 0) %>%
  mutate(
    date = as.Date(Date),
    within_baseline = (GPP_loess / base),
    recovered = if_else(within_baseline >= 0.8, "recovered", NA))%>%
  
  filter(ID=='AM')%>%
  ggplot(aes(x=Date, y=GPP))+
  geom_point(aes(color=recovered))+
  geom_point(aes(y=GPP_loess), alpha=0.3)+
  geom_line(aes(y=base), color='purple')+
  facet_wrap(~flood, scales='free')+
  theme(legend.position = "bottom")
  #   first_recovery = min(date[recovered == "recovered"], na.rm = TRUE),
  #   days_to_recovery = as.numeric(difftime(first_recovery, min(date), units = "days")),
  #   censored = is.infinite(first_recovery) | is.na(first_recovery),
  #   days_to_recovery = if_else(censored, NA_real_, days_to_recovery)
  # ) %>%
  # ungroup()
names(GPP.count)






test <- GPP.count %>%
  arrange(ID, Date)%>%
  fill(flood, .direction = "down")%>%
  left_join(GPP.base, by = c("ID", "flood")) %>%
  group_by(ID, flood) %>%
  filter(count >= 0) %>%  # post-peak only
  mutate(
    date = as.Date(Date),
    # Is this observation within threshold % of baseline?
    within_baseline = (GPP/base),
    recovered=if_else(within_baseline>=0.7, "recovered", NA)
    )
    
    
test%>% 
  filter(ID=='AM'#, flood==2
  )%>%
  ggplot(aes(x=count, y=within_baseline))+
  geom_point()+
  facet_wrap(~flood, scales='free')+
  theme(legend.position = "bottom")
    






flood.impacts.GPP<-
  full_join(recession.lm,GPP.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(GPP.min, by=c('ID', 'flood'))%>%
  full_join(GPP.base, by=c('ID', 'flood'))%>%
  mutate(variable='GPP')


write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP.csv")


recovery_time <- function(trim, base, variable, threshold = 0.10, consec_days = 3) {
  
  # Join baseline into trimmed data
  prep <- trim %>%
    filter(!is.na(flood), count >= 0) %>%  # post-peak only
    left_join(base, by = c("ID", "flood")) %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      date = as.Date(Date),
      # Is this observation within threshold % of baseline?
      within_baseline = abs(({{ variable }} - base) / base) <= threshold,
      # Rolling check: are the next consec_days rows also within baseline?
      recovered = rollapply(
        within_baseline,
        width = consec_days,
        FUN = all,
        fill = FALSE,
        align = "left",
        na.rm = FALSE
      )
    ) %>%
    summarise(
      peak_date = min(date[count == 0], na.rm = TRUE),
      recovery_date = min(date[recovered], na.rm = TRUE),
      recovery_days = as.numeric(difftime(recovery_date, peak_date, units = "days")),
      # Flag if no recovery detected within the flood window
      censored = is.infinite(recovery_date) | is.na(recovery_date),
      recovery_days = if_else(censored, NA_real_, recovery_days),
      recovery_date = if_else(censored, as.Date(NA), recovery_date),
      .groups = "drop"
    )
  
  prep
}

  