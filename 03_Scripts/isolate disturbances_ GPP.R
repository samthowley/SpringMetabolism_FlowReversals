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
         GPP=if_else(ID=='GB' & flood=='1' & Date>'2022-10-01'  & Date<'2022-10-06', NA, GPP),
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

GPP.trim<-trim(GPP.prep)

GPP.trim%>% 
  filter(ID=='LF'#, flood==2
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

recovery_time.GPP<-recovery_time(GPP_flagged, GPP.count, GPP.base, GPP)


depth <- read_csv("04_Outputs/flood impacts/depth.csv")%>%
  select(ID, flood, percent.change, maximum, base)%>%
  rename(percent.change.depth=percent.change, maximum.depth=maximum, base.depth=base)

flood.impacts.GPP<-
  full_join(recession.lm,GPP.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(GPP.min, by=c('ID', 'flood'))%>%
  full_join(GPP.base, by=c('ID', 'flood'))%>%
  full_join(recovery_time.GPP, by=c('ID', 'flood'))%>%
  mutate(variable='GPP',
         recession.days=(base-recess.intercept)/recess.slope,
         rise.day=(base-rise.intercept)/rise.slope
         )

write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP.csv")


flood.impacts.GPP%>%
  left_join(read_csv("04_Outputs/FR.class.csv"))%>%
  left_join(depth)%>%
  
  ggplot(aes(x=percent.change.depth, y=recess.slope))+
  geom_point(aes(shape=ID, color=class))+ scale_y_log10()


  names(flood.impacts.GPP)
  