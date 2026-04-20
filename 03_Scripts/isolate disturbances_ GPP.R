source("03_Scripts/disturbance isolation functions.R")

GPP <- read_csv("04_Outputs/master.metabolism.csv")%>%
  select(Date, ID, GPP)%>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv")%>%
      mutate(Date=as.Date(Date))%>%
      group_by(ID, Date)%>%
      summarise(depth=mean(depth, na.rm=T))
  )


floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(start=as.Date(start), end=as.Date(end))


GPP_flagged <- GPP %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>% 
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  filter(!is.na(GPP))

GPP.base<-baseline(GPP_flagged, GPP)

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
GPP.smooth <- smooth(GPP_flagged%>%
                       fill(flood, .direction = "down"),
                     GPP)%>%
  left_join(GPP.base)



GPP.clean<-prep.count.min(GPP.smooth, GPP)


recovery_days<-GPP.clean%>%
  distinct(ID, flood, first_recovery, last_recovery)%>%
  mutate(recovery_days=first_recovery-last_recovery)

GPP.min<-minimum(GPP.clean,  GPP)
GPP.duration<-duration(GPP.clean)

recession.lm<-fit_recessions(GPP.clean, GPP.base, GPP, base.GPP)
rise.lm<-fit_rise(GPP.clean, GPP.base, GPP, base.GPP) 



flood.impacts.GPP<-
  full_join(recession.lm,GPP.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(GPP.min, by=c('ID', 'flood'))%>%
  full_join(GPP.base, by=c('ID', 'flood'))%>%
  full_join(recovery_days, by=c('ID', 'flood'))%>%
  mutate(variable='GPP'         )


write_csv(flood.impacts.GPP, "test.csv")
#write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP.csv")


flood.impacts.GPP%>%
  left_join(read_csv("04_Outputs/FR.class.csv"))%>%
  left_join(depth)%>%
  
  ggplot(aes(x=percent.change.depth, y=recess.slope))+
  geom_point(aes(shape=ID, color=class))+ scale_y_log10()


  
GPP.clean %>%
    filter(ID == 'OS') %>%
    ggplot(aes(x = count, y = GPP_loess)) +
    geom_point() +
    geom_point(aes(y = GPP), color = 'blue') +
    geom_line(aes(y = base)) +
    geom_smooth(aes(x = count, y = GPP, group = flood.stage), method = 'lm', se=F)+
    # geom_vline(data = recovery_lines, aes(xintercept = last_recovery),  color = 'red',      linetype = 'dashed') +
    # geom_vline(data = recovery_lines, aes(xintercept = first_recovery), color = 'darkgreen', linetype = 'dashed') +
    facet_wrap(~flood, scales = 'free')
  
  