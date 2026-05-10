source("03_Scripts/disturbance isolation functions.R")

CO2 <- read_csv("02_Clean_data/Chem/CO2.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")

co2<-full_join(CO2, h)
floods <- read_csv("01_Raw_data/flood.periods.csv")

CO2_flagged <- co2 %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    date=as.Date(Date)
  )%>%
  mutate(
    # CO2=if_else(ID=='AM' & CO2<2000, NA, CO2),
    # CO2=if_else(ID=='AM' & Date<'2022-07-30', NA, CO2),
    # CO2=if_else(ID=='AM' & flood==4 & CO2 >12700, NA , CO2),
    # CO2=if_else(ID=='LF' & flood==3 & CO2 >2560, NA , CO2),
    
    )%>%
  filter(!is.na(CO2))%>%
  group_by(ID, date)%>%
  mutate(CO2.daily.min=min(CO2, na.rm=T))


CO2.base<-baseline(CO2_flagged, CO2)

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


CO2.count<-count.max(CO2_flagged, CO2)

CO2.remove.gaps<-remove_gaps(CO2.count, gap_days=7)

CO2.smooth<-smooth(CO2.remove.gaps, CO2.daily.min)%>%
  rename(CO2_loess=CO2.daily.min_loess)



CO2.prep<-prep.by.slope_increases(CO2.smooth, CO2_loess)%>%
  mutate(
    remove=if_else(count<120 & count> -120, 'keep', remove),
    remove=if_else(ID=='OS'& flood==1& count<300 & count> -120, 'keep', remove)
    
  )

CO2.trim<-trim(CO2.prep)


CO2.trim%>%
    filter(
      ID=='OS',
    )%>%
    ggplot(aes(x=count, y=CO2))+
    geom_point()+
    geom_point(aes(y=CO2_loess, color=remove))+
    geom_smooth(aes(group = stage, y=CO2.daily.min), method='lm')+
    facet_wrap(~flood, scales='free')


CO2.duration<-duration(CO2.trim)

CO2.max<-maximum(CO2.trim, CO2)


recession.lm<-fit_recessions(CO2.count, CO2.base, CO2, base) 
rise.lm<-fit_rise(CO2.count, CO2.base, CO2, base) 


flood.impacts.CO2<-
  full_join(recession.lm,CO2.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(CO2.max, by=c('ID', 'flood'))%>%
  full_join(CO2.base, by=c('ID', 'flood'))%>%
  mutate(variable='CO2')

write_csv(flood.impacts.CO2, "04_Outputs/flood impacts/CO2.csv")

