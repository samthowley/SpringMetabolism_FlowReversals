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
    day=as.Date(Date)
  )%>%filter(!is.na(CO2))


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
CO2.smooth<-smooth(CO2_flagged, CO2)

#function(flagged, base.df, base.variable, variable)
CO2.trimmed<-trim.increases(CO2.smooth, CO2_loess)%>%
  mutate(
    flood=if_else(ID=='AM' & flood==5 & Date> '2023-03-15', NA, flood),
    flood=if_else(ID=='ID' & flood==3 & Date> '2023-04-28', NA, flood),
    flood=if_else(ID=='LF' & flood==2 & Date> '2022-08-22', NA, flood),
    flood=if_else(ID=='OS' & flood==4 & Date> '2023-03-21', NA, flood),
    flood=if_else(ID=='OS' & flood==6, NA, flood)
  )

CO2.duration<-duration(CO2.trimmed)

CO2.count<-count.max(CO2.smooth, CO2_loess)

CO2.max<-maximum(CO2.trimmed, CO2)

CO2.compare<-flood.base_compare(CO2.max, CO2.base, maximum)

recession.lm<-fit_recessions(CO2.count, CO2.base, CO2, base) 
rise.lm<-fit_rise(CO2.count, CO2.base, CO2, base) 


flood.impacts.CO2<-
  full_join(recession.lm,CO2.duration)%>%
  full_join(CO2.compare, by=c('ID', 'flood'))%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(CO2.max, by=c('ID', 'flood'))%>%
  full_join(CO2.base, by=c('ID', 'flood'))

write_csv(flood.impacts.CO2, "04_Outputs/flood impacts/CO2.csv")

