source("03_Scripts/disturbance isolation functions.R")

DO <- read_csv("02_Clean_data/Chem/DO.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")

DO<-full_join(DO, h)%>%
  filter(!is.na(Date), !is.na(DO))%>% 
  mutate(
    date=as.Date(Date)
  )%>%
  group_by(ID, date)%>%
  mutate(
    DO.daily.min=min(DO, na.rm=T)
  )

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  
  select(-start, -end)%>%
  arrange(ID, Date)

DO.base<-baseline(DO_flagged, DO)

fit_loess_by_group <- 
  function(df, y_var, x_var = "t", group_var, span = 0.6, min_rows = 5) {
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


DO.smooth<-smooth(DO_flagged, DO.daily.min)%>%
  rename(DO_loess= DO.daily.min_loess)

DO.count<-count.min(DO.smooth, DO.daily.min)

DO.prep<-prep.by.slope_decreases(DO.count, DO.daily.min)%>%
  mutate(
    remove=if_else(ID %in% c('ID', 'GB') & DO_loess<5, 'keep', remove),
    remove=if_else(ID %in% c('AM', 'LF', 'OS') & DO_loess<3.5, 'keep', remove),
    DO=if_else(ID=='LF' & flood=='4' & count>700, NA, DO),
    DO=if_else(ID=='ID' & flood=='1' & count>500, NA, DO),
  )%>%
  drop_na(DO)


DO.trim<-trim(DO.prep)

DO.trim%>% filter(ID=='ID')%>%
  ggplot(aes(x=date, y=DO))+
  geom_point(aes(y=DO_loess), size=3)+
  geom_point()+
  geom_smooth(method = lm, aes(group=stage, y=DO))+
  facet_wrap(~flood, scales='free')+
  theme(legend.position = "bottom")


DO.min<-minimum(DO.trim,  DO)

DO.duration<-duration(DO.trim)

recession.lm<-fit_recessions(DO.trim, DO.base, DO, base.DO) 
rise.lm<-fit_rise(DO.trim, DO.base, DO, base.DO) 


flood.impacts.DO<-
  full_join(recession.lm,DO.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(DO.min, by=c('ID', 'flood'))%>%
  full_join(DO.base, by=c('ID', 'flood'))


ggplot(flood.impacts.DO, aes(x=flood))+
  geom_point(aes(y=base))+
  geom_point(aes(y=minimum), color='red')+
  geom_point(aes(y=rise.intercept), color='lightblue')+
  geom_point(aes(y=recess.intercept), color='blue')


write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO")



