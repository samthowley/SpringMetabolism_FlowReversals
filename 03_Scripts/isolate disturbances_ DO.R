source("03_Scripts/disturbance isolation functions.R")

DO <- read_csv("02_Clean_data/Chem/DO.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")

DO<-full_join(DO, h)%>%
  filter(!is.na(Date), !is.na(DO))

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  
  select(-start, -end)%>%
  arrange(ID, Date)


plot_grid(
  ggplot(DO_flagged %>% filter(ID=='ID'), 
         aes(x = Date, y=DO, color=as.factor(flood))) +
    geom_point()+
    facet_wrap(~ID, scales='free')
  ,
  ggplot(DO_flagged %>% filter(ID=='ID'), 
         aes(x = Date, y=depth, color=as.factor(flood))) +
    geom_point()+
    facet_wrap(~ID, scales='free')
  ,
  ncol=1
)


DO.base<-baseline(DO_flagged, DO)%>%
  mutate(base=if_else(ID=="AM" & flood==1, 5.387887, base))

fit_loess_by_group <- 
  function(df, y_var, x_var = "t", group_var, span = 0.5, min_rows = 5) {
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
DO.smooth<-smooth(DO_flagged, DO)

DO.count<-count.min(DO.smooth, DO_loess)




DO.smooth%>%
  filter(ID=='AM')%>%
ggplot(aes(x = Date, y = depth, color=as.factor(flood))) +
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_minimal()

(a<-DO.smooth %>%
    filter(ID=='AM', 
           !is.na(flood),
           flood %in% c(7)
     ) %>%
    ggplot(aes(x = Date, y = DO)) +
    geom_point()+
    geom_smooth(method = 'lm')+
    facet_wrap(~ flood, scales = "free")+theme_minimal())

ggplotly(a)




DO.min<-minimum(DO.trimmed,  DO)

DO.compare<-flood.base_compare(DO.min, DO.base, minimum)
DO.duration<-duration(DO.trimmed)

recession.lm<-fit_recessions(DO.count, DO.base, DO, base.DO) 
rise.lm<-fit_rise(DO.count, DO.base, DO, base.DO) 


flood.impacts.DO<-
  full_join(recession.lm,DO.duration)%>%
              full_join(DO.compare, by=c('ID', 'flood'))%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(DO.min, by=c('ID', 'flood'))%>%
  full_join(DO.base, by=c('ID', 'flood'))
  
write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO")



