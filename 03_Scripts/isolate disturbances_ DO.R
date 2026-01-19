source("03_Scripts/disturbance isolation functions.R")

DO <- read_csv("02_Clean_data/Chem/DO.csv")%>%
  mutate(DO=if_else(DO<0.25, DO==0.1, DO))
h <- read_csv("02_Clean_data/Chem/depth.csv")

DO<-full_join(DO, h)%>%
  filter(!is.na(Date), !is.na(DO))

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    flood=if_else(ID=='OS'& flood==6 & Date>'2023-09-01', NA, flood),
    flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    flood=if_else(ID=='LF' & Date>'2024-05-13', 16, flood),
    flood=if_else(ID=='OS' & Date>'2024-06-05', 12, flood),
    day=as.Date(Date),
    DO=if_else(ID=='AM'& flood==7& DO>7, NA, DO)
  )


DO.base<-baseline(DO_flagged, DO)%>%
  mutate(base.DO=if_else(ID=="AM" & flood==1, 5.387887, base.DO))


#function(flagged, base.df, base.variable, variable)
DO.trimmed<-trim.greater.than1(DO_flagged, DO.base, base.DO, DO)


edit<-DO.trimmed%>%
  mutate(
    #flood=if_else(ID== & flood== & count>, NA, flood),
    flood=if_else(ID=='LF'& flood==12 & Date>'2024-01-04', NA, flood)
  )


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
DO.smooth<-smooth(edit, DO)

DO.count<-count.min(DO.smooth, DO_loess)

DO.min<-DO.count%>% filter(count==0)

DO.compare<-flood.base_compare(DO.min, DO.base, DO)%>%
  select(Date, ID, flood, percent.change.DO, DO)

DO.time.btwn.and.duration<-time.btwn.and.duration(edit)

recession.lm<-fit_recessions(DO.count, DO.base, DO, base.DO) %>%
  mutate(
    recovery.days.DO = (base.DO- Intercept) / slope
  )

flood.impacts.DO<-
  full_join(recession.lm,DO.time.btwn.and.duration)%>%
              full_join(DO.compare, by=c('ID', 'flood'))
