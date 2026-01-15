source("03_Scripts/disturbance isolation functions.R")

DO <- read_csv("02_Clean_data/Chem/DO.csv")%>%
  mutate(DO=if_else(DO<0.25, DO==0.1, DO))
h <- read_csv("02_Clean_data/Chem/depth.csv")

DO<-full_join(DO, h)

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    DO=if_else(ID=='OS' & Date>'2023-07-12' & flood==6, NA, DO)
  )


fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.1, min_rows = 5) {
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
DO.smooth<-smooth(DO_flagged, DO)%>%
  mutate(Date=ymd_hms(Date))


DO.base<-baseline(DO_flagged, DO)%>%
  mutate(base.DO=if_else(ID=="AM" & flood==1, 5.387887, base.DO))




DO.count<-count.min(DO.smooth, DO_loess)

DO.min<-DO.count%>% filter(count==0)

DO.compare<-flood.base_compare(DO.min, DO.base, DO)%>%
  select(Date, ID, flood, percent.change.DO, DO)


DO.trim<-trim.greater.1(DO.count, DO.base, DO_loess, base.DO)%>%
  mutate(
    DO=if_else(ID=='AM' & DO>6 & flood==7, NA, DO),
    DO=if_else(ID=='AM' & Date> "2023-03-15" & flood==5, NA, DO),
    DO=if_else(ID=='AM' & Date> "2024-01-02" & flood==11, NA, DO),
    
    )



recession.lm<-fit_recessions(DO.trim, DO.base, base.DO)




DO.time.btwn<-time.btwn(DO_flagged)


flood.impacts.DO<-full_join(recession.lm,DO.time.btwn)%>%full_join(DO.compare, by=c('ID', 'flood'))






