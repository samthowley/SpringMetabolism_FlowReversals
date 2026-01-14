
DO <- read_csv("02_Clean_data/Chem/DO.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")
source("03_Scripts/disturbance isolation functions.R")

DO<-full_join(DO, h)

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    DO=if_else(ID=='AM' & DO> 7.84, NA, DO),
    DO=if_else(flood==7& Date>"2023-04-28", NA, DO),
  )%>%
  mutate(
    flood=if_else(ID %in% c('OS') & Date> '2024-05-31', 30, flood),
    flood=if_else(ID %in% c('LF') & Date> '2024-05-24', 30, flood),
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
DO.smooth<-smooth(DO_flagged, DO)
DO.base<-baseline(DO_flagged, DO)%>%
  mutate(base.DO=if_else(ID=="AM" & flood==1, 5.387887, base.DO))

DO.count<-count.min(DO.smooth, DO_loess)

DO.min<-DO.count%>% filter(count==0)

DO.compare<-flood.base_compare(DO.min, DO.base, DO)%>%
  select(Date, ID, flood, percent.change.DO, DO)

DO.trim<-trim(DO.count%>% filter(count>0), DO.base, DO_loess, base.DO)%>%filter(normalized<1.1)

recession.lm<-fit_recessions(DO.trim, DO.base, DO_loess, base.DO)

DO.time.btwn<-time.btwn(DO_flagged)


flood.impacts.DO<-full_join(recession.lm,DO.time.btwn)%>%full_join(DO.compare, by=c('ID', 'flood'))


ggplot(recession.lm, 
       aes(x = count, y=normalized)) +
  geom_line(aes(y=depth), color='pink')+
  geom_point()+
  geom_hline(yintercept = 1.1, color='red')+
  facet_wrap(~flood, scales='free')


ggplotly(ggplot(DO.trim%>% filter(!is.na(flood), ID=='LF'), 
                aes(x = Date, y=normalized)) +
           geom_line(aes(y=depth), color='pink')+
           geom_point()+
           geom_hline(yintercept = 1.1, color='red')+
           #geom_vline(xintercept = 0, color='blue')+
           facet_wrap(~flood, scales='free'))

