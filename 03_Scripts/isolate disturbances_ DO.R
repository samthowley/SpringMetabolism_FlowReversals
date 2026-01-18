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
    DO=if_else(ID=='OS' & Date>'2023-07-12' & flood==6, NA, DO),
    flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    flood=if_else(ID=='LF' & Date>'2024-05-13', 16, flood),
    flood=if_else(ID=='OS' & Date>'2024-06-05', 12, flood)
    
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

DO.smooth<-smooth(DO_flagged, DO)


DO.base<-baseline(DO_flagged, DO)%>%
  mutate(base.DO=if_else(ID=="AM" & flood==1, 5.387887, base.DO))


DO.count<-count.min(DO.smooth, DO_loess)

DO.min<-DO.count%>% filter(count==0)

DO.compare<-flood.base_compare(DO.min, DO.base, DO)%>%
  select(Date, ID, flood, percent.change.DO, DO)






prep <- DO.count %>% 
  left_join(DO.base) %>%
  fill(base.DO, .direction = "down") %>%
  mutate(
    normalized = DO_loess/base.DO,
    day        = as_date(Date),
    trim       = normalized >= 0.95
  )%>%
  mutate(
    day=as.Date(Date),
    stage=case_when(
      count<0~"before",
      count>0~'after'),
    DO=if_else(ID=='AM'& flood==7& DO>7, NA, DO)
    )

    
head<-bounds%>%
  filter(
    stage=='before')%>%
  select(-first.day, -stage, -trim)


remove.head<-prep%>%
  left_join(head, by=c('flood', 'ID'))%>%
  group_by(ID, flood)%>%
  mutate(
    remove=
      case_when(
        Date>last.day | is.na(last.day) ~"keep")
    )%>%
  filter(remove=='keep')%>%
  select(-last.day, -remove)


tail<-bounds%>%
  filter(
    stage=='after')%>%
  select(-last.day, -stage, -trim)


remove.tail<-remove.head%>%filter(ID=='AM')%>%
  left_join(tail, by=c('flood', 'ID'))%>%
  group_by(ID, flood)%>%
  mutate(
    remove=
      case_when(
        Date<first.day | is.na(first.day) ~"keep")
  )%>%
  filter(remove=='keep')%>%
  select(-first.day, -remove, -stage)%>%
  group_by(flood,ID, day)%>%
  mutate(
    DO.avg=max(DO, na.rm=T),
  )


slopes <- remove.tail %>%
  filter(!is.na(Date))%>%
  arrange(ID, Date) %>%
  group_by(ID)%>%
  mutate(
    day_index = as.numeric(day - min(day)) + 1,
    block3    = ((day_index - 1) %/% 8) + 1   # 3‑day group index
  ) %>%ungroup()%>%
  group_by(block3, ID) %>%
  summarise(
    start_date = min(day),
    end_date   = max(day),
    slope      = {
      x <- as.numeric(Date)      # seconds since origin
      y <- DO.avg
      coef(lm(y ~ x))[2] * 86400     # convert to units per day
    },
    .groups = "drop"
  )

stagnate <- remove.tail %>%
  mutate(day=as.Date(Date))%>%
  left_join(
    slopes, by = join_by(ID, between(day, start_date, end_date))
  ) %>%
  mutate(
    abs.slope=abs(slope),
    DO=if_else(ID=='AM', DO>2 & abs.slope<0.3 & flood !=1, NA, DO),
    DO=if_else(ID=='AM', flood==11 & count>55, NA, DO),
    DO=if_else(ID=='AM', flood==5 & count>250, NA, DO),
    
  )



(a<-ggplot(stagnate%>% 
             filter(!is.na(flood), ID=='AM'), 
           aes(x = count)) +
    geom_point(aes(y=DO, color=abs.slope), size=1)+
    scale_color_gradient(low='blue', high='orange')+
    geom_point(aes(y=depth), size=0.5)+
    facet_wrap(~flood, scales='free'))
ggplotly(a)   # group_by(flood, ID, day)%>%
  # mutate(
  #   )
#     )%>%
#   count.min(variable = depth) %>%  # Your min var
#   group_by(ID, flood) %>%
#   filter(n() >= 96) %>%             # Keep days w/ ≥7 hours data
#   ungroup()
# stagnate=abs(DO_loess-lag(DO_loess, 72)),




test<-DO.trim%>%filter(ID=='AM')



recession.lm<-fit_recessions(DO.trim, DO.base, base.DO) %>%
  mutate(
    recovery.days.DO = (base.DO- Intercept) / slope
  )

DO.time.btwn<-time.btwn(DO_flagged)


flood.impacts.DO<-full_join(recession.lm,DO.time.btwn)%>%full_join(DO.compare, by=c('ID', 'flood'))






