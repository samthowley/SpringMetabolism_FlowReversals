#call in data########
library(weathermetrics)
#might be worth seeing if GB has anymore DO data
source("03_Scripts/disturbance isolation functions.R")

master <- read_csv("02_Clean_data/master_chem1.csv")%>%
  mutate(
    min=minute(Date),
    depth=if_else(ID=='GB' & Date>'2024-07-01', NA, depth)
    )%>%
  filter(
    Date> '2022-01-01')


chem <- master %>%
  arrange(Date) %>%
  filter(!is.na(depth))%>%
  mutate(
    day    = as.Date(Date)
  ) %>%
  group_by(ID)%>%
  mutate(
    t_num = as.numeric(Date),
    depth_smooth = predict(loess(depth ~ t_num, span = 0.1)))


slopes <- chem %>%
  filter(!is.na(depth_smooth), !is.na(Date))%>%
  arrange(ID, Date) %>%
  group_by(ID)%>%
  mutate(
    norm=depth_smooth/mean(depth_smooth, na.rm=T),
    date      = as.Date(Date),
    # 3-day index starting at the first date in your data
    day_index = as.integer(date - min(date)),
    block3    = day_index %/% 3) %>%ungroup()%>%
  group_by(block3, ID) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    slope      = {
      x <- as.numeric(Date)      # seconds since origin
      y <- norm
      coef(lm(y ~ x))[2] * 86400     # convert to units per day
    },
    .groups = "drop")


isolate <- chem %>%
  mutate(day=as.Date(Date))%>%
  left_join(
    slopes, by = join_by(ID, between(day, start_date, end_date))
  ) %>%
  mutate(
    abs.slope=abs(slope),
    )%>%
  select(-start_date, -end_date, -day)%>%
  arrange(ID, Date)%>%filter(abs.slope>0.01)#%>%filter(abs.slope>0.01)


iso_flagged <- isolate %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(
    slope_pos = slope > 0,
    run_id    = consecutive_id(slope_pos)
  ) %>%
  ungroup()


# summarize each run
runs <- iso_flagged %>%
  group_by(ID, run_id, slope_pos) %>%
  summarise(
    run_start = min(Date),
    run_end   = max(Date),
    .groups = "drop"
  ) %>%
  arrange(ID, run_start) %>%
  group_by(ID) %>%
  mutate(
    next_pos   = lead(slope_pos),
    next_end   = lead(run_end),
    next_runid = lead(run_id)
  ) %>%
  ungroup()


# define flood periods: positive run followed immediately by negative run
flood.periods <- runs %>%
  filter(slope_pos == TRUE, next_pos == FALSE) %>%
  group_by(ID) %>%
  mutate(flood = row_number()) %>%   # flood event number per ID
  ungroup() %>%
  transmute(
    ID,
    flood,
    start = run_start,
    end   = next_end
  )

#check#######

depth_flagged <- master %>%
  left_join(
    flood.periods, by = join_by(ID, between(Date, start, end))
  ) 

depth_flagged%>%
  ggplot(aes(x=Date, y=depth, color=as.factor(flood)))+
  geom_line()+
  facet_wrap(~ID)
  

write_csv(flood.periods, "01_Raw_data/flood.periods.csv")

