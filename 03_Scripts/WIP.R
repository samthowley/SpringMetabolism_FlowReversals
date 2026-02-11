
slopes <- DO.count %>%
  filter(!is.na(DO_loess), !is.na(Date))%>%
  arrange(ID, Date) %>%
  group_by(ID)%>%
  mutate(
    norm=DO_loess, #/mean(depth_smooth, na.rm=T),
    date      = as.Date(Date),
    # 3-day index starting at the first date in your data
    day_index = as.integer(date - min(date)),
    block3    = day_index %/% 4) %>%ungroup()%>%
  group_by(block3, ID) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    slope      = {
      x <- as.numeric(Date)      # seconds since origin
      y <- norm
      coef(lm(y ~ x))[2] * 86400     # convert to units per day
    },
    .groups = "drop"
  )%>% group_by(ID) %>%
  ungroup()


remove<-DO.count %>% 
  mutate(day=as.Date(Date))%>%
  left_join(
    slopes, by = join_by(ID, between(day, start_date, end_date))
  ) %>%
  group_by(ID, flood)%>%
  
  mutate(
    abs.slope=abs(slope),
    remove=case_when(
      count<0 & slope>0 ~'remove',
      count>=0 & slope<0 ~'remove'      
      ),
    stage=case_when(
      count<0  ~'pre',
      count>=0 ~'post'      
    )
    )%>%
  select(-start_date, -end_date, -day)%>%
  arrange(ID, Date)%>%
  filter(is.na(remove))




remove%>%
  mutate(
    date= as.Date(Date)
  )%>%
  filter(
    ID=='AM',
    !is.na(flood), 
    flood==7
    )%>%
  ggplot(aes(x = count, y = DO, group=stage)) +
  geom_point()+
  geom_point(aes(y=DO_loess, color=date))+
  geom_smooth(method='lm')+
  #scale_color_viridis_c(name = "slopes") +
    theme_minimal()+
  facet_wrap(~flood)


