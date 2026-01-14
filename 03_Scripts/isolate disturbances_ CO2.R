
CO2 <- read_csv("02_Clean_data/Chem/CO2.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")

co2<-full_join(CO2, h)
floods <- read_csv("01_Raw_data/flood.periods.csv")

CO2_flagged <- co2 %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)

ggplot(CO2_flagged%>% filter(!is.na(flood), ID=='LF'), 
       aes(x = Date, y=CO2)) +
  geom_point(aes(y=CO2), color='black')+
  geom_point(aes(y=depth*2000), color='pink')+
  geom_smooth(method='loess', color='red')+
  facet_wrap(~flood, scales='free')
ggplotly()


str(CO2_flagged)
