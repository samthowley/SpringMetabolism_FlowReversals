
CO2 <- read_csv("02_Clean_data/Chem/CO2.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")

co2<-full_join(CO2, h)


floods <- read_excel("01_Raw_data/flood periods.xlsx", 
                     sheet = "CO2")%>%
  mutate(
    start = ymd(start), 
    end   = ymd(end))%>%
  select(ID, start, end, flood.event)

CO2_flagged <- co2 %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)

ggplot(CO2_flagged%>% filter(!is.na(flood.event), ID=='OS'), 
       aes(x = Date, y=CO2)) +
  geom_point(aes(y=CO2), color='black')+
  geom_point(aes(y=depth*2000), color='pink')+
  geom_smooth(method='loess', color='black')+
  facet_wrap(~flood.event, scales='free')
ggplotly()


str(CO2_flagged)
