
DO <- read_csv("02_Clean_data/Chem/DO.csv")
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
    DO=if_else(ID=='AM' & DO> 7.84, NA, DO)
  )

#CUT OFF RECOVERY AT THE MIN
ggplot(DO_flagged%>% filter(!is.na(flood), ID=='GB'), 
       aes(x = Date, y=DO)) +
  geom_point(aes(y=DO), color='black')+
  geom_point(aes(y=depth*5), color='pink')+
  geom_smooth(method='loess', color='black')+
  facet_wrap(~flood, scales='free')

ggplotly()

