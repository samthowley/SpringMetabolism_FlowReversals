met <- read_csv("04_Outputs/one.station.metabolism.csv")%>%
  mutate(
    Date=paste0(date, " ", "00:00:00"),
    Date=ymd_hms(Date)
  )%>%
  select(-date)
h <- read_csv("02_Clean_data/Chem/depth.csv")
DO <- read_csv("02_Clean_data/Chem/DO.csv")


met<-full_join(h, met)%>% full_join(DO)

#GPP##############
floods <- read_csv("01_Raw_data/flood.periods.csv")

gpp_flagged <- met %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)

ggplot(gpp_flagged%>% filter(!is.na(flood), ID=='LF'), 
       aes(x = Date, y=GPP)) +
  geom_point(aes(y=GPP), color='black')+
  geom_point(aes(y=depth*7), color='pink')+
  geom_point(aes(y=DO), color='lightgreen', shape=1)+
  geom_smooth(method='loess', color='black')+
  #geom_smooth(aes(y=DO), method='loess', color='lightgreen')+
  facet_wrap(~flood, scales='free')

#ER###########
floods <- read_excel("01_Raw_data/flood periods.xlsx", 
                     sheet = "stage")%>%
  mutate(
    start = ymd(start), 
    end   = ymd(end))%>%
  select(ID, start, end, flood.event)

gpp_flagged <- met %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)

ggplot(gpp_flagged%>% filter(!is.na(flood.event), ID=='ID'), 
       aes(x = Date, y=ER)) +
  geom_point(aes(y=ER), color='black')+
  geom_point(aes(y=depth*7), color='pink')+
  geom_point(aes(y=DO), color='lightgreen', shape=1)+
  geom_smooth(method='loess', color='black')+
  #geom_smooth(aes(y=DO), method='loess', color='lightgreen')+
  facet_wrap(~flood.event, scales='free')

