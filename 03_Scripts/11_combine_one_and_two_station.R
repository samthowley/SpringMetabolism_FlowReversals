library(plotly)

two_station <- read_csv("04_Outputs/two_station.csv")%>%
  select(Date, ID, depth, DO, K600, discharge, GPP, ER)%>%
  rename(GPP.2=GPP, ER.2=ER, K600.2=K600)%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

one_station_metabolism <- read_csv("04_Outputs/one.station.metabolism.csv")

both.methods<-left_join(two_station, one_station_metabolism, by=c("Date", "ID"))




both.methods%>%
  ggplot(aes(x=discharge))+
  geom_point(aes(y=GPP.1), color='black')+
  geom_point(aes(y=GPP.2), color='gray')+
  scale_x_log10()+
  facet_wrap(~ID, scales='free')

#K600 with depth########


SpC <- read_csv("02_Clean_data/Chem/SpC.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID)%>%
  summarise(
    SpC=mean(SpC, na.rm=T)
  )


K600<-onestation.df%>%
  separate(ID,into = c('ID', 'stage'),sep='_')%>%
  select(date, ID, K600_daily_mean)%>%
  rename(Date=date)%>%
  left_join(depth)%>%
  left_join(SpC)%>%
  filter (!ID %in% c('IU'))


K600%>%
  ggplot(aes(x=depth, y=K600_daily_mean, color=SpC))+
  geom_point()+
  scale_color_viridis_b()+
  facet_wrap(~ID, scales='free')+
  ggtitle("One Station K600")


