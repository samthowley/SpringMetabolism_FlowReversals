file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  filter(Date> '2022-01-01', ID %in% c('GB', 'AM', 'LF', 'OS', 'ID'))


floods <- read_csv("01_Raw_data/flood.periods.csv")


flagged <- master %>%
  full_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    flood=if_else(ID=='LF' & Date>'2024-05-22', 16, flood),
    flood=if_else(ID=='OS' & Date>'2024-05-21', 12, flood),
  )



ggplot(flagged%>% filter(!is.na(flood), ID=='AM'), 
       aes(x = Date, y=depth)) +
  geom_point(aes(y=CO2/10^3), color='black', alpha=0.3)+
  geom_point(aes(y=DO), color='red', alpha=0.3)+
  
  geom_smooth(aes(y=CO2/10^3),method='loess', color='black')+
  geom_smooth(aes(y=DO),method='loess', color='red')+
  
  scale_y_continuous(
    name = "DO mg/L",
    sec.axis = sec_axis(~ . * 10^3, name = "CO2 ppm")) +

  theme_minimal()+
  facet_wrap(~flood, scales='free')


ggplot(flagged%>% filter(!is.na(flood), ID=='AM'), 
       aes(x = Date, y=depth)) +
  geom_point(aes(y=depth), color='blue', alpha=0.3)+
  geom_point(aes(y=SpC/100), color='purple', alpha=0.3)+

  geom_smooth(aes(y=depth),method='loess', color='blue')+
  geom_smooth(aes(y=SpC/100),method='loess', color='purple')+
  
  scale_y_continuous(
    name = "depth (m)",
    sec.axis = sec_axis(~ . * 100, name = "SpC")) +
  
  facet_wrap(~flood, scales='free')

