one <- read_csv("04_Outputs/one.station.metabolism.csv")%>%
  rename(Date=date,
         GPP1=GPP,
         ER1=ER)%>%
  mutate(method="one")

two<- read_csv("04_Outputs/two.station.results.csv")%>%
  select(Date, GPP, ER, K600_1.d_daily, ID)%>%
  rename(K600=K600_1.d_daily,
         GPP2=GPP,
         ER2=ER)%>%
  mutate(method="two")

all.met<-full_join(one, two)


ggplot(all.met, aes(x = Date)) +
  geom_point(aes(y = GPP1), color='gray')+
  geom_point(aes(y = GPP2), shape=1)+
  facet_wrap(~ID, scales='free')







(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
file.names<-file.names[c(2, 3, 1, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID)%>%
  summarise(
    depth=mean(depth, na.rm=T),
    discharge=mean(discharge, na.rm = T),
    CO2=mean(CO2, na.rm = T),
    DO=mean(DO, na.rm = T),
    Temp=mean(Temp, na.rm = T),
    pH=min(pH, na.rm = T),
    SpC=min(SpC, na.rm = T)
    
  )

all<-left_join(all.met, master)%>% distinct(Date, ID, .keep_all = T)%>%arrange(ID, Date)

ggplot(all, aes(x = Date, color=method)) +
  geom_point(aes(y = GPP))+
  geom_point(aes(y = ER))+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')

write_csv(all, "04_Outputs/master.metabolism.csv")
