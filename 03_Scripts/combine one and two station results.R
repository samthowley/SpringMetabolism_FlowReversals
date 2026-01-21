file.names <- list.files(path="04_Outputs/one station results", pattern=".csv", full.names=TRUE)
onestation.df <- data.frame()
for(fil in file.names){
  df <- read_csv(fil)
  onestation.df <- rbind(onestation.df, df)}

onestation<-onestation.df%>%
  rename(GPP1=GPP_daily_mean,
         ER1=ER_daily_mean,
         K6001=K600_daily_mean,
         Date=date)%>%
  separate(ID,into = c('ID', 'stage'),sep='_')%>%
  select(-ER_Rhat, -K600_daily_Rhat,-stage)%>%
  arrange(ID, Date)
unique(onestation$ID)

two<- read_csv("04_Outputs/two.station.results.csv")%>%
  select(Date, GPP, ER, K600_1.d_daily, ID)%>%
  rename(K600=K600_1.d_daily,
         GPP2=GPP,
         ER2=ER)%>%
  mutate(method="two")

all.met<-full_join(onestation, two)%>%
  mutate(
         GPP2=if_else(is.na(GPP2), GPP1, GPP2),
         ER2=if_else(is.na(ER2), GPP1, ER2),
         GPP=(GPP1+GPP2)/2,
         ER=(ER1+ER2)/2,
         GPP=if_else(GPP<0, 0, GPP)
         )


ggplot(all.met, aes(x = Date)) +
  geom_point(aes(y = GPP1), color='gray')+
  geom_point(aes(y = GPP2), shape=1)+
  #geom_point(aes(y = GPP), shape=1, color='red')+
  facet_wrap(~ID, scales='free')+
  theme_minimal()

ggplot(all.met, aes(x = Date)) +
  geom_point(aes(y = GPP), shape=1)+
  facet_wrap(~ID, scales='free')+
  theme_minimal()


write_csv(all.met, "04_Outputs/master.metabolism.csv")













