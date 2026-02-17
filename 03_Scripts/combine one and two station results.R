library(plotly)

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
  mutate(GPP1=if_else(GPP1<0, 0, GPP1))%>%
  select(-ER_Rhat, -K600_daily_Rhat,-stage)%>%
  arrange(ID, Date)%>%
  drop_na()

range(onestation$GPP1)


two<- read_csv("04_Outputs/two.station.results.csv")%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, GPP, ER, K600_1.d_daily, ID)%>%
  rename(K600=K600_1.d_daily,
         GPP2=GPP,
         ER2=ER)%>%
  group_by(ID, Date) %>%
  summarise(
    across(
      c(ER2, GPP2, K600),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

range(two$GPP2)




read_csv("04_Outputs/two.station.results.csv")%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, GPP, ER, K600_1.d_daily, depth, ID)%>%
  group_by(ID, Date)%>%
  summarise(
    across(
      c(GPP, ER, K600_1.d_daily, depth),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )%>%
ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP, color=K600_1.d_daily), shape=1) +
  geom_hline(yintercept = 15)+
  facet_wrap(~ID, scales = "free") +
  scale_color_viridis_c(name = "K600") +
  theme_minimal()

depth <- read_csv("02_Clean_data/Chem/depth.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(
    across(
      c(depth),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

all.met<-full_join(onestation, two)%>%
  left_join(depth)%>%
  mutate(
    GPP = rowMeans(
      select(., GPP1, GPP2),
      na.rm = TRUE))%>%
  mutate(
    ER = rowMeans(
      select(., ER1, ER2),na.rm = TRUE),
    GPP=if_else(ID=='GB'& Date>='2022-09-09' & Date<='2022-10-08' & GPP< 1, NA, GPP),
    GPP=if_else(ID=='LF'& Date>='2023-11-30' & Date<='2023-12-05' & GPP< 1, NA, GPP),
    GPP=if_else(ID=='OS'& Date=='2024-03-19' & GPP< 1, NA, GPP),
    
    )


write_csv(all.met, "04_Outputs/master.metabolism.csv")

#p1<-
  ggplot(all.met, aes(x = Date)) +
  geom_point(aes(y = GPP1, color='GPP1')) +
  geom_point(aes(y = GPP2, color='GPP2')) +
    geom_point(aes(y = GPP, color='GPP')) +
  geom_hline(yintercept = 15)+
  facet_wrap(~ID, scales = "free") +
  #scale_color_viridis_c(name = "K600") +
  theme_minimal()


p2<-ggplot(all.met, aes(x = depth.daily)) +
  geom_point(aes(y = GPP1,color='GPP1'),)+
  geom_point(aes(y = GPP2,color=), shape=1)+
  #geom_point(aes(y = GPP), shape=1, color='red')+
  facet_wrap(~ID, scales='free')+
  theme_minimal()


p3<-ggplot(all.met, aes(x = Date)) +
  #geom_point(aes(y = GPP1,color='GPP1'),)+
  #geom_point(aes(y = GPP2,color='GPP2'), shape=1)+
  geom_point(aes(y = GPP), shape=1, color='red')+
  facet_wrap(~ID, scales='free')+
  ggtitle('GPP Averaged')+
  theme_minimal()

p4<-ggplot(all.met, aes(x = depth.daily)) +
  #geom_point(aes(y = GPP1,color='GPP1'),)+
  #geom_point(aes(y = GPP2,color='GPP2'), shape=1)+
  geom_point(aes(y = GPP), shape=1, color='red')+
  facet_wrap(~ID, scales='free')+
  ggtitle('GPP Averaged')+
  theme_minimal()

plot_grid(p3, p4, ncol=1)
library(cowplot)

ggplot(all.met, aes(x = Date)) +
  #geom_point(aes(y = GPP1,color='GPP1'),)+
  #geom_point(aes(y = GPP2,color='GPP2'), shape=1)+
  geom_point(aes(y = GPP), shape=1, color='red')+
  facet_wrap(~ID, scales='free')+
  ggtitle('GPP Averaged')+
  theme_minimal()



ggplot(all.met, aes(x = Date)) +
  geom_point(aes(y = GPP), shape=1)+
  facet_wrap(~ID, scales='free')+
  theme_minimal()















