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

#check#######

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

all.met<-
full_join(onestation, two)%>%
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
    )#%>%
  # filter(
  #   #ID=='LF',
  #   depth<1,
  #   )%>%
  # ggplot(aes(x = depth)) +
  # geom_point(aes(y = GPP2, color='GPP2'),) +
  # geom_point(aes(y = GPP1, color='GPP1'), shape=1) +
  # geom_hline(yintercept = 15)+
  # ggtitle('GPP')+
  # facet_wrap(~ID, scales = "free") +
  # theme_minimal()


##########
write_csv(all.met, "04_Outputs/master.metabolism.csv")

all.met%>%
  filter(depth<1)%>%
  ggplot(aes(x = depth)) +
    geom_point(aes(y = GPP2, color='GPP2'),) +
    geom_point(aes(y = GPP1, color='GPP1'), shape=1) +
  ggtitle('GPP')+
    facet_wrap(~ID, scales = "free") +
    theme_minimal()



all.met%>%
  ggplot(aes(x = depth)) +
  
  geom_point(aes(y = ER2, color='ER2'), color='black') +
  geom_point(aes(y = ER1, color='ER1'), color='gray') +
  geom_point(aes(y = ER, color='ER'), color='darkred', shape=1) +
  
  ggtitle('NEP')+
  facet_wrap(~ID, scales = "free") +
  theme_minimal()






all.met%>%
  ggplot(aes(x = depth)) +
  
  geom_point(aes(y = GPP2, color='GPP2'), color='black') +
  geom_point(aes(y = GPP1, color='GPP1'), color='gray') +
  geom_point(aes(y = GPP, color='GPP'), color='darkgreen') +
  
  ggtitle('NEP')+
  facet_wrap(~ID, scales = "free") +
  theme_minimal()




