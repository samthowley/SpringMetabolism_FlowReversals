library(plotly)


######
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
  mutate(GPP1=if_else(GPP1<0, 0, GPP1),
         model="1")%>%
  #select(-ER_Rhat, -K600_daily_Rhat,-stage)%>%
  arrange(ID, Date)%>%
  drop_na()


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
  )%>%mutate(model='2')



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


  all.met <- onestation %>%
    full_join(two, by = c("Date", "ID"), suffix = c("_onestation", "_two")) %>%
    mutate(
      # Prioritize "two" over "onestation"
      GPP = coalesce(GPP2, GPP1),
      ER = coalesce(ER2, ER1),
      
      # Track which dataset was used
      source = case_when(
        !is.na(GPP2) ~ "two",
        !is.na(GPP1) ~ "one", 
        TRUE ~ "neither"
      )
    ) %>%
    arrange(ID, Date)%>%
    left_join(depth)


  all.met%>%
    ggplot(aes(x = depth, color=source)) +
    geom_point(aes(y = GPP, shape='GPP'),) +
    geom_point(aes(y = ER, shape='ER'), shape=1) +
    ggtitle('Two Station: Linear RC')+
    facet_wrap(~ID, scales = "free") +
    theme_minimal()
  
  #######
   write_csv(all.met, "04_Outputs/master.metabolism.csv")

plot_grid(
all.met%>%
  ggplot(aes(x = depth)) +
    geom_point(aes(y = GPP1, color='GPP1')) +
    geom_point(aes(y = ER1, color='ER1'), shape=1) +
  ggtitle('One Station')+
    facet_wrap(~ID, scales = "free") +
    theme_minimal()
,

all.met%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP2, color='GPP2'),) +
  geom_point(aes(y = ER2, color='ER2'), shape=1) +
  geom_smooth(aes(y = GPP2), method = 'loess', se=F)+
  geom_smooth(aes(y = ER2), method = 'loess', se=F)+
  ggtitle('Two Station')+
  facet_wrap(~ID, scales = "free") +
  theme_minimal(),
nrow=1
)







