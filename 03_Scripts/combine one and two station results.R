library(plotly)

title<-"Subset Data"
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
    ggtitle(title)+
    facet_wrap(~ID, scales = "free") +
    theme_minimal()
  
  
#######
depth <- read_csv("02_Clean_data/Chem/depth.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(across(c(depth), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
two <- read_csv("04_Outputs/two.station.results.csv") %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, GPP, ER, K600_1.d_daily, ID) %>%
    rename(K600 = K600_1.d_daily) %>%
    group_by(ID, Date) %>%
    summarise(across(c(ER, GPP, K600), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(model = '2') %>%
    left_join(depth)
  
max.depths <- two %>%
    group_by(ID) %>%
    summarise(max.depth = max(depth, na.rm = TRUE))
  
  file.names <- list.files(path = "04_Outputs/one station results", pattern = ".csv", full.names = TRUE)
  onestation.df <- data.frame()
  for (fil in file.names) {
    df <- read_csv(fil)
    onestation.df <- rbind(onestation.df, df)
  }
  
onestation <- onestation.df %>%
    rename(GPP = GPP_daily_mean, ER = ER_daily_mean, K600 = K600_daily_mean, Date = date) %>%
    separate(ID, into = c('ID', 'stage'), sep = '_') %>%
    mutate(GPP = if_else(GPP < 0, 0, GPP), model = "1") %>%
    arrange(ID, Date) %>%
    left_join(depth) %>%
    left_join(max.depths) %>%
    filter(depth > max.depth) %>%
    select(names(two))
  
rbind(two, onestation) %>%
    arrange(ID, Date) %>%
    ggplot(aes(x = depth)) +
    geom_point(aes(y = GPP, shape = 'GPP')) +
    geom_point(aes(y = ER), shape = 1) +
    ggtitle(title) +
    facet_wrap(~ID, scales = "free") +
    theme_minimal()



write_csv(rbind(two, onestation), "04_Outputs/master.metabolism.csv")



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


