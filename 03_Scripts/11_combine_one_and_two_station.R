library(plotly)
library(weathermetrics)
library(tidyverse)
library(cowplot)

SpC <- read_csv("02_Clean_data/Chem/SpC.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID)%>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))


two_station <- read_csv("04_Outputs/two_station.csv")%>%
  mutate(
    Date = as.Date(Date),
    k_O2_perd = streamMetabolizer::convert_k600_to_kGAS(
      K600, temperature = fahrenheit.to.celsius(Temp), gas = "O2"),
    kTau = k_O2_perd * travel.time.hr / 24     # dimensionless
  )%>%
  group_by(ID, Date) %>%  
  dplyr::select(Date, ID, depth, DO, K600, discharge, GPP, ER, kTau)%>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
    rename(GPP.2=GPP, ER.2=ER, K600.2=K600)%>%
  left_join(SpC,by=c("Date", "ID"))


range(two_station$SpC, na.rm=T)


two_station.clean<-two_station%>%
  filter(SpC>200,
    kTau<=2.5,
    )%>%
  mutate(
     GPP.2=ifelse(ID=='ID' & kTau>1, NA, GPP.2),
    ER.2=ifelse(ER.2< -35, NA, ER.2)
  )


two_station.clean%>%
  mutate(
    GPP.2=ifelse(GPP.2<0, NA, GPP.2),
    ER.2=ifelse(ER.2>0, NA, ER.2),
  )%>%
  ggplot(aes(x=Date, color=kTau))+
  geom_point(aes(y=GPP.2))+
  geom_point(aes(y=ER.2))+
  scale_color_viridis_b()+
  #scale_x_log10()+
  facet_wrap(~ID, scales='free')

plot_grid(
  two_station.clean%>%
    ggplot(aes(x=depth, color=kTau))+
    geom_point(aes(y=GPP.2))+
    scale_color_viridis_b()+
    scale_x_log10()+
    facet_wrap(~ID, scales='free'),


  two_station.clean%>%
    ggplot(aes(x=depth, color=kTau))+
    geom_point(aes(y=ER.2))+
    scale_color_viridis_b()+
    scale_x_log10()+
    facet_wrap(~ID, scales='free')
)



one_station_metabolism <- read_csv("04_Outputs/one.station.metabolism.csv")

both.methods<-full_join(two_station.clean, one_station_metabolism, by=c("Date", "ID"))


met.coalesce<-both.methods%>%
  mutate(
    GPP.coalesce=coalesce(GPP.2, GPP.1),
    ER.coalesce=coalesce(ER.2, ER.1),,
    K600.coalesce=coalesce(K600.2, K600.1),
    
    GPP.coalesce=ifelse(ID=='IU', GPP.1, GPP.coalesce),
    ER.coalesce=ifelse(ID=='IU', ER.1, ER.coalesce),
    K600.coalesce=ifelse(ID=='IU', K600.1, K600.coalesce),
    
    
    GPP.coalesce=ifelse(GPP.2<0, NA, GPP.coalesce),
    ER.coalesce=ifelse(ER.2>0, NA, ER.coalesce),
    
  )%>%
  rename(K600=K600.coalesce, GPP=GPP.coalesce, ER=ER.coalesce)%>%
  dplyr::select(Date, ID, depth, DO, K600, discharge, GPP, ER)



plot_grid(
  met.coalesce%>%
  ggplot(aes(x=Date))+
  geom_point(aes(y=GPP.1), color='black')+
  geom_point(aes(y=GPP.2), color='gray')+
  geom_point(aes(y=GPP.coalesce), color='red', shape=1)+
  #scale_x_log10()+
  facet_wrap(~ID, scales='free')+
  theme_bw(),


  met.coalesce%>%
  ggplot(aes(x=Date))+
  geom_point(aes(y=ER.1), color='black')+
  geom_point(aes(y=ER.2), color='gray')+
  geom_point(aes(y=ER.coalesce), color='red', shape=1)+
  #scale_x_log10()+
  facet_wrap(~ID, scales='free')+
  theme_bw()
)



plot_grid(
  met.coalesce%>%
    ggplot(aes(x=Date))+
    geom_point(aes(y=GPP), color='red', shape=1)+
    #scale_x_log10()+
    facet_wrap(~ID, scales='free')+
    theme_bw(),
  
  
  met.coalesce%>%
    ggplot(aes(x=Date))+
    geom_point(aes(y=ER), color='red', shape=1)+
    #scale_x_log10()+
    facet_wrap(~ID, scales='free')+
    theme_bw()
)


write_csv(met.coalesce, "04_Outputs/combined metabolism methods.csv")




