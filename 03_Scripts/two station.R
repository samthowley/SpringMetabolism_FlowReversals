rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(grid)
library(weathermetrics)
library('StreamMetabolism')
library("hydroTSM")
library(dataRetrieval)
library(tidyverse)
library(cowplot)
library(streamMetabolizer)
library(writexl)


width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")%>%select(-km)%>%rename(ID=site)
area<-left_join(width, length)%>% mutate(area=w*m)%>%select(-w)

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

velocity <- read_csv("02_Clean_data/Chem/velocity.csv")
discharge <- read_csv("02_Clean_data/Chem/discharge.csv")
K600 <- read_csv("02_Clean_data/Chem/K600.csv")
DO <- read_csv("02_Clean_data/master_depth2.csv") %>%select(Date, ID, DO, depth, Temp)
VentDO <- read_csv("02_Clean_data/Chem/VentDO.csv")%>%mutate(Date=as.Date(Date))

df_list <- list(velocity, discharge, K600, DO, VentDO)
vars <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%filter(!ID=='OS')%>%
  left_join(area)%>%
  left_join(lat.lon)

DO.saturation<-vars%>%
  fill(VentDO, VentTemp, .direction = c('downup'))%>%
  mutate(
    Vent.DO.sat=Cs(VentTemp),
    stat2.DO.sat=Cs(fahrenheit.to.celsius(Temp)),
    Vent.DO.deficit.perc= VentDO/Vent.DO.sat,
    stat2.DO.deficit.perc=DO/stat2.DO.sat,
    DO.deficit.perc=((Vent.DO.deficit.perc+stat2.DO.deficit.perc)/2)*100,
    DO.deficit=((Vent.DO.sat-VentDO)+(stat2.DO.sat-DO))/2
    )%>%select(-Vent.DO.deficit.perc, -stat2.DO.deficit.perc)

change.DO.flux<-DO.saturation%>%
  mutate(
    change.DO.flux=((DO-VentDO)*discharge*86400)/area
  )

K.rearation<-change.DO.flux%>%
  mutate(K.flux=k600_1d*depth*DO.deficit)

air.water.xchange<-K.rearation%>% 
  mutate(
    air.water.xchange=K.flux*(DO.deficit.perc/100),
    not.air.water.xchange=change.DO.flux-air.water.xchange
    )%>%
  filter(!is.na(depth))%>%
  distinct(ID, Date, .keep_all = T)

reichert <- air.water.xchange %>% 
  mutate(
    small = 0.4 * (velocity * 86400) / k600_1d,
    large = (velocity * 86400) / k600_1d,
    QC = case_when(
      m < small ~ "toss",
      m > large ~ "toss",
      TRUE ~ "keep"
    )
  )
 

 ggplot(reichert, aes(x = Date, y = not.air.water.xchange,color=QC)) +
  geom_line()+
  facet_wrap(~ID, scales='free')
  

write.csv(air.water.xchange, file ="air.water.xchange.csv")
