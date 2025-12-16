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

#call in variables ###########
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area<-left_join(width, length)%>% mutate(area=w*m)


(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
file.names<-file.names[c(2,4,6,11)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%filter(!ID=='OS')%>%
  left_join(area)#%>%
  #left_join(lat.lon)

VentDO <- read_csv("04_Outputs/VentDO.csv")

master<-full_join(master, VentDO)%>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction= "downup")%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  distinct(ID, Date, .keep_all = T)

ggplot(master, aes(x = Date, y = DO)) +
  geom_line()+
  facet_wrap(~ID, scales='free')

#1. interpolate Q####
discharge<-master%>%
  mutate(discharge=w*depth*velocity*86400)

# discharge<-discharge%>%select(Date, ID, discharge)
# write_csv(discharge, "02_Clean_data/Chem/discharge.csv")

#2. Estimate change in total DO flux####
change.DO.flux<-discharge%>%
  mutate(
    change.DO.flux=((DO-VentDO)*discharge)/area)

#3. DO deficit from saturation####
DO.deficit<-change.DO.flux%>%  mutate(
  Vent.DO.sat=Cs(VentTemp),
  stat2.DO.sat=Cs(fahrenheit.to.celsius(Temp)),
  
  DO.deficit.from.sat=((Vent.DO.sat-VentDO)+(stat2.DO.sat-DO))/2,
)

#4. K rearation####
K.rearation<-DO.deficit%>%
  mutate(K.flux=K600_1.d_daily*depth*DO.deficit.from.sat)

#5. air-water gas exchange####
air.water.xchange<-K.rearation%>% 
  mutate(
    DO.Sat.fraction=1-((VentDO/Vent.DO.sat+DO/stat2.DO.sat)/2),
    air.water.xchange=K.flux*DO.Sat.fraction,
    not.air.water.xchange=change.DO.flux-air.water.xchange
    )

write.csv(air.water.xchange, file ="air.water.xchange.csv")
#check: estimating reach#####

active.reach <- air.water.xchange %>% 
  mutate(reach.km=( (velocity*86400) /K600_1.d_daily)/10^3,
         reach.test=if_else(reach.km>3*km, 'above', 'passes'),
         reach.test=if_else(reach.km<0.4*km, 'below', reach.test),
         reach.test=if_else(velocity<0, 'below', reach.test)
  )%>%
  filter(reach.test=='passes')%>%
  mutate(date = as_date(Date)) %>%  # extract calendar day
  group_by(date) %>%
  filter(n() >= 20) %>%                 # keep only days with ≥ 20 hours
  ungroup()%>%select(-date)

test<-active.reach %>% filter(Date> '2024-01-01', ID=='AM')

ggplot(active.reach, aes(x = Date, y = velocity)) +
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')

onestation <- read_csv("04_Outputs/one.station.metabolism.csv")

ggplot(onestation, aes(x = date, y = K600)) +
  geom_line()+
  facet_wrap(~ID, scales='free')+
  ggtitle("Gas Dome Experiments")

  
#parse day from night####

lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

 day.parse <- left_join(active.reach, lat.lon) %>%
   group_by(ID) %>%
   mutate(
     solar.time = convert_UTC_to_solartime(Date, lon = lon),
     light = calc_light(solar.time, latitude = first(lat), longitude = first(lon))) %>%
   ungroup()%>%
   mutate(time=case_when(light<=200~ 'night',light>200~ 'day'))%>%
   select(-lat, -lon)
 
#isolate ER####
 isolate<-day.parse%>% 
   mutate(day=as.Date(Date))%>%
   group_by(day,ID,time) %>%
   summarize(avg = mean(not.air.water.xchange, na.rm=T))
 
 
ER<-isolate%>%filter(time=='night')%>%rename(ER=avg)%>%select(-time)
GPP<-isolate%>%filter(time=='day')%>%rename(GPP=avg)%>%select(-time)
NEP<-left_join(GPP, ER)

ggplot(NEP, aes(x = day, y = GPP)) +
  geom_line(aes(y = GPP), color='green')+
  geom_line(aes(y = ER), color='red')+
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')
