rm(list=ls())

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

#call in variables ###########
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area<-left_join(width, length)%>% mutate(area=w*m)


(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
file.names<-file.names[c(2,4,6,11)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  left_join(area)#%>%
  #left_join(lat.lon)

VentDO <- read_csv("04_Outputs/VentDO.csv")

master<-full_join(master, VentDO)%>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction= "downup")%>%
  filter(!ID %in% c('OS', 'IU'))%>%
  distinct(ID, Date, .keep_all = T)

ggplot(master%>%filter(ID=='AM'), aes(x = Date, y = DO)) +
  geom_line()+
  facet_wrap(~ID, scales='free')

#1. interpolate Q####
discharge<-master%>%
  mutate(discharge=w*depth*velocity*86400)

prepped.for.one<-discharge%>% select(ID, Date, DO, discharge, depth, Temp)
write_csv(prepped.for.one, "01_Raw_data/prepped.for.one.station.csv")

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
  mutate(K.flux=(K600_1.d_daily)*depth*DO.deficit.from.sat)

#5. air-water gas exchange####
air.water.xchange<-K.rearation%>% 
  mutate(
    not.air.water.xchange=change.DO.flux-K.flux
    )
#5.5 filter: estimating reach#####

active.reach <- air.water.xchange %>% 
  mutate(reach.km=( (velocity*86400) /K600_1.d_daily)/10^3,
         reach.test=if_else(reach.km>3*km, 'above', 'passes'),
         reach.test=if_else(reach.km<0.4*km, 'below', reach.test),
         reach.test=if_else(velocity<0, 'below', reach.test)
  )%>%
  #filter(reach.test=='passes')%>%
  mutate(date = as_date(Date)) %>%  # extract calendar day
  group_by(date) %>%
  filter(n() >= 20) %>%                 # keep only days with ≥ 20 hours
  ungroup()%>%select(-date)

# ggplot(active.reach%>%filter(ID=='AM'), aes(x = Date, y = depth, color=reach.test)) +
#   geom_line()+
#   geom_hline(yintercept = 0)+
#   facet_wrap(~ID, scales='free')


#6.parse day from night####
lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

 day.parse <- left_join(active.reach, lat.lon) %>%
   ungroup()%>%
   mutate(time=case_when(not.air.water.xchange>0~ 'day',
                         not.air.water.xchange<0~ 'night'))%>%
   select(-lat, -lon)%>%
  filter(time!='remove') %>%
   mutate(date = as_date(Date)) %>%
   group_by(date) %>%
   filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>%
   ungroup()
 
#7. isolate ER####
 isolate<-day.parse%>% 
   group_by(date,ID,time) %>%
   summarize(avg = mean(not.air.water.xchange, na.rm=T))
 
 
ER<-isolate%>%filter(time=='night')%>%rename(ER=avg)%>%select(-time)
GPP<-isolate%>%filter(time=='day')%>%rename(GPP=avg)%>%select(-time)
NEP<-left_join(GPP, ER)

all.the.data<-left_join(day.parse, NEP)
#8. Create datasets####

for.two <- all.the.data %>%
  filter(
    reach.test == "passes" | is.na(reach.test),
    )%>%
  mutate(Date=if_else(ID=='GB' & GPP>30, NA, Date),
         Date=if_else(ID=='GB' & Date<'2022-12-25', NA, Date))


#%>%
  #filter(GPP<40, ER>-40)

does.not.pass<-all.the.data%>% filter(reach.test != "passes")
too.large<-all.the.data%>%filter(ER < -37 | GPP > 37)
for.one<-rbind(does.not.pass, too.large)%>%distinct(ID, Date, .keep_all = T)

ggplot(for.two, aes(x = Date)) +
  geom_point(aes(y = GPP), color='darkgreen')+
  geom_point(aes(y = ER), color='red')+
  #geom_point(aes(y = depth*10), color='black')+
  
  geom_hline(yintercept = 0)+
  facet_wrap(~ID, scales='free')

ggplot(for.two, aes(x = Date)) +
  geom_point(aes(y = DO, colour = "Station two DO"))+
  geom_point(aes(y = VentDO, color='Vent DO'))+
  
  facet_wrap(~ID, scales='free')

#write_csv(all.the.data%>%filter(ID=='AM'), "check.csv")
write_csv(for.two, "04_Outputs/two.station.results.csv")
#prepare data for one station######

prepped.for.one<-for.one%>% select(ID, Date, DO, discharge, depth, Temp)

write_csv(prepped.for.one, "01_Raw_data/prepped.for.one.station.csv")
