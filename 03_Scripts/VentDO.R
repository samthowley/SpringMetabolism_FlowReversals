library(tidyverse)
library(readxl)
library(dataRetrieval)
library(weathermetrics)


VentDO <- read_csv("01_Raw_data/VentDO.csv")%>%
  mutate(Date = mdy_hms(paste0(Date, " 00:00:00"))) 


startDate <- "2021-04-03"
endDate <- "2024-08-06"
parameterCd <- c('00300','00065','00010', '00060')
ventID<-'02322700'

IU<- readNWISuv(ventID, parameterCd, startDate, endDate)
IU.edit<-IU %>% 
  rename('Date'='dateTime', 'VentDO'='X_00300_00000', 'VentTemp'='X_00010_00000')%>%
  mutate(
    ## readNWISuv returns UTC; our loggers are fixed EST (UTC-5, no DST).
    ## Put NWIS on the logger clock (stored with a "UTC" label like every other file).
    ## Without this, ID's vent series ran 5 h ahead of the stream DO sensor.
    Date = force_tz(with_tz(Date, "Etc/GMT+5"), "UTC"),
    min=minute(Date), 
    ID='ID', 
    ) %>% 
  filter(min==0)%>%
  select(names(VentDO))%>%
  drop_na()

rbind(VentDO, IU.edit)%>%
  filter(ID=='AM')%>%
  ggplot(aes(x=Date, y=VentDO))+geom_point()

write_csv(rbind(VentDO, IU.edit), "04_Outputs/VentDO.csv")



