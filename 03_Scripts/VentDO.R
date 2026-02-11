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



