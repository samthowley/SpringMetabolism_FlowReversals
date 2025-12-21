library(tidyverse)
library(readxl)
library(dataRetrieval)
library(weathermetrics)


VentDO <- read_csv("01_Raw_data/VentDO.csv")%>%
mutate(Date=paste(Date, "00:00:00"),
       Date=mdy_hms(Date))%>%
  group_by(ID)%>%
  mutate(VentDO=mean(VentDO, na.rm=T),
         VentTemp=mean(VentTemp, na.rm=T))%>%
  distinct(ID, .keep_all = T)



startDate <- "2021-04-03"
endDate <- "2024-08-06"
parameterCd <- c('00300','00065','00010', '00060')
ventID<-'02322700'

IU<- readNWISuv(ventID, parameterCd, startDate, endDate)
IU<-IU %>% 
  rename('Date'='dateTime', 'VentDO'='X_00300_00000', 'VentTemp'='X_00010_00000')%>%
  mutate(min=minute(Date), ID='ID', day=Date) %>% 
  filter(min==0) %>%select(ID, Date, VentDO, VentTemp)

check<-rbind(VentDO, IU)


write_csv(rbind(VentDO, IU), "04_Outputs/VentDO.csv")


ggplot(IU, aes(log(Q)))+geom_histogram()

