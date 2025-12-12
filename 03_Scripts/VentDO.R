library(tidyverse)
library(readxl)
library(dataRetrieval)
library(weathermetrics)


rC_k600 <- read_excel("04_Outputs/rC_k600.xlsx", 
                      sheet = "Vent DO")%>%
  mutate(VentTemp=fahrenheit.to.celsius(VentTemp))
  


startDate <- "2021-04-03"
endDate <- "2024-08-06"
parameterCd <- c('00300','00065','00010', '00060')
ventID<-'02322700'

IU<- readNWISuv(ventID, parameterCd, startDate, endDate)
IU<-IU %>% 
  rename('Date'='dateTime', 'VentDO'='X_00300_00000', 'VentTemp'='X_00010_00000')%>%
  mutate(min=minute(Date), ID='ID') %>% 
  filter(min==0) %>%select(ID, Date, VentDO, VentTemp)

write_csv(rbind(rC_k600, IU), "02_Clean_data/Chem/VentDO.csv")



startDate <- "2024-09-06"
endDate <- "2025-09-06"
parameterCd <- c('00300','00065','00010', '00060')
ventID<-'02322700'

IU<- readNWISuv(ventID, parameterCd, startDate, endDate)
IU<-IU %>% 
  rename('Date'='dateTime', 'VentDO'='X_00300_00000', 'VentTemp'='X_00010_00000', 'Q'='X_00060_00000')%>%
  mutate(min=minute(Date), ID='ID') %>% 
  filter(min==0) %>%select(ID, Date, VentDO, VentTemp, Q)


library(MASS)
bc <- boxcox(lm(IU$Q ~ 1)) 

ggplot(IU, aes(log(Q)))+geom_histogram()

