rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(weathermetrics)
library(lme4)

#k600 calculation constants######
dome_length<-0.38
dome_width<-0.22
dome_height<-0.185
domeVol_m3<-0.015466
domeFoot_m2<-0.0836
domeVol_L<-15.466
domeFoot_L<-83.6
R<-0.08205
dome_length<-0.38
library(tools)

velocity <- read_csv("02_Clean_data/Chem/velocity.csv")
 
stream <- read_csv("02_Clean_data/master_depth2.csv")
stream<-left_join(stream, velocity)%>%
  fill(CO2, .direction = "up") %>%
  fill(depth, velocity, .direction = "updown")
GasDome <- function(gas,stream) {
  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))%>%
    select(CO2_enviro,Temp,depth, velocity, day, hour,month,yr,ID)

  gas<-gas %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  gas<-left_join(gas,stream, by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas<-gas%>%mutate(Temp_F=mean(Temp, na.rm=T))%>% mutate(Temp_C=fahrenheit.to.celsius(Temp_F))%>%
    mutate(Temp_K=Temp_C+273.15,SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
           SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)
  
  gas<-gas %>%
    mutate(pCO2_water=CO2_enviro/1000000, day=as.Date(Date),
           pCO2_air=max(gas$CO2, na.rm=T)/1000000, sec=second(Date))%>%mutate( 
    sec_cumulative=cumsum(sec))
  
  diffuse<-lm(CO2~ sec_cumulative, data = gas) #CO2 ppm/sec
  gas$slope<-coef(diffuse)[2]
  
  gas$deltaCO2_atm<- (abs(gas$slope)/1000000) #change in CO2 during float
  
  gas$n<-(gas$deltaCO2_atm*15.466)/0.085/gas$Temp_K #CO2 mol/sec
  

  gas$FCO2<-(gas$n/domeFoot_m2)*60*60 #mol/m^2/h
  gas$KH<-0.034*exp(2400*((1/gas$Temp_K)-(1/298.15)))
  gas$KH_1000<-gas$KH*1000 #mol/m^3/atm

  gas$KCO2_m.day<-gas$FCO2/gas$KH_1000/(gas$pCO2_air-gas$pCO2_water)#m/h
  gas$kO2_m.day<-gas$KCO2_m.day*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)#m/h
  gas$k600_m.day<- gas$KCO2_m.day*(600/gas$SchmidtCO2hi)^(-2/3) #m/h
  
  gas$KO2_1.day<-(gas$kO2_m.day/gas$depth)*24
  gas$KCO2_1.day<-(gas$KCO2_m.day/gas$depth)*24
  gas$k600_1.day<- (gas$k600_m.day/gas$depth)*24
  
  gas<-gas%>% select(day,ID,rep,CO2,CO2_enviro,depth,k600_1.day,velocity)


  return(gas)
}


#compile Gas Dome####

gasdome<-data.frame()
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  
  site<-site %>%
    mutate(ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
           ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
           ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
           ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
           ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)))
  
  gas<-GasDome(site, stream)
  gasdome<-rbind(gasdome,gas)
}

k600 <- gasdome%>%
  distinct(k600_1.day,ID,day, .keep_all = T)

k600<-k600 %>% rename(Date=day)%>%select(Date,depth,ID,k600_1.day)

split<-k600 %>% split(k600$ID)
write.xlsx(split, file = '04_Outputs/rC_k600_edited.xlsx')
