library(tidyverse)
library(readxl)
library(openxlsx)
library(weathermetrics)
library(lme4)

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


(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
file.names<-file.names[c(1,4,2)]

data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

stream <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  fill(CO2, .direction = "updown") %>%
  rename("CO2_enviro"='CO2')%>% 
  mutate(day=as.Date(Date), hour=hour(Date))%>%
  select(CO2_enviro,Temp,depth,ID, day, hour)%>%
  group_by(ID, day)%>%
  mutate(Temp=if_else(is.na(Temp)| is.nan(Temp), mean(Temp, na.rm=T), Temp))

  
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
  
  gasdome<-rbind(gasdome,site)
}

gas.reps<-gasdome%>%mutate(day=as.Date(Date), hour=hour(Date))


gas.stream<-left_join(gas.reps,stream, by=c('ID', 'day', 'hour'), relationship = "many-to-many")%>%
  distinct(Date, ID, rep, .keep_all = T)
  
gas<-gas.stream%>%
  group_by(ID, day)%>%
  mutate(Temp_C=fahrenheit.to.celsius(mean(Temp, na.rm=T)))%>%
  mutate(Temp_K=Temp_C+273.15,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3)
  

gas<-gas %>%
  group_by(ID, day, rep)%>%
    mutate(pCO2_water=CO2_enviro/1000000,
           pCO2_air=max(CO2, na.rm=T)/1000000, 
           sec=second(Date),
           sec_cumulative = as.numeric(difftime(Date, first(Date), units = "secs")))



diffuse <- gas %>%
  group_by(ID, day,rep) %>%
  summarise(
    slope = lm(CO2 ~ sec_cumulative)$coefficients[2],  # slope (ppm/sec)
    .groups = "drop"
  )



gas.slope<-left_join(diffuse,gas, by=c('ID', 'day', 'rep'))%>%
  mutate(
    deltaCO2_atm=abs(slope)/1000000, #change in CO2 during float
    n=(deltaCO2_atm*15.466)/0.085/Temp_K,
    FCO2=(n/domeFoot_m2)*60*60, #mol/m^2/h
    KH=0.034*exp(2400*((1/ Temp_K)-(1/298.15))),
    KH_1000=KH*1000,#mol/m^3/atm

    KCO2_m.day= FCO2/ KH_1000/(pCO2_air- pCO2_water),#m/h
    kO2_m.day= KCO2_m.day*( SchmidtCO2hi/ SchmidtO2hi)^(-2/3),#m/h
    k600_m.day=  KCO2_m.day*(600/ SchmidtCO2hi)^(-2/3), #m/h

    KO2_1.day=(kO2_m.day/ depth)*24,
    KCO2_1.day= (KCO2_m.day/depth)*24,
    k600_1.day=(k600_m.day/depth)*24)%>% 
  select(day,ID,rep,Temp_C,CO2,CO2_enviro,depth,k600_1.day,KCO2_m.day)%>% 
  rename(Date=day)%>%
  distinct(ID, k600_1.day, .keep_all = T)


# gas.slope%>%
#   drop_na(depth)%>%
#   filter(ID=='GB')%>%
# ggplot(aes(x = depth)) +
#   geom_point(aes(y = k600_1.day))+
#   facet_wrap(~ID, scales='free')


split<-gas.slope %>% split(gas.slope$ID)
write.xlsx(split, file = '04_Outputs/rC_k600.xlsx')

