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


stream <- read_csv("02_Clean_data/master_depth2.csv")
gas <- read_csv("04_Outputs/compiled_GasDome.csv")

GasDome <- function(gas,stream) {
  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  y<-c("CO2_enviro",'Temp','depth',"day","hour", 'month', 'yr', 'ID')
  stream<-stream[,y]

  gas<-gas %>% mutate(day=day(Date), hour=hour(Date), month=month(Date),yr=year(Date))
  gas<-left_join(gas, stream,by=c('hour', 'day', 'month', 'yr', 'ID'), relationship = "many-to-many")

  gas$mouthTemp_F<-mean(gas$Temp, na.rm=T)
  gas$mouthTemp_C<-fahrenheit.to.celsius(gas$mouthTemp_F)
  gas$mouthTemp_K<-gas$mouthTemp_C+273.15
  gas$SchmidtO2hi<-1568-86.04*gas$mouthTemp_C+2.142*gas$mouthTemp_C^2-0.0216*gas$mouthTemp_C^3
  gas$SchmidtCO2hi<-1742-91.24*gas$mouthTemp_C+2.208*gas$mouthTemp_C^2-0.0219*gas$mouthTemp_C^3

  gas<-gas %>%
    group_by(day,month, yr,ID,rep) %>%
    mutate(cat = cur_group_id(), .before = c('ID', 'day')) %>%
    mutate(pCO2_water=max(CO2, na.rm=T)/1000000, day=as.Date(Date),
           pCO2_air=min(gas$CO2, na.rm=T)/1000000)%>%
    ungroup

  gas<-gas %>%
    split(.$cat) %>%
    map(~ lm(CO2 ~ Date, data = .x)) %>%
    map_dfr(~ as.data.frame(t(as.matrix(coef(.))))) %>% rename('slope'='Date') %>%
    mutate(cat= 1:124)%>%left_join(gas, by='cat')

  gas$deltaCO2_atm<- (abs(gas$slope)*2/1000000) #change in CO2 during float

  gas$n<-(gas$deltaCO2_atm*domeVol_L/R/gas$mouthTemp_K)
  gas$FCO2<-gas$n/domeFoot_m2*60
  gas$exp<-2400*((1/gas$mouthTemp_K)-(1/298.15))
  gas$KH<-0.034*((gas$exp)*(gas$exp))#mol/L/atm
  gas$KH_1000<-gas$KH*1000

  gas$KCO2_md<-(gas$FCO2/gas$KH_1000/(gas$pCO2_water-gas$pCO2_air))*24 #m/d
  gas$kO2<-gas$KCO2_md*(gas$SchmidtCO2hi/gas$SchmidtO2hi)^(-2/3)
  gas$k600_md<- gas$KCO2_md*(600/gas$SchmidtCO2hi)^(-2/3) #m/d

  (gas$KO2_1d<-gas$kO2/gas$depth)
  (gas$KCO2_1d<-gas$KCO2_md/gas$depth)
  (gas$k600_1d<- as.numeric(gas$k600_md/gas$depth))

  x<-c("day","depth","KO2_1d","KCO2_1d","k600_1d","ID",'rep')
  gas<-gas[,x]
  #gas <- gas[!duplicated(gas[c('k600_1d','ID')]),]

  return(gas)
}

#MAKE A FOLDER FOR EOSENSE GAS DOME. NEED TO MAKE A NEW SECTION FOR VAISALA IN ORDER TO
#INCLUDE VAISALA MULTIPLIER

#compile Gas Dome####
library(tools)
gas<-data.frame()
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/AllenMill",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  gas<-rbind(gas,site)
}


file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Gilchrist Blue",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  gas<-rbind(gas,site)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Ichetucknee",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  gas<-rbind(gas,site)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/LittleFanning",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  gas<-rbind(gas,site)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Otter",pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  site$ID<-strsplit(basename(fil), '_')[[1]][1]
  site$rep<-strsplit(file_path_sans_ext(fil), '_')[[1]][5]
  gas<-rbind(gas,site)
}

gas<-gas %>%
  mutate(ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
         ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
         ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)))

write_csv(gas, "04_Outputs/compiled_GasDome.csv")
#calculate####
k600<-GasDome(gas, stream)
k600<-rename(k600, 'Date'='day')
k600 <- k600[!duplicated(k600[c('k600_1d','ID','rep')]),]
k600 <- k600[complete.cases(k600[ ,c('depth')]), ]

u <- read_csv("01_Raw_data/u.csv")
u$Date<-mdy(u$Date)

k600<-left_join(k600,u, by=c('Date','ID'))
k600$uh<-k600$u/k600$depth
x<-c("Date","depth","u","ID",'rep',"KO2_1d","KCO2_1d","uh","k600_1d", "VentDO", "VentTemp")
k600<-k600[,x]

split<-k600 %>% split(k600$ID)
write.xlsx(split, file = '04_Outputs/rC_k600.xlsx')

LF_rC<- read_excel("C:/SpringMetabolism_FlowReversals/04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
AM_rC<- read_excel("C:/SpringMetabolism_FlowReversals/04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
GB_rC<- read_excel("C:/SpringMetabolism_FlowReversals/04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
OS_rC<- read_excel("C:/SpringMetabolism_FlowReversals/04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
ID_rC<-read_excel("C:/SpringMetabolism_FlowReversals/04_Outputs/rC_k600_edited.xlsx",sheet = "ID")

rC_all<-rbind(LF_rC, AM_rC, GB_rC, OS_rC, ID_rC)
write.csv(rC_all, file = 'C:/SpringMetabolism_FlowReversals/04_Outputs/rC_all.csv')
