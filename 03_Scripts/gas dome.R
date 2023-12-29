rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(weathermetrics)
master <- read_csv("02_Clean_data/master.csv")
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
#######

GasDome <- function(gas,stream) {
  stream<-stream %>%  rename("CO2_enviro"='CO2')%>% mutate(day=day(Date), hour=hour(Date))
  y<-c("CO2_enviro",'Temp','depth',"day","hour")
  stream<-stream[,y]
  gas<-gas %>% mutate(day=day(Date), hour=hour(Date))
  gas<-left_join(gas, stream,by=c('hour', 'day'), relationship = "many-to-many")
  day <- as.Date(gas$Date)[1]

  mouthTemp_F<-mean(gas$Temp, na.rm=T)
  mouthTemp_C<-fahrenheit.to.celsius(mouthTemp_F)
  mouthTemp_K<-mouthTemp_C+273.15
  SchmidtO2hi<-1568-86.04*mouthTemp_C+2.142*mouthTemp_C^2-0.0216*mouthTemp_C^3
  SchmidtCO2hi<-1742-91.24*mouthTemp_C+2.208*mouthTemp_C^2-0.0219*mouthTemp_C^3

  pCO2_water<-	mean(gas$CO2_enviro, na.rm=T)/1000000
  depth<-mean(gas$depth, na.rm=T)

  (m<-lm(CO2~Date, data = gas))
  cf <- coef(m)
  (slope <- cf[2])
  deltaCO2_atm<- ((slope*-1)*2/1000000) #change in CO2 during float
  pCO2_air<-max(gas$CO2, na.rm=T)/1000000

  n<-(deltaCO2_atm*domeVol_L/R/mouthTemp_K)
  FCO2<-n/domeFoot_m2*60
  exp<-2400*((1/mouthTemp_K)-(1/298.15))
  (KH<-0.034*exp(exp)) #mol/L/atm
  KH_1000<-KH*1000

  KCO2_md<-(FCO2/KH_1000/(pCO2_air-pCO2_water))*24 #m/d
  kO2<-KCO2_md*(SchmidtCO2hi/SchmidtO2hi)^(-2/3)
  k600_md<- KCO2_md*(600/SchmidtCO2hi)^(-2/3) #m/d

  (KO2_1d<-kO2/depth)
  (KCO2_1d<-KCO2_md/depth)
  (k600_1d<- as.numeric(k600_md/depth))
  return(list(day, depth, k600_1d))
}
k600_list <- function(stage,k600,date) {
  col2<-list(stage,k600) #stage and k600 columns
  col2<-as.data.frame(do.call(cbind, col2))
  col2<- col2 %>% rename('stage'="V1",'k600'="V2") #assign correct column names

  col1<-data.frame(date=date) #date column
  col1<-pivot_longer(col1, cols = 1:23, values_to = 'Date') #wide to long
  col1<-col1[,-c(1)]

  done<-cbind(col1, col2)
  return(done)}

##### AM k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="AM")
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/AllenMill",pattern=".csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_AM<-k600_list(stage, k600,date)
write_csv(done_AM, "04_Outputs/k600/GB.csv")
##### GB k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="GB")
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Gilchrist Blue",pattern=".csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_GB<-k600_list(stage, k600,date)
write_csv(done_GB, "04_Outputs/k600/GB.csv")

##### ID k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="ID")
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Ichetucknee",pattern=".csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_ID<-k600_list(stage, k600,date)
write_csv(done_ID, "04_Outputs/k600/ID.csv")


##### LF k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="LF")
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/LittleFanning",pattern=".csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_LF<-k600_list(stage, k600,date)
write_csv(done_LF, "04_Outputs/k600/LF.csv")



##### OS k600 ########
x<-c('Date','CO2')
k600<- vector("numeric")
stage<- vector("numeric")
date<- vector("integer")

stream<-master%>%filter(ID=="OS")
file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Otter",pattern=".csv", full.names=TRUE)

for(i in file.names){
  gas<-read_csv(i)
  output<-GasDome(gas, stream)
  stage[i] <- output[2]
  k600[i] <- output[3]
  date[i]<-output[1]
}

done_OS<-k600_list(stage, k600,date)
write_csv(done_OS, "04_Outputs/k600/OS.csv")




##############
# w<-c('Date', 'CO2')
# destination_folder<-"01_Raw_data/CampbellSci/GasDome/Otter"
# file.names <- list.files(path="01_Raw_data/CampbellSci/GasDome/Otter/raw", full.names=TRUE, pattern = '.xlsx')
# lapply(file.names, function(x, sheet='Sheet2') {
#   t <- read_xlsx(x, sheet = 'Sheet2')
#   t<-t[,w]
#   t$CO2<-t$CO2*6
#   write_csv(t,gsub("xlsx", "csv", x), paste0(destination_folder,"/", basename(x)))
# })

