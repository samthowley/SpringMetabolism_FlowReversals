###packages###
library(tidyverse)
library(readxl)
library(measurements)
library(tools)
###function####
pH_xl <- function(fil) {
  pH <- read_excel(fil)
pH<-pH[,c(1,5)]
colnames(pH)[1] <- "Date"
colnames(pH)[2] <- "pH"
pH$pH<-as.numeric(pH$pH)
pH$ID<-strsplit(basename(fil), '_')[[1]][1]

return(pH)}
pH_csv <- function(fil) {
  pH <- read_csv(fil, skip=3)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  pH$ID<-strsplit(basename(fil), '_')[[1]][1]

  return(pH)}
pH_HOBO <- function(fil) {
  pH <- read_excel(fil)
  pH<-pH[,c(2,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  pH<-pH[order(as.Date(pH$Date, format="%Y-%m-%d %H:%M:%S")),]
  pH$ID<-strsplit(basename(fil), '_')[[1]][1]

  return(pH)}
DO_formatted <- function(fil) {
  DO <- read_csv(fil)
  DO<-DO[,c(1,2,3)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  DO<-filter(DO, DO>0)
  DO$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(DO)}
DO_unformatted <- function(fil) {
  DO <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  DO<-filter(DO, DO>0)
  DO$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(DO)}
SpC_formatted <- function(fil) {
  SpC <- read_csv(fil)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  SpC$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(SpC)}
SpC_unformatted <- function(fil) {
  SpC <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  SpC<-SpC[order(as.Date(SpC$Date, format="%Y-%m-%d %H:%M:%S")),]
  SpC$ID<-strsplit(basename(fil), '_')[[1]][1]

  return(SpC)}


####pH#####
file.names <- list.files(path="01_Raw_data/CampbellSci/pH/Everything", pattern=".xlsx", full.names=TRUE)
pH_everything <- data.frame()
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/dat everything", pattern=".dat", full.names=TRUE)
for(fil in file.names){
  pH <- pH_csv(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/Hobo/pH", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_HOBO(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

pH_everything<-pH_everything %>%
  mutate(ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilBlue04272022.xlsx", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
         ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
         ID = ifelse(as.character(ID) == "RovingBox", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "OtterCO2", "OS", as.character(ID)),
         ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)))
pH_everything<-filter(pH_everything, pH>4 & pH<9)
ggplot(pH_everything, aes(Date, pH)) + geom_line() + facet_wrap(~ ID, ncol=2)

unique(pH_everything$ID)

write_csv(pH_everything, "02_Clean_data/Chem/pH.csv")
###DO#####
DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)
  DO_everything <- DO_everything[!duplicated(DO_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/HOBO/DO/unformated", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)
  DO_everything <- DO_everything[!duplicated(DO_everything[c('Date','ID')]),]
}

DO_everything<-DO_everything %>%
  mutate(ID = ifelse(as.character(ID) == "AllenMillPond", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "AllenMillDO", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
         ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
         ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)))
unique(DO_everything$ID)
ggplot(DO_everything, aes(Date, DO)) + geom_line() + facet_wrap(~ ID, ncol=2)

write_csv(DO_everything, "02_Clean_data/Chem/DO.csv")

###SpC####
SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)
  SpC_everything <- SpC_everything[!duplicated(SpC_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/Hobo/SpC/unformatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)
  SpC_everything <- SpC_everything[!duplicated(SpC_everything[c('Date','ID')]),]
}

SpC_everything<-SpC_everything %>%
  mutate(ID = ifelse(as.character(ID) == "AllenMillPond", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "Gilichrist", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilichristBlue", "GB", as.character(ID)),

         ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
         ID = ifelse(as.character(ID) == "Ichetuckneel", "ID", as.character(ID)),

         ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
         ID = ifelse(as.character(ID) == "LittleFanningSpC", "LF", as.character(ID)),

         ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)),
         ID = ifelse(as.character(ID) == "OtterSpC", "OS", as.character(ID)))

SpC_everything$Date<-ymd_hms(SpC_everything$Date)
ggplot(SpC_everything, aes(Date, SpC)) + geom_line() + facet_wrap(~ ID, ncol=2)

write_csv(SpC_everything, "02_Clean_data/Chem/SpC.csv")

###IU####
library(dataRetrieval)
startDate <- "2022-05-12"
endDate <- "2024-02-05"
parameterCd <- c('00010','00300','00095','00400')
ventID<-'02322700'

#what<-whatNWISdata(site='02322700')
IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime', 'Temp'='X_00010_00000',
                  'DO'='X_00300_00000', 'SpC'='X_00095_00000',
                  'pH'='X_00400_00000')%>%
  mutate(min=minute(Date), day=day(Date), mnth=month(Date), yr=year(Date))%>%
  mutate(min=minute(Date),CO2=NA)%>%
  filter(min==0)
IU$ID<-'IU'
IU<-IU[,c("Date", "DO","Temp", "ID","CO2","pH","SpC","min" )]
###compile####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(3,1,5,8)]

data <- lapply(file.names,function(x) {read_csv(x)})
library(plyr)

master<-join_all(data, by=c('Date','ID'), type='left')
master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0)
master <- master[!duplicated(master[c('Date','ID')]),]
master<-rbind(master, IU)
detach("package:plyr", unload = TRUE)


ggplot(master, aes(Date, pH)) + geom_line() + facet_wrap(~ ID, ncol=2)

write_csv(master, "02_Clean_data/master_chem1.csv")
