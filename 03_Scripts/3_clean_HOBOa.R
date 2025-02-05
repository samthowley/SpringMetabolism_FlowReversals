###packages###
library(tidyverse)
library(readxl)
library(measurements)
library(tools)
#function####
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

  DO$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(DO)}
DO_unformatted <- function(fil) {
  DO <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  keep<-c('Date', "DO", "Temp")
  DO<-DO[,keep]
  DO$Date <- mdy_hms(DO$Date)

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
  return(SpC)}
rename_ID<-function(site){
  site<-site %>%
    mutate(ID = ifelse(as.character(ID) == "AllenMillPond", "AM", as.character(ID)),
           ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),
           ID = ifelse(as.character(ID) == "AllenMillDO", "AM", as.character(ID)),
           
           ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
           ID = ifelse(as.character(ID) == "Gilichrist", "GB", as.character(ID)),
           ID = ifelse(as.character(ID) == "GilichristBlue", "GB", as.character(ID)),
           
           ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
           ID = ifelse(as.character(ID) == "Ichetuckneel", "ID", as.character(ID)),
        

           ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),
           ID = ifelse(as.character(ID) == "LittleFanningSpC", "LF", as.character(ID)),
           
           ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)),
           ID = ifelse(as.character(ID) == "OtterSpC", "OS", as.character(ID)))
return(site)}

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

#pH#####
pH_everything <- data.frame()

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/Everything", pattern=".xlsx", full.names=TRUE)
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

pH_everything<-rename_ID(pH_everything)

GB_pH <- read_xlsx("01_Raw_data/02322350_pH.xlsx")
GB_pH <- GB_pH %>%
  mutate(Hour = list(0:23)) %>%  # Create a list of 24 hours for each day
  unnest(Hour) %>%  # Expand into multiple rows
  mutate(Date = ymd_hm(paste(Date, Hour, "00"))) %>%  # Convert to ymd_hm format
  select(-Hour)  # Remove temporary Hour column if not needed

pH_everything<-rbind(GB_pH,pH_everything)

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

DO_everything<-rename_ID(DO_everything)
DO_everything<-DO_everything %>% filter(Date>"2022-01-01")%>%mutate(DO=abs(DO))

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

SpC_everything<-rename_ID(SpC_everything)
SpC_everything$Date<-ymd_hms(SpC_everything$Date)

write_csv(SpC_everything, "02_Clean_data/Chem/SpC.csv")

###compile####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2,8,1,3,5)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, left_join, by = c("ID", 'Date'))

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%select(-min)
master <- master[!duplicated(master[c('Date','ID')]),]

master<-master %>% mutate(SpC=if_else(SpC>600, NA, SpC),
                          SpC=if_else(SpC<30, NA, SpC))%>%
   mutate(SpC=if_else(ID=='GB'& SpC<360 | ID=='GB'& SpC>420, NA, SpC),
          SpC=if_else(ID=='AM'&depth<2&SpC<300 | ID=='AM' & SpC>430, NA, SpC),
          SpC=if_else(ID=='LF'&SpC<470, NA, SpC),
          SpC=if_else(ID=='ID'&depth<1.2& SpC<300 |ID=='ID'& SpC>360 , NA, SpC),
          SpC=if_else(ID=='ID'& SpC<300, NA, SpC),
          SpC=if_else(ID=='OS'& depth<1.2 & SpC<200, NA, SpC))


master<-master %>% mutate(DO=if_else(DO>12, NA, DO))%>%
  mutate(DO=if_else(ID=='GB'& DO>9 | ID=='GB'& DO<3.8, NA, DO),
         DO=if_else(ID=='GB'& Date<='2023-01-01' & DO>8 , NA, DO),
         DO=if_else(ID=='GB'& Date>'2024-05-01' & DO<4 , NA, DO),
         
         DO=if_else(ID=='LF'& DO<1 , NA, DO),
         DO=if_else(ID=='LF'& Date<'2022-08-20'& DO<2, NA, DO),
         DO=if_else(ID=='LF'& Date<'2022-07-01'& DO>6, NA, DO),
         DO=if_else(ID=='LF'& Date>'2024-01-01'& DO>8, NA, DO),
         DO=if_else(ID=='LF'& Date<'2024-05-01'& DO<1.5, NA, DO),
         
         DO=if_else(ID=='ID'& Date<'2024-05-01'& Date>'2023-05-01'& DO<3.87, NA, DO),
         DO=if_else(ID=='ID'& Date<'2023-03-01'&  DO>9, NA, DO),
         DO=if_else(ID=='ID'& Date<'2023-03-01'&  DO<4.2, NA, DO),
         
         DO=if_else(ID=='AM'& Date>'2023-06-01'& Date<'2024-02-01'& DO>9.5, NA, DO),
         DO=if_else(ID=='AM'& Date>'2024-01-01'& DO>8, NA, DO),
         DO=if_else(ID=='AM' &depth<1.1 & Date<'2023-04-01'& DO<3.7, NA, DO))
  
master<-master %>%mutate(pH=if_else(pH>10, NA, pH),
                       pH=if_else(ID=='AM'& Date<'2023-01-01'& pH<4, NA, pH),
                       pH=if_else(ID=='AM'& Date>'2024-04-01'& pH>7.6, NA, pH),
                       pH=if_else(ID=='AM'& depth<0.8 & pH>9, NA, pH),
                       
                       pH=if_else(ID=='LF'& Date<'2023-01-01' & pH<7, NA, pH),
                       pH=if_else(ID=='LF'& Date<'2023-01-01' & pH>8, NA, pH),
                       
                       pH=if_else(ID=='ID'&pH<7.27, NA, pH),
                       pH=if_else(ID=='ID'&pH<7.48 & Date>'2023-10-01', NA, pH),
                       
                       pH=if_else(ID=='OS'&pH<6, NA, pH),
                       pH=if_else(ID=='OS'&pH>8, NA, pH),
                       pH=if_else(ID=='OS'&pH<7&Date<'2023-01-01', NA, pH)
                       )

ggplot(data=master, aes(x=Date)) +
    geom_line(aes(y=pH),size=1, color='blue')+geom_hline(yintercept =7.48)+
    facet_wrap(~ID, scale='free')


ggplot(data=SpC_everything, aes(x=Date)) +
  geom_line(aes(y=SpC/100),size=1, color='blue')+
  facet_wrap(~ID, scale='free')

write_csv(master, "02_Clean_data/master_chem1.csv")

###IU####
startDate <- "2022-05-12"
endDate <- "2024-07-25"
parameterCd <- c('00065')
ventID<-'02322700'
IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime')%>%
  mutate(min=minute(Date)) %>% filter(min==0) %>%
  mutate(ID='IU', depth=X_00065_00000-13.72)
IU<-IU[,x]

stage<-rbind(AM, GB, LF, ID, OS, IU)

ggplot(stage, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=2)

library(dataRetrieval)
startDate <- "2024-06-18"
endDate <- "2024-07-25"
parameterCd <- c('00010','00300','00095','00400')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime', 'Temp'='X_00010_00000',
                  'DO'='X_00300_00000', 'SpC'='X_00095_00000',
                  'pH'='X_00400_00000')%>%
  mutate(min=minute(Date), day=day(Date), mnth=month(Date), yr=year(Date))%>%
  mutate(min=minute(Date),CO2=NA)%>%
  filter(min==0)
IU$ID<-'IU'
IU<-IU[,c("Date", "DO","Temp", "ID","CO2","pH","SpC" )]
write_csv(IU, "01_Raw_data/IU/IU_0725.csv")

IU<-data.frame()
file.names <- list.files(path="01_Raw_data/IU", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  site <- read_csv(fil)
  IU<-rbind(IU,site)}
master<-rbind(master, IU)
