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

pH.edit<-pH_everything %>% filter(pH<9,pH>4, ID!="RovingBox")%>%
  mutate(ID=if_else(ID=="GilBlue04272022.xlsx", "GB", ID),
         pH=if_else(ID=='AM'& Date<'2024-01-01', pH-1, pH)
         )

# ggplot(data=pH.edit, aes(x=Date, y=pH)) +
#   geom_point()+
#   facet_wrap(~ID)#+geom_hline(yintercept=360)


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

DO_everything<-rename_ID(DO_everything) %>% filter(Date>"2022-01-01")%>%mutate(DO=abs(DO))%>%
  distinct(Date, ID, .keep_all = T)

DO_CQ<-DO_everything%>%
  filter(DO<11.5,
         Temp<80,
         Temp>50)%>%
  mutate(DO=if_else(ID=='LF'& Date<'2022-07-01'& DO>6, NA, DO),
         DO=if_else(ID=='LF' & DO<2 |DO>10, NA, DO),
         DO=if_else(ID=='ID' & DO<2.8 |DO>10, NA, DO),
         DO=if_else(ID=='GB' & DO<3.5 |DO>8.5, NA, DO),
         
         Temp=if_else(ID=='LF' & Temp<60, NA, Temp),
         Temp=if_else(ID=='ID' & Temp>76, NA, Temp),
         Temp=if_else(ID=='GB' & Temp>78, NA, Temp)
         )

ggplot(data=DO_CQ, aes(x=Date)) +
  geom_line(aes(y=DO))+
  #geom_line(aes(y=DO), color='red')+
  facet_wrap(~ID)


write_csv(DO_CQ, "02_Clean_data/Chem/DO.csv")

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
SpC.edited<-SpC_everything %>%
  mutate(
    Date=ymd_hms(Date),
    SpC=if_else(ID=='AM' & SpC>450, NA, SpC),
    SpC=if_else(ID=='GB' & SpC>430, NA, SpC),
    SpC=if_else(ID=='ID' & SpC>400, NA, SpC)
    )%>%
  filter(SpC<575)
  

ggplot(data=SpC.edited, aes(x=Date, y=SpC)) +
  geom_point()+
  facet_wrap(~ID)#+geom_hline(yintercept=360)



write_csv(SpC_everything, "02_Clean_data/Chem/SpC.csv")

###compile####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%select(-min)
master <- master[!duplicated(master[c('Date','ID')]),]
    

###Include IU####
library(dataRetrieval)
startDate <- "2022-05-12"
endDate <- "2024-07-25"
parameterCd <- c('00010','00300','00095','00400','00065')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% 
  rename('Date'='dateTime', 'Temp'='X_00010_00000',
                  'DO'='X_00300_00000', 'SpC'='X_00095_00000',
                  'pH'='X_00400_00000', 'depth'='X_00065_00000')%>%
  mutate(min=minute(Date), day=day(Date), mnth=month(Date), yr=year(Date))%>%
  mutate(min=minute(Date),CO2=NA,depth=depth-13.72, ID='IU')%>%
  filter(min==0)%>% select(Date, depth, ID, SpC, CO2, DO, Temp, pH)

master<- rbind(master, IU)

write_csv(master, "02_Clean_data/master_chem1.csv")
