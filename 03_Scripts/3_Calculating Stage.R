###packages###
library(tidyverse)
library(readxl)
library(measurements)
library(dataRetrieval)
###function####
PT_formatted <- function(fil) {
  PT <- read_csv(fil)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(PT)}
PT_unformatted <- function(fil) {
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  PT$ID<-strsplit(basename(fil), '_')[[1]][1]
  return(PT)}
FAWN_unformatted <- function(fil) {
  FAWN <- read_csv(fil,col_types = cols(`FAWN Station` = col_skip(),
                                        Period = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        `N (# obs)` = col_skip()))
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  FAWN$gageID<-strsplit(basename(fil), '_')[[1]][1]
  return(FAWN)}

###PT####
PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/PT/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)
  PT_everything <- PT_everything[!duplicated(PT_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/Hobo/PT/unformatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)
  PT_everything <- PT_everything[!duplicated(PT_everything[c('Date','ID')]),]
  }

PT_everything<-filter(PT_everything, PT<30)
PT_everything<-PT_everything %>%
  mutate(ID = ifelse(as.character(ID) == "AllenMillPond", "AM", as.character(ID)),
         ID = ifelse(as.character(ID) == "AllenMill", "AM", as.character(ID)),

         ID = ifelse(as.character(ID) == "GilchristBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilBlue", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchristBluel", "GB", as.character(ID)),
         ID = ifelse(as.character(ID) == "GilchrsitBlue", "GB", as.character(ID)),

         ID = ifelse(as.character(ID) == "Ichetucknee", "ID", as.character(ID)),
         ID = ifelse(as.character(ID) == "Ichetuckneel", "ID", as.character(ID)),

         ID = ifelse(as.character(ID) == "LittleFanning", "LF", as.character(ID)),

         ID = ifelse(as.character(ID) == "Otter", "OS", as.character(ID)))

PT_everything<-PT_everything %>%
  mutate(stageID =case_when(
    ID =="OS" ~"1",
    ID == "AM"~"1",
    ID  == "GB"~ "2",
    ID== "ID"~ "2",
    ID  == "LF"~"3"))

###FAWN####
FAWN_everything<-data.frame()

file.names <- list.files(path="01_Raw_data/Hobo/FAWN", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)
  FAWN_everything <- FAWN_everything[!duplicated(FAWN_everything[c('Date','gageID')]),]
}

FAWN_everything<-FAWN_everything %>%
  mutate(stageID =case_when(
    gageID =="Mayo" ~"1",
    gageID  == "Alachua"~ "2",
    gageID  == "Bronson"~"3"))
#ggplot(FAWN_everything, aes(Date, PSI)) + geom_line() + facet_wrap(~ stageID, ncol=5)

##Calculation#####

stage<-left_join(PT_everything, FAWN_everything, by=c('stageID', 'Date'))
stage <- stage[complete.cases(stage[ , c('PSI', 'PT')]), ]

for(i in 1:nrow(stage)) {if(stage$ID[i]=='OS') {

  stage$depth[i]<-((stage$PT[i]-stage$PSI[i])/(0.6894/0.372))+0.6995}

  else if (stage$ID[i]=='ID'){

    stage$depth[i]<-((stage$PT[i]-stage$PSI[i])/(1.64))+0.11}

  else if(stage$ID[i]=='GB'){

    stage$depth[i]<-((stage$PT[i]-stage$PSI[i])/(1.47/0.634))+0.176}

  else if(stage$ID[i]=='LF'){

    stage$depth[i]<-((stage$PT[i]-stage$PSI[i])/(0.6894/0.372))+0.6995}

  else if(stage$ID[i]=='AM'){

    stage$depth[i]<-((stage$PT[i]-stage$PSI[i])/(1.41/0.634))+0.515}

  else {stage$depth[i]<- NULL }}
write_csv(stage, "02_Clean_data/Chem/PSI.csv")


###Interpolation####

data_retrieval <- function(site_id) {
  parameterCd <- c('00065')
  startDate <- "2022-05-12"
  endDate <- "2024-06-25"

  river <- readNWISuv(site_id,parameterCd, startDate, endDate)
  split<-split(river, river$site_no)

  down <-split(river, river$site_no)[[2]]
  down<-down[,c(3,4)]
  down<-rename(down, 'Date'='dateTime', 'stage_down'='X_00065_00000')

  up <-split(river, river$site_no)[[1]]
  up<-up[,c(3,4)]
  up<-rename(up, 'Date'='dateTime', 'stage_up'='X_00065_00000')

  elevation_diff<-left_join(up,down, by='Date')

  elevation_diff<- elevation_diff %>% mutate(minute = minute(Date))
  elevation_diff<-filter(elevation_diff, minute==0)

  return(elevation_diff)}
stage_relationship <- function(site) {
  summary(modInter<-lm( depth~ elevation, data = site))
  cf <- coef(modInter)
  site$interpolated<-site$elevation*cf[2]+cf[1]

  site$depth <- ifelse(is.na(site$depth), site$interpolated, site$depth)

  return(site)}

master<-read_csv("02_Clean_data/Chem/PSI.csv")
x<-c('Date','depth','ID')

AM<-filter(master, ID=='AM')
site_id <- c('02319800','02320000')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.501
AM<-left_join(elevation_diff,AM)
AM<-stage_relationship(AM)
AM$ID<-'AM'
AM<-AM[,x]

GB<-filter(master, ID=='GB')
site_id <- c('02321958','02322500')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.79
GB<-left_join(elevation_diff, GB)
GB<-stage_relationship(GB)
GB<-GB %>% mutate(ID=='GB') %>% filter(depth>0.3)
GB$ID<-'GB'
GB<-GB[,x]

OS<-filter(master, ID=='OS')
site_id <- c('02323000','02323500')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.72
OS<-left_join(elevation_diff,OS)
OS<-stage_relationship(OS)
OS$ID<-'OS'
OS<-OS[,x]

LF<-filter(master, ID=='LF')
site_id <- '02323500'
parameterCd <- c('00065')
startDate <- "2022-04-12"
endDate <- "2024-06-18"
riverLF <- readNWISuv(site_id,parameterCd, startDate, endDate)
riverLF<-riverLF[,c(1,3,2,4)]
riverLF<-rename(riverLF, 'Date'='dateTime', 'elevation'='X_00065_00000')
riverLF<- riverLF %>% mutate(minute = minute(Date))
riverLF<-filter(riverLF, minute==0)
riverLF<-riverLF[,c(2,1,3,5,4)]
LF<-LF %>% mutate(depth=depth-0.6)
LF<-left_join(riverLF, LF)
LF<-stage_relationship(LF)
LF$ID<-'LF'
LF<-LF[,x]

ID<-filter(master, ID=='ID')
SF<- read_xlsx("01_Raw_data/Hobo/PT/02322703_Level.xlsx",skip = 25)
SF<-SF %>%rename('depth_gage'="Level NAVD88") %>%
  filter(Date>'2021-04-02')%>%mutate(depth_gage=conv_unit(depth_gage,'ft','m'),
                                     day=as.Date(Date)) %>%mutate(depth_gage=depth_gage-2)
ID$day<-as.Date(ID$Date)
ID<-ID[,-c(1)]
SF<-SF[,c('depth_gage','day')]
ID<-left_join(SF, ID, by='day')

modInter<-lm( depth~ depth_gage, data = ID)
cf <- coef(modInter)
ID$depth<-ID$depth_gage*cf[2]+cf[1]
ID<-ID %>% filter(depth>0) %>% rename("Date"='day') %>%mutate(ID= 'ID', depth=depth+0.75)
ID<-ID[,x]

ggplot(ID, aes(Date, depth)) + geom_line()

startDate <- "2022-05-12"
endDate <- "2024-06-25"
parameterCd <- c('00065')
ventID<-'02322700'
IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime')%>%
  mutate(min=minute(Date)) %>% filter(min==0) %>%
  mutate(ID='IU', depth=X_00065_00000-13.72)
IU<-IU[,x]

stage<-rbind(AM, GB, LF, ID, OS, IU)

ggplot(stage, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=2)

write_csv(stage, "02_Clean_data/Chem/depth.csv")

#Combine#####

stage<-read_csv("02_Clean_data/Chem/depth.csv")
master<-read_csv("02_Clean_data/master_chem1.csv")

stage<-stage %>% mutate(day=day(Date), mnth=month(Date), yr=year(Date))
master<-master %>% mutate(day=day(Date), mnth=month(Date), yr=year(Date))

stage<-stage[,-1]
master<-left_join(stage, master, by=c('ID', 'day', 'mnth', 'yr'))
master <- master[!duplicated(master[c('Date','ID')]),]

ggplot(master, aes(Date, SpC)) + geom_line() + facet_wrap(~ ID, ncol=2)
ggplot(master, aes(Date, DO)) + geom_line() + facet_wrap(~ ID, ncol=2)


write_csv(master, "02_Clean_data/master_depth2.csv")
