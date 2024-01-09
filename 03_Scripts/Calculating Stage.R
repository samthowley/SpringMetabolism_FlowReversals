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
  return(PT)}
PT_unformatted <- function(fil) {
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  return(PT)}
FAWN_formatted <- function(fil) {
  FAWN <- read_csv(fil)
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  return(FAWN)}
FAWN_unformatted <- function(fil) {
  FAWN <- read_csv(fil,col_types = cols(`FAWN Station` = col_skip(),
                                        Period = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        `N (# obs)` = col_skip()))
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  return(FAWN)}




###PT####
PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/PT/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/PT", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
AM_PT <- PT_everything[!duplicated(PT_everything[c('Date')]),]
AM_PT$ID<-'AM'

PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/stage/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/stage", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
LF_PT <- PT_everything[!duplicated(PT_everything[c('Date')]),]
LF_PT$ID<-'LF'

PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/PT/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/PT", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
ID_PT <- PT_everything[!duplicated(PT_everything[c('Date')]),]
ID_PT$ID<-'ID'

PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Otter/PT/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
file.names <- list.files(path="01_Raw_data/Hobo/Otter/PT", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
OS_PT <- PT_everything[!duplicated(PT_everything[c('Date')]),]
OS_PT$ID<-'OS'

PT_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/stage/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_formatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/stage", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  PT <- PT_unformatted(fil)
  PT_everything<-rbind(PT_everything,PT)}
GB_PT <- PT_everything[!duplicated(PT_everything[c('Date')]),]
GB_PT$ID<-'GB'

PT<-rbind(OS_PT, GB_PT, ID_PT, LF_PT, AM_PT)

###FAWN####
FAWN_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/FAWN/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_formatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/FAWN", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
AM_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
AM_FAWN$ID<-'AM'

FAWN_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/FAWN/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_formatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/FAWN", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
LF_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
LF_FAWN$ID<-'LF'

FAWN_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/FAWN/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_formatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/FAWN", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
GB_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
GB_FAWN$ID<-'GB'

FAWN_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/FAWN", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
ID_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
ID_FAWN$ID<-'ID'

file.names <- list.files(path="01_Raw_data/Hobo/Otter/FAWN", pattern=".csv", full.names=TRUE)
FAWN_everything<-data.frame()
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
OS_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
OS_FAWN$ID<-'OS'

FAWN<-rbind(AM_FAWN, LF_FAWN, GB_FAWN, OS_FAWN, ID_FAWN)

##Calculation#####

stage<-left_join(PT, FAWN, by=c('Date', 'ID'))
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

stage<- filter(stage,depth<20)
ggplot(stage, aes(Date, depth)) + geom_line() + facet_wrap(~ ID, ncol=5)
write_csv(stage, "02_Clean_data/Chem/depth.csv")


###Interpolation####
data_retrieval <- function(site_id) {
  parameterCd <- c('00065')
  startDate <- "2022-04-12"
  endDate <- "2024-01-04"

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
stage_relationship <- function(site,elevation_diff) {
  elevation_diff<-elevation_diff[,c(1,5)]

  site<-left_join(site, elevation_diff)

  summary(modInter<-lm( depth~ elevation, data = site))
  cf <- coef(modInter)
  (InterceptmodInter<- cf[1])
  (SlopemodInter<- cf[2])

  return(list(InterceptmodInter,SlopemodInter))}
master<-read_csv("02_Clean_data/master.csv")
x<-c('Date','depth','ID')

AM<-filter(master, ID=='AM')
site_id <- c('02319800','02320000')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.501
(stage_slope<-stage_relationship(AM, elevation_diff))
elevation_diff$depth<-elevation_diff$elevation*stage_slope[[2]]+stage_slope[[1]]
elevation_diff$ID<-'AM'
AM<-elevation_diff[,x]

GB<-filter(master, ID=='GB')
site_id <- c('02321958','02322500')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.79
(stage_slope<-stage_relationship(GB, elevation_diff))
elevation_diff$depth<-elevation_diff$elevation*stage_slope[[2]]+stage_slope[[1]]
elevation_diff$ID<-'GB'
GB<-elevation_diff[,x]

OS<-filter(master, ID=='OS')
site_id <- c('02323000','02323500')
elevation_diff<-data_retrieval(site_id)
elevation_diff$elevation<-(elevation_diff$stage_up-elevation_diff$stage_down)*0.72
(stage_slope<-stage_relationship(OS, elevation_diff))

elevation_diff$depth<-elevation_diff$elevation*stage_slope[[2]]+stage_slope[[1]]
elevation_diff$ID<-'OS'
OS<-elevation_diff[,x]

LF<-filter(master, ID=='LF')
site_id <- '02323500'
parameterCd <- c('00065')
startDate <- "2022-04-12"
endDate <- "2023-11-09"
riverLF <- readNWISuv(site_id,parameterCd, startDate, endDate)
riverLF<-riverLF[,c(1,3,2,4)]
riverLF<-rename(riverLF, 'Date'='dateTime', 'elevation'='X_00065_00000')
riverLF<- riverLF %>% mutate(minute = minute(Date))
riverLF<-filter(riverLF, minute==0)
riverLF<-riverLF[,c(2,1,3,5,4)]
(stage_slope<-stage_relationship(LF, riverLF))
riverLF$depth<-riverLF$elevation*stage_slope[[2]]+stage_slope[[1]]
riverLF$ID<-'LF'
LF<-riverLF[,x]


ID<-filter(master, ID=='ID')
SF<-read_csv("01_Raw_data/02322703_Level.csv",col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
SF<-rename(SF, 'elevation'="Level NAVD88")
(stage_slope<-stage_relationship(ID, SF))
SF$depth<-SF$elevation*stage_slope[[2]]+stage_slope[[1]]
SF$ID<-'ID'
ID<-SF[,x]

stage<-rbind(AM, GB, LF, ID, OS)
write_csv(stage, "02_Clean_data/Chem/depth.csv")
