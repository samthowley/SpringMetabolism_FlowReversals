###packages###
library(tidyverse)
library(readxl)
library(measurements)
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
write_csv(PT, "02_Clean_data/Chem/PT.csv")

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

FAWN_everything<-data.frame()
for(fil in file.names){
  FAWN <- FAWN_unformatted(fil)
  FAWN_everything<-rbind(FAWN_everything,FAWN)}
OS_FAWN <- FAWN_everything[!duplicated(FAWN_everything[c('Date')]),]
OS_FAWN$ID<-'OS'

FAWN<-rbind(AM_FAWN, LF_FAWN, GB_FAWN, OS_FAWN, ID_FAWN)
write_csv(FAWN, "02_Clean_data/Chem/FAWN.csv")
