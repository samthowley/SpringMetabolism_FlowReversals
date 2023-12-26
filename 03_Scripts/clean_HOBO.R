###packages###
library(tidyverse)
library(readxl)
library(measurements)
###function####
pH_xl <- function(fil) {
  pH <- read_excel(fil)
pH<-pH[,c(1,5)]
colnames(pH)[1] <- "Date"
colnames(pH)[2] <- "pH"
pH$pH<-as.numeric(pH$pH)
return(pH)}
pH_csv <- function(fil) {
  pH <- read_csv(fil, skip=3)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  return(pH)}
pH_HOBO <- function(fil) {
  pH <- read_excel(fil)
  pH<-pH[,c(2,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  return(pH)}
DO_formatted <- function(fil) {
  DO <- read_csv(fil)
  DO<-DO[,c(1,2,3)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  return(DO)}
DO_unformatted <- function(fil) {
  DO <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  return(DO)}
SpC_formatted <- function(fil) {
  SpC <- read_csv(fil)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  return(SpC)}
SpC_unformatted <- function(fil) {
  SpC <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  return(SpC)}
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


####pH#####
file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything", pattern=".xlsx", full.names=TRUE)
pH_everything <- data.frame()
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)}
file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)}
file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/dat everything", pattern=".dat", full.names=TRUE)
for(fil in file.names){
  pH <- pH_csv(fil)
  pH_everything<-rbind(pH_everything,pH)}
pH<-filter(pH, pH<9.25 & pH>4)
AM_pH <- pH[!duplicated(pH[c('Date')]),]
AM_pH$ID<-'AM'

pH_everything <- data.frame()
file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/Everything", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)}
file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/everything dat", pattern=".dat", full.names=TRUE)
for(fil in file.names){
  pH <- pH_csv(fil)
  pH_everything<-rbind(pH_everything,pH)}
GB_pH <- pH[!duplicated(pH[c('Date')]),]
GB_pH$ID<-'GB'

pH_everything <- data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/pH", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_HOBO(fil)
  pH_everything<-rbind(pH_everything,pH)}
ID_pH <- pH[!duplicated(pH[c('Date')]),]
ID_pH$ID<-'ID'

pH_everything <- data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/pH", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_HOBO(fil)
  pH_everything<-rbind(pH_everything,pH)}
LF_pH <- pH[!duplicated(pH[c('Date')]),]
LF_pH$ID<-'LF'

pH_everything <- data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Otter/pH", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_HOBO(fil)
  pH_everything<-rbind(pH_everything,pH)}
OS_pH <- pH[!duplicated(pH[c('Date')]),]
OS_pH$ID<-'OS'

pH<-rbind(AM_pH, LF_pH, GB_pH, OS_pH, ID_pH)
write_csv(pH, "02_Clean_data/Chem/pH.csv")
###DO#####
DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/DO", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
AM_DO <- DO_everything[!duplicated(DO_everything[c('Date')]),]
AM_DO$ID<-'AM'

DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/DO", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
GB_DO <- DO_everything[!duplicated(DO_everything[c('Date')]),]
GB_DO$ID<-'AM'

DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/DO", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
ID_DO <- DO_everything[!duplicated(DO_everything[c('Date')]),]
ID_DO$ID<-'AM'

DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/DO", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
LF_DO <- DO_everything[!duplicated(DO_everything[c('Date')]),]
LF_DO$ID<-'AM'

DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Otter/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
file.names <- list.files(path="01_Raw_data/Hobo/Otter/DO", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO_everything<-rbind(DO_everything,DO)}
OS_DO <- DO_everything[!duplicated(DO_everything[c('Date')]),]
OS_DO$ID<-'AM'

DO<-rbind(AM_DO, GB_DO, LF_DO, ID_DO, OS_DO)
write_csv(DO, "02_Clean_data/Chem/DO.csv")

###SpC####
SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
file.names <- list.files(path="01_Raw_data/Hobo/Allen Mill/SpC", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
AM_SpC <- SpC_everything[!duplicated(SpC_everything[c('Date')]),]
AM_SpC$ID<-'AM'

SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
file.names <- list.files(path="01_Raw_data/Hobo/Little Fanning/SpC", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
LF_SpC <- SpC_everything[!duplicated(SpC_everything[c('Date')]),]
LF_SpC$ID<-'LF'

SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
file.names <- list.files(path="01_Raw_data/Hobo/Ichetucknee/SpC", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
ID_SpC <- SpC_everything[!duplicated(SpC_everything[c('Date')]),]
ID_SpC$ID<-'ID'

SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
file.names <- list.files(path="01_Raw_data/Hobo/GilchristBlue/SpC", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
GB_SpC <- SpC_everything[!duplicated(SpC_everything[c('Date')]),]
GB_SpC$ID<-'GB'

SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/Otter/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
file.names <- list.files(path="01_Raw_data/Hobo/Otter/SpC", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)}
OS_SpC <- SpC_everything[!duplicated(SpC_everything[c('Date')]),]
OS_SpC$ID<-'OS'

SpC<-rbind(OS_SpC, GB_SpC, ID_SpC, LF_SpC, AM_SpC)
write_csv(SpC, "02_Clean_data/Chem/SpC.csv")
