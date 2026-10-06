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

