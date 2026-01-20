###packages###
library(tidyverse)
library(readxl)
library(measurements)
library(dataRetrieval)
source("03_Scripts/disturbance isolation functions.R")

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

stage<-stage%>%
  mutate(
    depth=if_else(ID=='ID', depth+0.7, depth)
  )



fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.03, min_rows = 5) {
  y_name <- rlang::as_name(rlang::enquo(y_var))
  x_name <- rlang::as_name(rlang::enquo(x_var))
  g_name <- rlang::as_name(rlang::enquo(group_var))
  
  split_list <- split(df, df[[g_name]])
  
  lapply(split_list, function(.x) {
    # Remove NAs pairwise for this group/var
    complete_cases <- complete.cases(.x[[y_name]], .x[[x_name]])
    .x_clean <- .x[complete_cases, ]
    
    if (nrow(.x_clean) < min_rows) {
      message("Skip group with only ", nrow(.x_clean), " complete cases (min: ", min_rows, ")")
      return(NULL)
    }
    
    fit <- loess(.x_clean[[y_name]] ~ .x_clean[[x_name]], span = span)
    
    # Predict on full original rows (fills NA with NA)
    .x %>%
      mutate(!!paste0(y_name, "_loess") := predict(fit, newdata = .x[[x_name]]))
  }) %>%
    compact() %>%
    bind_rows()
}
stage<-stage%>%
  group_by(ID) %>%
  mutate(
    t = as.numeric(Date - min(Date))) %>%
  ungroup()

stage.smooth<-fit_loess_by_group(stage, depth, t, ID)

ggplot(stage.smooth%>%filter(ID=='ID'), aes(x = Date)) +
  geom_point(aes(y = depth), shape=1)+
  geom_point(aes(y = depth_loess), shape=1, color='blue')+
  
  facet_wrap(~ID, scales='free')+
  theme_minimal()

write_csv(stage, "02_Clean_data/Chem/PSI.csv")
