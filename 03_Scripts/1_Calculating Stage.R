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

write_csv(PT_everything, 
          "02_Clean_data/Chem/PT.csv")

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

write_csv(FAWN_everything, 
          "02_Clean_data/Chem/FAWN.csv")

##Calculation#####
PT <- read_csv("02_Clean_data/Chem/PT.csv")
FAWN <- read_csv("02_Clean_data/Chem/FAWN.csv")

stage <- left_join(PT, FAWN)%>%
  drop_na(PSI, PT)%>%
  select(-stageID, -`BP avg (mb)`, -gageID)
  
  
conversion_factors <- data.frame(
  ID = c('OS', 'ID', 'GB', 'LF', 'AM'),
  factor = c(0.6894/0.372, 1.64, 1.47/0.634, 0.6894/0.372, 1.41/0.634),
  offset = c(0.6995, 0.11, 0.176, 0.6995, 0.515)
)

# Join and calculate
stage <- stage %>%
  left_join(conversion_factors, by = "ID") %>%
  mutate(depth = ((PT - PSI)/factor) + offset) %>%
  select(-factor, -offset)


stage.edit<-stage %>%
  mutate(
    remove=
      case_when(
        ID=='GB' & depth>2 ~'a',
        ID=='GB' & depth<0.35 ~'a',
        ID=='GB' &  Date>'2023-05-01'&Date<'2023-06-15'& depth>0.44 ~'a',
        ID=='LF' &  Date>'2023-08-20'&Date<'2023-09-04'& depth>0.53 ~'a',
        ID=='ID' &  Date>'2023-05-10'&Date<'2023-06-15'& depth>0.92 ~'a',
        ID=='OS' &  Date>'2023-08-20'&Date<'2023-09-03'& depth>1 ~'a'
      ))%>%
  filter(is.na(remove))


write_csv(stage.edit, 
          "02_Clean_data/raw.depth.csv")

###Interpolation###depth###Interpolation####

data_retrieval <- function(site_id) {
  parameterCd <- c('00065')
  startDate <- "2022-04-01"
  endDate <- "2024-10-04"
  
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

depth<-read_csv("02_Clean_data/Chem/raw.depth.csv")
x<-c('Date','elevation','ID')


AM<-filter(depth, ID=='AM')
site_id <- c('02319800','02320000')
AMinterp<-data_retrieval(site_id)%>%
  mutate(elevation= (stage_up-stage_down)*0.501)%>%
  select(Date, elevation)%>%
    mutate(ID='AM'
         )%>%
  select(x)


GB<-filter(depth, ID=='GB')
site_id <- c('02321958','02322500')
GBinterp<-data_retrieval(site_id)%>%
  mutate(elevation= (stage_up-stage_down)*0.79)%>%
  select(Date, elevation)%>%
  mutate(ID='GB'
  )%>%
  select(x)


OS<-filter(depth, ID=='OS')
site_id <- c('02323000','02323500')
OSinterp<-data_retrieval(site_id)%>%
  mutate(elevation= (stage_up-stage_down)*0.72)%>%
  select(Date, elevation)%>%
  mutate(ID='OS'
  )%>%
  select(x)


LF<-filter(depth, ID=='LF')
site_id <- '02323500'
parameterCd <- c('00065')
startDate <- "2022-04-01"
endDate <- "2024-10-04"
riverLF <- readNWISuv(site_id,parameterCd, startDate, endDate)
riverLF<-riverLF[,c(1,3,2,4)]
riverLF<-rename(riverLF, 'Date'='dateTime', 'elevation'='X_00065_00000')
riverLF<- riverLF %>% mutate(minute = minute(Date))
riverLF<-filter(riverLF, minute==0)%>%
  select(Date, elevation)%>%
  mutate(ID='LF'
  )%>%
  select(x)


SF<- read_xlsx("01_Raw_data/Hobo/PT/02322703_Level.xlsx",skip = 25)%>%
  rename('depth_gage'="Level NAVD88") %>%
  filter(Date>'2022-08-10')%>%
  mutate(
    elevation=conv_unit(depth_gage,'ft','m'),
    ID='ID'
    )%>%select(x)

confluence<-rbind(AMinterp, GBinterp, OSinterp, riverLF, SF)
write_csv(confluence, 
          "02_Clean_data/river_elevation.csv")

#edit##########
confluence <- read_csv("02_Clean_data/river_elevation.csv")
stage.edit <- read_csv("02_Clean_data/raw.depth.csv")

fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.005, min_rows = 5) {
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

confluence<-confluence%>%
  group_by(ID) %>%
  mutate(
    t = as.numeric(Date - min(Date))) %>%
  ungroup()%>%
  drop_na(elevation)


confluence.smooth<-fit_loess_by_group(confluence, elevation, t, ID)


interp <- full_join(confluence.smooth, stage.edit)%>%
  mutate(
    depth.interp=NA,
    depth.interp=if_else(ID=='LF', (elevation_loess/4.2), depth.interp),
    depth.interp=if_else(ID=='GB', (elevation_loess/3.7)+0.06, depth.interp),
    depth.interp=if_else(ID=='AM', (elevation_loess*1.6)-2.8, depth.interp),
    depth.interp=if_else(ID=='ID', (elevation_loess/1.2)-2, depth.interp),
    depth.interp=if_else(ID=='OS', (elevation_loess/2.65)-0.14, depth.interp),
    depth=if_else(is.na(depth), depth.interp, depth)
  )

stage.smooth<-fit_loess_by_group(interp, depth, t, ID)

stage.smooth.edit<-stage.smooth%>%
  mutate(
    depth_loess=if_else(ID=='AM', depth_loess-0.4, depth_loess),
    depth_loess=if_else(ID=='LF', depth_loess-0.55, depth_loess),
    depth_loess=if_else(ID=='ID', depth+0.7, depth_loess)
  )%>%
  select(Date, ID, depth_loess)%>%
  rename(depth=depth_loess)#%>%
  

# stage.smooth.edit%>%
#   ggplot(aes(x = Date)) +
#   #geom_point(aes(y = depth_loess))+
#   geom_point(aes(y = depth),color='blue', alpha=0.1)+
#   geom_hline(yintercept = 0)+
#   facet_wrap(~ID, scales='free')+
#   theme_minimal()
# 
# test<-stage.smooth.edit%>%
#   filter(ID=='ID')


write_csv(stage.smooth.edit, "02_Clean_data/Chem/depth.csv")

  


