##==============================================================================
## Script for getting StreamLight data for subset of sites
## Code author: J.R. Blaszczak
##==============================================================================
# ---
#   title: "1. Download and process NLDAS"
# author: "Phil Savoy"
# date: "5/17/2021"
# output: html_document
# ---
#Modified by Samantha T Howley
#date:06/30/2024
  
library(StreamLight)
library(StreamLightUtils)
library(tidyverse)

#Question for Matt: Do I need to change the defaults? ex) blank slope, bank height
#water level

#functions#####
azimuth_adj <- function(driver_file, Lat, Lon){
  driver_file$Hour <- driver_file[, "Hour"] + (1 / 60 / 24)
  return(solar_geo_calc(driver_file, Lat, Lon)$solar_azimuth2)}

meandaily_PARv2 <- function(y){
  df <- y %>%
    group_by(jday) %>%
    summarize_at(.vars = c("DOY","Year","PAR_surface"), .funs = mean, na.rm = TRUE)
  
  df$origin <- as.Date(paste0(df$Year, "-01-01"),tz = "UTC") - days(1)
  df$Date <- as.Date(df$DOY, origin = df$origin, tz = "UTC") 
  
  return(df)
}

estimate_par<-function(Site_ID,width, Lat, Lon,select.driver_file){
  siteID_TH_extract<-extract_height(
    Site_ID, 
    Lat,
    Lon,
    site_crs = 4326,
    simard_loc="C:/SpringMetabolism_FlowReversals/Stream Biomass files/simard2011_SWR.asc")
  
  
  siteID_TH<-siteID_TH_extract[,4]
  siteID_azimuth<-mean(azimuth_adj(select.driver_file, Lat,Lon))
  
  siteID_modeled <- stream_light(
    ID.driver_file, 
    Lat, 
    Lon, 
    channel_azimuth = siteID_azimuth, 
    bottom_width = width, 
    BH = 0.1, 
    BS = 100, 
    WL = 0.1, 
    TH = siteID_TH, 
    overhang = siteID_TH*0.1, 
    overhang_height = NA, 
    x_LAD = 1)
  
  siteID_modeled$PAR_surface[which(siteID_modeled$PAR_surface == 0)] <- NA
  siteID_modeled <- meandaily_PARv2(siteID_modeled)
  
  return(siteID_modeled)}

#working directory and site locations####

working_dir <- "C:/SpringMetabolism_FlowReversals/Stream Biomass files"
sites<- read_csv("Stream Biomass files/site_locs.csv")
#NLDAS#########
#Download NLDAS data at NC_NHC

NLDAS_DL_bulk(save_dir = "Stream Biomass files/NLDAS",
              site_locs = sites, startDate = "2022-05-01")

NLDAS_list <- stringr::str_sub(list.files("C:/SpringMetabolism_FlowReversals/Stream Biomass files/NLDAS"), 1, -11)

NLDAS_processed <- StreamLightUtils::NLDAS_proc(read_dir = "C:/SpringMetabolism_FlowReversals/Stream Biomass files/NLDAS",Site_IDs = NLDAS_list)


#MODIS######

#Export your sites as a .csv for the AppEEARS request  
write.table(
  sites, 
  paste0(working_dir, "MODIS/NC_sites.csv"), 
  sep = ",", 
  row.names = FALSE,
  quote = FALSE, 
  col.names = FALSE
)

#navigate to (https://lpdaacsvc.cr.usgs.gov/appeears/ "LP DAAC AppEEARS")
#Type MCD15A3H into the layers search box and select the product. Add all of the layers to the selected layers. 
MOD_unpack <- AppEEARS_unpack_QC(
  zip_file = "MODIS_06252024.zip", 
  zip_dir = "C:/SpringMetabolism_FlowReversals/Stream Biomass files/MODIS", 
  c("ID", "IU", "GB", "AM", "OS", "LF")
)

#graph modUnpacked
MOD_processed <- AppEEARS_proc(
  unpacked_LAI = MOD_unpack,  
  fit_method = "Gu", 
  plot = TRUE
)

#driver file#####
site_locs <- sites
site_locs$epsg_crs <- 4326

source("C:/SpringMetabolism_FlowReversals/Stream Biomass files/make_driver_mod.R")
driver_file<-make_driver(
  site_locs,
  NLDAS_processed,
  MOD_processed,write_output = FALSE, save_dir = NULL)



#Run Stream Light########

source("C:/SpringMetabolism_FlowReversals/Stream Biomass files/extract_height_mod.R")

ID.driver_file<-driver_file$ID
ID_modeled<-estimate_par(Site_ID= "ID",
                         width=28,
                         Lat=site_locs$Lat[1],
                         Lon=site_locs$Lon[1],
                         ID.driver_file)
#ggplot(IU_modeled, aes(Date, PAR_surface))+geom_line()



IU.driver_file<-driver_file$IU
IU_modeled<-estimate_par(Site_ID= "IU",
                         width=28,
                         Lat=site_locs$Lat[2],
                         Lon=site_locs$Lon[2],
                         IU.driver_file)
#ggplot(IU_modeled, aes(Date, PAR_surface))+geom_line()



GB.driver_file<-driver_file$GB
GB_modeled<-estimate_par(Site_ID= "GB",
                         width=23,
                         Lat=site_locs$Lat[3],
                         Lon=site_locs$Lon[3],
                         GB.driver_file)
#ggplot(GB_modeled, aes(Date, PAR_surface))+geom_line()


AM.driver_file<-driver_file$AM
AM_modeled<-estimate_par(Site_ID= "AM",
                         width=24,
                         Lat=site_locs$Lat[4],
                         Lon=site_locs$Lon[4],
                         AM.driver_file)
#ggplot(AM_modeled, aes(Date, PAR_surface))+geom_line()



OS.driver_file<-driver_file$OS
OS_modeled<-estimate_par(Site_ID= "OS",
                         width=17,
                         Lat=site_locs$Lat[5],
                         Lon=site_locs$Lon[5],
                         OS.driver_file)
#ggplot(OS_modeled, aes(Date, PAR_surface))+geom_line()



LF.driver_file<-driver_file$LF
LF_modeled<-estimate_par(Site_ID= "LF",
                         width=20,
                         Lat=site_locs$Lat[6],
                         Lon=site_locs$Lon[6],
                         LF.driver_file)
#ggplot(LF_modeled, aes(Date, PAR_surface))+geom_line()





LF_modeled$ID<-'LF'
AM_modeled$ID<-'AM'
GB_modeled$ID<-'GB'
IU_modeled$ID<-'IU'
ID_modeled$ID<-'ID'
OS_modeled$ID<-'OS'

all_site_light <- rbind(ID_modeled, IU_modeled, GB_modeled,LF_modeled,AM_modeled,AM_modeled)

saveRDS(all_site_light, "C:/SpringMetabolism_FlowReversals/Stream Biomass files/StreamLight_daily.rds")
#q TWO YR FLOOD#####
two_year_flood <- function(Q){
  
  #Grouping minimum average daily discharge for each year
  min.by.year<-Q %>% group_by(year=floor_date(Date, "year")) %>% summarize(amount=min(Q_m.s))
  
  
  #Recording the minimum discharges by year and removing N.A. values
  minimas<-min.by.year$amount
  minimas<-minimas[!is.na(minimas)]
  
  #Sorting minima by decreasing order
  sorted.minimas<-as.data.frame(sort(minimas,decreasing=T))
  sorted.minimas$rank <- seq(length=nrow(sorted.minimas))
  colnames(sorted.minimas) <- c("Q_min","rank")
  
  #Fit relationship
  sorted.minimas$ln_Q_min <- log(abs(sorted.minimas$Q_min))
  sorted.minimas$exceed_prob <- sorted.minimas$rank/(length(sorted.minimas$rank)+1)
  
  #visualize
  ggplot(sorted.minimas, aes(ln_Q_min, exceed_prob))+
    geom_point()+
    geom_smooth(method = "lm")+
    scale_y_reverse()
  
  #extract coefficients and calc 
  mod <- lm(exceed_prob ~ ln_Q_min, data = sorted.minimas)
  l <- as.data.frame(t(as.matrix(coefficients(mod))))
  colnames(l) <- c("int","slope")
  yr_2 <- exp((0.5 - l$int)/l$slope)
  
  return(yr_2)
  
}

library(plyr)
library(tidyverse)
Q<-read_csv("02_Clean_data/discharge.csv")
Q_daily<-Q %>%mutate(Date=as.Date(Date))
Q_daily <- Q_daily[!duplicated(Q_daily[c('Date','ID')]),]


sites<-split(Q_daily, Q_daily$ID)
RI_two <- ldply(lapply(sites, function(y) two_year_flood(y)), data.frame)

write.csv(RI_two, "Stream Biomass files/RI_2yr_flood_6riv.csv") ## in cfs
