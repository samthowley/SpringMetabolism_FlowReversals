library(StreamLight)
library(StreamLightUtils)
library(tidyverse)
#functions#####
azimuth_adj <- function(driver_file, Lat, Lon){
  driver_file$Hour <- driver_file[, "Hour"] + (1 / 60 / 24)
  return(solar_geo_calc(driver_file, Lat, Lon)$solar_azimuth2)}

#####

#Set the download location (add your own directory)
working_dir <- "C:/SpringMetabolism_FlowReversals"
#29.935078600027087, -82.80015641597691
#Download NLDAS data at NC_NHC
NLDAS_DL(
  save_dir = working_dir,
  Site_ID = "NC_NHC",
  Lat = 29.935, 
  Lon = -82.800, 
  startDate = "2022-01-01"
)

#Process the downloaded data
ID_NLDAS_processed <- NLDAS_proc(
  read_dir = working_dir, 
  Site_IDs = "NC_NHC"
)

#Make a table for the MODIS request 
sites <- read_csv("Modis practice.csv")
request_sites <- sites[, c("Site_ID", "Lat", "Lon")] 

#Export your sites as a .csv for the AppEEARS request  
write.table(
  request_sites, 
  paste0(working_dir, "/NC_sites.csv"), 
  sep = ",", 
  row.names = FALSE,
  quote = FALSE, 
  col.names = FALSE
)

MOD_unpack <- AppEEARS_unpack_QC(
  zip_file = "modis practice.zip", 
  zip_dir = working_dir, 
  request_sites[, "Site_ID"]
)

#graph modUnpacked
ID_mod_processed <- AppEEARS_proc(
  unpacked_LAI = MOD_unpack,  
  fit_method = "Gu", 
  plot = TRUE
)

driver_file.ID<-make_driver(
  site_locs = sites,
  NLDAS_processed = ID_NLDAS_processed,
  MOD_processed = ID_mod_processed
)

#Question for Matt: Do I need to change the defaults? ex) blank slope, bank height
#water level
Lat=29.935
Lon=-82.8
ID_azimuth<-azimuth_adj(driver_file.ID, Lat, Lon)

NC_NHC_modeled <- stream_light(
  NC_NHC_driver, 
  Lat = 29.935, 
  Lon = -82.800, 
  channel_azimuth = 330, 
  bottom_width = 18.9, 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = 23, 
  overhang = 2.3, 
  overhang_height = NA, 
  x_LAD = 1
)