library(StreamLight)
library(StreamLightUtils)
library(tidyverse)

#Question for Matt: Do I need to change the defaults? ex) blank slope, bank height
#water level

#functions#####
azimuth_adj <- function(driver_file, Lat, Lon){
  driver_file$Hour <- driver_file[, "Hour"] + (1 / 60 / 24)
  return(solar_geo_calc(driver_file, Lat, Lon)$solar_azimuth2)}
#####

#Set the download location (add your own directory)
working_dir <- "C:/SpringMetabolism_FlowReversals/Stream Biomass files"
#Download NLDAS data at NC_NHC
NLDAS_DL(
  save_dir = working_dir,
  Site_ID = "ID",
  Lat = 29.935, 
  Lon = -82.800, 
  startDate = "2022-01-01"
)

#Process the downloaded data
ID_NLDAS_processed <- NLDAS_proc(
  read_dir = working_dir, 
  Site_IDs = "ICHE"
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

site_locs <- request_sites
site_locs$epsg_crs <- 4326

source("C:/SpringMetabolism_FlowReversals/Stream Biomass files/make_driver_mod.R")
driver_file.ID<-make_driver(
  site_locs,
  ID_NLDAS_processed,
  ID_mod_processed,write_output = FALSE, save_dir = NULL)

driver_file.df<-driver_file.ID$ID
Lat=29.935
Lon=-82.8

(azimuth_adj(driver_file.df, Lat, Lon))
source("C:/SpringMetabolism_FlowReversals/Stream Biomass files/extract_height_mod.R")
extract_height(
  Site_ID = 'ICHE', 
  Lat = Lat,
  Lon = Lon,
  site_crs = 4326,
  simard_loc="C:/SpringMetabolism_FlowReversals/Stream Biomass files/simard2011_SWR.asc")

test_modeled <- stream_light(
  NC_NHC_driver, 
  Lat = 29.935, 
  Lon = -82.800, 
  channel_azimuth = 1.57096, 
  bottom_width = 28, 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = 18, 
  overhang = 1.8, 
  overhang_height = NA, 
  x_LAD = 1
)
