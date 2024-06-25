# stream light practice
library(StreamLight)
library(StreamLightUtils)

working_dir<-'01_Raw_data'
#Download NLDAS data at NC_NHC
NLDAS_DL(
  save_dir = working_dir,
  Site_ID = "NC_NHC",
  Lat = 29.932678, 
  Lon = -82.800051, 
  startDate = "2017-01-01"
)

#Process the downloaded data (individual)
NLDAS_processed <- NLDAS_proc(
  read_dir = working_dir, 
  Site_IDs = site.number
)

#Download NLDAS data in bulk
NLDAS_DL_bulk(
  save_dir = working_dir,
  site_locs = sites
)

#List of successfully downloaded sites
NLDAS_list <- stringr::str_sub(list.files(working_dir), 1, -11)
NLDAS_list <- intersect(NLDAS_list, sites$Site_ID)
NLDAS_list <- NLDAS_list[-c(93:95)]

#Processing the downloaded NLDAS data
NLDAS_processed <- StreamLightUtils::NLDAS_proc(read_dir = working_dir, NLDAS_list)
NLDAS_processed <- NLDAS_processed2

str(request_sites)
#Make a table for the MODIS request 
request_sites <- sites[, c("Site_ID", "Lat", "Lon")] 
request_sites$Site_ID <- as.numeric(request_sites$Site_ID)
#Export your sites as a .csv for the AppEEARS request  
write.table(
  request_sites, 
  paste0(working_dir, "/LAI.csv"), 
  sep = ",", 
  row.names = FALSE,
  quote = FALSE, 
  col.names = FALSE
)
install.packages('evoper')
library(evoper)
str(lai$ID[2])
lai <- read.csv('USMetabolism_LAI/USMetabolism-LAI-MCD15A3H-006-results.csv')
for(i in 1:nrow(lai)){
  if(Magnitude(as.numeric(lai$ID[i])) == 6){lai$ID[i] <- as.character(paste0('nwis_','0', lai$ID[i]))}
}
write.csv(lai, 'USMetabolism_LAI/USMetabolism-LAI-MCD15A3H-006-results.csv', row.names=F)

#unpack zip file
MOD_unpack <- AppEEARS_unpack_QC(
  zip_file = "USMetabolism_LAI.zip", 
  zip_dir = working_dir, 
  request_sites[, "Site_ID"]
)

MOD_processed <- AppEEARS_proc(
  unpacked_LAI = MOD_unpack,  
  fit_method = "Gu", 
  plot = TRUE
)

saveRDS(MOD_processed, file="LAI.RData")
MOD_processed <- load('LAI.RData')

sites$crs <- '4326'
sites <- sites[,-c(4)]
site_crs='4326'
#Make the driver files
make_driver(
  site_locs = sites,
  NLDAS_processed = NLDAS_processed,
  MOD_processed = MOD_processed,
  
)

site_cr



#########################################################################
modis <- read.csv('streamlight\\USMetabolism_LAI\\USMetabolism-LAI-results.csv')
modis.split <- split(modis, modis$ID)

for(i in 1:nrow(sites)){
  
  site.name <- sites$Site_ID[i]
  site.number <- as.numeric(sites$Site_ID[i])
  modis.site <- subset(modis, modis$ID==site.number)
  
  save.file <- paste0('streamlight/USMetabolism_LAI/',site.name,'.csv')
  write.csv(modis.site, save.file, row.names=F)
  
}

sites <- sites[,-4]
S