make_driver <- function(site_locs, NLDAS_processed, MOD_processed, write_output = FALSE, save_dir = NULL){
  #Find sites that have both NLDAS and MODIS data
  site_intersect <- Reduce(intersect, list(names(ID_NLDAS_processed), names(ID_mod_processed),
                                           site_locs$Site_ID))
  
  #Function for merging and making a driver file
  driver_merge <- function(Site, write_output, save_dir){
    #Merging all of the dataframes together where all data is present
    merged <- merge(NLDAS_processed[[Site]], MOD_processed[[Site]], by = c("Year",
                                                                           "DOY"))
    
    #Placing the data in the correct order
    ordered <- merged[order(merged[, "Year"], merged[, "DOY"], merged[, "Hour"]), ]
    
    #-------------------------------------------------
    #Converting from UTC to local time
    #-------------------------------------------------
    #Adding POSIX time column in UTC
    ordered$UTC_time <- as.POSIXct(paste(ordered[, "Date"], " ", sprintf("%02d",
                                                                         ordered[, "Hour"]), sep = ""), format = "%Y-%m-%d %H", tz = "UTC")
    
    #Getting the name of the local timezone
    Lat <- site_locs[site_locs$Site_ID == Site, "Lat"]
    Lon <- site_locs[site_locs$Site_ID == Site, "Lon"]
    
    tz_name <- get_tz(Lat = Lat, Lon = Lon, site_crs = 4326)
    
    #Convert UTC to local time
    ordered$local_time <- as.POSIXct(format(ordered[, "UTC_time"], format =
                                              "%Y-%m-%d %H:%M:%S", tz = tz_name), tz = tz_name)
    
    #Calculate offset (used by solar_c function)
    offset <- as.numeric(ordered[, "local_time"] - as.POSIXct(format(ordered[, "local_time"],
                                                                     format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
    
    #TEMPORARY ERROR CATCH TO GET CORRECT SIGN
    if(Lon < 0){offset <- -offset}
    
    #Make a new table with all of the local time information
    local <- setNames(data.frame(ordered[, "local_time"], offset,
                                 as.numeric(strftime(ordered[, "local_time"], format = "%Y", tz = tz_name)),
                                 as.numeric(strftime(ordered[, "local_time"], format = "%j", tz = tz_name)),
                                 as.numeric(strftime(ordered[, "local_time"], format = "%H", tz = tz_name)),
                                 ordered[, "SW"], ordered[, "LAI_proc"]),
                      c("local_time", "offset", "Year", "DOY", "Hour", "SW_inc", "LAI"))
    
    #-------------------------------------------------
    #Assembling the final table
    #-------------------------------------------------
    #Making jday so I have a unique identifier
    local$jday <- as.matrix(as.numeric(paste(local[, "Year"],
                                             sprintf("%03d", local[, "DOY"]), sep = "")))
    
    final <-  local[, c("local_time", "offset", "jday", "Year", "DOY", "Hour",
                        "SW_inc", "LAI")]
    
    #If write_output == TRUE, save the driver to disk
    if(write_output == TRUE){
      saveRDS(final, paste0(save_dir, "/", Site, "_driver.rds"))
    } else{
      return(final)
    } #End if else statement
    
  } #End driver_merge function
  
  #Apply the function to make all driver files
  if(write_output == TRUE){
    lapply(site_intersect, FUN = driver_merge, write_output = write_output, save_dir = save_dir)
  } else{
    driver_final <- lapply(site_intersect, FUN = driver_merge, write_output = write_output)
    names(driver_final) <- site_intersect
    
    return(driver_final)
  } #End if else statement
  
}