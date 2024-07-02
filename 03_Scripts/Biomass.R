lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "parallel","tidyverse","rstan","devtools","shinystan",
         "MCMCglmm"), require, character.only=T)
#rm(data, l, SL, data_siteyears, dat)

#combine SL and met
met <- read_csv("02_Clean_data/master_metabolism4.csv")

SL <- readRDS("Stream Biomass files/StreamLight_daily.rds")
#colnames(SL)[colnames(SL) == "Date"] <- "date"
met.SL <- left_join(met, SL, by=c("ID", "Date"))
met.SL <- met.SL[!duplicated(met.SL[c('Date','ID')]),]


site_order_list <- c("IU","ID","GB",
                     "LF","AM","OS")
met.SL[which(met.SL$GPP < 0),]$GPP <- sample(exp(-3):exp(-2), 1)
met.SL$GPP_sd<-sd(met.SL$GPP, na.rm = T)
#met.SL$GPP_sd <- (((met.SL$GPP.upper - met.SL$GPP)/1.96) + ((met.SL$GPP.lower - met.SL$GPP)/-1.96))/2

l <- split(met.SL, met.SL$ID)

#normalizing?
rel_LQT <- function(x){
  x$light_rel_PPFD <- x$light/max(x$light)
  x$light_rel_PAR <- x$PAR_surface/max(x$PAR_surface)
  x$temp_rel <- x$temp/max(x$temp)
  x$tQ <- x$Q/max(x$Q)
  
  x<-x[order(x$date),]
  return(x)
}

dat <- lapply(l, function(x) rel_LQT(x))
df <- dat

rm(met.SL, l, SL, met.SL_siteyears, dat)

#Biomass Recovery####

stan_data_compile_PAR <- function(x){
  data <- list(Ndays=length(x$GPP), light = x$light_rel_PAR, GPP = x$GPP,
               GPP_sd = x$GPP_sd, tQ = x$tQ)
  return(data)
}

stan_data_PAR <- lapply(list("Potomac" = ex, "Paint Branch" = ex2), function(x) stan_data_compile_PAR(x))
