lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "parallel","tidyverse","rstan","devtools","shinystan",
         "MCMCglmm"), require, character.only=T)
library(streamMetabolizer)

#rm(data, l, SL, data_siteyears, dat)

#create required dataset####
chem <- read_csv("02_Clean_data/master_chem1.csv")
Q <- read_csv("02_Clean_data/discharge.csv")

site_locs<- read_csv("Stream Biomass files/site_locs.csv")
sites<-split(chem,chem$ID)
#names(sites)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]

light_calc <- function(site, Lat, Lon){
  site$solar.time <-as.POSIXct(site$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  site$light<-calc_light(site$solar.time,  Lat, Lon)
  return(site)}
IU<-light_calc(IU, site_locs$Lat[1], site_locs$Lat[1])
ID<-light_calc(ID, site_locs$Lat[2], site_locs$Lat[2])
GB<-light_calc(GB, site_locs$Lat[3], site_locs$Lat[3])
LF<-light_calc(IU, site_locs$Lat[4], site_locs$Lat[4])
AM<-light_calc(AM, site_locs$Lat[5], site_locs$Lat[5])
OS<-light_calc(OS, site_locs$Lat[6], site_locs$Lat[6])

light<-rbind(IU, ID, GB, LF, AM, OS)
light<-light[,c('Date','ID','light')]
light$light[light$light<=0]<-NA
light<-light %>%mutate(Date=as.Date(Date)) %>% group_by(Date, ID) %>% mutate(light=mean(light, na.rm=T))
light<-left_join(light, Q, by=c('Date', 'ID'))
light<-light[,c('Date','ID','light','Q_m.s')]
light <- light[!duplicated(light[c('Date','ID')]),]


met <- read_csv("02_Clean_data/master_metabolism4.csv")
met<-left_join(met, light, by=c('Date','ID'))

SL <- readRDS("Stream Biomass files/StreamLight_daily.rds")

met.SL <- left_join(met, SL, by=c("ID", "Date"))
met.SL <- met.SL[!duplicated(met.SL[c('Date','ID')]),]
met.SL <- met.SL[complete.cases(met.SL[ , c('GPP','Temp','Q_m.s','PAR_surface','light')]), ]

site_order_list <- c("IU","ID","GB",
                     "LF","AM","OS")
met.SL[which(met.SL$GPP < 0),]$GPP <- sample(exp(-3):exp(-2), 1)
met.SL<-met.SL %>% group_by(ID)%>% mutate(GPP_sd=sd(GPP, na.rm = T))
#met.SL$GPP_sd <- (((met.SL$GPP.upper - met.SL$GPP)/1.96) + ((met.SL$GPP.lower - met.SL$GPP)/-1.96))/2

data <- rbind(data[which(data$site_name == "nwis_02336526" & data$year %in% c(2015)),],
              data[which(data$site_name == "nwis_01649190" & data$year %in% c(2010)),],
              data[which(data$site_name == "nwis_07191222" & data$year %in% c(2009)),],
              data[which(data$site_name == "nwis_01608500" & data$year %in% c(2012)),],
              data[which(data$site_name == "nwis_11044000" & data$year %in% c(2015)),],
              data[which(data$site_name == "nwis_08447300" & data$year %in% c(2012)),])


l <- split(met.SL, met.SL$ID)
#normalizing?
rel_LQT <- function(x){
  x$light_rel_PPFD <- x$light/max(x$light)
  x$light_rel_PAR <- x$PAR_surface/max(x$PAR_surface)
  x$temp_rel <- x$Temp/max(x$Temp)
  x$tQ <- x$Q_m.s/max(x$Q_m.s)
  
  x<-x[order(x$Date),]
  return(x)
}

dat <- lapply(l, function(x) rel_LQT(x))
df <- dat

#Biomass Recovery Models####
#ex <- df$IU #South Branch Potomac River, WV

stan_data_compile_PAR <- function(x){
  data <- list(Ndays=length(x$GPP), light = x$light_rel_PAR, GPP = x$GPP,
               GPP_sd = x$GPP_sd, tQ = x$tQ)
  return(data)
}

stan_data_PAR <- lapply(df, function(x) stan_data_compile_PAR(x))

rstan_options(auto_write=TRUE)
options(mc.cores= 6) 
# parallel::detectCores()

## S-TS model
STS_output_PAR <- lapply(stan_data_PAR,
                         function(x) stan("Stream Biomass files/Stan_ProductivityModel1_STS.stan",
                                          data=x,chains=4,iter=1000,
                                          control=list(max_treedepth=12, adapt_delta = 0.95)))

## LB-TS model
init_Ricker <- function(...) {
  list(c = 0.5, s = 1.5)
}

LBTS_output_PAR <- lapply(stan_data_PAR,
                          function(x) stan("Stream Biomass files/Stan_ProductivityModel2_LBTS.stan",
                                           data=x,chains=4,iter=1000,init = init_Ricker,
                                           control=list(max_treedepth=12, adapt_delta = 0.95)))

## Save initial model fit
init_model_output <- list(STS_output_PAR, LBTS_output_PAR)
saveRDS(init_model_output, "Stream Biomass files/Param_Rec_Test_init_output_20240702.rds")
rm(init_model_output); rm(STS_output_PAR); rm(LBTS_output_PAR)


#Extract Model Parameters########

## Extract parameter estimates from simulation
init_model_output <- readRDS("code/rds files/Param_Rec_Test_init_output_2022_07_10.rds")
STS_output_PAR <- init_model_output[[1]]
LBTS_output_PAR <- init_model_output[[2]]

#extract
p_STS <- lapply(STS_output_PAR, function(x) extract(x, c("phi","alpha","beta","sig_p","sig_o")))
p_LBTS<- lapply(LBTS_output_PAR, function(x) extract(x, c("r","lambda","s","c","sig_p","sig_o")))

#mean and sd
mean_STS <- lapply(p_STS, function(x) lapply(x, function(y) mean(y)))
sd_STS <- lapply(p_STS, function(x) lapply(x, function(y) sd(y)))
mean_LBTS <- lapply(p_LBTS, function(x) lapply(x, function(y) mean(y)))
sd_LBTS <- lapply(p_LBTS, function(x) lapply(x, function(y) sd(y)))

##############################################################
## (5) Simulate GPP ts using extracted parameter estimates
##############################################################

## Bring in simulation code
source("Stream Biomass files/Predicted_ProductivityModel_STS.R") 
source("Stream Biomass files/Predicted_ProductivityModel_LBTS.R")

## STS - simulate GPP again for recovery test data set
Pot_STS_GPP <- PM_AR(phi=mean_STS$Potomac$phi,
                     alpha=mean_STS$Potomac$alpha,
                     beta=mean_STS$Potomac$beta,
                     sig_p=mean_STS$Potomac$sig_p,
                     sig_o=mean_STS$Potomac$sig_o, df=ex)
Paint_STS_GPP <- PM_AR(phi=mean_STS$`Paint Branch`$phi,
                       alpha=mean_STS$`Paint Branch`$alpha,
                       beta=mean_STS$`Paint Branch`$beta,
                       sig_p=mean_STS$`Paint Branch`$sig_p,
                       sig_o=mean_STS$`Paint Branch`$sig_o, df=ex2)
plot(1:length(Pot_STS_GPP), Pot_STS_GPP, type="l")
points(ex$GPP) #good
plot(1:length(Paint_STS_GPP), Paint_STS_GPP, type="l")
points(ex2$GPP) #divergent fall peak in predicted (line) GPP

## LBTS - simulate GPP again for recovery test data set
Pot_LBTS_GPP <- PM_Ricker(r = mean_LBTS$Potomac$r,
                          lambda = mean_LBTS$Potomac$lambda,
                          s = mean_LBTS$Potomac$s,
                          c = mean_LBTS$Potomac$c, 
                          sig_p = mean_LBTS$Potomac$sig_p,
                          sig_o = mean_LBTS$Potomac$sig_o, df = ex)
Paint_LBTS_GPP <- PM_Ricker(r = mean_LBTS$`Paint Branch`$r,
                            lambda = mean_LBTS$`Paint Branch`$lambda,
                            s = mean_LBTS$`Paint Branch`$s,
                            c = mean_LBTS$`Paint Branch`$c, 
                            sig_p = mean_LBTS$`Paint Branch`$sig_p,
                            sig_o = mean_LBTS$`Paint Branch`$sig_o, df = ex2)
plot(1:length(Pot_LBTS_GPP), Pot_LBTS_GPP, type="l")
points(ex$GPP) #good
plot(1:length(Paint_LBTS_GPP), Paint_LBTS_GPP, type="l")
points(ex2$GPP) #much better than S-TS prediction, just delayed spring peak


#############################################################################
## (6) Replace original GPP with predicted GPP in stan data list
#############################################################################

## Compile for Stan again
# STS
stan_simPot_STS <- list(Ndays=length(Pot_STS_GPP), light=ex$light_rel_PAR, GPP = Pot_STS_GPP,
                        prior_sig_o_mean = mean_STS$Potomac$sig_o,
                        prior_sig_o_sd = sd_STS$Potomac$sig_o, tQ = ex$tQ)
stan_simPaint_STS <- list(Ndays=length(Paint_STS_GPP), light=ex2$light_rel_PAR, GPP = Paint_STS_GPP,
                          prior_sig_o_mean = mean_STS$`Paint Branch`$sig_o,
                          prior_sig_o_sd = sd_STS$`Paint Branch`$sig_o, tQ = ex2$tQ)
stan_STS_sim_list <- list("Potomac_STS"=stan_simPot_STS,
                          "PaintBranch_STS"=stan_simPaint_STS)
#LBTS
stan_simPot_LBTS <- list(Ndays=length(Pot_LBTS_GPP), light=ex$light_rel_PAR, GPP = Pot_LBTS_GPP,
                         prior_sig_o_mean = mean_LBTS$Potomac$sig_o,
                         prior_sig_o_sd = sd_LBTS$Potomac$sig_o, tQ = ex$tQ)
stan_simPaint_LBTS <- list(Ndays=length(Paint_LBTS_GPP), light=ex2$light_rel_PAR, GPP = Paint_LBTS_GPP,
                           prior_sig_o_mean = mean_LBTS$`Paint Branch`$sig_o,
                           prior_sig_o_sd = sd_LBTS$`Paint Branch`$sig_o, tQ = ex2$tQ)
stan_LBTS_sim_list <- list("Potomac_LBTS"=stan_simPot_LBTS,
                           "PaintBranch_LBTS"=stan_simPaint_LBTS)

###########################################################################
## (7) Fit models to simulated data from initial parameter estimates
###########################################################################
rstan_options(auto_write=TRUE)
options(mc.cores=8)#parallel::detectCores())
#STS
recov_STS_output <- lapply(stan_STS_sim_list,
                           function(x) stan("code/Stan_ProductivityModel1_STS_recovery_simulation.stan",
                                            data=x,chains=4,iter=5000,
                                            control=list(max_treedepth=12, adapt_delta = 0.95)))

#LBTS
init_Ricker <- function(...) {
  list(c = 0.5, s = 1.5)
}
recov_LBTS_output <- lapply(stan_LBTS_sim_list,
                            function(x) stan("Stan_ProductivityModel2_LBTS_recovery_simulation.stan",
                                             data=x,chains=4,iter=5000,init = init_Ricker,
                                             control=list(max_treedepth=12)))

## Save parameter recovery model fit
recovery_model_output <- list(recov_STS_output, recov_LBTS_output)
saveRDS(recovery_model_output, "./rds files/Param_Rec_Test_recovery_output_2022_07_10.rds")
rm(recovery_model_output); rm(recov_STS_output); rm(recov_LBTS_output)


#################################################################################
## (8) Compare parameters from simulation to posterior distributions
################################################################################

recovery_model_output <- readRDS("./rds files/Param_Rec_Test_recovery_output_2022_07_10.rds")
recov_STS_output <- recovery_model_output[[1]]
recov_LBTS_output <- recovery_model_output[[2]]

## STS - vis_recovery
STS_sim_recp <- lapply(recov_STS_output, function(x) ldply(extract(x, c("phi","alpha","beta","sig_p","sig_o")),data.frame))
# need STS_sim_recp, and mean_STS from part 4
STS_vis_recovery <- function(final_distributions, used_parameters, plot.title){
  
  ## distributions of most recent fit
  recpars <- final_distributions
  colnames(recpars) <- c("parameter","value")
  
  ## params used to simulate
  orig_meanpars <- ldply(used_parameters, data.frame)
  colnames(orig_meanpars) <- c("parameter","value")
  
  
  ggplot(recpars, aes(value))+
    geom_density(fill="red", alpha=0.2)+
    facet_wrap(~parameter, scales = "free")+
    geom_vline(data=orig_meanpars,aes(xintercept = value))+
    labs(x="Value",y="Density",title=plot.title)+
    theme(legend.position = "none",
          strip.background = element_rect(fill="white", color="black"),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=12, angle = 45, hjust = 0.5),
          axis.text.y = element_text(size=12),
          axis.title = element_text(size=14), title = element_text(size=14))
}

STS_vis_recovery(STS_sim_recp$Potomac_STS, mean_STS$Potomac, "S-TS Potomac Parameter Recovery")
STS_vis_recovery(STS_sim_recp$PaintBranch_STS, mean_STS$`Paint Branch`, "S-TS Paint Branch Parameter Recovery")


## LBTS - vis_recovery
LBTS_sim_recp <- lapply(recov_LBTS_output, function(x) ldply(extract(x, c("r","lambda","s","c","sig_p","sig_o")),data.frame))
# need LBTS_sim_recp, and mean_LBTS from part 4
LBTS_vis_recovery <- function(final_distributions, used_parameters, plot.title){
  
  ## distributions of most recent fit
  recpars <- final_distributions
  colnames(recpars) <- c("parameter","value")
  
  ## params used to simulate
  orig_meanpars <- ldply(used_parameters, data.frame)
  colnames(orig_meanpars) <- c("parameter","value")
  
  
  ggplot(recpars, aes(value))+
    geom_density(fill="red", alpha=0.2)+
    facet_wrap(~parameter, scales = "free")+
    geom_vline(data=orig_meanpars,aes(xintercept = value))+
    labs(x="Value",y="Density",title=plot.title)+
    theme(legend.position = "none",
          strip.background = element_rect(fill="white", color="black"),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=12, angle = 45, hjust = 0.5),
          axis.text.y = element_text(size=12),
          axis.title = element_text(size=14), title = element_text(size=14))
}

LBTS_vis_recovery(LBTS_sim_recp$Potomac_LBTS, mean_LBTS$Potomac, "LB-TS Potomac Parameter Recovery")
LBTS_vis_recovery(LBTS_sim_recp$PaintBranch_LBTS, mean_LBTS$`Paint Branch`, "LB-TS Paint Branch Parameter Recovery")


