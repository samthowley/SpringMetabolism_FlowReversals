lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "parallel","tidyverse","rstan","devtools","shinystan",
         "MCMCglmm"), require, character.only=T)
library(streamMetabolizer)

#############################
#1. Create required dataset####
############################
chem <- read_csv("02_Clean_data/master_chem1.csv") #for temp
Q <- read_csv("02_Clean_data/discharge.csv") # for discharge

####eventually the light section wont be needed

met <- read_csv("02_Clean_data/master_metabolism4.csv") #GPP data
met<-left_join(met, light, by=c('Date','ID'))

SL <- readRDS("Stream Biomass files/Outputs/StreamLight_daily.rds") #Stream Light

met.SL <- left_join(met, SL, by=c("ID", "Date"))
met.SL <- met.SL[!duplicated(met.SL[c('Date','ID')]),]
met.SL <- met.SL[complete.cases(met.SL[ , c('GPP','Temp','Q_m.s','PAR_surface','light')]), ]

site_order_list <- c("IU","ID","GB",
                     "LF","AM","OS")
met.SL[which(met.SL$GPP < 0),]$GPP <- sample(exp(-3):exp(-2), 1)
met.SL<-met.SL %>% group_by(ID)%>% mutate(GPP_sd=sd(GPP, na.rm = T))
#met.SL$GPP_sd <- (((met.SL$GPP.upper - met.SL$GPP)/1.96) + ((met.SL$GPP.lower - met.SL$GPP)/-1.96))/2

met.SL<-filter(met.SL, Date< '2023-05-12') #select for the first year

l <- split(met.SL, met.SL$ID)
rel_LQT <- function(x){
  x$light_rel_PPFD <- x$light/max(x$light)
  x$light_rel_PAR <- x$PAR_surface/max(x$PAR_surface)
  x$temp_rel <- x$Temp/max(x$Temp)
  x$tQ <- x$Q_m.s/max(x$Q_m.s)
  
  x<-x[order(x$Date),]
  return(x)
} #relative, normalize

dat <- lapply(l, function(x) rel_LQT(x)) #apply relaove function to all
complete_list_OG <- dat

stan_data_compile_PAR <- function(x){
  data <- list(Ndays=length(x$GPP), light = x$light_rel_PAR, GPP = x$GPP,
               GPP_sd = x$GPP_sd, tQ = x$tQ)
  return(data)
}

stan_data_PAR <- lapply(complete_list_OG, function(x) stan_data_compile_PAR(x))

###############################
#fit first year of data####
###############################
rstan_options(auto_write=TRUE)
options(mc.cores= 6) 
# parallel::detectCores()

## S-TS model
STS_output_PAR <- lapply(stan_data_PAR,
                         function(x) stan("Stream Biomass files/functions and stans/Stan_ProductivityModel1_STS.stan",
                                          data=x,chains=4,iter=1000,
                                          control=list(max_treedepth=12, adapt_delta = 0.95)))

## LB-TS model
init_Ricker <- function(...) {
  list(c = 0.5, s = 1.5)
}

LBTS_output_PAR <- lapply(stan_data_PAR,
                          function(x) stan("Stream Biomass files/functions and stans/Stan_ProductivityModel2_LBTS.stan",
                                           data=x,chains=4,iter=1000,init = init_Ricker,
                                           control=list(max_treedepth=12, adapt_delta = 0.95)))

## Save initial model fit
init_model_output <- list(STS_output_PAR, LBTS_output_PAR)
saveRDS(init_model_output, "Stream Biomass files/Param_Rec_Test_init_output_20240702.rds")

source("Stream Biomass files/functions and stans/Predicted_ProductivityModel_LBTS.R")
## Extract parameter estimates from simulation
init_model_output <- readRDS("Stream Biomass files/Param_Rec_Test_init_output_20240702.rds")
STS_output_PAR <- init_model_output[[1]]
LBTS_output_PAR <- init_model_output[[2]]


p_STS <- lapply(STS_output_PAR, function(x) extract(x, c("phi","alpha","beta","sig_p","sig_o")))
p_LBTS<- lapply(LBTS_output_PAR, function(x) extract(x, c("r","lambda","s","c","sig_p","sig_o")))

mean_STS <- lapply(p_STS, function(x) lapply(x, function(y) mean(y)))
sd_STS <- lapply(p_STS, function(x) lapply(x, function(y) sd(y)))
mean_LBTS <- lapply(p_LBTS, function(x) lapply(x, function(y) mean(y)))
sd_LBTS <- lapply(p_LBTS, function(x) lapply(x, function(y) sd(y)))

##############################################################
## (5) Simulate GPP ts using extracted parameter estimates
##############################################################

## Bring in simulation code
source("Stream Biomass files/functions and stans/Predicted_ProductivityModel_STS.R") 
source("Stream Biomass files/functions and stans/Predicted_ProductivityModel_LBTS.R")

## check models
AM_STS_GPP <- PM_AR(phi=mean_STS$AM$phi,
                     alpha=mean_STS$AM$alpha,
                     beta=mean_STS$AM$beta,
                     sig_p=mean_STS$AM$sig_p,
                     sig_o=mean_STS$AM$sig_o, 
                    df=complete_list_OG$AM)
IU_STS_GPP <- PM_AR(phi=mean_STS$IU$phi,
                       alpha=mean_STS$IU$alpha,
                       beta=mean_STS$IU$beta,
                       sig_p=mean_STS$IU$sig_p,
                       sig_o=mean_STS$IU$sig_o, df=complete_list_OG$IU)
# plot(1:length(AM_STS_GPP), AM_STS_GPP, type="l") #better
# points(complete_list_OG$AM$GPP) 
# plot(1:length(IU_STS_GPP), IU_STS_GPP, type="l")
# points(complete_list_OG$IU$GPP) 

## LBTS - simulate GPP again for recovery test data set
AM_LBTS_GPP <- PM_Ricker(r = mean_LBTS$AM$r,
                          lambda = mean_LBTS$AM$lambda,
                          s = mean_LBTS$AM$s,
                          c = mean_LBTS$AM$c, 
                          sig_p = mean_LBTS$AM$sig_p,
                          sig_o = mean_LBTS$AM$sig_o, df = complete_list_OG$AM)
IU_LBTS_GPP <- PM_Ricker(r = mean_LBTS$IU$r,
                            lambda = mean_LBTS$IU$lambda,
                            s = mean_LBTS$IU$s,
                            c = mean_LBTS$IU$c, 
                            sig_p = mean_LBTS$IU$sig_p,
                            sig_o = mean_LBTS$IU$sig_o, df = complete_list_OG$IU)
# plot(1:length(AM_LBTS_GPP), AM_LBTS_GPP, type="l")
# points(complete_list_OG$AM$GPP) #bad
# plot(1:length(IU_LBTS_GPP), IU_LBTS_GPP, type="l")
# points(complete_list_OG$IU$GPP) #why is it so hight??

#############################################################################
## (6) Replace original GPP with predicted GPP in stan data list
#############################################################################


stan_simAM_STS <- list(Ndays=length(AM_STS_GPP), 
                       light=complete_list_OG$AM$light_rel_PAR, 
                       GPP = AM_STS_GPP,
                        prior_sig_o_mean = mean_STS$AM$sig_o,
                        prior_sig_o_sd = sd_STS$AM$sig_o, 
                       tQ = complete_list_OG$AM$tQ)
stan_simIU_STS <- list(Ndays=length(IU_STS_GPP), 
                       light=complete_list_OG$IU$light_rel_PAR, 
                       GPP = IU_STS_GPP,
                          prior_sig_o_mean = mean_STS$IU$sig_o,
                          prior_sig_o_sd = sd_STS$IU$sig_o, 
                       tQ = complete_list_OG$IU$tQ)
stan_STS_sim_list <- list("AM_STS"=stan_simAM_STS,
                          "IU_STS"=stan_simIU_STS)

stan_simAM_LBTS <- list(Ndays=length(AM_LBTS_GPP), 
                        light=complete_list_OG$AM$light_rel_PAR, 
                        GPP = AM_LBTS_GPP,
                         prior_sig_o_mean = mean_LBTS$AM$sig_o,
                         prior_sig_o_sd = sd_LBTS$AM$sig_o, 
                        tQ = complete_list_OG$AM$tQ)
stan_simIU_LBTS <- list(Ndays=length(IU_LBTS_GPP), 
                        light=complete_list_OG$IU$light_rel_PAR, GPP = IU_LBTS_GPP,
                           prior_sig_o_mean = mean_LBTS$IU$sig_o,
                           prior_sig_o_sd = sd_LBTS$IU$sig_o, 
                        tQ = complete_list_OG$IU$tQ)
stan_LBTS_sim_list <- list("AM_LBTS"=stan_simAM_LBTS,
                           "IU_LBTS"=stan_simIU_LBTS)

###########################################################################
## (7) Fit models to simulated data from initial parameter estimates
###########################################################################
rstan_options(auto_write=TRUE)
options(mc.cores=5)
parallel::detectCores()
#STS
recov_STS_output <- lapply(stan_STS_sim_list,
                           function(x) stan("Stream Biomass files/functions and stans/Stan_ProductivityModel1_STS_recovery_simulation.stan",
                                            data=x,chains=4,iter=1000,
                                            control=list(max_treedepth=12, adapt_delta = 0.95)))

#LBTS
init_Ricker <- function(...) {
  list(c = 0.5, s = 1.5)
}
recov_LBTS_output <- lapply(stan_LBTS_sim_list,
                            function(x) stan("Stream Biomass files/functions and stans/Stan_ProductivityModel2_LBTS_recovery_simulation.stan",
                                             data=x,chains=4,iter=2000,init = init_Ricker,
                                             control=list(max_treedepth=12)))

## Save parameter recovery model fit
recovery_model_output <- list(recov_STS_output, recov_LBTS_output)
saveRDS(recovery_model_output, "Stream Biomass files/Param_Rec_Test_recovery_output_20240703.rds")


#################################################################################
## DNU: (8) Compare parameters from simulation to posterior distributions
################################################################################

###################################################
## Run Stan to get parameter estimates - all sites
###################################################

rstan_options(auto_write=TRUE)
options(mc.cores=4)

stan_data_compile <- function(x){
  data <- list(Ndays=length(x$GPP), light = x$light_rel_PAR, GPP = x$GPP,
               GPP_sd = x$GPP_sd, tQ = x$tQ)
  return(data)
}

stan_data_l <- lapply(complete_list_OG, function(x) stan_data_compile(x))


## PM 1 - Standard time series (STS)
PM_outputlist_AR <- lapply(stan_data_l,
                           function(x) rstan::stan("Stream Biomass files/functions and stans/Stan_ProductivityModel1_STS.stan",
                                                   data=x, chains=4, iter=2000,
                                                   control=list(max_treedepth=12, adapt_delta=0.95)))
saveRDS(PM_outputlist_AR, "Stream Biomass files/stan_6riv_output_AR_20240703.rds")


## PM 2 - Latent Biomass (LBTS)
init_Ricker <- function(...) {
  list(c = 0.5, s = 1.5)
}
PM_outputlist_Ricker <- lapply(stan_data_l,
                               function(x) stan("Stream Biomass files/functions and stans/Stan_ProductivityModel2_LBTS.stan",
                                                data=x, init = init_Ricker, chains=4, iter=2000,
                                                control=list(max_treedepth=12, adapt_delta=0.95)))
saveRDS(PM_outputlist_Ricker, "Stream Biomass files/stan_6riv_output_Ricker_20240703.rds")

## Summary of divergent transitions
PM_outputlist_AR <- readRDS("./rds files/stan_6riv_output_AR_2022_02_22.rds")
PM_outputlist_Ricker <- readRDS("./rds files/stan_6riv_output_Ricker_2022_02_27.rds")


launch_shinystan(PM_outputlist_AR$AM)

launch_shinystan(PM_outputlist_Ricker$AM)

########################################
#####Biomass WS Predictions#####
source("Stream Biomass files/functions and stans/Predicted_ProductivityModel_STS.R") # parameters: phi, alpha, beta, sig_p
source("Stream Biomass files/functions and stans/Predicted_ProductivityModel_LBTS.R") # parameters: r, lambda, s, c, sig_p

# colors
PM_AR.col <- "#d95f02"
PM_Ricker.col <- "#7570b3"

## Import stan fits - simulate one at a time
stan_model_output_AR <- readRDS("Stream Biomass files/stan_6riv_output_AR_2022_02_22.rds")
stan_model_output_Ricker <- readRDS("Stream Biomass files/stan_6riv_output_Ricker_2022_02_27.rds")

##########################
## Model 1 Output - S-TS (referred to as "AR" for autoregressive)
#########################
names(complete_list_OG); names(stan_model_output_AR)
AR_list <- Map(c, stan_model_output_AR, complete_list_OG)

AR_sim_fxn <- function(x){
  #separate data
  output <- x[[1]]
  df <- x
  
  # extract
  pars1 <- extract(output, c("phi","alpha","beta","sig_p","sig_o"))
  simmat1<-matrix(NA,length(df$GPP),length(unlist(pars1$phi)))
  rmsemat1<-matrix(NA,length(df$GPP),1)
  
  # Simulate
  for (i in 1:length(pars1$phi)){
    simmat1[,i]<-PM_AR(pars1$phi[i],pars1$alpha[i],pars1$beta[i],pars1$sig_p[i],pars1$sig_o[i],df)
    rmsemat1[i]<-sqrt(sum((simmat1[,i]-df$GPP)^2)/length(df$GPP))
  }
  
  l <- list(simmat1, rmsemat1)
  return(l)
  
}

#test <- AR_sim_fxn(AR_list$nwis_01608500)
AR_sim <- lapply(AR_list, function(x) AR_sim_fxn(x))

## Save simulation
saveRDS(AR_sim, "Stream Biomass files/Sim_6riv_AR_ws_2022_02_27.rds")


###############################
## Model 2 Output - LB-TS (also referred to as Ricker)
###############################
names(complete_list_OG); names(stan_model_output_Ricker)
Ricker_list <- Map(c, stan_model_output_Ricker, complete_list_OG[(names(stan_model_output_Ricker))])

Ricker_sim_fxn <- function(x){
  #separate data
  output <- x[[1]]
  complete_list_OG <- x
  
  # extract
  pars3<-extract(output, c("r","lambda","s","c","B","P","pred_GPP","sig_p","sig_o"))
  simmat3<-matrix(NA,length(complete_list_OG$GPP),length(unlist(pars3$sig_p)))
  biomat3<-matrix(NA,length(complete_list_OG$GPP),length(unlist(pars3$sig_p)))
  rmsemat3<-matrix(NA,length(complete_list_OG$GPP),1)
  #Simulated
  for (i in 1:length(pars3$r)){
    simmat3[,i]<-PM_Ricker(pars3$r[i],pars3$lambda[i],pars3$s[i],pars3$c[i],pars3$sig_p[i],pars3$sig_o[i],df)
    biomat3[,i]<-PM_Ricker_B(pars3$r[i],pars3$lambda[i],pars3$s[i],pars3$c[i],pars3$sig_p[i],pars3$sig_o[i],df)
    rmsemat3[i]<-sqrt(sum((simmat3[,i]-df$GPP)^2)/length(df$GPP))
  }
  
  l <- list(simmat3, rmsemat3, biomat3)
  return(l)
  
}

Ricker_sim <- lapply(Ricker_list, function(x) Ricker_sim_fxn(x))

## Save simulation
saveRDS(Ricker_sim, "Stream Biomass files/Sim_6riv_Ricker_ws_2022_02_27.rds")

