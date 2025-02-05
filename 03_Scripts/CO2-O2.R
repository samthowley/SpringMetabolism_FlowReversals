rm(list=ls())

library(broom)
library(ggplot2)
library(StreamMetabolism)
library(car)
library(imputeTS)
library(zoo)
library(tidyverse)
library(readxl)
library(measurements)
library(weathermetrics)
library(data.table)

prelim <- function(site) {
  (site$Mouth_Temp_C<- fahrenheit.to.celsius(site$Temp))
  site$Mouth_DO_sat<-Cs(site$Mouth_Temp_C)

  x<-c("Date","CO2" ,"DO","Mouth_DO_sat","Mouth_Temp_C")
  site<-site[,x]

  site$mouthTemp_K<-site$Mouth_Temp_C+273.15
  site$exp<-2400*((1/site$mouthTemp_K)-(1/298.15))
  site$KH<-0.034*2.178^(site$exp)#mol/L/atm

  site$CO2_atm<-site$CO2/1000000
  site$CO2_mol<-site$CO2_atm*site$KH
  site$DO_mol<-site$DO/32000

  site$Do_Sat<-Cs(site$Mouth_Temp_C)
  site$DO_Sat_mol<-site$Do_Sat/32000
  (site$CO2_Sat_mol<-(420/1000000)*site$KH)

  site$'O2_mol_L'<-site$KH*(site$DO_mol-site$DO_Sat_mol)
  site$'CO2_mol_L'<-site$KH*(site$CO2_mol-site$CO2_Sat_mol)

  site$'CO2_mmol_L'<-site$'CO2_mol_L'*10^3
  site$'O2_mmol_L'<-site$'O2_mol_L'*10^3

  site$day <- as.Date(site$Date)
  site.day <- aggregate(site, by=list(site$day), FUN='mean')
  x<-c('Date',"CO2_mmol_L","O2_mmol_L","day")
  site<-site[,x]

  return(site)}
dist_point_line <- function(a, slope, intercept) {
  b = c(1, intercept+slope)
  c = c(-intercept/slope,0)
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  return(abs(det(m))/sqrt(sum(v1*v1)))
}
VachonEllipse <- function(site) {
  site$O2_mmol_L <- na_interpolation(site$O2_mmol_L, option='linear')
  site$CO2_mmol_L <- na_interpolation(site$CO2_mmol_L, option='linear')

  days <- unique(site$day) #extract day
  days <- days[-1] #remove first da
  last <- length(days-4) #last possible day for moving average (a week)

  coupling.indicator <- data.frame(matrix(ncol=9, nrow=last-3))
  names(coupling.indicator) <- c('first.day', 'mid.day', 'last.day', 'cen.x', 'cen.y', 'offset', 'stretch', 'width', 'slope')

  for(i in 4:last){
    print(i)

    first.day <- days[i-3] #first day of a week
    week <- seq(first.day, first.day +6, by='day') #matrix of a week

    data.week <- site[site$day %in% week,] #extract data for a week

    ellipse <- data.frame(dataEllipse(data.week$CO2_mmol_L, data.week$O2_mmol_L, levels=c(0.95))) #make an ellipse

    lm <- lm(y~x, data=ellipse) #linear regression
    half<- nrow(ellipse)/2; start.2nd <- 1+half
    e.1st <- ellipse[c(1:half),]; e.2nd <- ellipse[-c(1:half),] #divide an ellipse into 2
    e.diff <- abs(e.1st-e.2nd) #difference of first and second halfs of an ellipse
    e.dis <- sqrt(e.diff$x^2+e.diff$y^2) #calculate the distance between two opposite points in an ellipse

    cen.x <- mean(ellipse$x); cen.y <- mean(ellipse$y) #centroid
    dist <- dist_point_line(c(cen.x, cen.y), -1, 0) # offset
    if(cen.y < -cen.x){dist <- -dist} #if centroid is below -1:1 line, then make distance negative

    c <- i-3
    coupling.indicator$first.day[c] <- first.day #first day of a week
    coupling.indicator$mid.day[c] <- days[i] #middle day of a week
    coupling.indicator$last.day[c] <- first.day+6 #last day of a week

    coupling.indicator$slope[c] <- as.numeric(lm$coefficients[2])
    coupling.indicator$cen.x[c] <- cen.x
    coupling.indicator$cen.y[c] <- cen.y
    coupling.indicator$offset[c] <- dist
    coupling.indicator$stretch[c] <- max(e.dis)
    coupling.indicator$width[c] <- min(e.dis)
  }

  coupling.indicator$mid.day <- as.Date(coupling.indicator$mid.day)
  coupling.indicator$first.day <- as.Date(coupling.indicator$first.day)
  coupling.indicator$last.day <- as.Date(coupling.indicator$last.day)

  first.day<-coupling.indicator[,-c(2,3)]
  last.day<-coupling.indicator[,-c(1,2)]
  mid.day<-coupling.indicator[,-c(1,3)]
  return(first.day)}
compile <- function(ellipse,site_mol) {

  ellipse<- ellipse %>% mutate(Day = day(first.day),Month = month(first.day),year = year(first.day))

  site_mol<- site_mol %>% mutate(Day = day(Date),Month = month(Date),year = year(Date))

  site<-left_join(ellipse,site_mol, by=c('Day', 'Month', 'year'))
  site<-na.omit(site)

  dt=data.table(site)
  regression<-dt[,as.list(coef(lm(CO2_mmol_L~O2_mmol_L))), by = day]
  regression<-filter(regression, O2_mmol_L> -0.001)
  drop.days<-regression$first.day
  site <- site[!(site$first.day %in% drop.days), ]

  x<-c("Date","O2_mmol_L","CO2_mmol_L","slope","offset")
  site<-site[,x]
  return(site)}

master <- read_csv("02_Clean_data/master.csv")
metabolism <- read_csv("02_Clean_data/master_metabolism4.csv")

###AM######
AM_chem<- master %>% filter(ID=="AM")
AM_metabolism<- metabolism %>% filter(ID=="AM")
AM<-left_join(AM_chem, AM_metabolism)

AM_mol<-prelim(AM)
AM_ellipse<-VachonEllipse(AM_mol)

AM<-compile(AM_ellipse,AM_mol)
AM$ID<-'AM'
###LF######
LF_chem<- master %>% filter(ID=="LF")
LF_metabolism<- metabolism %>% filter(ID=="LF")
LF<-left_join(LF_chem, LF_metabolism)

LF_mol<-prelim(LF)
LF_ellipse<-VachonEllipse(LF_mol)

LF<-compile(LF_ellipse,LF_mol)
LF$ID<-'LF'
###GB######
GB_chem<- master %>% filter(ID=="GB")
GB_metabolism<- metabolism %>% filter(ID=="GB")
GB<-left_join(GB_chem, GB_metabolism)

GB_mol<-prelim(GB)
GB_ellipse<-VachonEllipse(GB_mol)

GB<-compile(GB_ellipse,GB_mol)
GB$ID<-'GB'

###OS######
OS_chem<- master %>% filter(ID=="OS")
OS_metabolism<- metabolism %>% filter(ID=="OS")
OS<-left_join(OS_chem, OS_metabolism)

OS_mol<-prelim(OS)
OS_ellipse<-VachonEllipse(OS_mol)

OS<-compile(OS_ellipse,OS_mol)
OS$ID<-'OS'

###ID######
ID_chem<- master %>% filter(ID=="ID")
ID_metabolism<- metabolism %>% filter(ID=="ID")
ID<-left_join(ID_chem, ID_metabolism)

ID_mol<-prelim(ID)
ID_ellipse<-VachonEllipse(ID_mol)

ID<-compile(ID_ellipse,ID_mol)
ID$ID<-'ID'

######
master<-rbind(AM, LF, GB, OS, ID)
write_csv(master, "02_Clean_data/master_CO2-O2.csv")

ggplot(master, aes(CO2_mmol_L, O2_mmol_L)) + geom_point() +
  facet_wrap(~ ID, ncol=2)
