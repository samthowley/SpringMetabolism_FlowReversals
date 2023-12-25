rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(cowplot)
library(readxl)
library(weathermetrics)
library(measurements)
library(dataRetrieval)
library('StreamMetabolism')
library("hydroTSM")
library(rnoaa)
library(corrplot)
library("broom")
library(car)
library(imputeTS)
library(ggExtra)
library("devtools")
library(lubridate)
library(ggplot2)


####CO2#######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/CampbellSci/AllenMill/Not Formatted")

file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- ((CO2$CO2/5.0614)-328.16)
  CO2_interpolated <- rbind(CO2_interpolated, CO2)
}

file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_everything <- rbind(CO2_everything, CO2)
}

file.names <- list.files(path="CO2 CS", pattern=".xlsx", full.names=TRUE)

CO2_CS <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_CS <- rbind(CO2_CS, CO2)
}

file.names <- list.files(path="CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)

CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_Sht2 <- rbind(CO2_Sht2, CO2)
}

file.names <- list.files(path="dat everything", pattern=".dat", full.names=TRUE)

CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_dat <- rbind(CO2_dat, CO2)
}

CO2<-rbind(CO2_interpolated,CO2_everything, CO2_CS,CO2_Sht2, CO2_dat)
CO2<-filter(CO2, CO2<15000 & CO2> 1100)
CO2 <- CO2[!duplicated(CO2[c('Date')]),]

ggplot(CO2, aes(Date, CO2))+
  geom_line()

write_xlsx(CO2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/CO2.xlsx")

######FDOM#####
file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

FDOM_interpolated <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,7)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM*6.4-160
  FDOM_interpolated <- rbind(FDOM_interpolated, FDOM)
}

file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

FDOM_everything <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,7)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM*6.4-160
  FDOM_everything <- rbind(FDOM_everything, FDOM)
}

file.names <- list.files(path="CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)

FDOM_Sht2 <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,7)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM*6.4-160
  FDOM_Sht2 <- rbind(FDOM_Sht2, FDOM)
}


file.names <- list.files(path="dat everything", pattern=".dat", full.names=TRUE)

FDOM_dat <- data.frame()
for(fil in file.names){
  FDOM <- read_csv(fil, skip=3)
  FDOM<-FDOM[,c(1,7)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM*6.4-160
  FDOM_dat <- rbind(FDOM_dat, FDOM)
}

FDOM<-rbind(FDOM_interpolated,FDOM_everything, FDOM_Sht2, FDOM_dat)
FDOM<-filter(FDOM, FDOM<125)
FDOM <- FDOM[!duplicated(FDOM[c('Date')]),]

ggplot(FDOM, aes(Date, FDOM))+
  geom_line()

write_xlsx(FDOM, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/FDOM.xlsx")

####pH#####

file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

pH_everything <- data.frame()
for(fil in file.names){
  pH <- read_excel(fil)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  pH_everything <- rbind(pH_everything, pH)
}

file.names <- list.files(path="CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)

pH_Sht2 <- data.frame()
for(fil in file.names){
  pH <- read_excel(fil)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  pH_Sht2 <- rbind(pH_Sht2, pH)
}


file.names <- list.files(path="dat everything", pattern=".dat", full.names=TRUE)

pH_dat <- data.frame()
for(fil in file.names){
  pH <- read_csv(fil, skip=3)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  #pH$Date<-as.Date(pH$Date)
  pH$pH<-as.numeric(pH$pH)
  pH_dat <- rbind(pH_dat, pH)
}

pH<-rbind(pH_everything,pH_Sht2, pH_dat)
pH<-filter(pH, pH<9.25 & pH>4)
pH <- pH[!duplicated(pH[c('Date')]),]

ggplot(pH, aes(Date, pH))+
  geom_line()

write_xlsx(pH, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/pH.xlsx")

######DO#######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/HOBO/Allen Mill")


file.names <- list.files(path="DO/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  DO <- read_csv(fil)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  formatted <- rbind(formatted, DO)
}

file.names <- list.files(path="DO", pattern=".csv", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  DO <- read_csv(fil,
                 col_types = cols(`#` = col_skip()),
                 skip = 1)
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp"
  DO$Date <- mdy_hms(DO$Date)
  unformatted <- rbind(unformatted, DO)
}

DO<-rbind(formatted, unformatted)
DO<-filter(DO, DO<10)
DO <- DO[!duplicated(DO[c('Date')]),]

DO<-filter(DO, Temp<76)
for(i in 1:nrow(DO)){
  if( DO$Date[i] >= as.Date("2023-03-09") & DO$Temp[i]< 68 ){
    DO$Temp[i]<- NA}
  else {DO$stage[i]<- DO$stage[i]-0 }
}
DO$Temp[is.na(DO$Temp)]<-mean(DO$Temp,na.rm=TRUE)

#for(i in 1:nrow(DO)){
  if( DO$Date[i] >= as.Date("2023-04-11") & 
      DO$Date[i] <= as.Date("2023-05-09")
      & DO$DO[i]< 2){
    DO$DO[i]<- NA}
  else {DO$DO[i]<- DO$DO[i]-0 }
}

ggplot(DO, aes(Date, DO))+
  geom_line()


write_xlsx(DO, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/DO.xlsx")

#######SpC#####

file.names <- list.files(path="SpC/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  SpC <- read_csv(fil)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  formatted <- rbind(formatted, SpC)
}

file.names <- list.files(path="SpC", pattern=".csv", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  SpC <- read_csv(fil,
                  col_types = cols(`#` = col_skip()),
                  skip = 1)
  SpC<-SpC[,c(1,2)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  unformatted <- rbind(unformatted, SpC)
}

SpC<-rbind(formatted, unformatted)
SpC<-filter(SpC, SpC<500 & SpC>50)
SpC <- SpC[!duplicated(SpC[c('Date')]),]

ggplot(SpC, aes(Date, SpC))+
  geom_line()

write_xlsx(SpC, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/SpC.xlsx")

#######PT#####

file.names <- list.files(path="PT/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  PT <- read_csv(fil)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  formatted <- rbind(formatted, PT)
}

file.names <- list.files(path="PT", pattern=".csv", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  PT <- read_csv(fil,
                 col_types = cols(`#` = col_skip()),
                 skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  unformatted <- rbind(unformatted, PT)
}

PT<-rbind(formatted, unformatted)
PT <- PT[!duplicated(PT[c('Date')]),]

ggplot(PT, aes(Date, PT))+
  geom_line()

write_xlsx(PT, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/PT.xlsx")

#######FAWN#####

file.names <- list.files(path="FAWN", pattern=".csv", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  FAWN <- read_csv(fil, col_types = cols(`FAWN Station` = col_skip(), 
                                         `N (# obs)` = col_skip()))
  colnames(FAWN)[1] <- "Date"
  FAWN$Date <- mdy_hm(FAWN$Date)
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  unformatted <- rbind(unformatted, FAWN)
}

FAWN<-rbind(unformatted)
ggplot(FAWN, aes(Date, PSI))+
  geom_line()

write_xlsx(FAWN, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/AllenMill/FAWN.xlsx")


####Compile####

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted")
file.names <- list.files(path="AllenMill", pattern=".xlsx", full.names=TRUE)
file.names<-file.names[c(6,1,2,3,4,5,7)]


data <- lapply(file.names,
               function(x) {
                 read_excel(x)
               })
library(plyr)
AM<-join_all(data, by='Date', type='left')
AM <- AM[!duplicated(AM[c('Date')]),]

AM$stage<-((AM$PT-AM$PSI)/(1.41/0.634))+0.515

#for(i in 1:nrow(AM)){
 # if( AM$Date[i] >= as.Date("2023-06-23")){
    #AM$stage[i]<-AM$stage[i]-0.22}
#  else {AM$stage[i]<- AM$stage[i]-0 }
#}

for(i in 1:nrow(AM)){
  if(AM$stage[i]<=0.98 & AM$SpC[i]<=200) { AM$SpC[i]<- AM$SpC[i]}
    else {AM$SpC[i]<- AM$SpC[i]-0 }}

(a<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm)))

(b<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    scale_color_manual(values='orange')+
    ylab("DO mg/L"))

(c<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=SpC, color="LowRangeSpC"), size=0.8)+
    ylab("Conductivity"))

(d<-ggplot(AM, aes(x=Date))+
    geom_point(aes(y=stage, color="AbsPres"), size=0.2)+
    scale_color_manual(values='black')+
    ggtitle('AM')+
    ylab("Stage (m)"))

(e<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=pH), size=0.8)+
    xlab("Date"))

(f<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=FDOM), size=0.8)+
    xlab("Date"))

(g<-ggplot(AM, aes(x=Date))+
    geom_line(aes(y=Temp), size=0.8)+
    xlab("Date"))

plot_grid(d,a,b,c,ncol=1, align='v') 

write_xlsx(AM, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/AllenMill.xlsx")
