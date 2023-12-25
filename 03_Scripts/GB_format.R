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
library(plyr)
####CO2#######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/CampbellSci/Gilchrist Blue/edited")

file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (((CO2$CO2/8.8067)+863.5))*3
  CO2_everything <- rbind(CO2_everything, CO2)
}

file.names <- list.files(path="CO2 dat", pattern=".dat", full.names=TRUE)

CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil,skip=3)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_dat <- rbind(CO2_dat, CO2)
  CO2_dat<-filter(CO2_dat, Date> '2023-09-30')
}

CO2<-rbind(CO2_everything,CO2_dat)
CO2<-filter(CO2,CO2<10000 & CO2>4000)
CO2 <- CO2[!duplicated(CO2[c('Date')]),]

ggplot(CO2, aes(Date, CO2))+
  geom_line()
range(CO2$Date, na.rm = T)

write_xlsx(CO2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/CO2.xlsx")


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

file.names <- list.files(path="everything dat", pattern=".dat", full.names=TRUE)

pH_dat <- data.frame()
for(fil in file.names){
  pH <- read_csv(fil, skip=3)
  pH<-pH[,c(1,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  pH_dat <- rbind(pH_dat, pH)
}  


pH<-rbind(pH_everything, pH_dat)
pH<-filter(pH, pH<9.5)
pH <- pH[!duplicated(pH[c('Date')]),]

ggplot(pH, aes(Date, pH))+
  geom_line()

write_xlsx(pH, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/pH.xlsx")

######DO#######
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/HOBO/GilchristBlue")


file.names <- list.files(path="DO/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  DO <- read_csv(fil)
  DO<-DO[,c(1,2,3)]
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
DO<-filter(DO, DO>3)
ggplot(DO, aes(Date, DO))+
  geom_line()
write_xlsx(DO, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/DO.xlsx")

#######SpC#####

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/HOBO/GilchristBlue")

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
SpC<-filter(SpC, SpC<450 & SpC>350)
ggplot(SpC, aes(Date, SpC))+
  geom_line()

write_xlsx(SpC, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/SpC.xlsx")

#######PT#####


file.names <- list.files(path="stage/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  PT <- read_csv(fil)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  formatted <- rbind(formatted, PT)
}

file.names <- list.files(path="stage", pattern=".csv", full.names=TRUE)

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
ggplot(PT, aes(Date, PT))+
  geom_line()

write_xlsx(PT, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/PT.xlsx")

#######FAWN#####

file.names <- list.files(path="FAWN/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  FAWN <- read_csv(fil)
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  formatted <- rbind(formatted, FAWN)
}


file.names <- list.files(path="FAWN", pattern=".csv", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  FAWN <- read_csv(fil,col_types = cols(`FAWN Station` = col_skip(), 
                                    Period = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                    `N (# obs)` = col_skip()))
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  unformatted <- rbind(unformatted, FAWN)
}

FAWN<-rbind(unformatted)

ggplot(FAWN, aes(Date, PSI))+
  geom_line()

write_xlsx(FAWN, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Gilchrist Blue/FAWN.xlsx")


####compile####

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted")
file.names <- list.files(path="Gilchrist Blue", pattern=".xlsx", full.names=TRUE)
file.names<-file.names[c(5,1,2,3,4,6)]

data <- lapply(file.names,
               function(x) {
                 read_excel(x)
               })

GB<-join_all(data, by='Date', type='left')

GB$stage<-((GB$PT-GB$PSI)/(0.883/ 0.448))-0.025


(a<-ggplot(GB, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm)))

(b<-ggplot(GB, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    scale_color_manual(values='orange')+
    ylab("DO mg/L"))

(c<-ggplot(GB, aes(x=Date))+
    geom_line(aes(y=SpC, color="LowRangeSpC"), size=0.8)+
    ylab("Conductivity"))

(d<-ggplot(GB, aes(x=Date))+
    geom_line(aes(y=stage, color="AbsPres"), size=0.8)+
    scale_color_manual(values='black')+
    ggtitle('GB')+
    ylab("Stage (m)"))

(e<-ggplot(GB, aes(x=Date))+
    geom_line(aes(y=pH), size=0.8)+
    xlab("Date"))


write_xlsx(GB, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/GilchristBlue.xlsx")

