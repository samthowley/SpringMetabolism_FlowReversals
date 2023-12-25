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
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/CampbellSci/Otter/edited")

file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil,col_types = c("date", "numeric", "numeric"))
  CO2<-CO2[,c(1,2)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_interpolated <- rbind(CO2_interpolated, CO2)
}
ggplot(CO2_interpolated, aes(Date, CO2))+geom_line()


file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet="Sheet1")
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_everything <- rbind(CO2_everything, CO2)
}

file.names <- list.files(path="Everything dat", pattern=".dat", full.names=TRUE)

ggplot(CO2_everything, aes(Date, CO2))+geom_line()



CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_dat <- rbind(CO2_dat, CO2)
}

CO2<-rbind(CO2_interpolated,CO2_everything, CO2_dat)
CO2 <- CO2[!duplicated(CO2[c('Date')]),]

CO2$CO2<-CO2$CO2*6

ggplot(CO2, aes(Date, CO2))+geom_line()
range(CO2$Date, na.rm=T)
write_xlsx(CO2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/CO2.xlsx")

######FDOM#####

file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

FDOM_interpolated <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil,col_types = c("date", "numeric", "numeric"))
  FDOM<-FDOM[,c(1,3)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<- ((FDOM$FDOM/7.2417)-377.38)
  FDOM_interpolated <- rbind(FDOM_interpolated, FDOM)
}

file.names <- list.files(path="Everything", pattern=".xlsx", full.names=TRUE)

FDOM_everything <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,5)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM_everything <- rbind(FDOM_everything, FDOM)
}


file.names <- list.files(path="Everything dat", pattern=".dat", full.names=TRUE)

FDOM_dat <- data.frame()
for(fil in file.names){
  FDOM <- read_csv(fil, skip=3)
  FDOM<-FDOM[,c(1,5)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM_dat <- rbind(FDOM_dat, FDOM)
}

FDOM<-rbind(FDOM_interpolated, FDOM_everything, FDOM_dat)
FDOM<-filter(FDOM, FDOM<400)
FDOM <- FDOM[!duplicated(FDOM[c('Date')]),]

ggplot(FDOM, aes(Date, FDOM))+
  geom_line()

write_xlsx(FDOM, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/FDOM.xlsx")

####pH#####
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/HOBO/Otter")

file.names <- list.files(path="pH/formatted", pattern=".xlsx", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  pH <- read_excel(fil)
  pH<-pH[,c(1,4)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  formatted <- rbind(formatted, pH)
}

file.names <- list.files(path="pH", pattern=".xlsx", full.names=TRUE)

unformatted <- data.frame()
for(fil in file.names){
  pH <- read_excel(fil)
  pH<-pH[,c(2,5)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  pH$pH<-as.numeric(pH$pH)
  unformatted <- rbind(unformatted, pH)
}



pH<-rbind(formatted, unformatted)
pH<-filter(pH,pH>4)
pH <- pH[!duplicated(pH[c('Date')]),]

ggplot(pH, aes(Date, pH))+
  geom_line()

write_xlsx(pH, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/pH.xlsx")

######DO#######

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
DO <- DO[!duplicated(DO[c('Date')]),]

ggplot(DO, aes(Date, DO))+
  geom_line()
range(DO$Date)
write_xlsx(DO, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/DO.xlsx")

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
SpC<-filter(SpC, SpC<600 & SpC>50)
SpC <- SpC[!duplicated(SpC[c('Date')]),]

ggplot(SpC, aes(Date, SpC))+
  geom_line()
range(SpC$Date)
write_xlsx(SpC, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/SpC.xlsx")

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
PT<-filter(PT, PT<500)
PT <- PT[!duplicated(PT[c('Date')]),]
PT$PT[is.na(PT$PT)]<-mean(PT$PT, na.rm=T)

ggplot(PT, aes(Date, PT))+
  geom_line()
range(PT$Date)
write_xlsx(PT, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/PT.xlsx")

#######FAWN#####

file.names <- list.files(path="FAWN/formatted", pattern=".csv", full.names=TRUE)

formatted <- data.frame()
for(fil in file.names){
  FAWN <- read_csv(fil)
  FAWN<-FAWN[,c(1,2)]
  colnames(FAWN)[1] <- "Date"
  FAWN$PSI<-conv_unit(FAWN$`BP avg (mb)`, "mbar", "psi")
  formatted <- rbind(formatted, FAWN)
}

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

FAWN<-rbind(formatted, unformatted)
FAWN <- FAWN[!duplicated(FAWN[c('Date')]),]
FAWN$PSI[is.na(FAWN$PSI)]<-mean(FAWN$PSI, na.rm=T)

ggplot(FAWN, aes(Date, PSI))+
  geom_line()

write_xlsx(FAWN, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/Otter/FAWN.xlsx")


####Compile####

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted")
file.names <- list.files(path="Otter", pattern=".xlsx", full.names=TRUE)
file.names<-file.names[c(2,1,5,3,4,6,7)]


data <- lapply(file.names,
               function(x) {
                 read_excel(x)
               })
library(plyr)
OS<-join_all(data, by='Date', type='left')
OS$PSI<-na_interpolation(OS$PSI, option = "linear")
OS$stage<-((OS$PT-OS$PSI)/(0.6894/0.372))+0.6995


(a<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm)))

(b<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    scale_color_manual(values='orange')+
    ylab("DO mg/L"))

(c<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=SpC, color="LowRangeSpC"), size=0.8)+
    ylab("Conductivity"))

(d<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=stage, color="AbsPres"), size=0.8)+
    scale_color_manual(values='black')+
    ggtitle('OS')+
    ylab("Stage (m)"))

(e<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=pH), size=0.8)+
    xlab("Date"))

(e<-ggplot(OS, aes(x=Date))+
    geom_line(aes(y=FDOM), size=0.8)+
    xlab("Date"))

plot_grid(d,a,b,c,ncol=1, align='v') 

write_xlsx(OS, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/Otter.xlsx")
