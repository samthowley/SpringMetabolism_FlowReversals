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
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/CampbellSci/LittleFanning/edited")

file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- ((CO2$CO2/4.8065)-0.3248)
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

file.names <- list.files(path="CO2 Sheet2", pattern=".xlsx", full.names=TRUE)

CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_Sht2 <- rbind(CO2_Sht2, CO2)
}

file.names <- list.files(path="CO2 dat", pattern=".dat", full.names=TRUE)

CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2/6
  CO2_dat <- rbind(CO2_dat, CO2)
}

CO2<-rbind(CO2_interpolated,CO2_everything,CO2_Sht2, CO2_dat)
CO2 <- CO2[!duplicated(CO2[c('Date')]),]

CO2<-filter(CO2,Date<'2024-01-01')
CO2$CO2<-CO2$CO2*6
CO2<-filter(CO2, CO2<25000 & CO2>1000)
range(CO2$Date, na.rm=T)
ggplot(CO2, aes(Date, CO2))+
  geom_line()
range(CO2$Date, na.rm = T)
write_xlsx(CO2, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/CO2.xlsx")

######FDOM#####
file.names <- list.files(path="Everything/interpolated", pattern=".xlsx", full.names=TRUE)

FDOM_interpolated <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,7)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM/1.714-31.022
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
  FDOM$FDOM<-FDOM$FDOM/1.714-31.022
  FDOM_everything <- rbind(FDOM_everything, FDOM)
}

file.names <- list.files(path="CO2 Sheet2", pattern=".xlsx", full.names=TRUE)

FDOM_Sht2 <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,5)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM/1.714-31.022
  FDOM_Sht2 <- rbind(FDOM_Sht2, FDOM)
}

file.names <- list.files(path="FDOM CS", pattern=".xlsx", full.names=TRUE)

FDOM_CS <- data.frame()
for(fil in file.names){
  FDOM <- read_excel(fil)
  FDOM<-FDOM[,c(1,5)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM/1.714-31.022
  FDOM_CS <- rbind(FDOM_CS, FDOM)
}


file.names <- list.files(path="FDOM dat", pattern=".dat", full.names=TRUE)

FDOM_dat <- data.frame()
for(fil in file.names){
  FDOM <- read_csv(fil, skip=3)
  FDOM<-FDOM[,c(1,5)]
  colnames(FDOM)[1] <- "Date"
  colnames(FDOM)[2] <- "FDOM"
  FDOM$FDOM<-as.numeric(FDOM$FDOM)
  FDOM$FDOM<-FDOM$FDOM/1.714-31.022
  FDOM_dat <- rbind(FDOM_dat, FDOM)
}

FDOM<-rbind(FDOM_interpolated,FDOM_everything, FDOM_Sht2, FDOM_CS, FDOM_dat)
FDOM<-filter(FDOM, FDOM<1000)
FDOM <- FDOM[!duplicated(FDOM[c('Date')]),]

ggplot(FDOM, aes(Date, FDOM))+
  geom_line()

write_xlsx(FDOM, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/FDOM.xlsx")

####pH#####
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/HOBO/Little Fanning")

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
pH<-filter(pH, pH<9.25 & pH>4)
pH <- pH[!duplicated(pH[c('Date')]),]

ggplot(pH, aes(Date, pH))+
  geom_line()

write_xlsx(pH, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/pH.xlsx")

######DO#######

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
DO <- DO[!duplicated(DO[c('Date')]),]

ggplot(DO, aes(Date, DO))+
  geom_line()
range(DO$Date, na.rm = T)
write_xlsx(DO, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/DO.xlsx")

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
SpC<-filter(SpC, SpC<600 & SpC>400)
SpC <- SpC[!duplicated(SpC[c('Date')]),]

ggplot(SpC, aes(Date, SpC))+
  geom_line()

write_xlsx(SpC, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/SpC.xlsx")

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
PT <- PT[!duplicated(PT[c('Date')]),]

ggplot(PT, aes(Date, PT))+
  geom_line()

write_xlsx(PT, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/PT.xlsx")

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

ggplot(FAWN, aes(Date, PSI))+
  geom_line()

write_xlsx(FAWN, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted/LittleFanning/FAWN.xlsx")


####Compile####

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Formatted")
file.names <- list.files(path="LittleFanning", pattern=".xlsx", full.names=TRUE)
file.names<-file.names[c(6,2,3,4,5,7,1)]


data <- lapply(file.names,
               function(x) {
                 read_excel(x)
               })

LF<-join_all(data, by='Date', type='left')

LF$stage<-((LF$PT-LF$PSI)/(1.47/0.634))+0.176


(a<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=CO2, color="CO2"), size=0.8)+
    ylab(expression(CO[2]~ppm)))

(b<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=DO, color="DO"), size=0.8)+
    scale_color_manual(values='orange')+
    ylab("DO mg/L"))

(c<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=SpC, color="LowRangeSpC"), size=0.8)+
    ylab("Conductivity"))

(d<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=stage, color="AbsPres"), size=0.8)+
    scale_color_manual(values='black')+
    ggtitle('LF')+
    ylab("Stage (m)"))

(e<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=pH), size=0.8)+
    xlab("Date"))

(e<-ggplot(LF, aes(x=Date))+
    geom_line(aes(y=FDOM), size=0.8)+
    xlab("Date"))

plot_grid(d,a,b,c,ncol=1, align='v') 

write_xlsx(LF, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/chemistry/LittleFanning.xlsx")
