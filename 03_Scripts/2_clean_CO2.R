###packages###
library(tidyverse)
library(readxl)

####AM CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything/interpolated", pattern=".xlsx", full.names=TRUE)

CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  
  CO2<-CO2%>%mutate(
    CO2=as.numeric(CO2),
    Date=ymd_hms(Date),
    CO2=(CO2/5.0614)-328.16
    )
  CO2_interpolated <- rbind(CO2_interpolated, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything", pattern=".xlsx", full.names=TRUE)

CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_everything <- rbind(CO2_everything, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 CS", pattern=".xlsx", full.names=TRUE)
CO2_CS <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_CS <- rbind(CO2_CS, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_Sht2 <- rbind(CO2_Sht2, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/dat everything", pattern=".dat", full.names=TRUE)
CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_dat <- rbind(CO2_dat, CO2)}

AM_CO2<-rbind(CO2_interpolated,CO2_everything, CO2_CS,CO2_Sht2, CO2_dat)%>%
  filter(Date<'2026-01-01')%>%
  mutate(
    CO2=if_else(Date>'2022-08-01' & Date<'2022-08-29', CO2/6, CO2),
    CO2=if_else(Date<'2022-07-01', CO2/4.2, CO2),
    ID='AM'
  )%>%
  distinct(Date, ID, .keep_all = T)%>%
  filter(CO2<6*10^3)


ggplot(AM_CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)

####GB CO2#######

file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/Everything", pattern=".xlsx", full.names=TRUE)
CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (((CO2$CO2/8.8067)+863.5))*3
  CO2_everything <- rbind(CO2_everything, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/everything dat", pattern=".dat", full.names=TRUE)
CO2_everythingdat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil,skip=3)
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (((CO2$CO2/8.8067)+863.5))*3
  CO2_everythingdat <- rbind(CO2_everythingdat, CO2)
}


file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/CO2 dat", pattern=".dat", full.names=TRUE)
CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil,skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  CO2_dat <- rbind(CO2_dat, CO2)
}

GB_CO2<-rbind(CO2_everything,CO2_dat)%>%
  mutate(ID='GB',
         #CO2=if_else(Date<'2022-04-28', CO2*6, CO2),
  )#%>%
  filter(CO2<8*10^3 & CO2>3.5*10^3)
ggplot(GB_CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)

####ID CO2#######

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Interpolated", pattern=".xlsx", full.names=TRUE)
CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil,
                    col_types = c("date", "numeric", "numeric",
                                  "numeric", "text", "numeric", "numeric",
                                  "numeric", "numeric", "text", "numeric"))
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*10.538-33003)
  CO2_interpolated <- rbind(CO2_interpolated, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Everything", pattern=".xlsx", full.names=TRUE)
CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*6)
  CO2_everything <- rbind(CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Everything dat", pattern=".dat", full.names=TRUE)
CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*6)
  CO2_dat <- rbind(CO2_dat, CO2)}

ID_CO2<-rbind(CO2_interpolated,CO2_everything,CO2_dat)%>%
  mutate(
    CO2=if_else(Date>'2022-09-16' & Date<'2022-09-16', CO2/6, CO2),
    CO2=if_else(Date>'2022-08-09'& Date<'2022-08-15', CO2/6, CO2),
    ID='ID')%>%
  filter(CO2<7000)
    

ggplot(ID_CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)

####LF CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/Everything/interpolated", pattern=".xlsx", full.names=TRUE)
CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- ((CO2$CO2/4.8065)-0.3248)
  CO2_interpolated <- rbind(CO2_interpolated, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/Everything", pattern=".xlsx", full.names=TRUE)
CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_everything <- rbind(CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/CO2 Sheet2", pattern=".xlsx", full.names=TRUE)
CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_Sht2 <- rbind(CO2_Sht2, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/CO2 dat", pattern=".dat", full.names=TRUE)
CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2/6
  CO2_dat <- rbind(CO2_dat, CO2)}

LF_CO2<-rbind(CO2_interpolated,CO2_everything,CO2_Sht2, CO2_dat)%>%
  filter(Date<'2026-01-01', CO2<7000)%>%
  mutate(ID='LF')%>%
  distinct(ID, Date, .keep_all = T)

ggplot(LF_CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)

####OS CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything/interpolated", pattern=".xlsx", full.names=TRUE)
CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil,col_types = c("date", "numeric", "numeric"))
  CO2<-CO2[,c(1,2)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_interpolated <- rbind(CO2_interpolated, CO2)}


file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything", pattern=".xlsx", full.names=TRUE)
CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet="Sheet1")
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_everything <- rbind(CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything dat", pattern=".dat", full.names=TRUE)
CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2_dat <- rbind(CO2_dat, CO2)}

OS_CO2<-rbind(CO2_interpolated,CO2_everything, CO2_dat)%>%mutate(ID='OS')

ggplot(OS_CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)



############
CO2<-rbind(AM_CO2, GB_CO2, ID_CO2, LF_CO2, OS_CO2)%>%
  filter(CO2>500 & CO2<15000)
ggplot(CO2, aes(Date, CO2)) + geom_line() + facet_wrap(~ ID, ncol=2)

write_csv(CO2, "02_Clean_data/CO2.csv")



