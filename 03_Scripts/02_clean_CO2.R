###packages###
library(tidyverse)
library(readxl)
library(plotly)

####AM CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything/interpolated", pattern=".xlsx", full.names=TRUE)

AM.CO2_interpolated <- data.frame()
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
  AM.CO2_interpolated <- rbind(AM.CO2_interpolated, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/Everything", pattern=".xlsx", full.names=TRUE)

AM.CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  AM.CO2_everything <- rbind(AM.CO2_everything, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 CS", pattern=".xlsx", full.names=TRUE)
AM.CO2_CS <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  AM.CO2_CS <- rbind(AM.CO2_CS, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
AM.CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  AM.CO2_Sht2 <- rbind(AM.CO2_Sht2, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/AllenMill/dat everything", pattern=".dat", full.names=TRUE)
AM.CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  AM.CO2_dat <- rbind(AM.CO2_dat, CO2)}

AM_CO2<-rbind(AM.CO2_interpolated,AM.CO2_everything, AM.CO2_CS, AM.CO2_Sht2, AM.CO2_dat)%>%
  filter(Date<'2026-01-01', CO2>500, Date> '2022-07-01')%>%
  mutate(
    # CO2=if_else(Date>'2022-08-01' & Date<'2022-08-29', CO2/6, CO2),
    # CO2=if_else(Date<'2022-07-01', CO2/4.2, CO2),
    CO2=if_else(Date<'2023-01-01' & CO2>15000, CO2/6, CO2),
    
    ID='AM'
  )%>%
  distinct(Date, ID, .keep_all = T)


#ggplot(AM_CO2, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=2)

####GB CO2#######

file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/Everything", pattern=".xlsx", full.names=TRUE)
GB.CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (((CO2$CO2/8.8067)+863.5))*3
  GB.CO2_everything <- rbind(GB.CO2_everything, CO2)
}

file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/everything dat", pattern=".dat", full.names=TRUE)
GB.CO2_everythingdat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil,skip=3)
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (((CO2$CO2/8.8067)+863.5))*3
  GB.CO2_everythingdat <- rbind(GB.CO2_everythingdat, CO2)
}


file.names <- list.files(path="01_Raw_data/CampbellSci/Gilchrist Blue/CO2 dat", pattern=".dat", full.names=TRUE)
GB.CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil,skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2*6
  GB.CO2_dat <- rbind(GB.CO2_dat, CO2)
}
+5900
GB_CO2<-rbind(GB.CO2_everything, GB.CO2_dat, GB.CO2_everythingdat)%>%
  mutate(ID='GB',
         CO2=if_else(Date>'2023-01-01' & CO2< 4000, NA, CO2),
         CO2=if_else(Date>'2023-09-01' & Date< '2023-10-03', NA, CO2),
         CO2=if_else(Date> '2023-10-04', CO2/1.7+3000 , CO2),
         
         day=as.Date(Date)
  )%>%
  distinct(Date, .keep_all = T)%>%
  filter(
    CO2 < 8000& CO2>3700,
    !day %in% as.Date(c(
      "2023-11-01","2023-12-06","2023-12-19",
      "2022-07-20","2024-01-05", "2023-12-24", "2023-12-25",
      "2023-10-31", "2023-12-21"
    ))
  )%>%
  select(-day)

ggplot(GB_CO2, aes(Date, CO2)) + geom_point()
ggplotly(ggplot(GB_CO2%>%filter(Date>'2023-07-01'), aes(Date, CO2)) + geom_point(size=0.5))

####ID CO2#######

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Interpolated", pattern=".xlsx", full.names=TRUE)
ID.CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil,
                    col_types = c("date", "numeric", "numeric",
                                  "numeric", "text", "numeric", "numeric",
                                  "numeric", "numeric", "text", "numeric"))
  CO2<-CO2[,c(1,7)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*10.538-33003)
  ID.CO2_interpolated <- rbind(ID.CO2_interpolated, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Everything", pattern=".xlsx", full.names=TRUE)
ID.CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*6)
  ID.CO2_everything <- rbind(ID.CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Ichetucknee/Everything dat", pattern=".dat", full.names=TRUE)
ID.CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- (CO2$CO2*6)
  ID.CO2_dat <- rbind(ID.CO2_dat, CO2)}

ID_CO2<-rbind(ID.CO2_interpolated,ID.CO2_everything,ID.CO2_dat)%>%
  mutate(
     #CO2=if_else(Date>'2022-09-16' & Date<'2022-09-16', CO2/6, CO2),
    # CO2=if_else(Date>'2022-08-09'& Date<'2022-08-15', CO2/6, CO2),
    CO2=if_else(CO2>10000, CO2/6, CO2),
    
    ID='ID')%>%
  filter(CO2>500 &CO2<4500)
    

#ggplot(ID_CO2, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=2)

####LF CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/Everything/interpolated", pattern=".xlsx", full.names=TRUE)
LF.CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<- ((CO2$CO2/4.8065)-0.3248)
  LF.CO2_interpolated <- rbind(LF.CO2_interpolated, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/Everything", pattern=".xlsx", full.names=TRUE)
LF.CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil)
  CO2<-CO2[,c(1,6)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  LF.CO2_everything <- rbind(LF.CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/CO2 Sheet2", pattern=".xlsx", full.names=TRUE)
LF.CO2_Sht2 <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet = 'CO2')
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  LF.CO2_Sht2 <- rbind(LF.CO2_Sht2, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/LittleFanning/CO2 dat", pattern=".dat", full.names=TRUE)
LF.CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,5)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  CO2$CO2<-CO2$CO2/6
  LF.CO2_dat <- rbind(LF.CO2_dat, CO2)}

LF_CO2<-rbind(LF.CO2_interpolated, LF.CO2_everything, LF.CO2_Sht2, LF.CO2_dat)%>%
  mutate(ID='LF',
         # CO2=if_else(Date>'2023-12-18', CO2*15, CO2),
         # CO2=if_else(Date>'2022-08-24' & Date< '2022-09-07', CO2*6+1500, CO2),
         # CO2=if_else(Date>'2023-08-16' & Date< '2023-08-29', CO2*6+500, CO2)
         )%>%
  distinct(ID, Date, .keep_all = T)%>%
  filter(CO2<4000, CO2>500, Date<'2026-01-01')

#ggplotly(ggplot(LF_CO2, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=2))

write_csv(LF_CO2, "test.csv")

####OS CO2#######
file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything/interpolated", pattern=".xlsx", full.names=TRUE)
OS.CO2_interpolated <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil,col_types = c("date", "numeric", "numeric"))
  CO2<-CO2[,c(1,2)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  OS.CO2_interpolated <- rbind(OS.CO2_interpolated, CO2)}


file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything", pattern=".xlsx", full.names=TRUE)
OS.CO2_everything <- data.frame()
for(fil in file.names){
  CO2 <- read_excel(fil, sheet="Sheet1")
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  OS.CO2_everything <- rbind(OS.CO2_everything, CO2)}

file.names <- list.files(path="01_Raw_data/CampbellSci/Otter/Everything dat", pattern=".dat", full.names=TRUE)
OS.CO2_dat <- data.frame()
for(fil in file.names){
  CO2 <- read_csv(fil, skip=3)
  CO2<-CO2[,c(1,4)]
  colnames(CO2)[1] <- "Date"
  colnames(CO2)[2] <- "CO2"
  OS.CO2_dat <- rbind(OS.CO2_dat, CO2)}

OS_CO2<-rbind(OS.CO2_interpolated,OS.CO2_everything, OS.CO2_dat)%>%
  mutate(ID='OS',
         CO2=if_else(Date<'2022-07-01', CO2/2, CO2)
         )%>%
  filter(CO2>500, CO2<4500)

#ggplot(OS_CO2, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=2)



############
CO2<-rbind(AM_CO2, GB_CO2, ID_CO2, LF_CO2, OS_CO2)%>% distinct(Date, ID, .keep_all=TRUE)
#ggplot(CO2, aes(Date, CO2)) + geom_point() + facet_wrap(~ ID, ncol=2)

write_csv(CO2, "02_Clean_data/Chem/CO2.csv")

names(LF_CO2)

