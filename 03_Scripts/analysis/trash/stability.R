rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(xlsx)
library(openxlsx)
library(gridExtra)
library(lubridate)
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
library(zoo)


flux<-expression(paste('g'~O[2]/m^2/'day'))
NEPflux<-expression(paste('NEP'~'(g'~O[2]/m^2/'day)'))
colstwo <- c("ER" = "red","NEP" = "blue","GPP" = "green3")
x<-c("GPPavg","ER","NEP","Date" )


#get data####
Otter<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Otter.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
Otter<-filter(Otter, Date> '2022-07-20' & Date <='2023-09-07')


GB<- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/GilchristBlue.xlsx", 
                col_types = c("date", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric"))
GB<-filter(GB, Date> '2022-07-10' & Date <='2023-09-07')

LF <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/LittleFanning.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric","numeric",
                               "numeric"))
LF<-filter(LF, Date> '2022-07-12' & Date <='2023-09-07')



AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))
AM<-filter(AM, Date> '2022-07-20' & Date <='2023-09-07')




Ich <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/Ichetucknee.xlsx", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
Ich<-filter(Ich, Date> '2022-07-20' & Date <='2023-09-07')



US27bridge <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/US27bridge.xlsx")
US27bridge$GPPavg <- na_interpolation(US27bridge$GPPavg, option='linear')
US27bridge$ER <- na_interpolation(US27bridge$ER, option='linear')

#####CoV US27###########
US27bridge$depth<-conv_unit(US27bridge$depth, "ft", "m")
US27bridge$depth<-US27bridge$depth-(max(US27bridge$depth, na.rm=T)-1.5)


US27_cov<-US27bridge[,x]
US27_cov$day <- as.Date(US27_cov$Date)
US27_cov<- aggregate(US27_cov, by=list(US27_cov$day), FUN='mean')

US27_cov$day1<-US27_cov$day + 1
US27_cov$day2<-US27_cov$day + 2
US27_cov$day3<-US27_cov$day + 3
US27_cov$day4<-US27_cov$day + 4
US27_cov$day5<-US27_cov$day + 5
US27_cov$day6<-US27_cov$day + 6
US27_cov$day7<-US27_cov$day + 7
US27_cov$day8<-US27_cov$day + 8
US27_cov$day9<-US27_cov$day + 9
US27_cov$day10<-US27_cov$day + 10
US27_cov$day15<-US27_cov$day + 15
US27_cov$day20<-US27_cov$day + 20
US27_cov$day25<-US27_cov$day + 25
US27_cov$day30<-US27_cov$day + 30

names(US27_cov)

US27_cov_day0<-US27_cov[,c(6,2,3,4)]
US27_cov_day0 <- US27_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

US27_cov_day1<-US27_cov[,c(7,2,3,4)]
US27_cov_day1<-rename(US27_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

US27_cov_day2<-US27_cov[,c(8,2,3,4)]
US27_cov_day2<-rename(US27_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

US27_cov_day3<-US27_cov[,c(9,2,3,4)]
US27_cov_day3<-rename(US27_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

US27_cov_day4<-US27_cov[,c(10,2,3,4)]
US27_cov_day4<-rename(US27_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

US27_cov_day5<-US27_cov[,c(11,2,3,4)]
US27_cov_day5<-rename(US27_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

US27_cov_day6<-US27_cov[,c(12,2,3,4)]
US27_cov_day6<-rename(US27_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

US27_cov_day7<-US27_cov[,c(13,2,3,4)]
US27_cov_day7<-rename(US27_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

US27_cov_day8<-US27_cov[,c(14,2,3,4)]
US27_cov_day8<-rename(US27_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

US27_cov_day9<-US27_cov[,c(15,2,3,4)]
US27_cov_day9<-rename(US27_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

US27_cov_day10<-US27_cov[,c(16,2,3,4)]
US27_cov_day10<-rename(US27_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

US27_cov_day15<-US27_cov[,c(17,2,3,4)]
US27_cov_day15<-rename(US27_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

US27_cov_day20<-US27_cov[,c(18,2,3,4)]
US27_cov_day20<-rename(US27_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

US27_cov_day25<-US27_cov[,c(19,2,3,4)]
US27_cov_day25<-rename(US27_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

US27_cov_day30<-US27_cov[,c(20,2,3,4)]
US27_cov_day30<-rename(US27_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')




US27_cov_1<-left_join(US27_cov_day0, US27_cov_day1, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day2, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day3, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day4, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day5, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day6, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day7, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day8, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day9, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day10, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day15, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day20, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day25, by='day')
US27_cov_1<-left_join(US27_cov_1, US27_cov_day30, by='day')

US27_cov_1<-na.omit(US27_cov_1)

ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

US27_cov_1ER<-US27_cov_1[,ER]
US27_cov_1GPP<-US27_cov_1[,GPP]
US27_cov_1NEP<-US27_cov_1[,NEP]

corrplot(cor(x=US27_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

US27_covER<-as.data.frame(as.table(cor(x=US27_cov_1ER,use="complete.obs")))
US27_covGPP<-as.data.frame(as.table(cor(x=US27_cov_1GPP,use="complete.obs")))

US27_covGPP<-filter(US27_covGPP, Var1=="GPPavg")
US27_covER<-filter(US27_covER, Var1=="ER")

US27_covGPP$site<-'IU'
US27_covER$site<-'IU'

US27_covGPP$ID<-1
US27_covER$ID<-1

#####CoV Ich###########

Ich_cov<-Ich[,x]
Ich_cov$day <- as.Date(Ich_cov$Date)
Ich_cov<- aggregate(Ich_cov, by=list(Ich_cov$day), FUN='mean')

Ich_cov$day1<-Ich_cov$day + 1
Ich_cov$day2<-Ich_cov$day + 2
Ich_cov$day3<-Ich_cov$day + 3
Ich_cov$day4<-Ich_cov$day + 4
Ich_cov$day5<-Ich_cov$day + 5
Ich_cov$day6<-Ich_cov$day + 6
Ich_cov$day7<-Ich_cov$day + 7
Ich_cov$day8<-Ich_cov$day + 8
Ich_cov$day9<-Ich_cov$day + 9
Ich_cov$day10<-Ich_cov$day + 10
Ich_cov$day15<-Ich_cov$day + 15
Ich_cov$day20<-Ich_cov$day + 20
Ich_cov$day25<-Ich_cov$day + 25
Ich_cov$day30<-Ich_cov$day + 30


Ich_cov_day0<-US27_cov[,c(6,2,3,4)]
Ich_cov_day0 <- Ich_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

Ich_cov_day1<-US27_cov[,c(7,2,3,4)]
Ich_cov_day1<-rename(Ich_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

Ich_cov_day2<-US27_cov[,c(8,2,3,4)]
Ich_cov_day2<-rename(Ich_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

Ich_cov_day3<-US27_cov[,c(9,2,3,4)]
Ich_cov_day3<-rename(Ich_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

Ich_cov_day4<-US27_cov[,c(10,2,3,4)]
Ich_cov_day4<-rename(Ich_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

Ich_cov_day5<-US27_cov[,c(11,2,3,4)]
Ich_cov_day5<-rename(Ich_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

Ich_cov_day6<-US27_cov[,c(12,2,3,4)]
Ich_cov_day6<-rename(Ich_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

Ich_cov_day7<-US27_cov[,c(13,2,3,4)]
Ich_cov_day7<-rename(Ich_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

Ich_cov_day8<-US27_cov[,c(14,2,3,4)]
Ich_cov_day8<-rename(Ich_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

Ich_cov_day9<-US27_cov[,c(15,2,3,4)]
Ich_cov_day9<-rename(Ich_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

Ich_cov_day10<-US27_cov[,c(16,2,3,4)]
Ich_cov_day10<-rename(Ich_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

Ich_cov_day15<-US27_cov[,c(17,2,3,4)]
Ich_cov_day15<-rename(Ich_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

Ich_cov_day20<-US27_cov[,c(18,2,3,4)]
Ich_cov_day20<-rename(Ich_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

Ich_cov_day25<-US27_cov[,c(19,2,3,4)]
Ich_cov_day25<-rename(Ich_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

Ich_cov_day30<-US27_cov[,c(20,2,3,4)]
Ich_cov_day30<-rename(Ich_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')



Ich_cov_1<-left_join(Ich_cov_day0, Ich_cov_day1, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day2, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day3, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day4, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day5, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day6, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day7, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day8, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day9, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day10, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day15, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day20, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day25, by='day')
Ich_cov_1<-left_join(Ich_cov_1, Ich_cov_day30, by='day')

Ich_cov_1<-na.omit(Ich_cov_1)

ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

Ich_cov_1ER<-Ich_cov_1[,ER]
Ich_cov_1GPP<-Ich_cov_1[,GPP]
Ich_cov_1NEP<-Ich_cov_1[,NEP]

Ich_covER<-as.data.frame(as.table(cor(x=Ich_cov_1ER,use="complete.obs")))
Ich_covGPP<-as.data.frame(as.table(cor(x=Ich_cov_1GPP,use="complete.obs")))

Ich_covGPP<-filter(Ich_covGPP, Var1=="GPPavg")
Ich_covER<-filter(Ich_covER, Var1=="ER")

Ich_covGPP$site<-'Ich'
Ich_covER$site<-'Ich'

Ich_covGPP$ID<-2
Ich_covER$ID<-2


####LF CoV#####


LF_cov<-LF[,x]

LF_cov$day <- as.Date(LF_cov$Date)
LF_cov<- aggregate(LF_cov, by=list(LF_cov$day), FUN='mean')

LF_cov$day1<-LF_cov$day + 1
LF_cov$day2<-LF_cov$day + 2
LF_cov$day3<-LF_cov$day + 3
LF_cov$day4<-LF_cov$day + 4
LF_cov$day5<-LF_cov$day + 5
LF_cov$day6<-LF_cov$day + 6
LF_cov$day7<-LF_cov$day + 7
LF_cov$day8<-LF_cov$day + 8
LF_cov$day9<-LF_cov$day + 9
LF_cov$day10<-LF_cov$day + 10
LF_cov$day15<-LF_cov$day + 15
LF_cov$day20<-LF_cov$day + 20
LF_cov$day25<-LF_cov$day + 25
LF_cov$day30<-LF_cov$day + 30


LF_cov_day0<-LF_cov[,c(6,2,3,4)]
LF_cov_day0 <- LF_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

LF_cov_day1<-LF_cov[,c(7,2,3,4)]
LF_cov_day1<-rename(LF_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

LF_cov_day2<-LF_cov[,c(8,2,3,4)]
LF_cov_day2<-rename(LF_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

LF_cov_day3<-LF_cov[,c(9,2,3,4)]
LF_cov_day3<-rename(LF_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

LF_cov_day4<-LF_cov[,c(10,2,3,4)]
LF_cov_day4<-rename(LF_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

LF_cov_day5<-LF_cov[,c(11,2,3,4)]
LF_cov_day5<-rename(LF_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

LF_cov_day6<-LF_cov[,c(12,2,3,4)]
LF_cov_day6<-rename(LF_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

LF_cov_day7<-LF_cov[,c(13,2,3,4)]
LF_cov_day7<-rename(LF_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

LF_cov_day8<-LF_cov[,c(14,2,3,4)]
LF_cov_day8<-rename(LF_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

LF_cov_day9<-LF_cov[,c(15,2,3,4)]
LF_cov_day9<-rename(LF_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

LF_cov_day10<-LF_cov[,c(16,2,3,4)]
LF_cov_day10<-rename(LF_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

LF_cov_day15<-LF_cov[,c(17,2,3,4)]
LF_cov_day15<-rename(LF_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

LF_cov_day20<-LF_cov[,c(18,2,3,4)]
LF_cov_day20<-rename(LF_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

LF_cov_day25<-LF_cov[,c(19,2,3,4)]
LF_cov_day25<-rename(LF_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

LF_cov_day30<-LF_cov[,c(20,2,3,4)]
LF_cov_day30<-rename(LF_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')



LF_cov_1<-left_join(LF_cov_day0, LF_cov_day1, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day2, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day3, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day4, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day5, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day6, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day7, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day8, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day9, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day10, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day15, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day20, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day25, by='day')
LF_cov_1<-left_join(LF_cov_1, LF_cov_day30, by='day')


LF_cov_1<-na.omit(LF_cov_1)

ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

LF_cov_1ER<-LF_cov_1[,ER]
LF_cov_1GPP<-LF_cov_1[,GPP]
LF_cov_1NEP<-LF_cov_1[,NEP]

corrplot(cor(x=LF_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

LF_covER<-as.data.frame(as.table(cor(x=LF_cov_1ER,use="complete.obs")))
LF_covGPP<-as.data.frame(as.table(cor(x=LF_cov_1GPP,use="complete.obs")))

LF_covGPP<-filter(LF_covGPP, Var1=="GPPavg")
LF_covER<-filter(LF_covER, Var1=="ER")

LF_covGPP$site<-'LF'
LF_covER$site<-'LF'

LF_covGPP$ID<- 4
LF_covER$ID<- 4


####GB CoV#####


GB_cov<-GB[,x]

GB_cov$day <- as.Date(GB_cov$Date)
GB_cov<- aggregate(GB_cov, by=list(GB_cov$day), FUN='mean')


GB_cov$day1<-GB_cov$day + 1
GB_cov$day2<-GB_cov$day + 2
GB_cov$day3<-GB_cov$day + 3
GB_cov$day4<-GB_cov$day + 4
GB_cov$day5<-GB_cov$day + 5
GB_cov$day6<-GB_cov$day + 6
GB_cov$day7<-GB_cov$day + 7
GB_cov$day8<-GB_cov$day + 8
GB_cov$day9<-GB_cov$day + 9
GB_cov$day10<-GB_cov$day + 10
GB_cov$day15<-GB_cov$day + 15
GB_cov$day20<-GB_cov$day + 20
GB_cov$day25<-GB_cov$day + 25
GB_cov$day30<-GB_cov$day + 30



GB_cov_day0<-GB_cov[,c(6,2,3,4)]
GB_cov_day0 <- GB_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

GB_cov_day1<-GB_cov[,c(7,2,3,4)]
GB_cov_day1<-rename(GB_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

GB_cov_day2<-GB_cov[,c(8,2,3,4)]
GB_cov_day2<-rename(GB_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

GB_cov_day3<-GB_cov[,c(9,2,3,4)]
GB_cov_day3<-rename(GB_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

GB_cov_day4<-GB_cov[,c(10,2,3,4)]
GB_cov_day4<-rename(GB_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

GB_cov_day5<-GB_cov[,c(11,2,3,4)]
GB_cov_day5<-rename(GB_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

GB_cov_day6<-GB_cov[,c(12,2,3,4)]
GB_cov_day6<-rename(GB_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

GB_cov_day7<-GB_cov[,c(13,2,3,4)]
GB_cov_day7<-rename(GB_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

GB_cov_day8<-GB_cov[,c(14,2,3,4)]
GB_cov_day8<-rename(GB_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

GB_cov_day9<-GB_cov[,c(15,2,3,4)]
GB_cov_day9<-rename(GB_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

GB_cov_day10<-GB_cov[,c(16,2,3,4)]
GB_cov_day10<-rename(GB_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

GB_cov_day15<-GB_cov[,c(17,2,3,4)]
GB_cov_day15<-rename(GB_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

GB_cov_day20<-GB_cov[,c(18,2,3,4)]
GB_cov_day20<-rename(GB_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

GB_cov_day25<-GB_cov[,c(19,2,3,4)]
GB_cov_day25<-rename(GB_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

GB_cov_day30<-GB_cov[,c(20,2,3,4)]
GB_cov_day30<-rename(GB_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')



GB_cov_1<-left_join(GB_cov_day0, GB_cov_day1, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day2, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day3, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day4, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day5, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day6, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day7, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day8, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day9, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day10, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day15, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day20, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day25, by='day')
GB_cov_1<-left_join(GB_cov_1, GB_cov_day30, by='day')


GB_cov_1<-na.omit(GB_cov_1)

ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

GB_cov_1ER<-GB_cov_1[,ER]
GB_cov_1GPP<-GB_cov_1[,GPP]
GB_cov_1NEP<-GB_cov_1[,NEP]

c<-corrplot(cor(x=GB_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

GB_covER<-as.data.frame(as.table(cor(x=GB_cov_1ER,use="complete.obs")))
GB_covGPP<-as.data.frame(as.table(cor(x=GB_cov_1GPP,use="complete.obs")))

GB_covGPP<-filter(GB_covGPP, Var1=="GPPavg")
GB_covER<-filter(GB_covER, Var1=="ER")

GB_covGPP$site<-'GB'
GB_covER$site<-'GB'

GB_covGPP$ID<-3
GB_covER$ID<-3



####Otter CoV#####

Otter_cov<-Otter[,x]

Otter_cov$day <- as.Date(Otter_cov$Date)
Otter_cov<- aggregate(Otter_cov, by=list(Otter_cov$day), FUN='mean')


Otter_cov$day1<-Otter_cov$day + 1
Otter_cov$day2<-Otter_cov$day + 2
Otter_cov$day3<-Otter_cov$day + 3
Otter_cov$day4<-Otter_cov$day + 4
Otter_cov$day5<-Otter_cov$day + 5
Otter_cov$day6<-Otter_cov$day + 6
Otter_cov$day7<-Otter_cov$day + 7
Otter_cov$day8<-Otter_cov$day + 8
Otter_cov$day9<-Otter_cov$day + 9
Otter_cov$day10<-Otter_cov$day + 10
Otter_cov$day15<-Otter_cov$day + 15
Otter_cov$day20<-Otter_cov$day + 20
Otter_cov$day25<-Otter_cov$day + 25
Otter_cov$day30<-Otter_cov$day + 30


Otter_cov_day0<-Otter_cov[,c(6,2,3,4)]
Otter_cov_day0 <- Otter_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

Otter_cov_day1<-Otter_cov[,c(7,2,3,4)]
Otter_cov_day1<-rename(Otter_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

Otter_cov_day2<-Otter_cov[,c(8,2,3,4)]
Otter_cov_day2<-rename(Otter_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

Otter_cov_day3<-Otter_cov[,c(9,2,3,4)]
Otter_cov_day3<-rename(Otter_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

Otter_cov_day4<-Otter_cov[,c(10,2,3,4)]
Otter_cov_day4<-rename(Otter_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

Otter_cov_day5<-Otter_cov[,c(11,2,3,4)]
Otter_cov_day5<-rename(Otter_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

Otter_cov_day6<-Otter_cov[,c(12,2,3,4)]
Otter_cov_day6<-rename(Otter_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

Otter_cov_day7<-Otter_cov[,c(13,2,3,4)]
Otter_cov_day7<-rename(Otter_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

Otter_cov_day8<-Otter_cov[,c(14,2,3,4)]
Otter_cov_day8<-rename(Otter_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

Otter_cov_day9<-Otter_cov[,c(15,2,3,4)]
Otter_cov_day9<-rename(Otter_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

Otter_cov_day10<-Otter_cov[,c(16,2,3,4)]
Otter_cov_day10<-rename(Otter_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

Otter_cov_day15<-Otter_cov[,c(17,2,3,4)]
Otter_cov_day15<-rename(Otter_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

Otter_cov_day20<-Otter_cov[,c(18,2,3,4)]
Otter_cov_day20<-rename(Otter_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

Otter_cov_day25<-Otter_cov[,c(19,2,3,4)]
Otter_cov_day25<-rename(Otter_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

Otter_cov_day30<-Otter_cov[,c(20,2,3,4)]
Otter_cov_day30<-rename(Otter_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')



Otter_cov_1<-left_join(Otter_cov_day0, Otter_cov_day1, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day2, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day3, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day4, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day5, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day6, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day7, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day8, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day9, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day10, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day15, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day20, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day25, by='day')
Otter_cov_1<-left_join(Otter_cov_1, Otter_cov_day30, by='day')





ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

Otter_cov_1ER<-Otter_cov_1[,ER]
Otter_cov_1GPP<-Otter_cov_1[,GPP]
Otter_cov_1NEP<-Otter_cov_1[,NEP]

d<-corrplot(cor(x=Otter_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

Otter_covER<-as.data.frame(as.table(cor(x=Otter_cov_1ER,use="complete.obs")))
Otter_covGPP<-as.data.frame(as.table(cor(x=Otter_cov_1GPP,use="complete.obs")))

Otter_covGPP<-filter(Otter_covGPP, Var1=="GPPavg")
Otter_covER<-filter(Otter_covER, Var1=="ER")

Otter_covGPP$site<-'Otter'
Otter_covER$site<-'Otter'

Otter_covGPP$ID<-5
Otter_covER$ID<-5


####AM CoV#####
AM_cov<-AM[,x]

AM_cov$day <- as.Date(AM_cov$Date)
AM_cov<- aggregate(AM_cov, by=list(AM_cov$day), FUN='mean')


AM_cov$day1<-AM_cov$day + 1
AM_cov$day2<-AM_cov$day + 2
AM_cov$day3<-AM_cov$day + 3
AM_cov$day4<-AM_cov$day + 4
AM_cov$day5<-AM_cov$day + 5
AM_cov$day6<-AM_cov$day + 6
AM_cov$day7<-AM_cov$day + 7
AM_cov$day8<-AM_cov$day + 8
AM_cov$day9<-AM_cov$day + 9
AM_cov$day10<-AM_cov$day + 10
AM_cov$day15<-AM_cov$day + 15
AM_cov$day20<-AM_cov$day + 20
AM_cov$day25<-AM_cov$day + 25
AM_cov$day30<-AM_cov$day + 30


AM_cov_day0<-AM_cov[,c(6,2,3,4)]
AM_cov_day0 <- AM_cov_day0 %>% arrange(day) %>%  group_by(consec = cumsum(c(TRUE, diff(day) >= 1))) %>% ungroup()

AM_cov_day1<-AM_cov[,c(7,2,3,4)]
AM_cov_day1<-rename(AM_cov_day1, 'day'='day1', 'ER1'='ER', 'GPP1'='GPPavg', "NEP1"='NEP')

AM_cov_day2<-AM_cov[,c(8,2,3,4)]
AM_cov_day2<-rename(AM_cov_day2, 'day'='day2', 'ER2'='ER', 'GPP2'='GPPavg', "NEP2"='NEP')

AM_cov_day3<-AM_cov[,c(9,2,3,4)]
AM_cov_day3<-rename(AM_cov_day3, 'day'='day3', 'ER3'='ER', 'GPP3'='GPPavg', "NEP3"='NEP')

AM_cov_day4<-AM_cov[,c(10,2,3,4)]
AM_cov_day4<-rename(AM_cov_day4, 'day'='day4', 'ER4'='ER', 'GPP4'='GPPavg', "NEP4"='NEP')

AM_cov_day5<-AM_cov[,c(11,2,3,4)]
AM_cov_day5<-rename(AM_cov_day5, 'day'='day5', 'ER5'='ER', 'GPP5'='GPPavg', "NEP5"='NEP')

AM_cov_day6<-AM_cov[,c(12,2,3,4)]
AM_cov_day6<-rename(AM_cov_day6, 'day'='day6', 'ER6'='ER', 'GPP6'='GPPavg', "NEP6"='NEP')

AM_cov_day7<-AM_cov[,c(13,2,3,4)]
AM_cov_day7<-rename(AM_cov_day7, 'day'='day7', 'ER7'='ER', 'GPP7'='GPPavg', "NEP7"='NEP')

AM_cov_day8<-AM_cov[,c(14,2,3,4)]
AM_cov_day8<-rename(AM_cov_day8, 'day'='day8', 'ER8'='ER', 'GPP8'='GPPavg', "NEP8"='NEP')

AM_cov_day9<-AM_cov[,c(15,2,3,4)]
AM_cov_day9<-rename(AM_cov_day9, 'day'='day9', 'ER9'='ER', 'GPP9'='GPPavg', "NEP9"='NEP')

AM_cov_day10<-AM_cov[,c(16,2,3,4)]
AM_cov_day10<-rename(AM_cov_day10, 'day'='day10', 'ER10'='ER', 'GPP10'='GPPavg', "NEP10"='NEP')

AM_cov_day15<-AM_cov[,c(17,2,3,4)]
AM_cov_day15<-rename(AM_cov_day15, 'day'='day15', 'ER15'='ER', 'GPP15'='GPPavg', "NEP15"='NEP')

AM_cov_day20<-AM_cov[,c(18,2,3,4)]
AM_cov_day20<-rename(AM_cov_day20, 'day'='day20', 'ER20'='ER', 'GPP20'='GPPavg', "NEP20"='NEP')

AM_cov_day25<-AM_cov[,c(19,2,3,4)]
AM_cov_day25<-rename(AM_cov_day25, 'day'='day25', 'ER25'='ER', 'GPP25'='GPPavg', "NEP25"='NEP')

AM_cov_day30<-AM_cov[,c(20,2,3,4)]
AM_cov_day30<-rename(AM_cov_day30, 'day'='day30', 'ER30'='ER', 'GPP30'='GPPavg', "NEP30"='NEP')



AM_cov_1<-left_join(AM_cov_day0, AM_cov_day1, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day2, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day3, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day4, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day5, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day6, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day7, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day8, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day9, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day10, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day15, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day20, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day25, by='day')
AM_cov_1<-left_join(AM_cov_1, AM_cov_day30, by='day')

AM_cov_1<-na.omit(AM_cov_1)

ER<-c('ER','ER1','ER2','ER3','ER4','ER5','ER6','ER7','ER8','ER9','ER10','ER15','ER20','ER25','ER30')
GPP<-c('GPPavg','GPP1','GPP2','GPP3','GPP5',"GPP6","GPP7" ,'GPP8','GPP9','GPP10','GPP15','GPP20','GPP25','GPP30')
NEP<-c('NEP','NEP1','NEP2','NEP3','NEP5',"NEP6","NEP7" ,'NEP8','NEP9','NEP10','NEP15','NEP20','NEP25','NEP30')

AM_cov_1ER<-AM_cov_1[,ER]
AM_cov_1GPP<-AM_cov_1[,GPP]
AM_cov_1NEP<-AM_cov_1[,NEP]

e<-corrplot(cor(x=AM_cov_1NEP,use="complete.obs"),
         method = "number",
         type = "upper")

AM_covER<-as.data.frame(as.table(cor(x=AM_cov_1ER,use="complete.obs")))
AM_covGPP<-as.data.frame(as.table(cor(x=AM_cov_1GPP,use="complete.obs")))

AM_covGPP<-filter(AM_covGPP, Var1=="GPPavg")
AM_covER<-filter(AM_covER, Var1=="ER")

AM_covGPP$site<-'AM'
AM_covER$site<-'AM'

AM_covGPP$ID<-6
AM_covER$ID<-6
#######

covGPP<-rbind(US27_covGPP, Ich_covGPP,LF_covGPP,GB_covGPP,Otter_covGPP,AM_covGPP)
covER<-rbind(US27_covER, Ich_covER,LF_covER,GB_covER,Otter_covER,AM_covER)

covER<- covER %>% mutate(future = case_when(
  Var2=='ER' ~ NA_real_,
  Var2=='ER1' ~ 1,
  Var2=='ER2'~ 2,
  Var2=='ER3' ~ 3,
  Var2=='ER3' ~ 4,
  Var2=='ER5' ~ 5,
  Var2=='ER6' ~ 6,
  Var2=='ER7' ~ 7,
  Var2=='ER8' ~ 8,
  Var2=='ER9' ~ 9,
  Var2=='ER10' ~ 10,
  Var2=='ER15' ~ 15,
  Var2=='ER20' ~ 20,
  Var2=='ER25' ~ 25,
  Var2=='ER30' ~ 30))


covGPP<- covGPP %>% mutate(future = case_when(
  Var2=='GPPavg' ~ NA_real_,
  Var2=='GPP1' ~ 1,
  Var2=='GPP2'~ 2,
  Var2=='GPP3' ~ 3,
  Var2=='GPP3' ~ 4,
  Var2=='GPP5' ~ 5,
  Var2=='GPP6' ~ 6,
  Var2=='GPP7' ~ 7,
  Var2=='GPP8' ~ 8,
  Var2=='GPP9' ~ 9,
  Var2=='GPP10' ~ 10,
  Var2=='GPP15' ~ 15,
  Var2=='GPP20' ~ 20,
  Var2=='GPP25' ~ 25,
  Var2=='GPP30' ~ 30))

colors<-c(AR1='blue', AR10='orange')

AR_GPP<-filter(covGPP, future==1 | future==10 )
AR_ER<-filter(covER, future==1 | future==10 )

AR_GPP<- AR_GPP %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))
  
AR_ER<- AR_ER %>% mutate(AR = case_when(
  future==1 ~ 'AR1',
  future==10 ~ 'AR10'))

AR_GPP$Freq<-abs(AR_GPP$Freq)
AR_ER$Freq<-abs(AR_ER$Freq)

ar_er <- read_csv("Z:/SpringsProject_Sam&Paul/ar_er.csv")
ar_gpp <- read_csv("Z:/SpringsProject_Sam&Paul/ar_gpp.csv")

ggplot(ar_er, aes(x=ar1, fill=ar10)) +
  geom_density()+ggtitle("Appling et al., (2020)")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y =element_blank(),
        axis.title.x =element_blank(),
        plot.title = element_text(size = 19),
        legend.position = "none",
        legend.text= element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "white"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "white"))

ggplot(AR_ER, aes(x=ID,y=Freq,color=AR))+
    ylab("Autocorrelation")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+
    geom_hline(yintercept = 0.778608932)+
    ggtitle("ER Stability")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 16),
          axis.title.x =element_text(size = 16),
          plot.title = element_text(size = 19),
          legend.position = "bottom",
          legend.text= element_text(size = 12),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
(ERAR<-plot_grid(ERden,ERAR1,ERAR10, align = "v", ncol = 1, rel_heights = c(0.3,0.5,0.5)))



ggplot(AR_GPP, aes(x=ID,y=Freq,color=AR))+
    ylab("Autocorrelation")+
    xlab("RR Frequency Gradient")+
    scale_colour_manual(name="", values = colors )+
    geom_point(size=4)+
    geom_hline(yintercept = 0.480380458)+
    ggtitle("GPP Stability")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 16, angle=0),
          axis.title.y =element_text(size = 16),
          axis.title.x =element_text(size = 16),
          plot.title = element_text(size = 19),
          legend.position = "bottom",
          legend.text= element_text(size = 12),
          panel.background = element_rect(fill = 'white'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
 (GPPAR<-plot_grid(GPPden,GPPAR1,GPPAR10, align = "v", ncol = 1, rel_heights = c(0.3,0.5,0.5)))


plot_grid(a,b, nrow=1)
a
b
dev.new()
dev.new()

plot_grid(GPPAR,ERAR, ncol = 2)
dev.new
