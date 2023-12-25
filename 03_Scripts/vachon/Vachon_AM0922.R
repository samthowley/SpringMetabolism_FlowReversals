rm(list=ls())

library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(broom)
library(writexl)
library(epitools)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(readxl)
library(weathermetrics)
library(ggplot2)
library(StreamMetabolism)
library(car)
library(imputeTS)
library(zoo)

setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master")
AllenMill <- read_excel("AllenMill.xlsx",
                        col_types = c("date", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric","numeric",
                                      "numeric", "numeric","numeric"))

for(i in 1:nrow(AllenMill)){
 if( AllenMill$Date[i] >= as.Date("2023-04-11") & AllenMill$Date[i] >= as.Date("2023-05-09")){
   AllenMill$DO[i]<-NA}
  else {AllenMill$DO[i]<- AllenMill$DO[i]-0 }}

x<-c("Date","CO2" ,"DO","Mouth_DO_sat","Mouth_Temp_C")
AllenMill<-AllenMill[,x]


AllenMill$mouthTemp_K<-AllenMill$Mouth_Temp_C+273.15
AllenMill$exp<-2400*((1/AllenMill$mouthTemp_K)-(1/298.15))
AllenMill$KH<-0.034*2.178^(AllenMill$exp)#mol/L/atm

AllenMill$CO2_atm<-AllenMill$CO2/1000000
AllenMill$CO2_mol<-AllenMill$CO2_atm*AllenMill$KH
AllenMill$DO_mol<-AllenMill$DO/32000

AllenMill$Do_Sat<-Cs(AllenMill$Mouth_Temp_C)
AllenMill$DO_Sat_mol<-AllenMill$Do_Sat/32000
(AllenMill$CO2_Sat_mol<-(420/1000000)*AllenMill$KH)

AllenMill$'O2_mol_L'<-AllenMill$KH*(AllenMill$DO_mol-AllenMill$DO_Sat_mol)
AllenMill$'CO2_mol_L'<-AllenMill$KH*(AllenMill$CO2_mol-AllenMill$CO2_Sat_mol)

AllenMill$'CO2_mmol_L'<-AllenMill$'CO2_mol_L'*10^3
AllenMill$'O2_mmol_L'<-AllenMill$'O2_mol_L'*10^3

AllenMill$day <- as.Date(AllenMill$Date)
AllenMill.day <- aggregate(AllenMill, by=list(AllenMill$day), FUN='mean')

(a<-ggplot() + geom_point(data=AllenMill, aes(x=CO2_mmol_L, y=O2_mmol_L), alpha=0.1) + #make transparent cloud
    geom_point(data=AllenMill.day, aes(x=CO2_mmol_L, y=O2_mmol_L, color=day),size=2) + #daily mean
    theme_classic() + #white background
    xlab(expression('CO'[2]*' (mmol/m^2/day)')) + ylab(expression('O'[2]*' (mmol/m^2/day)'))+ #axis titles
    scale_color_gradient(low="red", high="green", trans='date')+
    ggtitle("Allen Mill") +
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7))+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"))#+  #-1:1 lin

x<-c('Date',"CO2_mmol_L","O2_mmol_L","day")
AllenMill<-AllenMill[,x]
#####
dist_point_line <- function(a, slope, intercept) {
  b = c(1, intercept+slope)
  c = c(-intercept/slope,0)       
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  return(abs(det(m))/sqrt(sum(v1*v1)))
}

AllenMill$O2_mmol_L <- na_interpolation(AllenMill$O2_mmol_L, option='linear')
AllenMill$CO2_mmol_L <- na_interpolation(AllenMill$CO2_mmol_L, option='linear')

days <- unique(AllenMill$day) #extract day
days <- days[-1] #remove first da
last <- length(days-4) #last possible day for moving average (a week)

coupling.indicator <- data.frame(matrix(ncol=9, nrow=last-3))
names(coupling.indicator) <- c('first.day', 'mid.day', 'last.day', 'cen.x', 'cen.y', 'offset', 'stretch', 'width', 'slope')

for(i in 4:last){
  print(i)
  
  first.day <- days[i-3] #first day of a week
  week <- seq(first.day, first.day +6, by='day') #matrix of a week
  
  data.week <- AllenMill[AllenMill$day %in% week,] #extract data for a week
  
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

library(zoo)
coupling.indicator$mid.day <- as.Date(coupling.indicator$mid.day)
coupling.indicator$first.day <- as.Date(coupling.indicator$first.day)
coupling.indicator$last.day <- as.Date(coupling.indicator$last.day)

first.day<-coupling.indicator[,-c(2,3)]
last.day<-coupling.indicator[,-c(1,2)]
mid.day<-coupling.indicator[,-c(1,3)]

write_xlsx(first.day, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill/AM_ellipse.xlsx")

########

AM_ellipse <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill/AM_ellipse.xlsx")
AM_ellipse<- AM_ellipse %>%
  mutate(Day = day(first.day), 
         Month = month(first.day),
         year = year(first.day))

AllenMill<- AllenMill %>%
  mutate(Day = day(Date), 
         Month = month(Date),
         year = year(Date))

AllenMill<-left_join(AM_ellipse,AllenMill, by=c('Day', 'Month', 'year'))

AllenMill<- AllenMill %>% mutate(slope_set = case_when(slope < -1.01 ~ "<-1",
                                                       slope >= -1.01 & slope <= -0.99~ "-1",
                                                       slope > -0.99 ~ ">-1"))
library(data.table)
dt=data.table(AllenMill)
regression<-dt[,as.list(coef(lm(CO2_mmol_L~O2_mmol_L))), by = first.day]
regression<-filter(regression, O2_mmol_L> -0.001)
drop.days<-regression$first.day
AllenMill <- AllenMill[!(AllenMill$first.day %in% drop.days), ]


(b<-ggplot()+
    geom_point(data=AllenMill, aes(x=CO2_mmol_L, y=O2_mmol_L))+
    geom_smooth(aes(x=CO2_mmol_L, y=O2_mmol_L, group=slope, color=slope_set), size=0.75, data=AllenMill, se = FALSE, method='lm')+
    xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
    scale_color_manual(values = c("<-1" = "purple",
                                  "-1"="orange",
                                  ">-1"="steelblue"))+
    ggtitle('')+
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
    geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
    geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size=15),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'),
         legend.text=element_text(size=15),
          legend.title=element_text(size=15),
          legend.key.size = unit(1, "cm"))+
    guides(color=guide_legend(title="Slope")))


write_xlsx(AllenMill, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill/AM_final.xlsx")
AllenMill <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill/AM_final.xlsx")
x<-c("Date","O2_mmol_L","CO2_mmol_L","slope","offset")
AllenMill<-AllenMill[,x]

AM <- read_excel("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))
master<-left_join(AM, AllenMill, by='Date')
write_xlsx(master, "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/SpringsProject_Sam&Paul/Master/AllenMill.xlsx")

