rm(list=ls())

library(ggpubr)
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(epitools)
library(xlsx)
library(openxlsx)
library(gridExtra)
library(grid)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(measurements)
library(ggnewscale)
#############



dev.new()


######
setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Otter")
Otter<- read_excel("Otter_final.xlsx", 
                          col_types = c("skip", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", "numeric", 
                          "skip", "skip", "date", "numeric", 
                          "numeric", "numeric", "skip"))

Otter<- Otter %>% mutate(RI = case_when(
  Date> "2023-01-26" & Date<"2023-4-25"~ 2))

Otter_springFR<-filter(Otter, RI== "2")
Otter_springFR$days <- as.Date(Otter_springFR$Date)

Otter_springFR <- Otter_springFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

#OtterFR2$consec<-as.numeric(OtterFR2$consec)


#OtterFR<- OtterFR2 %>% mutate(filter = case_when(days== "2023-01-28"|days== "2023-02-04"|
                                                  days== "2023-02-07"|days== "2023-02-09"|
                                                  days== "2023-02-14"|days== "2023-02-16"|
                                                  days== "2023-02-19"|days== "2023-02-21"|
                                                  days== "2023-02-27"|days== "2023-02-28"|
                                                  days== "2023-03-04"|
                                                  days== "2023-03-05"|days== "2023-03-06"|
                                                  days== "2023-03-07"|days== "2023-04-01"|
                                                  days== "2023-04-14"|
                                                  days== "2023-04-25"
                                                  ~2))
#OtterFR1<-filter(OtterFR,filter==2)

(a<-ggplot()+
  geom_point(data=Otter_springFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
   geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
               data=Otter_springFR, se = FALSE, method='lm', size=2) +
  ggtitle("Otter", subtitle ="01/26/2023-04/25/2023")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  scale_color_gradient(low="darkgreen", high="green")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        legend.position = "bottom",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+ 
  labs(color = "Days"))

(a_center<-ggplot()+
    geom_point(data=Otter_springFR, aes(x=cen.x, y=cen.y, color=consec, size=3))+
    ggtitle("Otter", subtitle ="01/26/2023-04/25/2023")+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    scale_color_gradient(low="darkgreen", high="green")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))

(a_slope<-ggplot()+
    geom_point(data=Otter_springFR, aes(x=stage_avg, y=slope, color=consec, size=3))+
    ggtitle("Otter", subtitle ="01/26/2023-04/25/2023")+
    xlab("Stage (m)") + ylab("slope")+
    scale_color_gradient(low="darkgreen", high="green")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))

(a_offset<-ggplot()+
    geom_point(data=Otter_springFR, aes(x=stage_avg, y=offset, color=consec, size=3))+
    ggtitle("Otter", subtitle ="01/26/2023-04/25/2023")+
    xlab("Stage (m)") + ylab("offset")+
    scale_color_gradient(low="darkgreen", high="green")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+ 
    labs(color = "Days"))

names(Otter_springFR)
plot_grid(a,a1, nrow=2)



Otter<- Otter %>% mutate(RI = case_when(
  Date> "2022-08-25" & Date<"2022-10-25"~ 2))

Otter_fallFR<-filter(Otter_fallFR, RI== "2")
Otter_fallFR$days <- as.Date(Otter_fallFR$Date)

Otter_fallFR <- Otter_fallFR %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

Otter_fallFR$consec<-as.numeric(Otter_fallFR$consec)


#OtterFR$consec<-as.numeric(OtterFR$consec)
#OtterFR$days <- as.Date(OtterFR$Date)
#days== "2022-08-26"| days== "2022-08-28"|
  
#OterFR<- OtterFR %>% mutate(filter = case_when(days== "2022-08-26"| days== "2022-08-28"|
                                                 days== "2022-09-01"|days== "2022-09-06"|
                                              days== "2022-09-08"|
                                              days== "2022-09-11"|days== "2022-09-21"
                                              |days== "2022-09-28"|days== "2022-10-03"
                                              |days== "2022-10-18"
                                              ~2))
                                              
#OtterFR2<-filter(OterFR,filter==2)

(a<-ggplot()+
  geom_point(data=Otter_fallFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.2))+
    geom_smooth(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
                data=Otter_fallFR, se = FALSE, method='lm', size=2) +
    ggtitle("Otter", subtitle ="08/25/2022-10/25/2022")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  scale_color_gradient(low="blue", high="purple")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        legend.position = "bottom",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Days"))

(a<-ggplot()+
    geom_point(data=Otter_fallFR, aes(x=cen.x, y=cen.y, color=consec, size=3))+
    ggtitle("Otter", subtitle ="08/25/2022-10/25/2022")+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    scale_color_gradient(low="blue", high="purple")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Days"))

(a<-ggplot()+
    geom_point(data=Otter_fallFR, aes(x=stage_avg, y=slope, color=consec, size=3))+
    ggtitle("Otter", subtitle ="08/25/2022-10/25/2022")+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    scale_color_gradient(low="blue", high="purple")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          legend.position = "bottom",
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Days"))

(a<-ggplot()+
         geom_point(data=Otter_fallFR, aes(x=stage_avg, y=offset, color=consec, size=3))+
         ggtitle("Otter", subtitle ="08/25/2022-10/25/2022")+
         xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
         scale_color_gradient(low="blue", high="purple")+
         theme(axis.text.x = element_text(size = 12, angle=0),
               axis.text.y = element_text(size = 12, angle=0),
               axis.title =element_text(size = 15, angle=0),
               plot.title = element_text(size = 15, angle=0),
               legend.key.size = unit(0.8, 'cm'),
               legend.text=element_text(size=12),
               legend.title =element_text(size=15),
               legend.position = "bottom",
               panel.background = element_rect(fill = 'white'),
               panel.grid.major = element_line(color = 'gray'),
               panel.grid.minor = element_line(color = 'gray'))+
         labs(color = "Days"))
)
###########


######
setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Allen Mill")
AM_final <- read_excel("AM_final.xlsx", col_types = c("skip", 
                                                      "skip", "skip", "skip", "numeric", "numeric", 
                                                      "numeric", "skip", "skip", "date", "numeric", 
                                                      "numeric", "numeric", "skip", "skip"))

AM<- AM_final %>% mutate(RI = case_when(
  Date> "2023-01-18" & Date<"2023-04-16"~ 2))

RI<-filter(AM, RI== "2")
RI$days <- as.Date(RI$Date)
RI <- RI %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()


AMFR<-rbind(RI)

AMFR$consec<-as.numeric(AMFR$consec)


OtterFR<- AMFR %>% mutate(filter = case_when(days== "2023-01-18"|days== "2023-01-26"|
                                            days== "2023-02-02"|days== "2023-02-11"|
                                            days== "2023-02-20"|
                                            days== "2023-02-25"|
                                            days== "2023-03-03"|days== "2023-03-06"|
                                            days== "2023-03-13"|days== "2023-03-20"|
                                            days== "2023-03-27"|days== "2023-04-05"|
                                            days== "2023-04-16"~2))
                                            

AMFR2<-filter(OtterFR,filter==2)

(b<-ggplot()+
  geom_point(data=AMFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=AMFR2, type = "norm", size=2) +
  ggtitle("Allen Mill", subtitle ="01/18/2023-04/16/2023")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  scale_color_gradient(low="blue", high="violet")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        legend.position ="bottom",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Days"))

######

library(ggnewscale)
  
ggplot() +
  geom_point(data=OtterFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec, alpha=0.001))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec),
               data=OtterFR1, type = "norm", size=2)+
xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
    scale_color_gradient(low="darkgreen", high="green")+
  labs(color = "Otter Days")+
  
  new_scale_color() +
  geom_point(data=AMFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=AMFR2, type = "norm", size=2) +
  xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
  scale_color_gradient(low="blue", high="violet")+
  labs(color = "Allen Mill Days")+
  ggtitle("01/18/2023-04/16/2023")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))
  
##########

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Gilchrist Blue")
GB_final <- read_excel("GB_final.xlsx", col_types = c("skip", 
                                                      "skip", "skip", "skip", "numeric", "numeric", 
                                                      "numeric", "skip", "skip", "skip", "date", 
                                                      "numeric", "numeric", "numeric", "skip", 
                                                      "skip", "skip"))
GB<- GB_final %>% mutate(RI = case_when(
  Date> "2022-08-17" & Date<"2022-10-28"~ 2))

RI<-filter(GB, RI== "2")

RI$days <- as.Date(RI$Date)
RI <- RI %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

RI$consec<-as.numeric(RI$consec)

GBRI<- RI %>% mutate(filter = case_when(days== "2022-08-17"|
                                        days== "2022-08-31"|
                                        days== "2022-09-09"|days== "2022-09-13"
                                        |days== "2022-09-18"|days== "2022-09-23"
                                        |days== "2022-10-13"|days== "2022-10-16"
                                        |days== "2022-10-15"|days== "2022-10-25"
                                        ~2))
                                             


GBstg<-filter(GBRI,filter==2)

(a<-ggplot()+
  geom_point(data=GBRI, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=GBstg, type = "norm",size=2) +
  ggtitle("Gilchrist Blue", subtitle ="08/17/2022-10/28/2022")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  scale_color_gradient(low="red", high="orange")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Days"))
######

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Little Fanning")
LF_final <- read_excel("LF_final.xlsx", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", 
                                                      "numeric", "skip"))
LF<- LF_final %>% mutate(RI = case_when(
  Date> "2023-02-02" & Date<"2023-03-26"~ 2))

RI<-filter(LF, RI== "2")
ggplot(RI, aes(x=Date))+
  geom_line(aes(y=O2_mmol_m2_1d, color="FDOM"), size=0.8)


RI$days <- as.Date(RI$Date)
RI <- RI %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()


LFFR<-rbind(RI)

LFFR$consec<-as.numeric(LFFR$consec)


OtterFR<- LFFR %>% mutate(filter = case_when(days== "2023-01-15"|days== "2023-02-02"
                                             |days== "2023-02-06"|days== "2023-02-11"
                                             |days== "2023-03-06"|days== "2023-03-22"|
                                              days== "2023-03-26"
                                             ~2))
                                               

LFFR2<-filter(OtterFR,filter==2)

(b<-ggplot()+
  geom_point(data=LFFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=LFFR2, type = "norm",size=2) +
  ggtitle("Little Fanning", subtitle ="02/02/2023-03/26/2023")+
  xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
  scale_color_gradient(low="darkgreen", high="green")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))+
  labs(color = "Days"))









#######

setwd("Z:/SpringsProject_Sam&Paul/Master/ellipse/Ichetucknee")
Ich_final <- read_excel("Ich_final.xlsx", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))
Ich<- Ich_final %>% mutate(RI = case_when(
  Date> "2023-01-25" & Date<"2023-04-01"~ 2))

RI<-filter(Ich, RI== "2")

ggplot(RI, aes(x=Date))+
  geom_line(aes(y=O2_mmol_m2_1d, color="FDOM"), size=0.8)


RI$days <- as.Date(RI$Date)
RI <- RI %>%
  arrange(days) %>% 
  group_by(consec = cumsum(c(TRUE, diff(days) >= 1))) %>% 
  ungroup()

RI$consec<-as.numeric(RI$consec)


IchFR<- RI %>% mutate(filter = case_when(days== "2023-01-25"|days== "2023-02-06"|
                                           days== "2023-02-21"|days== "2023-03-03"|
                                            days== "2023-03-13"| days== "2023-03-20"|
                                           days== "2023-03-28"|days== "2023-04-01"
                                             ~2))


Ichstg<-filter(IchFR,filter==2)

(c<-ggplot()+
  geom_point(data=IchFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=Ichstg, type = "norm", size=2) +
    ggtitle("Ichetucknee", subtitle ="01/25/2023-04/01/2023")+
    xlab(expression(CO[2]~('mmol'/m^2/'day'))) + ylab(expression(O[2]~('mmol'/m^2/'day')))+
    scale_color_gradient(low="darkblue", high="violet")+
    theme(axis.text.x = element_text(size = 12, angle=0),
          axis.text.y = element_text(size = 12, angle=0),
          axis.title =element_text(size = 15, angle=0),
          plot.title = element_text(size = 15, angle=0),
          legend.key.size = unit(0.8, 'cm'),
          legend.position = 'bottom',
          legend.text=element_text(size=12),
          legend.title =element_text(size=15),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
          panel.grid.minor = element_line(color = 'gray'))+
    labs(color = "Days"))






ggplot()+
  geom_point(data=IchFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=Ichstg, type = "norm", size=2) +
  xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
  scale_color_gradient(low="darkblue", high="violet")+
  labs(color = "Ichetucknee Days")+
  
  new_scale_color() +
  geom_point(data=GBRI, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=GBstg, type = "norm",size=2) +
  xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
  scale_color_gradient(low="red", high="orange")+
  labs(color = "Gilchrist Blue Days")+
  
  new_scale_color() +
  geom_point(data=LFFR, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, color=consec))+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d, group=slope, color=consec, alpha=0.2),
               data=LFFR2, type = "norm", size=2) +
  xlab(expression('CO'[2]*' ('*mu*'mol/L/day)')) + ylab(expression('O'[2]*' ('*mu*'mol/L/day)'))+
  scale_color_gradient(low="darkgreen", high="green")+
  labs(color = "Little Fanning Days")+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 15, angle=0),
        legend.key.size = unit(0.8, 'cm'),
        legend.position = 'right',
        legend.text=element_text(size=12),
        legend.title =element_text(size=15),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'))


#########################3

cols<-c("Allen Mill"="blue", "Otter"="green", "Little Fanning"="orange",
        "Gilchrist Blue"="red", "Ichetucknee"="violet")

ggplot()+
  geom_point(data=AM_final, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d,
                                alpha=0.1),color="blue")+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),color="blue",
               data=AM_final, type = "norm", size=2) +
  new_scale_color() +
  
  geom_point(data=Otter_final, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d,
                                   alpha=0.1),color="green")+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),color="green",
               data=Otter_final, type = "norm", size=2) +
  new_scale_color() +
  
  geom_point(data=LF_final, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d,
                                alpha=0.1),color="orange")+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),color="darkorange2",
               data=LF_final, type = "norm", size=2) +
  new_scale_color() +
  
  geom_point(data=GB_final, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d,
                                alpha=0.1),color="red")+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),color="maroon",
               data=GB_final, type = "norm", size=2) +
  new_scale_color() +
  
  geom_point(data=Ich_final, aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d,
                                 alpha=0.1),color="violet")+
  stat_ellipse(aes(x=CO2_mmol_m2_1d, y=O2_mmol_m2_1d),color="purple",
               data=Ich_final, type = "norm", size=2) +
  scale_colour_manual(values = cols)+
  xlab(expression(paste(CO[2]~('mmol'/m^2/'day'))))+
  ylab(expression(paste(O[2]~('mmol'/m^2/'day'))))+
  geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
  geom_hline(yintercept=0, linetype='dotted', size=1) + #horizontal line at y=0
  geom_vline(xintercept=0, linetype='dotted', size=1) + #vertical line at x=0
  geom_abline(slope=-1, intercept=0, linetype='dashed', size=0.7)+
  theme(axis.text.x = element_text(size = 12, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_blank(),
        plot.subtitle = element_text(size=12),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'gray'),
        panel.grid.minor = element_line(color = 'gray'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position="right")




  

dev.new()

names(ellipse)
ggplot(ellipse, aes(x=site, y=slope)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("slope")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(ellipse, aes(x=site, y=width)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("width")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(ellipse, aes(x=site, y=stretch)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("stretch")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(ellipse, aes(x=site, y=CO2_mmol_m2_1d)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("CO2_mmol_m2_1d")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')

ggplot(ellipse, aes(x=site, y=O2_mmol_m2_1d)) + 
  geom_boxplot(outlier.colour="red", outlier.size=1)+
  ylab("O2_mmol_m2_1d")+
  theme(axis.text.x = element_text(size = 15, angle=0),
        axis.text.y = element_text(size = 15, angle=0),
        axis.title =element_text(size = 15, angle=0),
        plot.title = element_text(size = 17, angle=0),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),
        legend.position = 'right')


dev.new()
