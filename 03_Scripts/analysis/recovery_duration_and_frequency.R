rm(list=ls())

####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)

get_season <- function(date) {
  month <- month(date)
  ifelse(
    month %in% c(12, 1, 2), "Winter",
    ifelse(
      month %in% c(3, 4, 5), "Spring",
      ifelse(
        month %in% c(6, 7, 8), "Summer", "Fall"
      )
    )
  )
}

#data####
chem <- read_csv("02_Clean_data/master_chem1.csv")

master_metabolism4 <- read_csv("02_Clean_data/master_metabolism4.csv")
master_metabolism4<-master_metabolism4 %>%select(Date, ID, GPP, ER)%>%
  mutate(Hour = list(0:23)) %>%  # Create a list of 24 hours for each day
  unnest(Hour) %>%  # Expand into multiple rows
  mutate(Date = ymd_hm(paste(Date, Hour, "00"))) %>%  # Convert to ymd_hm format
  select(-Hour) 

chem<-left_join(chem, master_metabolism4, by=c('Date', 'ID'))

chem<-chem %>%group_by(ID) %>% 
  mutate(depth_min=min(depth, na.rm=T))%>% 
  mutate(depth_diff=depth-depth_min)

chem<-chem %>% mutate(depthID = case_when(
  ID=='AM' & depth<0.9 ~ "low",
  ID=='AM' &depth>0.9 & depth<1.2 ~ "moderate",
  ID=='AM' &depth>=1.2 ~ "high",
  ID=='GB' &depth<0.55  ~ "low",
  ID=='GB' &depth>0.55 & depth<=0.75 ~ "moderate",
  ID=='GB' &depth>=0.75~ "high",
  ID=='OS' &depth<0.8 ~ "low",
  ID=='OS' &depth>0.8 & depth<1 ~ "moderate",
  ID=='OS' &depth>=1 ~ "high",
  ID=='LF' &depth<0.4 ~ "low",
  ID=='LF' &depth> 0.4 & depth<0.6~ "moderate",
  ID=='LF' &depth>=0.6 ~ "high",
  ID=='ID' &depth<1.1 ~ "low",
  ID=='ID'& depth>1.1 & depth<1.5 ~ "moderate",
  ID=='ID' &depth>=1.5 ~ "high",
  ID=='IU' &depth<1.7 ~ "low",
  ID=='IU'& depth>1.7 & depth<3 ~ "moderate",
  ID=='IU' &depth>=3 ~ "high",
  ))%>%
  mutate(SpC_disturb=case_when(
    ID=='IU'~'0',
    SpC<=300~'1',
    ID=='AM' & SpC<=340~'1',
    ID=='LF' & SpC<=400 & Date>'2024-01-20'~'1',
    ID=='GB' & SpC<=350~'1',
    
  ))%>%
  mutate(pH_disturb=case_when(
    ID=='LF' & pH<7.55 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7 & Date>'2024-01-01'~'1',
    ID=='AM' & pH<7.2 & Date<'2024-01-01'~'1',
    ID=='OS' & pH<7.4 & Date>'2023-01-01'~'1',
  ))

chem <- chem %>%
  mutate(pH_disturb = ifelse(is.na(pH_disturb), '0', pH_disturb),
         SpC_disturb = ifelse(is.na(SpC_disturb), '0', SpC_disturb))

#ID disturbance#####
chem <- chem %>%
  mutate(floodID = case_when(
    depthID == 'high' & SpC_disturb == 1 ~ "backwater",
    depthID == 'high' & pH_disturb == 1 ~ "backwater",
    depthID == 'high' & pH_disturb == 0 & SpC_disturb == 0 ~ "high-stage",
    TRUE ~ "baseline"))

#flood count
chem <- chem %>%
  group_by(ID) %>%
  mutate(flood_count = ifelse(
      !is.na(floodID) & (floodID %in% c("blackwater", "high-stage")),
      ave(floodID, cumsum(is.na(floodID)), FUN = seq_along),0  # Reset to 0 when it's NA or not in the target categories
    )) %>%ungroup()

#time between
chem <- chem %>%
  group_by(ID) %>%
  mutate(time_btwn = ifelse(
    !is.na(floodID) & (floodID %in% c("baseline")),
    ave(floodID, cumsum(is.na(floodID)), FUN = seq_along),0  # Reset to 0 when it's NA or not in the target categories
  )) %>%ungroup()


#baseline, hours to days
chem<-chem%>%  
  mutate(year=year(Date), season=get_season(Date))%>%
  mutate(time_btwn_days = as.numeric(time_btwn)/24, flood_count_days = as.numeric(flood_count)/24)%>%
  
  group_by(season, year, ID, floodID) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    h_count = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))%>%ungroup%>%
    
  group_by(year, season, ID)%>%
  mutate(
    GPP_baseline = mean(GPP[depthID == "low"], na.rm = TRUE),
    ER_baseline = mean(ER[depthID == "low"], na.rm = TRUE)) 

#formulate table
frequency <- chem %>%
  group_by(floodID, ID, season, year) %>%
  summarise(
    time_btwn=max(time_btwn_days),
    Date = mean(Date, na.rm = TRUE),
    .groups = 'keep')

duration <- chem %>%
  group_by(floodID, ID, season, year) %>%
  summarise(
    duration=max(flood_count_days),
    Date = mean(Date, na.rm = TRUE),
    GPP_disturb = ifelse(all(is.na(GPP[flood_count > 0 & flood_count < 5])), NA, mean(GPP[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    ER_disturb = ifelse(all(is.na(ER[flood_count > 0 & flood_count < 5])), NA, mean(ER[flood_count > 0 & flood_count < 5], na.rm = TRUE)),
    GPP_baseline = mean(GPP_baseline, na.rm = TRUE), 
    ER_baseline = mean(ER_baseline, na.rm = TRUE),
    .groups = 'keep') %>%
  filter(floodID != 'baseline') %>%
  mutate( GPP_reduce = (1 - (GPP_disturb / GPP_baseline)) * 100, 
          ER_reduce = (1 - (ER_baseline / ER_disturb)) * 100)

#write_csv(disturbance_table, "04_Outputs/duration_recovery.csv")
##########
  
recovery_time <- read_csv("04_Outputs/recovery_analysis.csv")
reduction <- read_csv("04_Outputs/reduction_analysis.csv")
reduction<-reduction %>% select(-IF, -num)

recovery <- reduction %>%
  rowwise() %>%
  mutate(closest_date = recovery_time$Date[which.min(abs(difftime(Date, recovery_time$Date, units = "days")))]
  ) %>%full_join(recovery_time, by = c("closest_date" = "Date", 'ID')) %>%ungroup()


duration <- read_csv("04_Outputs/duration_recovery.csv")


all <- recovery %>%
  mutate(
    closest_date = map(Date, ~ duration$Date[which.min(abs(difftime(.x, duration$Date, units = "days")))] %||% NA)
  ) %>%
  unnest(closest_date) %>%
  full_join(duration, by = c("closest_date" = "Date", "ID"))


write_csv(all, "02_Clean_data/recovery.csv")

#analysis#####


cols<-c( "high-stage"="deepskyblue3","backwater"="burlywood4")
h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)

theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             legend.position = "bottom",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))




ggplot(duration, aes(duration, shape=ID, color=floodID))+
    geom_point(aes(y=GPP_reduce), size=6)+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Backwater Flood"))+
    ggtitle("Backwater Flood Impacts on GPP")+
    ylab("GPP Reduction (%)")+theme_sam+theme(
      axis.title.y =element_text(size = 27, color="darkgreen"),
      axis.title.x =element_text(size = 27),
      plot.title = element_text(size = 22, color="darkgreen"))

names(disturbance_table)
