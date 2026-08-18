###packages####
library(tidyverse)
library(readxl)
library(measurements)
library(tools)
source("03_Scripts/clean functions.R")

#pH#####
pH_everything <- data.frame()

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/Everything", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/CO2 Sheet 2", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_xl(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/CampbellSci/pH/dat everything", pattern=".dat", full.names=TRUE)
for(fil in file.names){
  pH <- pH_csv(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/Hobo/pH", pattern=".xlsx", full.names=TRUE)
for(fil in file.names){
  pH <- pH_HOBO(fil)
  pH_everything<-rbind(pH_everything,pH)
  pH_everything <- pH_everything[!duplicated(pH_everything[c('Date','ID')]),]
}

pH_everything<-rename_ID(pH_everything)

GB_pH <- read_xlsx("01_Raw_data/02322350_pH.xlsx")
GB_pH <- GB_pH %>%
  mutate(Hour = list(0:23)) %>%  # Create a list of 24 hours for each day
  unnest(Hour) %>%  # Expand into multiple rows
  mutate(Date = ymd_hm(paste(Date, Hour, "00"))) %>%  # Convert to ymd_hm format
  select(-Hour)  # Remove temporary Hour column if not needed

pH_everything<-rbind(GB_pH,pH_everything)

pH.edit<-pH_everything %>% filter(pH<9,pH>4, ID!="RovingBox")%>%
  mutate(ID=if_else(ID=="GilBlue04272022.xlsx", "GB", ID),
         pH=if_else(ID=='AM'& Date<'2024-01-01', pH-1, pH),
         pH=if_else(ID=='GB'& pH>7.55, NA, pH),
         pH=if_else(ID=='OS'& pH>8, NA, pH),
         pH=if_else(ID=='ID'& Date< '2023-09-23' & pH<7.3, NA, pH)
         )

# a<-ggplot(data=pH.edit %>% filter(ID=='ID'), aes(x=Date, y=pH)) +
#   geom_point()+
#   facet_wrap(~ID)
# ggplotly(a)#+geom_hline(yintercept=360)

write_csv(pH.edit, "02_Clean_data/Chem/pH.csv")
###DO#####
DO_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/DO/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_formatted(fil)
  DO$source_file <- basename(fil)

  DO_everything<-rbind(DO_everything,DO)
  DO_everything <- DO_everything[!duplicated(DO_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/HOBO/DO/unformated", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  DO <- DO_unformatted(fil)
  DO$source_file <- basename(fil)
  
  DO_everything<-rbind(DO_everything,DO)
  DO_everything <- DO_everything[!duplicated(DO_everything[c('Date','ID')]),]
}

DO_everything<-rename_ID(DO_everything) %>% 
  filter(Date>"2022-01-01")%>%
  mutate(DO=abs(DO))#%>%
  #distinct(Date, ID, .keep_all = T)

DO_CQ<-DO_everything%>%
  filter(
    DO<12,
    !source_file %in% c('AllenMill_DO_11302023.csv', 'GB_DO_12142023.csv')
         # Temp<80,
         # Temp>50
    )%>%
  mutate(
    DO=abs(DO),
    remove=case_when(
      ID=='AM' & Date<="2022-06-01" & DO>7~ 'a',
      ID=='AM' & Date>'2024-01-01' & Date<'2024-04-01' & DO>8~ 'a',
      ID=='AM' & Date>'2023-04-04' & Date<'2023-05-15' & DO<2~ 'a',
      
      ID=='GB' & Date<="2022-06-01" & DO>7.5~ 'a',
      ID=='GB' & Date>'2022-08-01' & Date< '2022-10-01' & DO<4~ 'a',
      ID=='GB' & Date>'2022-08-01' & Date< '2022-10-01' & DO>6.9~ 'a',
      ID=='GB' & Date>'2022-12-01' & Date< '2023-10-01' & DO<3.6~ 'a',
      ID=='GB' & Date>'2024-01-01' & Date< '2024-04-01' & DO>8.8~ 'a',
      ID=='GB' & Date>'2024-07-01' & DO>8~ 'a',
      
      ID=='LF' & Date<'2022-06-01' & DO>6~ 'a',
      ID=='LF' & Date> '2022-06-20' & Date<'2022-07-01' & DO<3~'a',
      ID=='LF' & Date> '2023-11-01' & Date<'2024-01-01' & DO<2~'a',
      ID=='LF' & Date> '2023-11-01' & DO>6.4~'a',
      ID=='ID' & Date<'2023-02-03' & DO>8.7~'a',
      ID=='ID' & Date<'2023-02-03' & DO<3.5~'a',
      ID=='ID' & Date>'2023-05-03' & Date<'2023-06-03' & DO<5.6~'a',
      ID=='ID' & Date>'2023-06-03' & Date<'2023-08-03' & DO<3.6~'a',
      ID=='ID' & Date>'2023-08-03' & Date<'2023-12-03' & DO<3.7~'a',
      ID=='ID' & Date>'2023-12-03' & Date<'2024-04-03' & DO<3~'a',
      ID=='ID' & Date>'2023-09-18' & Date<'2023-10-04' & DO<4.2~'a',
      
      ID=='OS' & Date<'2022-08-03' & DO>9~'a',
      ID=='OS' & Date<'2022-10-15' & Date>'2022-09-01'& DO>6.5~'a',
      ID=='OS' & Date<'2023-07-01' & Date>'2023-05-15' & DO<2~'a',
      ID=='OS' & Date<'2023-08-15' & Date>'2023-07-15' & DO<0.2~'a',
      ID=='OS' & Date<'2023-08-15' & Date>'2023-07-15' & DO<0.2~'a',
      ID=='OS' & Date<'2023-12-30' & Date>'2023-11-15' & DO<0.2~'a',
      ID=='LF' & Date>'2023-04-03' & Date<'2023-04-17' & DO<1.7~'a',
      ID=='LF' & Date>'2023-04-03' & Date<'2023-04-17' & DO>8~'a'
      
      
    ))%>%
  distinct(Date, ID, .keep_all = T)%>%
  filter(is.na(remove))#%>%
  #select(Date, DO, Temp, ID)
 

  ggplot(data=DO_CQ %>% 
         filter(ID=='LF',
         Date>'2023-04-03',
         Date<'2023-04-17'
         ), 
       aes(x=Date)) +
  geom_point(aes(y=DO))+
  facet_wrap(~ID)
  


write_csv(DO_CQ, "02_Clean_data/Chem/DO.csv")

###SpC####
SpC_everything<-data.frame()
file.names <- list.files(path="01_Raw_data/Hobo/SpC/formatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_formatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)
  SpC_everything <- SpC_everything[!duplicated(SpC_everything[c('Date','ID')]),]
}

file.names <- list.files(path="01_Raw_data/Hobo/SpC/unformatted", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  SpC <- SpC_unformatted(fil)
  SpC_everything<-rbind(SpC_everything,SpC)
  SpC_everything <- SpC_everything[!duplicated(SpC_everything[c('Date','ID')]),]
}

SpC_everything<-rename_ID(SpC_everything)
SpC.edited<-SpC_everything %>%
  mutate(
    Date=ymd_hms(Date),
    SpC=if_else(ID=='AM' & SpC>450, NA, SpC),
    SpC=if_else(ID=='GB' & SpC>430, NA, SpC),
    SpC=if_else(ID=='ID' & SpC>400, NA, SpC),
    SpC=if_else(ID=='LF' &Date<'2024-01-01' & SpC< 400, NA, SpC),
    SpC=if_else(ID=='GB' &Date<'2023-08-01' & SpC< 350, NA, SpC),
    SpC=if_else(ID=='ID' &Date<'2023-12-01' & SpC< 300, NA, SpC),
    SpC=if_else(ID=='AM' &Date>='2022-08-22' & Date<='2022-08-23', NA, SpC),
    SpC=if_else(ID=='AM' &Date=='2023-07-19', NA, SpC),
    SpC=if_else(ID=='OS' &Date=='2022-05-15', NA, SpC),
    
    )%>%
  filter(SpC<575, SpC>50)
  

ggplot(data=SpC.edited %>% filter(ID=='ID'), aes(x=Date, y=SpC)) +
  geom_point()+
  facet_wrap(~ID)#+geom_hline(yintercept=360)


write_csv(SpC.edited, "02_Clean_data/Chem/SpC.csv")

###compile####
file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4,11, 7)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

master <- master %>%
  mutate(min = minute(Date)) %>%
  filter(min == 0) %>%
  select(-min, -source_file, -remove)
master <- master[!duplicated(master[c('Date','ID')]),]
    
names(master)
###Include IU####
library(dataRetrieval)
startDate <- "2022-05-12"
endDate <- "2024-07-25"
parameterCd <- c('00010','00300','00095','00400','00065')
ventID<-'02322700'

IU.raw<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU.raw %>% 
  rename('Date'='dateTime', 'Temp'='X_00010_00000',
                  'DO'='X_00300_00000', 'SpC'='X_00095_00000',
                  'pH'='X_00400_00000', 'depth'='X_00065_00000')%>%
 mutate(min=minute(Date),
        CO2=NA,
        depth=depth-13.72, 
        ID='IU')%>%
  filter(min==0)%>% 
  select(names(master))

master<- rbind(master, IU)

write_csv(master, "02_Clean_data/master_chem1.csv")

