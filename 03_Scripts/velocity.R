library(tidyverse)
library(readxl)
library(measurements)
library(openxlsx)

u <- read_csv("01_Raw_data/u.csv")%>%mutate(Date=mdy(Date))

h <- read_csv("02_Clean_data/master_depth2.csv") %>%select(Date, ID, depth)%>%
  fill(depth, .direction = 'updown')%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(depth=mean(depth, na.rm=T))

u.h<-left_join(u, h)%>% 
  select(ID, Date, depth, everything())


split<-u.h %>% split(u.h$ID)
write.xlsx(split, file = '04_Outputs/velocity.xlsx')

#check#####



ggplot(u, aes(x=depth, y=u))+
  geom_point()+geom_smooth(method = lm, se=F)+
  facet_wrap(~ID, scales='free')


#rating curve#############
sheet_names <- excel_sheets("04_Outputs/velocity_edit.xlsx")
list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/velocity_edit.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

u <- bind_rows(list_of_ks, .id = "ID")


depth <- read_csv("02_Clean_data/Chem/depth.csv")
library(lme4)
rC <- lmList(u ~ depth | ID, data=u)
(cf <- coef(rC))

velocity<-depth%>%mutate(
  velocity=case_when(
    ID=='AM'~depth*cf[1,2]+cf[1,1],
    ID=='GB'~depth*cf[2,2]+cf[2,1],
    ID=='ID'~depth*cf[3,2]+cf[3,1],
    ID=='LF'~depth*cf[4,2]+cf[4,1],
    ID=='OS'~depth*cf[5,2]+cf[5,1]
    )
  )%>%select(-depth)

#ggplot(velocity, aes(x=Date, y=velocity))+geom_line()+facet_wrap(~ID, scales='free')

write_csv(velocity, "02_Clean_data/Chem/velocity.csv")
