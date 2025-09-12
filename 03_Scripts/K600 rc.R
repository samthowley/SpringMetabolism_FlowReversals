#packages#####
library(tidyverse)
library(readxl)
library(lme4)

depth <- read_csv("02_Clean_data/Chem/depth.csv")

sheet_names <- excel_sheets("04_Outputs/rC_K600_edited.xlsx")
ks <- sheet_names[!sheet_names %in% c("velocity")]

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Outputs/rC_K600_edited.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

k600s <- bind_rows(list_of_ks, .id = "ID")

rC <- lmList(k600_1d ~ depth | ID, data=k600s)
(cf <- coef(rC))

depth <- read_csv("02_Clean_data/Chem/depth.csv")

k600s<-depth%>%mutate(
  k600_1d=case_when(
    ID=='AM'~depth*cf[1,2]+cf[1,1],
    ID=='GB'~depth*cf[2,2]+cf[2,1],
    ID=='ID'~depth*cf[3,2]+cf[3,1],
    ID=='LF'~depth*cf[4,2]+cf[4,1],
    ID=='OS'~depth*cf[5,2]+cf[5,1]
  )
)%>%select(Date, ID, k600_1d)%>%
  mutate(k600_1d=if_else(k600_1d<0, 0.1, k600_1d))

#ggplot(k600s, aes(x=Date, y=k600_1d))+geom_line()+facet_wrap(~ID, scales='free')

write_csv(k600s, "02_Clean_data/Chem/K600.csv")
