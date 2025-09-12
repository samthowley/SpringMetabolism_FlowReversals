library(tidyverse)
library(readxl)
library(measurements)

u <- read_excel("04_Outputs/rC_k600_edited.xlsx", 
                             sheet = "velocity")

#ggplot(u, aes(x=depth, y=u))+geom_point()+facet_wrap(~ID, scales='free')

rC <- lmList(u ~ depth | ID, data=u)
(cf <- coef(rC))

depth <- read_csv("02_Clean_data/Chem/depth.csv")

velocity<-depth%>%mutate(
  velocity=case_when(
    ID=='AM'~depth*cf[1,2]+cf[1,1],
    ID=='GB'~depth*cf[2,2]+cf[2,1],
    ID=='ID'~depth*cf[3,2]+cf[3,1],
    ID=='LF'~depth*cf[4,2]+cf[4,1],
    ID=='OS'~depth*cf[5,2]+cf[5,1]
    )
  )%>%select(-depth)

ggplot(velocity, aes(x=Date, y=velocity))+geom_line()+facet_wrap(~ID, scales='free')

write_csv(velocity, "02_Clean_data/Chem/velocity.csv")
