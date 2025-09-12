library(tidyverse)
library(readxl)

depth <- read_csv("02_Clean_data/Chem/depth.csv")
W <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",  sheet = "width ")
h.w<-left_join(depth, W, by='ID')%>%  distinct(Date, ID, .keep_all = T)


velocity <- read_csv("02_Clean_data/Chem/velocity.csv")%>%  distinct(Date, ID, .keep_all = T)
Q<-left_join(velocity, h.w)%>%mutate(discharge=velocity*depth*w)%>%
  select(Date, ID, discharge)%>%
  #filter(!is.na(discharge))%>%
  mutate(discharge=if_else(discharge<0, 0, discharge))

ggplot(Q, aes(x=Date, y=discharge))+geom_line()+facet_wrap(~ID, scales='free')

write_csv(Q, "02_Clean_data/Chem/discharge.csv")

