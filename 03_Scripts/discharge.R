library(tidyverse)
library(readxl)

depth <- read_csv("02_Clean_data/Chem/depth.csv")
W <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",  sheet = "width ")
h.w<-left_join(depth, W, by='ID')


velocity <- read_csv("02_Clean_data/Chem/velocity.csv")
Q<-left_join(velocity, h.w)%>%mutate(discharge=velocity*depth*w)%>%
  select(Date, ID, discharge)%>%
  distinct(Date, ID, .keep_all = T)#%>%
  filter(!is.na(discharge))%>%
  mutate(discharge=if_else(discharge<0, 0, discharge))

ggplot(Q, aes(x=Date, y=discharge))+geom_point()+facet_wrap(~ID)

write_csv(Q, "02_Clean_data/Chem/discharge.csv")
