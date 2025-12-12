#packages#####
library(tidyverse)
library(readxl)
library(lme4)
library(cowplot)
theme_set(theme(    strip.text = element_text(size = 12),
                    axis.title.y = element_text(size=13, angle=90),
                    axis.title.x = element_text(size=13),
                    axis.text.x = element_text(size=12),
                    axis.text.y = element_text(size=12),
                    panel.grid.major.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),  # Customize x-axis major gridlines
                    panel.grid.minor.y = element_blank(),
                    panel.background = element_rect(fill = 'white'),
                    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


depth <- read_csv("02_Clean_data/Chem/depth.csv")

u <- read_csv("02_Clean_data/Chem/velocity.csv")%>%
  mutate(Date=as.Date(Date))%>%rename(velocity.interpolated=velocity)

sheet_names <- excel_sheets("04_Outputs/rC_K600.xlsx")

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Outputs/rC_K600_edited.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

k600s <- bind_rows(list_of_ks, .id = "ID")%>%
  distinct(k600_1d, .keep_all = T)%>% filter(ID != 'Vent DO')%>%
  mutate(Date=mdy(Date))

l<- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",
               sheet = "length ")

k600s<-left_join(k600s, u)%>%distinct(ID, Date, k600_1d, .keep_all = T)%>%
  mutate(u_m.day=velocity.interpolated*86400, reach= (u_m.day/k600_1d)/10^3)

k600s<-left_join(k600s, l)%>%
  mutate(reach.test=if_else(reach>3*km, 'above', 'passes'),
         reach.test=if_else(reach<0.4*km, 'below', reach.test))


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


write_csv(k600s, "02_Clean_data/Chem/K600.csv")


#check #######

h.limit <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", 
                           sheet = "depth threshold")

ggplot(k600s, aes(x =velocity.interpolated, y = u)) +
  geom_point(size=2) +
  facet_wrap(~ ID, scales = "free")+
  geom_smooth(method = lm, se=F)

ggplot(k600s, aes(x =depth, y = k600_1d, color=reach.test)) +
  geom_point(size=2) +
  facet_wrap(~ ID, scales = "free")+
  geom_smooth(method = lm, se=F)

plot_grid(
  
  ggplot(k600s, aes(x = depth, y = k600_1d, color = reach.test)) +
    geom_point(size=2) +
    facet_wrap(~ ID, scales = "free", nrow = 1) +
    geom_hline(
      data = h.limit,
      aes(yintercept = max),
      colour = "red",
      linetype = "dashed",
      alpha = 0.7),
  
  ggplot(k600s, aes(x = Date, y = u, color = reach.test)) +
    geom_point(size=2) +
    facet_wrap(~ ID, scales = "free", nrow = 1),
  
    ncol=1)

