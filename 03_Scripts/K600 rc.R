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


#rating curve#####

sheet_names <- excel_sheets("04_Outputs/rC_K600_edited.xlsx")

list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/rC_K600_edited.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

k600s.raw <- bind_rows(list_of_ks, .id = "ID")%>%
  distinct(k600_1.day, .keep_all = T)%>% filter(ID != 'Vent DO')%>%
  mutate(Date=mdy(Date))

rC <- lmList(k600_1.day ~ depth | ID, data=k600s.raw)
(cf <- coef(rC))

depth <- read_csv("02_Clean_data/Chem/depth.csv")
u <- read_csv("02_Clean_data/Chem/velocity.csv")%>%
  mutate(Date=as.Date(Date))%>%rename(velocity.interpolated=velocity)

k600s<-
  depth%>%mutate(
  k600_1d=case_when(
    ID=='AM'~depth*cf[1,2]+cf[1,1],
    ID=='GB'~depth*cf[2,2]+cf[2,1],
    ID=='ID'~depth*cf[3,2]+cf[3,1],
    ID=='LF'~depth*cf[4,2]+cf[4,1],
    ID=='OS'~depth*cf[5,2]+cf[5,1]
  )
)
  
K600.daily<-k600s%>%mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(
    K600_1.d_daily=mean(k600_1d, na.rm=T))%>%
  ungroup()


k600s%>%filter(ID=='LF')%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(
    K600_1.d_daily=mean(k600_1d, na.rm=T))%>%
  ggplot(aes(x=Date, y=K600_1.d_daily))+geom_point()



Work<-K600.daily%>%
  mutate(Date=paste(Date, "00:00:00"))

write_csv(Work, "02_Clean_data/Chem/K600.csv")

# trying the power function#############
sheet_names <- excel_sheets("04_Outputs/rC_K600.xlsx")
list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}


k600s.raw <- bind_rows(list_of_ks, .id = "ID") %>%
  distinct(k600_1.day, .keep_all = T) %>% 
  mutate(Date = mdy(Date))

power_sites <- c("AM", "LF")  # Specify your linear sites
linear_sites <- c("ID", "GB")   # Specify your power sites

linear_data <- k600s.raw %>% filter(ID %in% linear_sites)
rC_linear <- lmList(k600_1.day ~ depth | ID, data = linear_data)
cf_linear <- coef(rC_linear)

power_data <- k600s.raw %>% 
  filter(ID %in% power_sites, k600_1.day > 0, depth > 0)  # Remove zeros/negatives for log
rC_power <- lmList(log(k600_1.day) ~ log(depth) | ID, data = power_data)
cf_power <- coef(rC_power)

# Apply the appropriate model to each site
depth <- read_csv("02_Clean_data/Chem/depth.csv")

k600s <- depth %>%
  mutate(
    k600_1d = case_when(
      # Linear relationships: k600 = a + b*depth
      ID == "ID" ~ depth * cf_linear["ID", "depth"] + cf_linear["ID", "(Intercept)"],
      ID == "GB" ~ depth * cf_linear["GB", "depth"] + cf_linear["GB", "(Intercept)"],
      
      # Power relationships: k600 = a * depth^b (from log(k600) = log(a) + b*log(depth))
      ID == "AM" ~ exp(cf_power["AM", "(Intercept)"]) * depth^cf_power["AM", "log(depth)"],
      ID == "LF" ~ exp(cf_power["LF", "(Intercept)"]) * depth^cf_power["LF", "log(depth)"],
      
      # Keep the linear model for OS if needed
      ID == "OS" ~ depth * cf_linear["OS", "depth"] + cf_linear["OS", "(Intercept)"]
    )
  )

# Rest of your code remains the same
K600.daily <- k600s %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(K600_1.d_daily = mean(k600_1d, na.rm = T), .groups = "drop")

Work <- K600.daily %>%
  mutate(Date = ymd_hms(paste(Date, "00:00:00")))
write_csv(Work, "02_Clean_data/Chem/K600.csv")


ggplot(Work, aes(x = Date)) +
  geom_point(aes(y = K600_1.d_daily))+
  facet_wrap(~ID, scales='free')




