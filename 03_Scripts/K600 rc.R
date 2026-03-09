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


# trying the power function#############
sheet_names <- excel_sheets("04_Outputs/rC_K600.xlsx")
list_of_ks <- list()
for (sheet in sheet_names) {
  df <- read_excel("04_Outputs/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}


k600s.raw <- bind_rows(list_of_ks, .id = "ID") %>%
  mutate(Date = mdy(Date),
         k600_1.day=if_else(ID=='AM', depth< 1, NA))

#power_sites <- c("AM", "LF")  # Specify your linear sites
linear_sites <- c("GB", "ID", "AM", "LF")   # Specify your power sites

linear_data <- k600s.raw %>% filter(ID %in% linear_sites)
rC_linear <- lmList(k600_1.day ~ depth | ID, data = linear_data)
cf_linear <- coef(rC_linear)

# power_data <- k600s.raw %>% 
#   filter(ID %in% power_sites, k600_1.day > 0, depth > 0)  # Remove zeros/negatives for log
# rC_power <- lmList(log(k600_1.day) ~ log(depth) | ID, data = power_data)
# cf_power <- coef(rC_power)

# Apply the appropriate model to each site
depth <- read_csv("02_Clean_data/Chem/depth.csv")

k600s <- depth %>%
  mutate(
    k600_1d = case_when(
      # Linear relationships: k600 = a + b*depth
      ID == "GB" ~ depth * cf_linear["GB", "depth"] + cf_linear["GB", "(Intercept)"],
      ID == "ID" ~ depth * cf_linear["ID", "depth"] + cf_linear["ID", "(Intercept)"],
      ID == "AM" ~ depth * cf_linear["AM", "depth"] + cf_linear["AM", "(Intercept)"],
      ID == "LF" ~ depth * cf_linear["LF", "depth"] + cf_linear["LF", "(Intercept)"],
      
      # Power relationships: k600 = a * depth^b (from log(k600) = log(a) + b*log(depth))
      # ID == "AM" ~ exp(cf_power["AM", "(Intercept)"]) * depth^cf_power["AM", "log(depth)"],
      # ID == "LF" ~ exp(cf_power["LF", "(Intercept)"]) * depth^cf_power["LF", "log(depth)"],

      # Keep the linear model for OS if needed
      ID == "OS" ~ depth * cf_linear["OS", "depth"] + cf_linear["OS", "(Intercept)"]
    )
  )

# Rest of your code remains the same
K600.daily <- k600s %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(K600_1.d_daily = max(k600_1d, na.rm = T), .groups = "drop")%>%
  mutate(
    # K600_1.d_daily=if_else(ID=='LF', 6.4, K600_1.d_daily),
    # K600_1.d_daily=if_else(ID=='AM', 15.7, K600_1.d_daily),
    # K600_1.d_daily=if_else(ID=='GB', 9.2, K600_1.d_daily),
    # K600_1.d_daily=if_else(ID=='ID', 4.45, K600_1.d_daily),
         )

ggplot(K600.daily, aes(x = Date)) +
  geom_point(aes(y = K600_1.d_daily))+
  facet_wrap(~ID, scales='free')

Work <- K600.daily %>%
  mutate(Date = ymd_hms(paste(Date, "00:00:00")))
write_csv(Work, "02_Clean_data/Chem/K600.csv")


ggplot(k600s.raw, aes(x = depth)) +
  geom_point(aes(y = k600_1.day))+
  facet_wrap(~ID, scales='free')


