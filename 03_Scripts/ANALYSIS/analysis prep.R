library(ggnewscale)
library(ggpmisc)
library(tidyverse)
library(cowplot)
library(ggh4x)
library(lme4)


site_colors <- c(AM = "#E41A1C", GB = "#377EB8", ID = "#4DAF4A",
                 LF = "#984EA3", OS = "#FF7F00", IU = "#A65628")

class_colors <- c(BO = "#A65628", FR = "black", HI = "#2171B5", baseline='lightblue')

site_shapes <- c(AM = 16, GB = 17, ID = 15, LF = 18, OS = 8, IU = 3)

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom"
    )
}

#extract average response##########
SpC<-read_csv("04_Outputs/flood impacts/SpC.csv")%>%
  select(ID, flood, minimum)%>%
  rename(SpC=minimum)
pH<-read_csv("04_Outputs/flood impacts/pH.csv")%>%
  select(ID, flood, minimum)%>%
  rename(pH=minimum)

flood.class<-full_join(SpC, pH)%>%
  filter(!is.na(flood))%>%
  left_join(read_csv("04_Outputs/flood impacts/FR_class.csv"))

vulnerability <- data.frame(
  ID = c('IU', 'ID', 'GB', 'LF', 'AM', 'OS'),
  vulnerable.score = c(1, 2, 3, 4, 5, 6))

depth <- read_csv("04_Outputs/flood impacts/depth.csv")
h.percent.change<-depth%>%
  mutate(h.percent.change=(maximum-base)/base*100)%>%
  select(ID, flood, h.percent.change)%>%
  left_join(flood.class)%>%left_join(vulnerability)


GPP <- read_csv("04_Outputs/flood impacts/GPP.csv")
ER <- read_csv("04_Outputs/flood impacts/ER.csv")
DO <- read_csv("04_Outputs/flood impacts/DO.csv")
CO2 <- read_csv("04_Outputs/flood impacts/CO2.csv")

declined<-rbind(GPP, DO)%>%rename(peak.response=minimum)
increased<-rbind(ER, CO2,depth)%>%rename(peak.response=maximum)

flood.response<-rbind(declined, increased)%>%
  filter(!is.na(flood))%>%
  left_join(h.percent.change)%>%
  mutate(reponse.percent.change=(peak.response-base)/base*100,
         flood=as.factor(flood),
         variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
         class = factor(class, levels = c("HI", "BO", "FR"))
         )%>%
  mutate(
    recess.slope=if_else(variable =='ER' & recess.slope>0, NA, recess.slope),
    r2.recess=if_else(variable =='ER'  & recess.slope>0, NA, r2.recess),

    recess.slope=if_else(variable =='CO2' & recess.slope>0, NA, recess.slope),
    r2.recess=if_else(variable =='CO2'  & recess.slope>0, NA, r2.recess),

    recess.slope=if_else(variable =='GPP' & recess.slope<0, NA, recess.slope),
    r2.recess=if_else(variable =='GPP' & recess.slope<0, NA, r2.recess),

    recess.slope=if_else(variable=='DO' & recess.slope<0, NA, recess.slope),
    r2.recess=if_else(variable=='DO' & recess.slope<0, NA, r2.recess),

    time.to.recover = as.numeric(flood.end - peak.Date),
    time2peak       = as.numeric(difftime(peak.Date, flood.start, units = "days"))
  )%>%
  group_by(ID, variable)%>%
  arrange(ID, variable, flood.start)%>%
  mutate(
    days.since.last.flood   = as.numeric(difftime(flood.start, lag(flood.end), units = "days")),
    avg.days.between.floods = mean(days.since.last.flood, na.rm = TRUE),
    variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS'))
  )%>%
  ungroup()%>%
  group_by(variable)%>%
  mutate(
    recess.slope.z = as.numeric(scale(recess.slope)),
    rise.slope.z   = as.numeric(scale(rise.slope))
  )%>%
  ungroup()

#write_csv(flood.response, "04_Outputs/flood impacts/flood.response.avg.csv")

#flood time series###########
GPP_flood_df <- read_csv("04_Outputs/flood impacts/GPP.flood.df.csv")
ER_flood_df  <- read_csv("04_Outputs/flood impacts/ER.flood.df.csv")
DO_flood_df  <- read_csv("04_Outputs/flood impacts/DO.flood.df.csv")
CO2_flood_df <- read_csv("04_Outputs/flood impacts/CO2.flood.df.csv")
depth_flood_df <- read_csv("04_Outputs/flood impacts/depth.flood.df.csv")

h.per.change.timeseries <- depth_flood_df%>%
  mutate(h.percent.change=(conc-base)/base*100)%>%
  select(ID, flood, Date, h.percent.change)

time.series <- rbind(GPP_flood_df, ER_flood_df, DO_flood_df, CO2_flood_df,depth_flood_df)%>%
  left_join(h.per.change.timeseries)%>%
  left_join(flood.class)%>%
  left_join(flood.response%>%select(ID, flood, variable, peak.Date)%>%mutate(flood=as.numeric(flood)), by=c("ID", "flood", "variable"))%>%
  mutate(
    Date = as.Date(Date), 
    peak.Date = as.Date(peak.Date),
    variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS')),
    class = factor(class, levels = c("HI", "BO", "FR")),
    perc.change = (conc - base) / base * 100,
    )%>%
  group_by(ID, flood, variable)%>%
  arrange(ID, flood, Date)%>%
  mutate(flood.count = as.integer(Date - peak.Date))%>%
  ungroup()%>%
  mutate(
    flood=as.factor(flood)
  )


peak_dates <- read_csv("04_Outputs/flood impacts/peak dates.csv")
#write_csv(time.series, "04_Outputs/flood impacts/flood.time.series.csv")

#needed df##########

metab <- read_csv("04_Outputs/master.metabolism.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

chem_hourly <- read_csv("02_Clean_data/master_chem1.csv", show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

velocity_hourly <- read_csv("02_Clean_data/Chem/velocity.csv",
                            show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

discharge_hourly <- read_csv("02_Clean_data/Chem/discharge.csv",
                             show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

flood_periods <- read_csv("01_Raw_data/flood.periods.csv",
                          show_col_types = FALSE) %>%
  mutate(start = as.POSIXct(start, tz = "UTC"),
         end   = as.POSIXct(end,   tz = "UTC"))


flood_class <- read_csv("04_Outputs/flood impacts/FR_class.csv",
                        show_col_types = FALSE)
unique(flood_class$class)

chem_daily <- chem_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(DO  = mean(DO,  na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE),
            pH  = mean(pH,  na.rm = TRUE),
            SpC = mean(SpC, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Date = day)

vel_daily <- velocity_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(velocity = mean(velocity, na.rm = TRUE), .groups = "drop") %>%
  rename(Date = day)

dis_daily <- discharge_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(discharge = mean(discharge, na.rm = TRUE), .groups = "drop") %>%
  rename(Date = day)

# Master daily dataset
master <- metab %>%
  left_join(chem_daily,  by = c("Date", "ID")) %>%
  left_join(vel_daily,   by = c("Date", "ID")) %>%
  left_join(dis_daily,   by = c("Date", "ID"))
unique(master$ID)


floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

analysis <- left_join(
  chem_hourly %>%
    select(ID, Date, DO, CO2, depth) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(DO    = mean(DO,    na.rm = TRUE),
              CO2   = mean(CO2,   na.rm = TRUE),
              depth = mean(depth, na.rm = TRUE),
              .groups = "drop"),
  metab %>% rename(Date = Date) %>% select(-depth, -K600) %>%
    distinct(ID, Date, .keep_all = TRUE) %>%
    mutate(NEP = GPP + ER),
  by = c("Date", "ID"),
  relationship = "one-to-one"
) %>% arrange(ID, Date)%>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )


analysis.long<-analysis%>%
  pivot_longer(
    cols = c('DO', 'CO2', 'depth', 'GPP', 'ER'),
    names_to = "variable",
    values_to='conc'
  )%>%
  mutate(
    flood=as.factor(flood),
    variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS')),
    #class = factor(class, levels = c("HI", "BO", "FR"))
  )%>%
  left_join(  flood.response%>%select(ID, flood, variable, peak.Date, flood.end, flood.start)
  )


