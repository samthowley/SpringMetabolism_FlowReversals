library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "ID", "LF")

#call in variables, same as two station.R####
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
length <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area <- left_join(width, length) %>% mutate(area = w * m) %>%
  mutate(m = if_else(ID == 'AM', 800, m))

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())))

VentDO <- read_csv("02_Clean_data/Chem/VentDO_all.csv", show_col_types = FALSE)%>%
  filter(VentDO>0)%>%
  mutate(VentDO=if_else(VentDO<2 & ID=='GB', NA, VentDO))

ggplot(VentDO, aes(x = Date, y = VentDO)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_hline(yintercept = 0, color = 'gray') +
  facet_wrap(~ ID, scales = "free") +
  theme_minimal()


lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

#1. same mass-balance pipeline as two station.R, wrapped so it can run per K600 method####
run_two_station <- function(k600) {
  master <- reduce(c(base_data, list(k600)), full_join, by = c("ID", "Date")) %>%
    left_join(area, by = "ID")

  master <- full_join(master, VentDO, by = c('ID', 'Date')) %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
    filter(!ID %in% c('OS', 'IU')) %>%
    distinct(ID, Date, .keep_all = TRUE)

  discharge <- master %>% mutate(discharge = w * depth * velocity * 86400)

  change.DO.flux <- discharge %>% mutate(change.DO.flux = ((DO - VentDO) * discharge) / area)

  DO.deficit <- change.DO.flux %>% mutate(
    Vent.DO.sat = Cs(VentTemp),
    stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
    DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2
  )

  K.rearation <- DO.deficit %>% mutate(K.flux = K600_1.d_daily * depth * DO.deficit.from.sat)

  air.water.xchange <- K.rearation %>% mutate(not.air.water.xchange = change.DO.flux - K.flux)

  active.reach <- air.water.xchange %>%
    mutate(reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
           reach.test = if_else(reach.km > 3 * km, 'above', 'passes'),
           reach.test = if_else(reach.km < 0.4 * km, 'below', reach.test),
           reach.test = if_else(velocity < 0, 'below', reach.test)) %>%
    filter(reach.test %in% c('passes', 'above')) %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>%
    filter(n() >= 20) %>%
    ungroup() %>% select(-date)

  day.parse <- left_join(active.reach, lat.lon, by = "ID") %>%
    ungroup() %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ 'day', not.air.water.xchange < 0 ~ 'night')) %>%
    select(-lat, -lon) %>%
    filter(time != 'remove') %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>%
    filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>%
    ungroup()

  isolate <- day.parse %>% group_by(date, ID, time) %>%
    summarize(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")
  ER <- isolate %>% filter(time == 'night') %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == 'day') %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = c("date", "ID"))

  left_join(day.parse, NEP, by = c("date", "ID"))
}

#2. run it once per K600 method####
k600_files <- c(
  M6_breakpoint_stat = "K600_M6_breakpoint_stat.csv",
  M7_breakpoint_domain = "K600_M7_breakpoint_domain.csv",
  M8_power = "K600_M8_power.csv",
  static_site_median = "K600_static_all_sites.csv",
  night_regression = "K600_night_regression.csv",
  one_station = "K600_one_station.csv"
)

two_station_all <- map_dfr(names(k600_files), function(method_name) {
  k600 <- read_csv(file.path(outdir, k600_files[[method_name]]), col_types = cols(ID = col_character())) %>%
    mutate(Date = as.POSIXct(as.character(Date), tz = "UTC")) %>%
    select(ID, Date, K600_1.d_daily)  # night_regression's file also carries depth -- drop it, base_data already has depth
  run_two_station(k600) %>% mutate(method = method_name)
})

write_csv(two_station_all, file.path(outdir, "compiled_two_station_all_methods.csv"))


#3. join against one-station####
depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  select(ID, date, GPP1 = GPP, ER1 = ER)

two_daily <- two_station_all %>% distinct(ID, date, method, GPP, ER)

compare <- two_daily %>%
  left_join(depth_daily, by = c("ID", "date")) %>%
  left_join(one_station, by = c("ID", "date"))

summary_tbl <- compare %>%
  group_by(ID, method) %>%
  summarise(
    n_days = n(),
    median_ER_two = median(ER, na.rm = TRUE),
    median_ER_one = median(ER1, na.rm = TRUE),
    ER_gap = median_ER_two - median_ER_one,
    median_GPP_two = median(GPP, na.rm = TRUE),
    median_GPP_one = median(GPP1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ID, abs(ER_gap))


#4. plot: ER & GPP vs depth, one-station vs every two-station method, per site####
one_station_plot <- one_station %>%
  left_join(depth_daily, by = c("ID", "date")) %>%
  transmute(ID, date, depth, source = "one-station", GPP = GPP1, ER = ER1)

two_plot <- two_daily %>%
  left_join(depth_daily, by = c("ID", "date")) %>%
  transmute(ID, date, depth, source = method, GPP, ER)

plot_data <- bind_rows(one_station_plot, two_plot) %>%
  pivot_longer(c(GPP, ER), names_to = "flux", values_to = "value")%>%
  mutate(source=if_else(source=="one_station", "one_station_K600", source))


unique(plot_data$source)

plot_data%>%
  filter(source %in% 
           c("one-station",'one_station_K600'))%>%
  ggplot(aes(x = depth, y = value, color = source)) +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_hline(yintercept = 0, color = 'gray') +
  facet_wrap(~ ID, scales = "free") +
  theme_minimal()



plot_data%>%
  filter(source %in% c("one-station","one_station_K600"))%>%
  ggplot(aes(x = date, y = value, color = source)) +
  geom_point(size = 0.8, alpha=0.4) +
  geom_hline(yintercept = 0, color = 'gray') +
  facet_wrap(~ ID, scales = "free") +
  theme_minimal()






