library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "ID", "LF")

#1. static K600: site-median of the trimmed gas-dome floats, all sites####
valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE)

judgment_drop <- tribble(
  ~ID, ~row,
  "AM", 24, "AM", 5, "GB", 16, "ID", 6, "LF", 5
)
trimmed <- valid %>% anti_join(judgment_drop, by = c("ID", "row"))

static_k600 <- trimmed %>%
  group_by(ID) %>%
  summarise(k600_static = median(k600_1.day), .groups = "drop")
static_k600

depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites)

depth_daily <- depth %>%
  mutate(date = as.Date(Date)) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

K600_static <- depth %>%
  left_join(static_k600, by = "ID") %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(K600_1.d_daily = mean(k600_static), .groups = "drop") %>%
  mutate(Date = ymd_hms(paste(Date, "00:00:00")))

write_csv(K600_static, file.path(outdir, "K600_static_all_sites.csv"))

#2. nighttime regression K600: dDO/dt vs DO deficit at night, one estimate
# per night per site -- independent of the gas-dome floats entirely. Night
# is true solar time (see 20_solar_time_two_station.R): raw Date is
# fixed-offset EST (UTC-5, no DST -- checked), so true UTC = Date + 5h.####
lat.lon <- data.frame(
  ID = c('AM', 'LF', 'GB', 'ID'),
  lat = c(30.155, 29.585, 29.83, 29.93),
  lon = c(-83.238, -82.93, -82.68, -82.8))

DO <- read_csv("02_Clean_data/Chem/DO.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

night_data <- DO %>%
  left_join(depth, by = c("ID", "Date")) %>%
  left_join(lat.lon, by = "ID") %>%
  arrange(ID, Date) %>%
  mutate(
    utc.time = as.POSIXct(Date, tz = "UTC") + hours(5),
    solar.time = convert_UTC_to_solartime(utc.time, lon, time.type = "mean solar"),
    light = calc_light(solar.time, lat, lon),
    time = if_else(light > 0, "day", "night")
  ) %>%
  group_by(ID) %>%
  mutate(
    block_id = cumsum(time != lag(time, default = first(time))),
    gap_hr = as.numeric(difftime(Date, lag(Date), units = "hours")),
    dDO_per_day = (DO - lag(DO)) * 24,
    DO.sat = Cs(fahrenheit.to.celsius(Temp)),
    DO.deficit = DO.sat - lag(DO)
  ) %>%
  ungroup() %>%
  filter(time == "night", gap_hr == 1, !is.na(dDO_per_day), !is.na(DO.deficit))

night_reg <- night_data %>%
  group_by(ID, block_id) %>%
  filter(n() >= 4) %>%
  summarise(
    Date = min(Date),
    depth = mean(depth, na.rm = TRUE),
    mean_Temp_C = mean(fahrenheit.to.celsius(Temp), na.rm = TRUE),
    KT = coef(lm(dDO_per_day ~ DO.deficit))[2],
    .groups = "drop"
  ) %>%
  filter(KT > 0, is.finite(KT)) %>%
  mutate(K600_1.d_daily = convert_kGAS_to_k600(KT, mean_Temp_C, "O2")) %>%
  filter(is.finite(K600_1.d_daily), K600_1.d_daily > 0, K600_1.d_daily < 100) %>%
  select(ID, Date, depth, K600_1.d_daily)

cat("nighttime-regression K600: n usable nights per site\n")
night_reg %>% count(ID)
write_csv(night_reg, file.path(outdir, "K600_night_regression.csv"))

#3. one-station's own predicted K600 -- its internal Bayesian estimate,
# independent of this whole depth-RC exercise####
one_station_k600 <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  transmute(ID, Date = ymd_hms(paste(date, "00:00:00")), K600_1.d_daily = K600) %>%
  filter(!is.na(K600_1.d_daily))

write_csv(one_station_k600, file.path(outdir, "K600_one_station.csv"))

#4. bring in the curve-based methods already fit (M6/M7/M8), tag by method####
M6 <- read_csv(file.path(outdir, "K600_M6_breakpoint_stat.csv"), show_col_types = FALSE) %>%
  mutate(method = "M6_breakpoint_stat")
M7 <- read_csv(file.path(outdir, "K600_M7_breakpoint_domain.csv"), show_col_types = FALSE) %>%
  mutate(method = "M7_breakpoint_domain")
M8 <- read_csv(file.path(outdir, "K600_M8_power.csv"), show_col_types = FALSE) %>%
  mutate(method = "M8_power")
static <- K600_static %>% mutate(method = "static_site_median")
night_reg_tagged <- night_reg %>% select(-depth) %>% mutate(method = "night_regression")
one_station_tagged <- one_station_k600 %>% mutate(method = "one_station")

all_k600 <- bind_rows(M6, M7, M8, static, night_reg_tagged, one_station_tagged)
write_csv(all_k600, file.path(outdir, "compiled_K600_all_methods.csv"))

#5. join to depth for plotting -- same depth_daily source for every method,
# including night_regression and one_station, so they're all on equal footing####
plot_data <- all_k600 %>%
  mutate(date = as.Date(Date)) %>%
  left_join(depth_daily, by = c("ID", "date"))

#6. K600 vs depth, every method overlaid, raw gas-dome floats underneath####
ggplot(plot_data, aes(x = depth, y = K600_1.d_daily, color = method)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_point(data = trimmed, aes(x = depth, y = k600_1.day), color = "black", size = 1.4,
             inherit.aes = FALSE) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()
#ggsave(file.path(outdir, "figures/17_compiled_K600_vs_depth.png"), width = 12, height = 8, dpi = 150)

#7. K600 over time, every method, per site (points for the two sparse
# methods, lines for the continuous depth-curve methods)####
ggplot(plot_data, aes(x = Date, y = K600_1.d_daily, color = method)) +
  geom_line(data = plot_data %>% filter(!method %in% c("night_regression", "one_station")), alpha = 0.7) +
  geom_point(data = plot_data %>% filter(method %in% c("night_regression", "one_station")), size = 0.8, alpha = 0.6) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()
#ggsave(file.path(outdir, "figures/18_compiled_K600_vs_time.png"), width = 12, height = 8, dpi = 150)
