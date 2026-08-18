# Checks GB's reach-length/area assumption. Couldn't find independent GPS-
# surveyed reach-length documentation to verify the 350m/16.5m (area=5775m2)
# values used throughout (checked "Depth Profiles and Staffs.xlsx" -- has
# cross-section depth profiles at "headwater" and "Mouth" confirming two
# distinct sampling cross-sections, but no along-channel distance between
# them). So: same approach as the VentDO check -- quantify how sensitive
# GB's ER is to the area value, and see what correction would be needed to
# close the gap, to judge plausibility without independent verification.
# area appears once, as a straight divisor: change.DO.flux = (DO-VentDO)*
# discharge/area -- so if the true area is smaller than assumed, the current
# calc under-scales the raw signal, which would bias ER toward zero (too
# shallow) relative to the truth. That's the same direction as GB's gap.

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

outdir <- "04_Outputs/Power Function RC"

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% filter(ID == "GB")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ") %>% filter(ID == "GB")
area_gb_base <- left_join(width, length_tbl, by = "ID") %>% mutate(area = w * m)
cat("GB base values: w=", area_gb_base$w, " m=", area_gb_base$m, " km=", area_gb_base$km,
    " area=", area_gb_base$area, "\n")

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())) %>% filter(ID == "GB"))
VentDO_gb <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>% filter(ID == "GB")
k600_gb <- read_csv(file.path(outdir, "K600_M6_breakpoint_stat.csv"), col_types = cols(ID = col_character())) %>%
  filter(ID == "GB") %>% mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))

master <- reduce(c(base_data, list(k600_gb)), full_join, by = c("ID", "Date")) %>%
  mutate(w = area_gb_base$w, km = area_gb_base$km)
master <- suppressWarnings(full_join(master, VentDO_gb, by = c("ID", "Date"))) %>%
  arrange(Date) %>% fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>% distinct(Date, .keep_all = TRUE)

run_with_area_mult <- function(area_mult) {
  area_adj <- area_gb_base$area * area_mult
  d <- master %>%
    mutate(discharge = w * depth * velocity * 86400,
           change.DO.flux = ((DO - VentDO) * discharge) / area_adj,
           Vent.DO.sat = Cs(VentTemp), stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
           DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2,
           K.flux = K600_1.d_daily * depth * DO.deficit.from.sat,
           not.air.water.xchange = change.DO.flux - K.flux,
           reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
           reach.test = if_else(reach.km > 3 * km, "above", "passes"),
           reach.test = if_else(reach.km < 0.4 * km, "below", reach.test),
           reach.test = if_else(velocity < 0, "below", reach.test)) %>%
    filter(reach.test %in% c("passes", "above")) %>%
    mutate(date = as_date(Date)) %>% group_by(date) %>% filter(n() >= 20) %>% ungroup() %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ "day", not.air.water.xchange < 0 ~ "night")) %>%
    filter(time != "remove") %>% mutate(date = as_date(Date)) %>%
    group_by(date) %>% filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>% ungroup()

  isolate <- d %>% group_by(date, time) %>% summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")
  ER <- isolate %>% filter(time == "night") %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == "day") %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = "date") %>% filter(GPP <= 34, ER >= -34)
  tibble(area_mult = area_mult, area = area_adj, n_days = nrow(NEP), median_ER = median(NEP$ER, na.rm = TRUE))
}

mults <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0)
results <- map_dfr(mults, run_with_area_mult)
cat("\n=== GB median ER vs area multiplier (1.0 = current assumption, area=5775 m2) ===\n")
print(results)

one_station_gb <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>% filter(ID == "GB")
target <- median(one_station_gb$ER, na.rm = TRUE)
cat("\none-station median ER target:", round(target, 2), "\n")

write_csv(results, file.path(outdir, "GB_area_sensitivity.csv"))

p <- ggplot(results, aes(x = area_mult, y = median_ER)) +
  geom_line(linewidth = 1, color = "#1b7837") + geom_point(size = 2, color = "#1b7837") +
  geom_hline(yintercept = target, linetype = "dashed", color = "#762a83") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "grey50") +
  annotate("text", x = 1.05, y = min(results$median_ER), label = "current assumption", hjust = 0, size = 3) +
  annotate("text", x = 2.7, y = target + 1, label = "one-station median ER", color = "#762a83", hjust = 0) +
  labs(title = "GB: two-station median ER vs. area/reach-length multiplier",
       subtitle = paste0("Current area = 5775 m2 (width 16.5m x reach length 350m)"),
       x = "area multiplier (1.0 = current)", y = "median two-station ER (g O2/m2/day)") +
  theme_bw(base_size = 12)
ggsave(file.path(outdir, "figures/15_GB_area_sensitivity.png"), p, width = 8, height = 6, dpi = 150)

cat("\nDone -> GB_area_sensitivity.csv, figures/15_GB_area_sensitivity.png\n")
