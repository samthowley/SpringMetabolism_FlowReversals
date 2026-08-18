# How much does GB's two-station ER shift if VentDO is systematically wrong
# by a plausible amount? VentDO enters the flux calc twice:
#   change.DO.flux = (DO - VentDO) * discharge / area
#   DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2
#   K.flux = K600 * depth * DO.deficit.from.sat
#   not.air.water.xchange = change.DO.flux - K.flux
# so d(not.air.water.xchange)/d(VentDO) = -discharge/area + K600*depth/2 --
# this reruns the actual calc under a grid of VentDO offsets rather than
# relying on that linear approximation, since discharge/area varies a lot
# hour to hour. Self-contained, doesn't touch any other script. Uses M7's
# K600 series (any of M6/M7/M8 would isolate the same VentDO effect).

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

outdir <- "04_Outputs/Power Function RC"

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% filter(ID == "GB")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ") %>% filter(ID == "GB")
area_gb <- left_join(width, length_tbl, by = "ID") %>% mutate(area = w * m)

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())) %>% filter(ID == "GB"))
VentDO_gb <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>% filter(ID == "GB")
k600_gb <- read_csv(file.path(outdir, "K600_M7_breakpoint_domain.csv"), col_types = cols(ID = col_character())) %>%
  filter(ID == "GB") %>% mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))

master <- reduce(c(base_data, list(k600_gb)), full_join, by = c("ID", "Date")) %>% left_join(area_gb, by = "ID")
master <- suppressWarnings(full_join(master, VentDO_gb, by = c("ID", "Date"))) %>%
  arrange(Date) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
  distinct(Date, .keep_all = TRUE)

lat.lon <- data.frame(ID = "GB", lat = 29.83, lon = -82.68)

run_with_offset <- function(ventdo_offset) {
  d <- master %>%
    mutate(VentDO_adj = VentDO + ventdo_offset,
           discharge = w * depth * velocity * 86400,
           change.DO.flux = ((DO - VentDO_adj) * discharge) / area,
           Vent.DO.sat = Cs(VentTemp),
           stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
           DO.deficit.from.sat = ((Vent.DO.sat - VentDO_adj) + (stat2.DO.sat - DO)) / 2,
           K.flux = K600_1.d_daily * depth * DO.deficit.from.sat,
           not.air.water.xchange = change.DO.flux - K.flux,
           reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
           reach.test = if_else(reach.km > 3 * km, "above", "passes"),
           reach.test = if_else(reach.km < 0.4 * km, "below", reach.test),
           reach.test = if_else(velocity < 0, "below", reach.test)) %>%
    filter(reach.test %in% c("passes", "above")) %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>% filter(n() >= 20) %>% ungroup()

  d <- left_join(d, lat.lon, by = "ID") %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ "day", not.air.water.xchange < 0 ~ "night")) %>%
    filter(time != "remove") %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>% filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>% ungroup()

  isolate <- d %>% group_by(date, time) %>% summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")
  ER <- isolate %>% filter(time == "night") %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == "day") %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = "date") %>% filter(GPP <= 34, ER >= -34)

  tibble(ventdo_offset = ventdo_offset, n_days = nrow(NEP), median_ER = median(NEP$ER, na.rm = TRUE))
}

offsets <- seq(-3, 3, by = 0.5)
results <- map_dfr(offsets, run_with_offset)
cat("=== GB median two-station ER (all valid days) under VentDO offsets ===\n")
print(results)

baseline_ER <- results$median_ER[results$ventdo_offset == 0]

# for reference: one-station's median ER at GB, and the actual spread across
# GB's 6 real VentDO grab samples
one_station_gb <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>% filter(ID == "GB")
cat("\none-station median ER at GB:", round(median(one_station_gb$ER, na.rm = TRUE), 2), "\n")
cat("two-station median ER at GB, VentDO as-is (offset=0):", round(baseline_ER, 2), "\n")

# linear sensitivity: fit slope of median_ER vs offset
slope <- coef(lm(median_ER ~ ventdo_offset, data = results))[2]
cat("\nsensitivity: d(median ER)/d(VentDO) =~", round(slope, 2), "g O2/m2/day per mg/L\n")

gap <- median(one_station_gb$ER, na.rm = TRUE) - baseline_ER
offset_needed <- gap / slope
cat("VentDO offset that would be needed to close the median ER gap:", round(offset_needed, 2), "mg/L\n")

cat("\nGB's actual observed grab-sample range: 4.6 to 6.25 mg/L (spread of 1.65 mg/L)\n")
cat("=> offset needed is", round(abs(offset_needed) / 1.65, 1),
    "x the entire observed grab-sample spread\n")

write_csv(results, file.path(outdir, "GB_ventdo_sensitivity.csv"))

p <- ggplot(results, aes(x = ventdo_offset, y = median_ER)) +
  geom_line(linewidth = 1, color = "#1b7837") + geom_point(size = 2, color = "#1b7837") +
  geom_hline(yintercept = median(one_station_gb$ER, na.rm = TRUE), linetype = "dashed", color = "#762a83") +
  annotate("text", x = -2.5, y = median(one_station_gb$ER, na.rm = TRUE) + 1,
           label = "one-station median ER", color = "#762a83", hjust = 0) +
  geom_vline(xintercept = c(-1.65/2, 1.65/2), linetype = "dotted", color = "grey50") +
  annotate("text", x = 0, y = min(results$median_ER) + 1,
           label = "observed grab-sample spread", color = "grey40", size = 3) +
  labs(title = "GB: two-station median ER vs. systematic VentDO offset",
       subtitle = "Dotted lines = the actual spread across GB's 6 real VentDO grab samples, centered at 0",
       x = "VentDO offset applied (mg/L)", y = "median two-station ER (g O2/m2/day)") +
  theme_bw(base_size = 12)
ggsave(file.path(outdir, "figures/13_GB_ventdo_sensitivity.png"), p, width = 8, height = 6, dpi = 150)

cat("\nDone -> GB_ventdo_sensitivity.csv, figures/13_GB_ventdo_sensitivity.png\n")
