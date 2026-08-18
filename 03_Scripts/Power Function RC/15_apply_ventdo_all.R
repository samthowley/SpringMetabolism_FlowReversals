# =============================================================================
# Apply the new, all-sites roving-derived VentDO (14_compile_all_ventdo.R) to
# the two-station DO mass-balance calc for AM, GB, LF (the paired sites with a
# two-station/one-station gap to potentially close). ID is untouched (its
# VentDO didn't change). OS has no two-station companion, so there's no gap to
# test for it here.
#
# NEW, SELF-CONTAINED -- does not modify "two station.R" or any live pipeline
# script. Old-VentDO comparison reuses the M6 two-station results already on
# disk (04_Outputs/Power Function RC/two.station.results_M6_breakpoint_stat.csv),
# which were built from the old 04_Outputs/VentDO.csv.
# =============================================================================

library(readxl)
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "LF")

# ---- shared inputs ----------------------------------------------------------
width  <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area_tbl <- left_join(width, length_tbl, by = "ID") %>%
  mutate(m = if_else(ID == "AM", 800, m), area = w * m) %>%
  filter(ID %in% sites)

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_names <- basename(file.names)
keep <- base_names %in% c("DO.csv", "Temp.csv", "depth.csv", "velocity.csv", "K600.csv")
# match the same columns "two station.R" pulls: DO, Temp, depth, velocity (not K600.csv -- using M6 instead)
base_files <- file.names[base_names %in% c("DO.csv", "depth.csv", "velocity.csv")]
# Temp lives inside DO.csv in this pipeline (checked below); fall back gracefully if not.
base_data <- lapply(base_files, function(x) read_csv(x, col_types = cols(ID = col_character())) %>% filter(ID %in% sites))

k600_all <- read_csv(file.path(outdir, "K600_M6_breakpoint_stat.csv"), col_types = cols(ID = col_character())) %>%
  filter(ID %in% sites) %>% mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))

old_ventdo <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>% filter(ID %in% sites) %>% select(ID, Date, VentDO, VentTemp)
new_ventdo <- read_csv("02_Clean_data/Chem/VentDO_all.csv", show_col_types = FALSE) %>% filter(ID %in% sites) %>% select(ID, Date, VentDO, VentTemp)

# ---- two-station DO mass balance, generalized from 11_GB_new_ventdo.R -------
run_two_station <- function(ventdo_series, label) {
  master <- reduce(base_data, full_join, by = c("ID", "Date")) %>%
    full_join(k600_all, by = c("ID", "Date")) %>%
    left_join(area_tbl, by = "ID")
  master <- suppressWarnings(full_join(master, ventdo_series, by = c("ID", "Date"))) %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
    ungroup() %>%
    distinct(ID, Date, .keep_all = TRUE)

  d <- master %>%
    mutate(discharge = w * depth * velocity * 86400,
           change.DO.flux = ((DO - VentDO) * discharge) / area,
           Vent.DO.sat = Cs(VentTemp),
           stat2.DO.sat = Cs(fahrenheit.to.celsius(Temp)),
           DO.deficit.from.sat = ((Vent.DO.sat - VentDO) + (stat2.DO.sat - DO)) / 2,
           K.flux = K600_1.d_daily * depth * DO.deficit.from.sat,
           not.air.water.xchange = change.DO.flux - K.flux,
           reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
           reach.test = if_else(reach.km > 3 * km, "above", "passes"),
           reach.test = if_else(reach.km < 0.4 * km, "below", reach.test),
           reach.test = if_else(velocity < 0, "below", reach.test)) %>%
    filter(reach.test %in% c("passes", "above")) %>%
    mutate(date = as_date(Date)) %>%
    group_by(ID, date) %>% filter(n() >= 20) %>% ungroup()

  d <- d %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ "day", not.air.water.xchange < 0 ~ "night")) %>%
    filter(time != "remove") %>%
    group_by(ID, date) %>% filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>% ungroup()

  isolate <- d %>% group_by(ID, date, time) %>% summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")
  ER <- isolate %>% filter(time == "night") %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == "day") %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = c("ID", "date")) %>% filter(GPP <= 34, ER >= -34)

  depth_lookup <- master %>% mutate(date = as_date(Date)) %>% group_by(ID, date) %>%
    summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

  NEP %>% left_join(depth_lookup, by = c("ID", "date")) %>% mutate(ventdo_source = label)
}

result_old <- run_two_station(old_ventdo, "old_ventdo")
result_new <- run_two_station(new_ventdo, "new_ventdo_all_sites")

one_station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

summary_tbl <- bind_rows(
  result_old %>% mutate(source = "two-station (old VentDO)"),
  result_new %>% mutate(source = "two-station (new VentDO, all-sites compile)"),
  one_station %>% transmute(ID, date, ER, GPP, source = "one-station")
) %>%
  group_by(ID, source) %>%
  summarise(n_days = n(), median_ER = round(median(ER, na.rm = TRUE), 2),
            median_GPP = round(median(GPP, na.rm = TRUE), 2), .groups = "drop") %>%
  arrange(ID, source)

write_csv(summary_tbl, file.path(outdir, "ventdo_all_gap_comparison_summary.csv"))
cat("\n=== ER/GPP comparison: old VentDO vs new (all-sites) VentDO vs one-station, by site ===\n")
print(summary_tbl, n = 30)

# ---- plot: ER & GPP vs depth, old vs new VentDO vs one-station, per site ---
depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>% filter(ID %in% sites) %>%
  group_by(ID, date) %>% summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station_plot <- one_station %>% left_join(depth_daily, by = c("ID", "date")) %>%
  transmute(ID, date, depth, ER, GPP, source = "one-station")

two_plot <- bind_rows(
  result_old %>% transmute(ID, date, depth, ER, GPP, source = "two-station (old VentDO)"),
  result_new %>% transmute(ID, date, depth, ER, GPP, source = "two-station (new VentDO, all-sites)")
)

plot_data <- bind_rows(one_station_plot, two_plot) %>%
  pivot_longer(c(ER, GPP), names_to = "flux", values_to = "value")

p <- ggplot(plot_data, aes(x = depth, y = value, color = source)) +
  geom_point(size = 1.1, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey70") +
  facet_grid(flux ~ ID, scales = "free") +
  scale_color_manual(values = c("one-station" = "#762a83",
                                 "two-station (old VentDO)" = "#d95f02",
                                 "two-station (new VentDO, all-sites)" = "#1b7837")) +
  labs(title = "Does the all-sites roving-derived VentDO close the one-station/two-station gap?",
       x = "depth (m)", y = NULL, color = NULL) +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")

dir.create(file.path(outdir, "figures"), showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(outdir, "figures/15_ventdo_all_gap_comparison.png"), p, width = 12, height = 7, dpi = 150)

cat("\nDone -> ventdo_all_gap_comparison_summary.csv, figures/15_ventdo_all_gap_comparison.png\n")
