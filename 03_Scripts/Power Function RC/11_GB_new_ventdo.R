# =============================================================================
# NEW, SELF-CONTAINED ATTEMPT -- does not modify any other script.
#
# Extracts GB vent DO from the raw roving-logger field files (previously
# unused -- only 6 grab samples from 04_Outputs/VentDO.csv were in the
# pipeline). Source:
# \\ad.ufl.edu\ifas\SFRC\Groups\Hydrology\Howley\trash\SpringsProject_Sam&Paul\Hobo\Roving_edited\GilchristBlue\DO
#
# Data-quality rules (from Samantha):
#   - Any reading >7 mg/L within a file is the sensor being pulled out of the
#     water -- discard those rows, keep the rest of that visit.
#   - Visit averages, not per-timestamp matching to the master dataset.
#   - Three specific visits (2022-08-15 = 0.95, 2023-01-03 = 0.57,
#     2023-03-15 = 0.22 mg/L) look like AM's vent water, not GB's -- AM runs
#     low, GB runs high, and both sites were sampled the same days, so this is
#     plausibly a filing mixup. Excluded from GB's series here.
# =============================================================================

library(readxl)
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')

roving_dir <- "//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Howley/trash/SpringsProject_Sam&Paul/Hobo/Roving_edited/GilchristBlue/DO"
outdir <- "04_Outputs/Power Function RC"

# ---- 1. read + extract each file -------------------------------------------
read_one <- function(f) {
  ext <- tools::file_ext(f)
  base <- basename(f)
  if (base == "Roving_GB_DO_50402023.csv") {
    df <- read_csv(f, skip = 1, show_col_types = FALSE) %>% select(1:3)
    names(df) <- c("Date", "DO", "Temp")
    return(df %>% mutate(Date = mdy_hm(Date), DO = as.numeric(DO), Temp = as.numeric(Temp)))
  }
  if (ext == "xlsx") {
    sh <- excel_sheets(f)
    target <- sh[str_detect(tolower(sh), "gilchrist|gb|roving|sheet1")]
    if (length(target) == 0) target <- sh[1]
    df <- read_excel(f, sheet = target[1]) %>% select(1:3)
  } else {
    df <- read_csv(f, show_col_types = FALSE) %>% select(1:3)
  }
  names(df) <- c("Date", "DO", "Temp")
  if (!inherits(df$Date, "POSIXct") && !inherits(df$Date, "Date")) df <- df %>% mutate(Date = mdy_hm(Date))
  df %>% mutate(DO = as.numeric(DO), Temp = as.numeric(Temp))
}

files <- list.files(roving_dir, full.names = TRUE)
extracted <- map_dfr(files, function(f) {
  df <- tryCatch(read_one(f), error = function(e) NULL)
  if (is.null(df)) return(tibble(file = basename(f), status = "READ ERROR"))
  df <- df %>% filter(!is.na(DO), !is.na(Date))
  if (nrow(df) == 0) return(tibble(file = basename(f), status = "NO DATA"))
  n_total <- nrow(df)
  valid <- df %>% filter(DO <= 7)  # discard sensor-out-of-water readings
  if (nrow(valid) == 0) {
    return(tibble(file = basename(f), status = "ALL READINGS >7, EXCLUDED",
                   date = as.character(as.Date(min(df$Date))), n_total = n_total))
  }
  tibble(file = basename(f), status = "OK", date = as.character(as.Date(min(valid$Date))),
         n_total = n_total, n_used = nrow(valid), n_discarded = n_total - nrow(valid),
         mean_DO = round(mean(valid$DO), 3), sd_DO = round(sd(valid$DO), 3),
         mean_Temp = round(mean(valid$Temp, na.rm = TRUE), 2))
})

cat("=== Extraction, all files ===\n")
print(extracted, n = 30, width = Inf)

# exclude the three visits flagged as likely AM, not GB
suspect_dates <- c("2022-08-15", "2023-01-03", "2023-03-15")
new_ventdo_gb <- extracted %>%
  filter(status == "OK", !date %in% suspect_dates) %>%
  transmute(ID = "GB", Date = ymd_hms(paste(date, "00:00:00")), VentDO = mean_DO, VentTemp = mean_Temp)

cat("\n=== Excluded as likely-AM (per Samantha) ===\n")
print(extracted %>% filter(date %in% suspect_dates) %>% select(file, date, mean_DO, sd_DO))

cat("\n=== New GB VentDO series (visit averages, n=", nrow(new_ventdo_gb), ") ===\n")
print(new_ventdo_gb %>% arrange(Date), n = 30)

# compare to what was already in the pipeline for GB
old_ventdo_gb <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>% filter(ID == "GB")
cat("\n=== Old (pipeline) GB VentDO, for comparison ===\n")
print(old_ventdo_gb)

write_csv(new_ventdo_gb, file.path(outdir, "GB_ventDO_new.csv"))

# ---- 2. rerun GB's two-station calc with the new VentDO series, M6 K600 ---
# (M6 per Samantha's stated preference)
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% filter(ID == "GB")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ") %>% filter(ID == "GB")
area_gb <- left_join(width, length_tbl, by = "ID") %>% mutate(area = w * m)

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())) %>% filter(ID == "GB"))
k600_gb <- read_csv(file.path(outdir, "K600_M6_breakpoint_stat.csv"), col_types = cols(ID = col_character())) %>%
  filter(ID == "GB") %>% mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))

run_gb <- function(ventdo_series, label) {
  master <- reduce(c(base_data, list(k600_gb)), full_join, by = c("ID", "Date")) %>% left_join(area_gb, by = "ID")
  master <- suppressWarnings(full_join(master, ventdo_series, by = c("ID", "Date"))) %>%
    arrange(Date) %>%
    fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
    distinct(Date, .keep_all = TRUE)

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
    group_by(date) %>% filter(n() >= 20) %>% ungroup()

  d <- d %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ "day", not.air.water.xchange < 0 ~ "night")) %>%
    filter(time != "remove") %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>% filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>% ungroup()

  isolate <- d %>% group_by(date, time) %>% summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")
  ER <- isolate %>% filter(time == "night") %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == "day") %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = "date") %>% filter(GPP <= 34, ER >= -34)

  depth_lookup <- master %>% mutate(date = as_date(Date)) %>% group_by(date) %>%
    summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

  NEP %>% left_join(depth_lookup, by = "date") %>% mutate(ventdo_source = label)
}

result_new <- run_gb(new_ventdo_gb, "new_roving_ventdo")
result_old <- run_gb(old_ventdo_gb %>% select(ID, Date, VentDO, VentTemp), "old_6_grab_samples")

cat("\n=== GB two-station ER: old VentDO vs new VentDO ===\n")
cat("old (6 grab samples): n_days=", nrow(result_old), " median ER=", round(median(result_old$ER, na.rm=TRUE), 2), "\n")
cat("new (roving-derived): n_days=", nrow(result_new), " median ER=", round(median(result_new$ER, na.rm=TRUE), 2), "\n")

one_station_gb <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>% filter(ID == "GB")
cat("one-station: median ER=", round(median(one_station_gb$ER, na.rm = TRUE), 2), "\n")

# ---- 3. plot: old vs new VentDO, ER vs depth, against one-station ---------
depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>% filter(ID == "GB") %>%
  group_by(date) %>% summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station_plot <- one_station_gb %>% left_join(depth_daily, by = "date") %>%
  transmute(date, depth, ER, GPP, source = "one-station")

two_plot <- bind_rows(
  result_old %>% transmute(date, depth, ER, GPP, source = "two-station (old VentDO)"),
  result_new %>% transmute(date, depth, ER, GPP, source = "two-station (new VentDO)")
)

plot_data <- bind_rows(
  one_station_plot %>% mutate(panel = "vs old VentDO"),
  one_station_plot %>% mutate(panel = "vs new VentDO"),
  two_plot %>% filter(source == "two-station (old VentDO)") %>% mutate(panel = "vs old VentDO"),
  two_plot %>% filter(source == "two-station (new VentDO)") %>% mutate(panel = "vs new VentDO")
) %>% pivot_longer(c(ER, GPP), names_to = "flux", values_to = "value")

p <- ggplot(plot_data, aes(x = depth, y = value, color = source)) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey70") +
  facet_grid(flux ~ panel, scales = "free_y") +
  scale_color_manual(values = c("one-station" = "#762a83", "two-station (old VentDO)" = "#d95f02",
                                 "two-station (new VentDO)" = "#1b7837")) +
  labs(title = "GB: does the roving-derived VentDO series close the ER gap?",
       subtitle = "Left: current pipeline (6 grab samples). Right: expanded roving-derived series (this script)",
       x = "depth (m)", y = NULL, color = NULL) +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/14_GB_new_ventdo_comparison.png"), p, width = 10, height = 7, dpi = 150)

cat("\nDone -> GB_ventDO_new.csv, GB_ventDO_extraction_raw (printed above),\n",
    "figures/14_GB_new_ventdo_comparison.png\n")
