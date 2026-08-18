# Re-runs the two-station DO mass-balance metabolism calc (the logic in
# 03_Scripts/one station.R -- note: despite the filename, that script is the
# paired-station reach-scale mass-balance method; it's what writes
# 04_Outputs/two.station.results.csv) once per K600 methodology, swapping in
# each candidate K600 series built by 02_fit_breakpoint_K600.R in place of
# 02_Clean_data/Chem/K600.csv. Nothing else about the calc changes: same
# reach-length test, same velocity<0 (flow-reversal/stagnant) exclusion, same
# day/night parsing and GPP<=34 / ER>=-34 plausibility filter as the original.

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')  # for Cs()
library(readxl)

outdir <- "04_Outputs/Power Function RC"
methods <- c("M6_breakpoint_stat", "M7_breakpoint_domain")

# ---- static inputs shared across all methodology runs ----------------------
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area <- left_join(width, length_tbl, by = "ID") %>%
  mutate(area = w * m) %>%
  mutate(m = if_else(ID == "AM", 800, m))

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
# [depth.csv, DO.csv, velocity.csv] -- K600.csv is swapped in per methodology below
base_files <- file.names[c(2, 4, 12)]
base_data <- lapply(base_files, function(x) read_csv(x, col_types = cols(ID = col_character())))

VentDO <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE)

run_two_station <- function(k600_path, out_suffix) {
  k600 <- read_csv(k600_path, col_types = cols(ID = col_character())) %>%
    mutate(Date = as.character(Date))

  data_list <- c(base_data, list(k600 %>% mutate(Date = as.POSIXct(Date, tz = "UTC"))))
  master <- reduce(data_list, full_join, by = c("ID", "Date")) %>%
    left_join(area, by = "ID")

  master <- full_join(master, VentDO, by = c("ID", "Date")) %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
    filter(!ID %in% c("OS", "IU")) %>%
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
    mutate(
      reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
      reach.test = if_else(reach.km > 3 * km, "above", "passes"),
      reach.test = if_else(reach.km < 0.4 * km, "below", reach.test),
      reach.test = if_else(velocity < 0, "below", reach.test)
    ) %>%
    filter(reach.test %in% c("passes", "above")) %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>%
    filter(n() >= 20) %>%
    ungroup() %>%
    select(-date)

  lat.lon <- data.frame(
    ID = c("AM", "LF", "GB", "ID"),
    lat = c(30.155, 29.585, 29.83, 29.93),
    lon = c(-83.238, -82.93, -82.68, -82.8)
  )

  day.parse <- left_join(active.reach, lat.lon, by = "ID") %>%
    ungroup() %>%
    mutate(time = case_when(not.air.water.xchange > 0 ~ "day", not.air.water.xchange < 0 ~ "night")) %>%
    select(-lat, -lon) %>%
    filter(time != "remove") %>%
    mutate(date = as_date(Date)) %>%
    group_by(date) %>%
    filter(sum(not.air.water.xchange > 0, na.rm = TRUE) >= 5) %>%
    ungroup()

  isolate <- day.parse %>% group_by(date, ID, time) %>%
    summarise(avg = mean(not.air.water.xchange, na.rm = TRUE), .groups = "drop")

  ER <- isolate %>% filter(time == "night") %>% rename(ER = avg) %>% select(-time)
  GPP <- isolate %>% filter(time == "day") %>% rename(GPP = avg) %>% select(-time)
  NEP <- left_join(GPP, ER, by = c("date", "ID"))

  result <- left_join(day.parse, NEP, by = c("date", "ID")) %>%
    filter(GPP <= 34, ER >= -34)

  out_path <- file.path(outdir, paste0("two.station.results_", out_suffix, ".csv"))
  write_csv(result, out_path)
  cat("wrote", out_path, " n_days=", nrow(result %>% distinct(ID, date)),
      " sites=", paste(sort(unique(result$ID)), collapse=","), "\n")
  invisible(result)
}

for (m in methods) {
  run_two_station(file.path(outdir, paste0("K600_", m, ".csv")), m)
}
