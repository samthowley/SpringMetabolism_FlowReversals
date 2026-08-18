# Revisits a flat, site-average K600 for GB (Samantha's original approach,
# from before this whole depth-RC investigation) instead of any depth curve.
# Justified independently of how it performs: GB's gas-dome data never
# supported a depth relationship in the first place (R^2 ~0.25-0.3 across
# every method tried), so a constant may just be the honest answer for this
# site specifically, whatever K600 methodology is used elsewhere.

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

outdir <- "04_Outputs/Power Function RC"

valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE) %>% filter(ID == "GB")
judgment_drop_gb <- 16  # same single-point exclusion as M1/M6/M7 (2.6x its own same-day replicates)
gb_trimmed <- valid %>% filter(row != judgment_drop_gb)

k600_mean <- mean(gb_trimmed$k600_1.day)
k600_median <- median(gb_trimmed$k600_1.day)
cat("GB trimmed gas-dome K600 (n=", nrow(gb_trimmed), "): mean=", round(k600_mean,2),
    " median=", round(k600_median,2), " sd=", round(sd(gb_trimmed$k600_1.day),2), "\n")

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% filter(ID == "GB")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ") %>% filter(ID == "GB")
area_gb <- left_join(width, length_tbl, by = "ID") %>% mutate(area = w * m)

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())) %>% filter(ID == "GB"))
VentDO_gb <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>% filter(ID == "GB")

run_gb_constant_k600 <- function(k600_value, label) {
  master <- reduce(base_data, full_join, by = c("ID", "Date")) %>% left_join(area_gb, by = "ID")
  master <- suppressWarnings(full_join(master, VentDO_gb, by = c("ID", "Date"))) %>%
    arrange(Date) %>% fill(VentDO, VentTemp, .direction = "downup") %>% distinct(Date, .keep_all = TRUE) %>%
    mutate(K600_1.d_daily = k600_value)

  d <- master %>%
    mutate(discharge = w * depth * velocity * 86400,
           change.DO.flux = ((DO - VentDO) * discharge) / area,
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

  depth_lookup <- master %>% mutate(date = as_date(Date)) %>% group_by(date) %>%
    summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")
  NEP %>% left_join(depth_lookup, by = "date") %>% mutate(k600_source = label)
}

result_mean <- run_gb_constant_k600(k600_mean, "constant_mean")
result_median <- run_gb_constant_k600(k600_median, "constant_median")

one_station_gb <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>% filter(ID == "GB")
target <- median(one_station_gb$ER, na.rm = TRUE)

cat("\n=== GB: constant K600 vs one-station ===\n")
cat("constant K600 = mean (", round(k600_mean,2), "): n_days=", nrow(result_mean),
    " median ER=", round(median(result_mean$ER, na.rm=TRUE), 2), "\n")
cat("constant K600 = median (", round(k600_median,2), "): n_days=", nrow(result_median),
    " median ER=", round(median(result_median$ER, na.rm=TRUE), 2), "\n")
cat("one-station target: median ER=", round(target, 2), "\n")

# for reference, pull M6's (depth-curve) result for GB from the earlier run
m6_result <- read_csv(file.path(outdir, "two.station.results_M6_breakpoint_stat.csv"), show_col_types = FALSE) %>%
  filter(ID == "GB") %>% distinct(date, .keep_all = TRUE)
cat("M6 (depth breakpoint curve) for comparison: n_days=", nrow(m6_result),
    " median ER=", round(median(m6_result$ER, na.rm=TRUE), 2), "\n")

write_csv(bind_rows(result_mean, result_median), file.path(outdir, "GB_constant_K600_results.csv"))

# ---- plot ----
depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>% filter(ID == "GB") %>%
  group_by(date) %>% summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")
one_station_plot <- one_station_gb %>% left_join(depth_daily, by = "date") %>%
  transmute(date, depth, ER, GPP, source = "one-station")

plot_data <- bind_rows(
  one_station_plot %>% mutate(panel = "constant = site mean K600"),
  one_station_plot %>% mutate(panel = "constant = site median K600"),
  result_mean %>% transmute(date, depth, ER, GPP, source = "two-station") %>% mutate(panel = "constant = site mean K600"),
  result_median %>% transmute(date, depth, ER, GPP, source = "two-station") %>% mutate(panel = "constant = site median K600")
) %>% pivot_longer(c(ER, GPP), names_to = "flux", values_to = "value")

p <- ggplot(plot_data, aes(x = depth, y = value, color = source)) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey70") +
  facet_grid(flux ~ panel, scales = "free_y") +
  scale_color_manual(values = c("one-station" = "#762a83", "two-station" = "#1b7837")) +
  labs(title = "GB: flat site-average K600 instead of a depth curve",
       x = "depth (m)", y = NULL, color = NULL) +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/16_GB_constant_K600.png"), p, width = 9, height = 6.5, dpi = 150)

cat("\nDone -> GB_constant_K600_results.csv, figures/16_GB_constant_K600.png\n")
