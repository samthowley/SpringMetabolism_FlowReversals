# =============================================================================
# NEW, SELF-CONTAINED ATTEMPT -- does not modify or depend on running any
# other script in this folder. Explores whether calibrating the RC's
# high-depth "flat" value against the one-station model's independent K600
# estimate (instead of just extrapolating/truncating the gas-dome power fit)
# closes GB's persistent ER gap.
#
# Rationale (see chat): our RC is fit only to each site's 12-20 gas-dome
# floats. The one-station Bayesian model estimates K600 completely
# independently (discharge-binned priors on the same raw floats, but fit
# against the full diel oxygen record, not the depth-only RC). Comparing the
# two revealed GB's and LF's RC systematically *underestimates* K600 beyond
# the gas-dome sampled range relative to what one-station infers -- and an
# underestimated K600 under-corrects reaeration, leaving two-station ER too
# close to zero, which is exactly the direction and rough size of GB's gap.
#
# Method (M8_hybrid_calibrated), independent per site:
#   - Below the max gas-dome sampled depth: same power-law fit to the
#     judgment-trimmed gas-dome floats only (direct physical measurement --
#     unchanged from M7's declining segment).
#   - At/beyond that depth: instead of holding flat at the power curve's own
#     endpoint value (M7) or a separately-searched statistical breakpoint's
#     endpoint (M6), hold flat at the MEDIAN of the one-station model's own
#     K600 estimates observed in that same beyond-sampled-depth zone.
# Caveat worth flagging: this isn't a fully independent calibration -- the
# one-station model's K600 priors were themselves partly informed by the
# gas-dome floats' median value, so there's some (partial, not full)
# circularity. It's still the best independent depth-coverage check we have.
# =============================================================================

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

sites <- c("AM", "GB", "ID", "LF")
outdir <- "04_Outputs/Power Function RC"

# ---- 1. gas-dome floats (judgment-trimmed, same exclusions as M1/M6/M7) ---
valid <- read_csv(file.path(outdir, "raw_valid_k600.csv"), show_col_types = FALSE)
judgment_drop <- tribble(
  ~ID, ~row,
  "AM", 24, "AM", 5, "GB", 16, "ID", 6, "LF", 5
)
dome_trimmed <- valid %>% anti_join(judgment_drop, by = c("ID", "row"))

# ---- 2. one-station's independent K600 estimates, joined to depth ---------
depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station_k600 <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  select(ID, date, K600_one = K600) %>%
  left_join(depth_daily, by = c("ID", "date")) %>%
  filter(!is.na(K600_one), !is.na(depth))

# ---- 3. fit: power decline (gas-dome only) below max sampled depth,       --
#            flat at one-station's median K600 beyond it -------------------
fit_hybrid <- function(site) {
  dome <- dome_trimmed %>% filter(ID == site)
  bp <- max(dome$depth)  # max gas-dome sampled depth, same principle as M7

  m <- lm(log(k600_1.day) ~ log(depth), data = dome)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])

  beyond <- one_station_k600 %>% filter(ID == site, depth > bp)
  flatval_calibrated <- median(beyond$K600_one, na.rm = TRUE)
  flatval_uncalibrated <- a * bp^b  # what M7 would have used, for comparison

  list(a = a, b = b, bp = bp,
       flatval = flatval_calibrated,
       flatval_uncalibrated = flatval_uncalibrated,
       n_beyond = nrow(beyond))
}

fits <- map(sites, fit_hybrid)
names(fits) <- sites

cat("=== M8 hybrid-calibrated fits (vs M7's uncalibrated flat value) ===\n")
print(map_dfr(sites, function(s) with(fits[[s]], tibble(
  ID = s, a, b, bp, flatval_calibrated = flatval, flatval_uncalibrated, n_days_beyond_bp = n_beyond))))

# ---- 4. apply to full depth series (same daily-max aggregation as before) -
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

applied <- depth %>%
  rowwise() %>%
  mutate(
    a = fits[[ID]]$a, b = fits[[ID]]$b, bp = fits[[ID]]$bp, flatval = fits[[ID]]$flatval,
    k600_1d = if_else(depth <= bp, a * depth^b, flatval)
  ) %>%
  ungroup()

daily_k600 <- applied %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(K600_1.d_daily = max(k600_1d, na.rm = TRUE), .groups = "drop") %>%
  mutate(K600_1.d_daily = na_if(K600_1.d_daily, -Inf),
         Date = ymd_hms(paste(Date, "00:00:00")))

write_csv(daily_k600, file.path(outdir, "K600_M8_hybrid_calibrated.csv"))
cat("\nwrote", file.path(outdir, "K600_M8_hybrid_calibrated.csv"), "\n")

# ---- 5. rerun two-station mass balance (self-contained copy of the calc, --
#         not sourced from 03_run_two_station_mass_balance.R) --------------
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area <- left_join(width, length_tbl, by = "ID") %>%
  mutate(area = w * m) %>%
  mutate(m = if_else(ID == "AM", 800, m))

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())))
VentDO <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE)

k600_for_join <- daily_k600 %>% mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))
master <- reduce(c(base_data, list(k600_for_join)), full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID")
master <- suppressWarnings(full_join(master, VentDO, by = c("ID", "Date"))) %>%
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

two_station_m8 <- left_join(day.parse, NEP, by = c("date", "ID")) %>%
  filter(GPP <= 34, ER >= -34)

write_csv(two_station_m8, file.path(outdir, "two.station.results_M8_hybrid_calibrated.csv"))
cat("wrote", file.path(outdir, "two.station.results_M8_hybrid_calibrated.csv"),
    " n_days=", nrow(two_station_m8 %>% distinct(ID, date)), "\n")

# ---- 6. QC-based fallback combine (same rule as script 07) ----------------
one_station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  distinct(ID, date, .keep_all = TRUE) %>%
  select(ID, date, GPP1 = GPP, ER1 = ER)

two_m8 <- two_station_m8 %>% distinct(ID, date, .keep_all = TRUE) %>% select(ID, date, GPP2 = GPP, ER2 = ER)

combined_m8 <- depth_daily %>%
  left_join(two_m8, by = c("ID", "date")) %>%
  left_join(one_station, by = c("ID", "date")) %>%
  mutate(
    source = case_when(
      !is.na(GPP2) & !is.na(ER2) ~ "two-station",
      !is.na(GPP1) & !is.na(ER1) ~ "one-station",
      TRUE ~ NA_character_
    ),
    GPP = coalesce(GPP2, GPP1),
    ER = coalesce(ER2, ER1)
  ) %>%
  filter(!is.na(source)) %>%
  select(ID, date, depth, source, GPP, ER)

write_csv(combined_m8, file.path(outdir, "combined_metabolism_M8_hybrid_calibrated.csv"))

# ---- 7. discontinuity score, same definition as script 04 -----------------
one_for_boundary <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>% rename(ER1 = ER, GPP1 = GPP) %>%
  left_join(depth_daily, by = c("ID", "date")) %>% filter(!is.na(depth)) %>%
  select(ID, date, depth, GPP1, ER1)

two_for_boundary <- two_station_m8 %>% distinct(ID, date, GPP, ER) %>%
  left_join(depth_daily, by = c("ID", "date")) %>% filter(!is.na(depth)) %>%
  select(ID, date, depth, GPP2 = GPP, ER2 = ER)

boundary <- two_for_boundary %>% group_by(ID) %>% summarise(boundary_depth = max(depth), .groups = "drop")

discontinuity_m8 <- map_dfr(sites, function(s) {
  b <- boundary %>% filter(ID == s) %>% pull(boundary_depth)
  two_near <- two_for_boundary %>% filter(ID == s, depth <= b) %>% arrange(desc(depth)) %>% slice_head(n = 15)
  one_near <- one_for_boundary %>% filter(ID == s, depth >= b) %>% arrange(depth) %>% slice_head(n = 15)
  tibble(ID = s, method = "M8_hybrid_calibrated", boundary_depth = b,
         ER_two_near = mean(two_near$ER2, na.rm = TRUE), ER_one_near = mean(one_near$ER1, na.rm = TRUE),
         GPP_two_near = mean(two_near$GPP2, na.rm = TRUE), GPP_one_near = mean(one_near$GPP1, na.rm = TRUE))
}) %>% mutate(ER_jump = abs(ER_two_near - ER_one_near), GPP_jump = abs(GPP_two_near - GPP_one_near))

cat("\n=== M8 discontinuity score, compare to M6/M7 from coalescence_discontinuity_by_method.csv ===\n")
print(discontinuity_m8 %>% select(ID, ER_jump, GPP_jump))

old <- read_csv(file.path(outdir, "coalescence_discontinuity_by_method.csv"), show_col_types = FALSE) %>%
  select(ID, method, ER_jump, GPP_jump)
comparison_table <- bind_rows(old, discontinuity_m8 %>% select(ID, method, ER_jump, GPP_jump)) %>% arrange(ID, ER_jump)
write_csv(comparison_table, file.path(outdir, "coalescence_discontinuity_with_M8.csv"))
print(comparison_table, n = 20)

# ---- 8. plot: full combined ER/GPP vs depth, M8 -----------------------------
plot_data_m8 <- combined_m8 %>% mutate(method_label = "M8 hybrid (calibrated by one-station)") %>%
  pivot_longer(c(GPP, ER), names_to = "flux", values_to = "value")

for (s in sites) {
  p <- plot_data_m8 %>% filter(ID == s) %>%
    ggplot(aes(x = depth, y = value, color = source)) +
    geom_point(size = 1.1, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "grey70") +
    facet_wrap(~flux, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c("two-station" = "#1b7837", "one-station" = "#762a83")) +
    labs(title = paste0(s, ": M8 hybrid-calibrated RC, combined with QC fallback"),
         subtitle = "Flat value beyond gas-dome sampled range set from one-station's own K600, not extrapolated",
         x = "depth (m)", y = NULL, color = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(outdir, paste0("figures/12_M8_", s, ".png")), p, width = 7, height = 6.5, dpi = 150)
}

cat("\nDone. New files (nothing else touched):\n",
    "- K600_M8_hybrid_calibrated.csv\n",
    "- two.station.results_M8_hybrid_calibrated.csv\n",
    "- combined_metabolism_M8_hybrid_calibrated.csv\n",
    "- coalescence_discontinuity_with_M8.csv\n",
    "- figures/12_M8_<ID>.png\n")
