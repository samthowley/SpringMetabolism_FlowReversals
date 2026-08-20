rm(list=ls())
library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(streamMetabolizer)
library(readxl)

# AM-only: refreshes discharge (w*depth*velocity*86400 from CURRENT depth.csv/
# velocity.csv, not the stale 02_Clean_data/Chem/discharge.csv -- see
# [[am_lf_er_investigation]] -- discharge.csv predates a later depth.csv
# revision and was never regenerated, but one station.R reads it directly,
# both for the hi/lo discharge threshold split and for the K600~Q binning
# node centers), then reruns just AM's one-station Bayesian fit with it.
# Self-contained, doesn't touch the live one station.R or its outputs.

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx",sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area <- left_join(width, length_tbl) %>% mutate(area=w*m) %>% mutate(m=if_else(ID=='AM', 800, m))

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names <- file.names[c(2,4,6,12)]  # depth, DO, K600, velocity
data <- lapply(file.names, function(x) read_csv(x, col_types = cols(ID = col_character())))
master <- reduce(data, full_join, by = c("ID","Date")) %>% left_join(area)

discharge_fresh <- master %>% filter(ID == "AM") %>%
  mutate(discharge = w*depth*velocity*86400) %>%
  select(Date, ID, depth, DO, Temp, discharge, K600_1.d_daily)

cat("=== Fresh AM discharge vs. old stale discharge.csv ===\n")
old <- read_csv("02_Clean_data/Chem/discharge.csv", show_col_types = FALSE) %>% filter(ID=="AM")
cat("Fresh median:", median(discharge_fresh$discharge, na.rm=TRUE),
    " | Old (stale) median:", median(old$discharge, na.rm=TRUE), "\n")

## ---- one station.R's AM prep, using fresh discharge in place of the stale file ----
df_tail <- discharge_fresh %>%
  mutate(discharge = if_else(discharge<=0, 0.01, discharge),
         threshold = if_else(discharge >= mean(discharge, na.rm=TRUE), 'hi', 'lo'),
         ID_q = paste(ID, threshold, sep="_"))

lat.lon <- data.frame(ID='AM', lat=30.155, lon=-83.238)

input <- df_tail %>% left_join(lat.lon, by="ID") %>%
  rename(DO.obs = DO) %>%
  mutate(temp.water = fahrenheit.to.celsius(Temp),
         DO.sat = Cs(temp.water),
         solar.time = as.POSIXct(Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
         light = calc_light(solar.time, lat, lon))

split_list <- input %>% group_by(ID_q) %>% group_split()
names(split_list) <- input %>% group_by(ID_q) %>% group_keys() %>% pull(ID_q)

rdy_for_sm <- lapply(split_list, function(df) {
  samplingperiod <- data.frame(solar.time = seq(from=as.POSIXct(min(df$solar.time)),
                                                 to=as.POSIXct(max(df$solar.time)), by="hour"))
  df <- left_join(samplingperiod, df, by="solar.time") %>% arrange(solar.time) %>%
    filter(c(TRUE, diff(as.numeric(solar.time)) > 0)) %>%
    select(solar.time, light, depth, discharge, DO.sat, DO.obs, temp.water) %>%
    distinct(solar.time, .keep_all = TRUE)
  df
})

# rC_K600_edited.xlsx (the dated, manually-trimmed gas-dome file the live
# one station.R expects) doesn't exist -- same gap flagged in
# [[project_overview]]. Falling back to the same single-prior approach the
# live script already uses for OS (pool_K600='normal', median K600 from the
# judgment-trimmed gas-dome floats in raw_valid_k600.csv) instead of the
# discharge-binned hi/lo prior, since that needs per-date floats we don't have.
raw_valid <- read_csv("04_Outputs/Power Function RC/raw_valid_k600.csv", show_col_types = FALSE)
judgment_drop <- tribble(~ID, ~row, "AM", 24, "AM", 5)
am_k600_median <- raw_valid %>% filter(ID=="AM") %>% anti_join(judgment_drop, by=c("ID","row")) %>%
  pull(k600_1.day) %>% median(na.rm=TRUE)
cat("\nAM judgment-trimmed gas-dome median K600 (prior):", am_k600_median, "\n")

am_input <- rdy_for_sm[["AM_hi"]] %>% bind_rows(rdy_for_sm[["AM_lo"]]) %>%
  distinct(solar.time, .keep_all=TRUE) %>% arrange(solar.time)

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs_am <- specs(bayes_name,
                         K600_daily_meanlog_meanlog = log(am_k600_median),
                         K600_daily_meanlog_sdlog = log(2),
                         GPP_daily_lower = 0,
                         burnin_steps = 1000, saved_steps = 1000)

cat("\n=== Fitting AM (this is the slow MCMC step) ===\n")
mm <- metab(bayes_specs_am, data = am_input %>% select(-discharge))
met_results_am <- mm@fit$daily %>% mutate(ID = "AM") %>%
  select(date, ID, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ER_Rhat, K600_daily_Rhat)

met_results_am_qc <- met_results_am %>%
  filter(GPP_daily_mean>0, ER_daily_mean<0, ER_Rhat>0.9 & ER_Rhat<1.2, K600_daily_Rhat>0.9 & K600_daily_Rhat<1.2)

cat("\n=== AM one-station refit (fresh discharge) vs. original AM one-station output ===\n")
orig <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types=FALSE) %>% filter(ID=="AM")
cat("Original AM: median GPP =", median(orig$GPP, na.rm=TRUE), " median ER =", median(orig$ER, na.rm=TRUE), " n =", nrow(orig), "\n")
cat("Refit AM (fresh discharge): median GPP =", median(met_results_am_qc$GPP_daily_mean, na.rm=TRUE),
    " median ER =", median(met_results_am_qc$ER_daily_mean, na.rm=TRUE), " n =", nrow(met_results_am_qc), "\n")

write_csv(met_results_am_qc, "04_Outputs/Power Function RC/24_am_onestation_refit_fresh_discharge.csv")
cat("\nDone -> 24_am_onestation_refit_fresh_discharge.csv\n")
