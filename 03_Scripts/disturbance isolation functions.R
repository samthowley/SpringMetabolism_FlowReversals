
library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(mmand)
library(weathermetrics)
library(lme4)
library(zoo)
library(strucchange)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Smoothing ---------------------------------------------------------------

smooth <- function(flagged, variable) {
  prep <- flagged %>%
    filter(!is.na({{variable}})) %>%
    arrange(flood, Date) %>%
    group_by(flood, ID) %>%
    mutate(
      t        = as.numeric(Date - min(Date)),
      group_ID = paste(ID, flood, sep = "_")
    ) %>%
    ungroup()

  fit_loess_by_group(prep, {{variable}}, t, group_ID)
}

# --- Baseline ----------------------------------------------------------------

baseline <- function(flagged, variable) {

  base_tbl <- flagged %>%
    mutate(flooded = case_when(is.na(flood) ~ "n", TRUE ~ "y")) %>%
    group_by(ID) %>%
    fill(flood, .direction = "updown") %>%
    mutate(group_ID = paste0(ID, "_", flood)) %>%
    filter(flooded == "n") %>%
    group_by(flood, ID) %>%
    summarise(base_1 = mean({{variable}}, na.rm = TRUE), .groups = "drop") %>%
    arrange(ID, flood)

  depth_i <- flagged %>%
    fill(flood, .direction = "downup") %>%
    group_by(ID) %>%
    mutate(
      depth25 = quantile(depth, 0.25, na.rm = TRUE),
      depth75 = quantile(depth, 0.75, na.rm = TRUE),
      depth_i = case_when(
        depth < depth25 ~ "low",
        depth > depth75 ~ "high",
        TRUE            ~ "normal"
      )
    ) %>%
    group_by(ID, depth_i, flood) %>%
    summarise(base_i = mean({{variable}}, na.rm = TRUE), .groups = "drop") %>%
    filter(depth_i == "low")

  full_join(base_tbl, depth_i, by = c("ID", "flood")) %>%
    mutate(base = (base_1 + base_i) / 2) %>%
    select(flood, ID, base) %>%
    fill(base, .direction = "downup") %>%
    group_by(flood, ID) %>%
    mutate(base = if_else(is.na(base), mean(base, na.rm = TRUE), base))
}

# --- Peak counters -----------------------------------------------------------

count.min <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      var_clean  = replace({{variable}}, is.na({{variable}}), Inf),
      min_val    = min(var_clean, na.rm = TRUE),
      max_height = max(which(var_clean == min_val)),
      count      = row_number() - max_height
    ) %>%
    select(-var_clean, -min_val) %>%
    ungroup()
}

count.max <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      var_clean  = replace({{variable}}, is.na({{variable}}), Inf),
      max_val    = max(var_clean, na.rm = TRUE),
      max_height = max(which(var_clean == max_val)),
      count      = row_number() - max_height
    ) %>%
    select(-var_clean, -max_val) %>%
    ungroup()
}

# Double-peak variants — anchor count=0 at last local min/max (second peak).
count.min.double <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      var_clean = replace({{variable}}, is.na({{variable}}), Inf),
      d         = c(NA_real_, diff(var_clean)),
      d_next    = c(tail(d, -1), NA_real_),
      local_min = !is.na(d) & !is.na(d_next) & d < 0 & d_next > 0,
      peak_row  = suppressWarnings(max(which(local_min))),
      peak_row  = if_else(is.finite(peak_row), peak_row, max(which(var_clean == min(var_clean)))),
      count     = row_number() - peak_row
    ) %>%
    select(-var_clean, -d, -d_next, -local_min, -peak_row) %>%
    ungroup()
}

count.max.double <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      var_clean = replace({{variable}}, is.na({{variable}}), -Inf),
      d         = c(NA_real_, diff(var_clean)),
      d_next    = c(tail(d, -1), NA_real_),
      local_max = !is.na(d) & !is.na(d_next) & d > 0 & d_next < 0,
      peak_row  = suppressWarnings(max(which(local_max))),
      peak_row  = if_else(is.finite(peak_row), peak_row, max(which(var_clean == max(var_clean)))),
      count     = row_number() - peak_row
    ) %>%
    select(-var_clean, -d, -d_next, -local_max, -peak_row) %>%
    ungroup()
}

# --- Summary helpers ---------------------------------------------------------

minimum <- function(df, variable) {
  df %>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.min(replace({{variable}}, is.na({{variable}}), -Inf)),
      minimum    = case_when(row_number() == max_height ~ 0)
    ) %>%
    filter(minimum == 0) %>%
    select(Date, ID, flood, {{variable}}) %>%
    rename(
      minimum = {{variable}},
      peak.Date=Date)
}

maximum <- function(df, variable) {
  df %>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.max(replace({{variable}}, is.na({{variable}}), -Inf)),
      maximum    = case_when(row_number() == max_height ~ 0)
    ) %>%
    filter(maximum == 0) %>%
    select(Date, ID, flood, {{variable}}) %>%
    rename(
      maximum = {{variable}},
      peak.Date=Date)
}

duration <- function(df) {
  df %>%
    filter(!is.na(flood)) %>%
    group_by(ID, flood) %>%
    mutate(duration = n_distinct(date)) %>%
    summarise(duration = max(duration))
}

# --- Regression helpers ------------------------------------------------------

fit_recessions <- function(trim, base, variable, base.var) {
  prep <- trim %>%
    filter(!is.na(flood), count > 0) %>%
    mutate(group_ID = paste0(ID, "_", flood))

  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)

  coef(rC) %>%
    as_tibble() %>%
    mutate(ID = names(rC), r2 = sapply(rC, function(m) if (!is.null(m)) summary(m)$r.squared else NA_real_)) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood")) %>%
    rename(recess.intercept = Intercept, recess.slope = slope, r2.recess = r2) %>%
    select(-base)
}

fit_rise <- function(trim, base, variable, base.var) {
  prep <- trim %>%
    filter(!is.na(flood), count < 0) %>%
    mutate(group_ID = paste0(ID, "_", flood))

  formula_str <- paste(as_label(enquo(variable)), "~ count | group_ID")
  rC <- lmList(as.formula(formula_str), data = prep)

  coef(rC) %>%
    as_tibble() %>%
    mutate(ID = names(rC), r2 = sapply(rC, function(m) if (!is.null(m)) summary(m)$r.squared else NA_real_)) %>%
    rename(Intercept = "(Intercept)", slope = "count") %>%
    separate(ID, into = c("ID", "flood"), sep = "_", convert = TRUE) %>%
    left_join(base, by = c("ID", "flood")) %>%
    rename(rise.intercept = Intercept, rise.slope = slope, r2.rise = r2) %>%
    select(-base)
}
