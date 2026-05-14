
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

master<- read_csv("02_Clean_data/master_chem1.csv")

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
# Peaks are located at the center of the 3-day rolling-mean window with the
# most extreme value, reducing sensitivity to point-in-time noise. Window size
# is auto-detected: k=72 for hourly data, k=3 for daily data.

.roll_k <- function(dates) {
  dt_h <- as.numeric(median(diff(as.POSIXct(dates)), na.rm = TRUE)) / 3600
  as.integer(max(1L, round(72 / dt_h)))
}

count.min <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      .k       = .roll_k(Date),
      roll_avg = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                fill = NA, align = 'center', na.rm = TRUE),
      roll_avg = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      peak_row = which.min(replace(roll_avg, is.na(roll_avg), Inf)),
      count    = row_number() - peak_row
    ) %>%
    select(-.k, -roll_avg, -peak_row) %>%
    ungroup()
}

count.max <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      .k       = .roll_k(Date),
      roll_avg = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                fill = NA, align = 'center', na.rm = TRUE),
      roll_avg = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      peak_row = which.max(replace(roll_avg, is.na(roll_avg), -Inf)),
      count    = row_number() - peak_row
    ) %>%
    select(-.k, -roll_avg, -peak_row) %>%
    ungroup()
}

# Double-peak variants — anchor count=0 at the last local min/max of the
# 3-day rolling mean (second peak).
count.min.double <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      .k        = .roll_k(Date),
      roll_avg  = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                 fill = NA, align = 'center', na.rm = TRUE),
      roll_avg  = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      d         = c(NA_real_, diff(roll_avg)),
      d_next    = c(tail(d, -1), NA_real_),
      local_min = !is.na(d) & !is.na(d_next) & d < 0 & d_next > 0,
      peak_row  = suppressWarnings(max(which(local_min))),
      peak_row  = if_else(is.finite(peak_row), peak_row,
                          which.min(replace(roll_avg, is.na(roll_avg), Inf))),
      count     = row_number() - peak_row
    ) %>%
    select(-.k, -roll_avg, -d, -d_next, -local_min, -peak_row) %>%
    ungroup()
}

count.max.double <- function(trim, variable) {
  trim %>%
    group_by(ID, flood) %>%
    arrange(Date) %>%
    mutate(
      .k        = .roll_k(Date),
      roll_avg  = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                 fill = NA, align = 'center', na.rm = TRUE),
      roll_avg  = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      d         = c(NA_real_, diff(roll_avg)),
      d_next    = c(tail(d, -1), NA_real_),
      local_max = !is.na(d) & !is.na(d_next) & d > 0 & d_next < 0,
      peak_row  = suppressWarnings(max(which(local_max))),
      peak_row  = if_else(is.finite(peak_row), peak_row,
                          which.max(replace(roll_avg, is.na(roll_avg), -Inf))),
      count     = row_number() - peak_row
    ) %>%
    select(-.k, -roll_avg, -d, -d_next, -local_max, -peak_row) %>%
    ungroup()
}

# --- Summary helpers ---------------------------------------------------------

minimum <- function(df, variable) {
  df %>%
    arrange(Date) %>%
    group_by(ID, flood) %>%
    mutate(
      .k       = .roll_k(Date),
      roll_avg = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                fill = NA, align = 'center', na.rm = TRUE),
      roll_avg = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      peak_row = which.min(replace(roll_avg, is.na(roll_avg), Inf))
    ) %>%
    filter(row_number() == peak_row) %>%
    select(Date, ID, flood, roll_avg) %>%
    rename(minimum = roll_avg, peak.Date = Date) %>%
    ungroup()
}

maximum <- function(df, variable) {
  df %>%
    arrange(Date) %>%
    group_by(ID, flood) %>%
    mutate(
      .k       = .roll_k(Date),
      roll_avg = zoo::rollapply({{variable}}, width = .k[1], FUN = mean,
                                fill = NA, align = 'center', na.rm = TRUE),
      roll_avg = if (all(is.na(roll_avg))) {{variable}} else roll_avg,
      peak_row = which.max(replace(roll_avg, is.na(roll_avg), -Inf))
    ) %>%
    filter(row_number() == peak_row) %>%
    select(Date, ID, flood, roll_avg) %>%
    rename(maximum = roll_avg, peak.Date = Date) %>%
    ungroup()
}

duration <- function(dates) {
  dates %>%
    mutate(duration = as.numeric(flood.end - flood.start))
}

# --- Consecutive-run helper --------------------------------------------------

first_of_three <- function(cond) {
  cond <- coalesce(as.logical(cond), FALSE)
  cond &
    lead(cond, 1, default = FALSE) &
    lead(cond, 2, default = FALSE)
}

# --- Flood date extraction ---------------------------------------------------

flood_dates <- function(df, variable, direction = c('min', 'max')) {

  direction <- match.arg(direction)

  count.df <- if (direction == 'min') count.min(df, {{variable}}) else count.max(df, {{variable}})

  df_aug <- count.df %>%
    filter(!is.na(flood)) %>%
    group_by(ID, flood) %>%
    mutate(
      stage.flood = if_else(count >= 0, 'post', 'pre'),
      .wb         = {{variable}} / base,
      .threshold  = if (direction == 'min')
                      if_else(any(.wb < 0.8, na.rm = TRUE), 0.8, 1.0)
                    else
                      if_else(any(.wb > 1.2, na.rm = TRUE), 1.2, 1.0)
    ) %>%
    ungroup()

  if (direction == 'min') {

    flood.start <- df_aug %>%
      filter(stage.flood == 'pre') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb < .threshold)) %>%
      filter(crit) %>%
      summarise(flood.start = min(as.Date(Date)), .groups = 'drop')

    flood.end <- df_aug %>%
      filter(stage.flood == 'post') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb >= .threshold)) %>%
      summarise(
        flood.end = if (any(crit, na.rm = TRUE))
                      min(as.Date(Date[crit]), na.rm = TRUE)
                    else
                      max(as.Date(Date)) + 1L,
        .groups = 'drop'
      )

  } else {

    flood.start <- df_aug %>%
      filter(stage.flood == 'pre') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb > .threshold)) %>%
      filter(crit) %>%
      summarise(flood.start = min(as.Date(Date)), .groups = 'drop')

    flood.end <- df_aug %>%
      filter(stage.flood == 'post') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb <= .threshold)) %>%
      summarise(
        flood.end = if (any(crit, na.rm = TRUE))
                      min(as.Date(Date[crit]), na.rm = TRUE)
                    else
                      max(as.Date(Date)) + 1L,
        .groups = 'drop'
      )
  }

  full_join(flood.start, flood.end, by = c('ID', 'flood'))
}

# --- Trim smooth data to flood window ----------------------------------------

trim_to_flood_dates <- function(df, dates) {
  df %>%
    left_join(select(dates, ID, flood, flood.start, flood.end),
              by = c('ID', 'flood')) %>%
    filter(!is.na(flood.start), !is.na(flood.end),
           as.Date(Date) > flood.start,
           as.Date(Date) < flood.end) %>%
    select(-flood.start, -flood.end)
}


plot_flood_dates <- function(df, variable, dates) {

  df_plot <- df %>%
    filter(!is.na(flood)) %>%
    left_join(dates, by = c('ID', 'flood')) %>%
    mutate(date = as.Date(Date))

  ggplot(df_plot, aes(x = date)) +
    geom_point(aes(y = {{variable}}), color = 'grey50', size = 0.5) +
    geom_line(aes(y = base), color = 'red', linetype = 'dashed', linewidth = 0.6) +
    geom_vline(aes(xintercept = flood.start), color = 'steelblue',
               linetype = 'dashed', linewidth = 0.7) +
    geom_vline(aes(xintercept = flood.end), color = 'darkorange',
               linetype = 'dashed', linewidth = 0.7) +
    facet_wrap(~ ID + flood, scales = 'free') +
    labs(
      x        = 'Date',
      y        = rlang::as_name(rlang::enquo(variable)),
      title    = 'Flood date extraction check',
      subtitle = 'Blue = flood.start | Orange = flood.end | Red = baseline'
    ) +
    theme_bw(base_size = 10)
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
