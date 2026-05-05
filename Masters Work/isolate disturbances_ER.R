source("03_Scripts/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
ER <- read_csv("04_Outputs/master.metabolism.csv") %>%
  select(Date, ID, ER) %>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv") %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(ID, Date) %>%
      summarise(depth = mean(depth, na.rm = TRUE))
  )

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods & quality filters -----------------------------------
ER_flagged <- ER %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(ER)) %>%
  mutate(
    date = Date,
    ER   = if_else(Date > "2023-08-01" & ID == 'AM' & flood == 3, NA, ER)
  )

# --- Baseline ---------------------------------------------------------------
ER.base <- baseline(ER_flagged, ER)

# --- Local loess (span = 0.65) ----------------------------------------------
fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.65, min_rows = 5) {
  y_name <- rlang::as_name(rlang::enquo(y_var))
  x_name <- rlang::as_name(rlang::enquo(x_var))
  g_name <- rlang::as_name(rlang::enquo(group_var))

  split_list <- split(df, df[[g_name]])

  lapply(split_list, function(.x) {
    complete_cases <- complete.cases(.x[[y_name]], .x[[x_name]])
    .x_clean       <- .x[complete_cases, ]
    if (nrow(.x_clean) < min_rows) {
      message("Skip group with only ", nrow(.x_clean), " complete cases (min: ", min_rows, ")")
      return(NULL)
    }
    fit <- loess(.x_clean[[y_name]] ~ .x_clean[[x_name]], span = span)
    .x %>%
      mutate(!!paste0(y_name, "_loess") := predict(fit, newdata = .x[[x_name]]))
  }) %>%
    compact() %>%
    bind_rows()
}

# --- Smooth -----------------------------------------------------------------
ER.smooth <- smooth(
  ER_flagged %>% fill(flood, .direction = "down"),
  ER
) %>%
  left_join(ER.base)

# Check: smooth fit
ER.smooth %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = Date, y = ER)) +
  geom_point(color = 'grey60', size = 0.5) +
  geom_line(aes(y = ER_loess), color = 'blue') +
  geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "ER: smooth check (OS)")

# --- prep.count.max ---------------------------------------------------------
ER.clean <- prep.count.max(ER.smooth, ER)

# Check: prep.count.max fit
ER.clean %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = count, y = ER_loess)) +
  geom_point() +
  geom_point(aes(y = ER), color = 'blue') +
  geom_line(aes(y = base)) +
  geom_smooth(aes(x = count, y = ER, group = flood.stage), method = 'lm', se = FALSE) +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "ER: prep.count.max check (OS)")

# --- Recovery, maximum, duration --------------------------------------------
recovery_days <- ER.clean %>%
  distinct(ID, flood, first_recovery, last_recovery) %>%
  mutate(recovery_days = first_recovery - last_recovery)

ER.max      <- maximum(ER.clean, ER)
ER.duration <- duration(ER.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(ER.clean, ER.base, ER, base.ER)
rise.lm      <- fit_rise(ER.clean,       ER.base, ER, base.ER)

# Check: recession.lm fit
ER.clean %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = count, y = ER, color = flood.stage)) +
  geom_point(size = 0.5) +
  geom_point(aes(y = ER_loess), color = 'blue', alpha = 0.4) +
  geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
  geom_smooth(aes(x = count, y = ER, group = flood.stage),
              method = 'lm', se = FALSE, color = 'darkgreen') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "ER: recession.lm check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.ER <-
  full_join(recession.lm, ER.duration) %>%
  full_join(rise.lm,       by = c('ID', 'flood')) %>%
  full_join(ER.max,        by = c('ID', 'flood')) %>%
  full_join(ER.base,       by = c('ID', 'flood')) %>%
  full_join(recovery_days, by = c('ID', 'flood')) %>%
  mutate(variable = 'ER')

write_csv(flood.impacts.ER, "04_Outputs/flood impacts/ER.csv")
