source("03_Scripts/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
DO  <- read_csv("02_Clean_data/Chem/DO.csv")
h   <- read_csv("02_Clean_data/Chem/depth.csv")
SpC <- read_csv("02_Clean_data/Chem/SpC.csv")

DO <- full_join(DO, h) %>%
  filter(!is.na(Date), !is.na(DO)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(ID, date) %>%
  mutate(DO.daily.min = min(DO, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(SpC)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date)

# Exploratory: raw DO by site
DO %>%
  ggplot(aes(x = Date, y = DO)) +
  geom_point(size = 0.3) +
  facet_wrap(~ID, scales = 'free')

# --- Baseline ---------------------------------------------------------------
DO.base <- baseline(DO_flagged, DO.daily.min)

# --- Local loess (span = 0.6) -----------------------------------------------
fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.2, min_rows = 5) {
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
DO.smooth <- smooth(
  DO_flagged %>% fill(flood, .direction = "down"),
  DO.daily.min) %>%
  rename(DO_loess = DO.daily.min_loess) %>%
  left_join(DO.base)

# Check: smooth fit
DO.smooth %>%
  filter(ID == 'GB') %>%
  ggplot(aes(x = Date, y = DO.daily.min)) +
  geom_point(color = 'grey60', size = 0.3) +
  geom_line(aes(y = DO_loess), color = 'blue') +
  geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: smooth check (OS)", y = "DO.daily.min")

# --- prep.count.min ---------------------------------------------------------
DO.clean <- prep.count.min(DO.smooth, DO.daily.min)

# Check: prep.count.min fit
DO.clean %>%
  filter(ID == 'GB', !is.na(flood)) %>%
  ggplot(aes(x = count, y = DO_loess)) +
  geom_point(aes(color='red')) +
  geom_point(aes(y = DO.daily.min), color = 'blue') +
  geom_line(aes(y = base)) +
  geom_smooth(aes(x = count, y = DO.daily.min, group = flood.stage), method = 'lm', se = FALSE) +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: prep.count.min check (OS)")

# --- Recovery, minimum, duration --------------------------------------------
recovery_days <- DO.clean %>%
  distinct(ID, flood, first_recovery, last_recovery) %>%
  mutate(recovery_days = as.numeric(first_recovery - last_recovery, units = "days"))

DO.min      <- minimum(DO.clean, DO.daily.min)
DO.duration <- duration(DO.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(DO.clean, DO.base, DO.daily.min, base.DO)
rise.lm      <- fit_rise(DO.clean,       DO.base, DO.daily.min, base.DO)

# Check: recession.lm fit
DO.clean %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = count, y = DO.daily.min, color = flood.stage)) +
  geom_point(size = 0.5) +
  geom_point(aes(y = DO_loess), color = 'blue', alpha = 0.4) +
  geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
  geom_smooth(aes(x = count, y = DO.daily.min, group = flood.stage),
              method = 'lm', se = FALSE, color = 'darkgreen') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: recession.lm check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.DO <-
  full_join(recession.lm, DO.duration) %>%
  full_join(rise.lm,       by = c('ID', 'flood')) %>%
  full_join(DO.min,        by = c('ID', 'flood')) %>%
  full_join(DO.base,       by = c('ID', 'flood')) %>%
  full_join(recovery_days, by = c('ID', 'flood')) %>%
  mutate(variable = 'DO')

write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO.csv")

# --- FR classification (requires depth.csv flood classes) -------------------
class <- read_csv("04_Outputs/flood impacts/depth.csv") %>%
  select(ID, flood, class)

FR.class <- DO.clean %>%
  left_join(class, by = c('ID', 'flood')) %>%
  left_join(SpC) %>%
  arrange(ID, Date) %>%
  fill(SpC, .direction = 'down') %>%
  filter(count > -7 * 24, count < 7 * 24) %>%   # assumes hourly data; adjust multiplier if needed
  mutate(
    class = if_else(class == 'RR' & SpC < 200 & DO > 4, "FR", class),
    class = if_else(class == 'RR', "BO", class)
  ) %>%
  group_by(ID, flood) %>%
  mutate(
    max_height = which.max(replace(DO, is.na(DO), -Inf)),
    minimum    = case_when(row_number() == max_height ~ 0)
  ) %>%
  filter(minimum == 0) %>%
  select(ID, flood, class)

unique(FR.class$class)
write_csv(FR.class, "04_Outputs/FR.class.csv")
