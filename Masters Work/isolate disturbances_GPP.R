source("03_Scripts/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
GPP <- read_csv("04_Outputs/master.metabolism.csv") %>%
  select(Date, ID, GPP) %>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv") %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(ID, Date) %>%
      summarise(depth = mean(depth, na.rm = TRUE), .groups = 'drop')
  )

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
GPP_flagged <- GPP %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(GPP)) %>%
  mutate(date = as.Date(Date))

# --- Baseline ---------------------------------------------------------------
GPP.base <- baseline(GPP_flagged, GPP)

# --- Local loess (span = 0.3) -----------------------------------------------
fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.3, min_rows = 5) {
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
    .x %>% mutate(!!paste0(y_name, "_loess") := predict(fit, newdata = .x[[x_name]]))
  }) %>% compact() %>% bind_rows()
}

# --- Smooth -----------------------------------------------------------------
GPP.smooth <- smooth(
  GPP_flagged %>% fill(flood, .direction = "down"),
  GPP) %>%
  left_join(GPP.base)

# --- Isolate disturbance ----------------------------------------------------
GPP.clean <- prep.for.slope.min(GPP.smooth, GPP, GPP_loess)

# --- Flood bounds -----------------------------------------------------------
flood.start <- GPP.clean %>% group_by(ID, flood) %>% summarise(start = min(as.Date(Date)), .groups = 'drop')
flood.end   <- GPP.clean %>% group_by(ID, flood) %>% summarise(end   = max(as.Date(Date)), .groups = 'drop')
flood.bounds <- left_join(flood.start, flood.end, by = c('ID', 'flood'))

# --- Minimum, duration ------------------------------------------------------
GPP.min      <- minimum(GPP.clean, GPP)
GPP.duration <- duration(GPP.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(GPP.clean, GPP.base, GPP, base.GPP)
rise.lm      <- fit_rise(GPP.clean,       GPP.base, GPP, base.GPP)

# --- Compile outputs --------------------------------------------------------
flood.impacts.GPP <-
  full_join(recession.lm, GPP.duration) %>%
  full_join(rise.lm,      by = c('ID', 'flood')) %>%
  full_join(GPP.min,      by = c('ID', 'flood')) %>%
  full_join(GPP.base,     by = c('ID', 'flood')) %>%
  full_join(flood.bounds, by = c('ID', 'flood')) %>%
  mutate(variable = 'GPP')

write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP.csv")
