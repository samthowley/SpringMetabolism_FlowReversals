source("03_Scripts/ANALYSIS/00-disturbance isolation functions daily.R")

# Stand-alone: runs on its own (only the shared function files are sourced).
# |ER| decreases during these floods, so they are analysed like GPPmin (minimum,
# fall to the trough, recovery back). The floods where |ER| increases are in
# 04-isolate disturbances_ER.R, which keeps its own copy of this list.
er.decrease.floods <- tibble(
  ID    = c('ID', 'ID', 'ID', 'ID', 'LF'),
  flood = c(1, 2, 3, 4, 3)
)

# --- Data loading -----------------------------------------------------------
ER <- read_csv("04_Outputs/combined metabolism methods.csv") %>%
  dplyr::select(Date, ID, ER) %>%
  left_join(read_csv("02_Clean_data/Chem/depth.csv")) %>%
  mutate(
    Date = as.Date(Date),
    ER   = abs(ER)
  )

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
ER_flagged <- ER %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  dplyr::select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(ER))

# --- Baseline ---------------------------------------------------------------
# computed with every flood (the baseline uses the days between floods), then
# kept for the decreasing floods only
ER.base <- baseline(ER_flagged, ER) %>%
  semi_join(er.decrease.floods, by = c('ID', 'flood'))
ER.min  <- minimum(ER_flagged, ER) %>%
  semi_join(er.decrease.floods, by = c('ID', 'flood'))

# --- Smooth -----------------------------------------------------------------
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


ER.smooth <- smooth(
  ER_flagged %>% group_by(ID) %>% fill(flood, .direction = "down") %>% ungroup() %>%
    filter(!is.na(ER)),
  ER) %>%
  semi_join(er.decrease.floods, by = c('ID', 'flood')) %>%
  left_join(ER.base)

# --- Isolate disturbance (|ER| decreases during floods) ---------------------
ER.clean <- prep.min.both.daily(ER.smooth, ER_loess, ER)

#Check: clean fit####
site = 'ID'

plot_grid(
  ER.clean %>%
    filter(ID == site, !is.na(flood)) %>%
    ggplot(aes(x = count, y = ER_loess)) +
    geom_point(color = 'red') +
    geom_point(aes(y = ER), color = 'blue') +
    geom_line(aes(y = depth*15), color = 'black') +
    geom_line(aes(y = base)) +
    geom_vline(xintercept = 0, color = 'yellow') +
    facet_wrap(~flood, scales = 'free'),

  ER.smooth %>%
    filter(ID == site) %>%
    ggplot(aes(x = Date, y = ER)) +
    geom_point(color = 'grey60', size = 0.3) +
    geom_line(aes(y = ER_loess), color = 'blue') +
    geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
    geom_line(aes(y = depth*15), color = 'black') +
    facet_wrap(~flood, scales = 'free'),
  ncol = 1
)

# --- Flood bounds -----------------------------------------------------------
flood.bounds <- flood_dates(ER.smooth, ER_loess, direction = 'min')
#plot_flood_dates(ER.smooth, ER_loess, flood.bounds)

# --- Minimum, duration ------------------------------------------------------
ER.duration <- duration(flood.bounds)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(ER.clean, ER.base, ER, base.ER)
rise.lm      <- fit_rise(ER.clean,       ER.base, ER, base.ER)

# --- Compile outputs --------------------------------------------------------
flood.impacts.ER <-
  full_join(recession.lm, ER.duration) %>%
  full_join(rise.lm,  by = c('ID', 'flood')) %>%
  full_join(ER.min,   by = c('ID', 'flood')) %>%
  full_join(ER.base,  by = c('ID', 'flood')) %>%
  mutate(variable = 'ER')

write_csv(flood.impacts.ER, "04_Outputs/flood impacts/ERmin.csv")


flood.bounds.join <- flood.bounds %>% mutate(keep = 'Y')

ER_trimmed <- ER.smooth %>%
  left_join(
    flood.bounds.join, by = join_by(ID, flood, between(Date, flood.start, flood.end))) %>%
  filter(keep == 'Y') %>%
  dplyr::select(-keep, -flood.start, -flood.end) %>%
  mutate(variable = 'ER') %>%
  rename(conc = ER, loess = ER_loess) %>%
  dplyr::select(Date, ID, flood, conc, loess, base, variable)

write_csv(ER_trimmed, "04_Outputs/flood impacts/ERmin.flood.df.csv")
