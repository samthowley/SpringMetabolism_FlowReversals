source("03_Scripts/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
CO2 <- read_csv("02_Clean_data/Chem/CO2.csv")
h   <- read_csv("02_Clean_data/Chem/depth.csv")

co2 <- full_join(CO2, h)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
CO2_flagged <- co2 %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  mutate(
     date = as.Date(Date),
    # CO2  = if_else(ID == 'AM' & CO2 < 2000,              NA_real_, CO2),
    # CO2  = if_else(ID == 'AM' & Date < '2022-07-30',      NA_real_, CO2),
    # CO2  = if_else(ID == 'AM' & flood == 4 & CO2 > 12700, NA_real_, CO2),
    # CO2  = if_else(ID == 'LF' & flood == 3 & CO2 > 2560,  NA_real_, CO2)
  ) %>%
  filter(!is.na(CO2)) %>%
  group_by(ID, date) %>%
  mutate(CO2.daily.max = max(CO2, na.rm = TRUE)) %>%
  ungroup()

# --- Baseline ---------------------------------------------------------------
CO2.base <- baseline(CO2_flagged, CO2.daily.max)%>%
  mutate(base = if_else(ID == 'GB' & base<5000, NA, base))%>%
  group_by(ID) %>%
  mutate(base = if_else(is.na(base), mean(base, na.rm = TRUE), base))

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
CO2_flagged<-CO2_flagged %>% fill(flood, .direction = "down")%>% filter(!is.na(CO2), !is.na(Date))
CO2.smooth <- smooth(
  CO2_flagged,
  CO2) %>%
  left_join(CO2.base)

# Check: smooth fit
# CO2.smooth %>%
#   filter(ID == 'AM') %>%
#   ggplot(aes(x = Date, y = CO2.daily.max)) +
#   geom_point(color = 'grey60', size = 0.3) +
#   geom_line(aes(y = CO2_loess), color = 'blue') +
#   geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
#   facet_wrap(~flood, scales = 'free') +
#   labs(title = "CO2: smooth check (OS)", y = "CO2.daily.max")

# --- Isolate disturbance (CO2 increases during floods) ----------------------
CO2.clean <- prep.max.both(CO2.smooth, CO2.daily.max, CO2_loess)

# Check: clean fit

plot_grid(
  CO2.smooth %>%
    filter(ID =='OS') %>%
    ggplot(aes(x = Date, y = CO2)) +
    geom_point(color = 'grey60', size = 0.3) +
    geom_point(aes(y = CO2_loess), color = 'blue') +
    geom_point(aes(y = base), color = 'red', linetype = 'dashed') +
    facet_wrap(~flood, scales = 'free') 
  ,
CO2.clean %>%
  filter(ID =='OS', !is.na(flood)) %>%
  ggplot(aes(x = Date, y = CO2_loess)) +
  geom_point(aes(color = 'red')) +
  geom_point(aes(y = CO2), color = 'grey60') +
  geom_line(aes(y = base)) +
  #geom_smooth(aes(x = count, y = CO2.daily.max, group = stage.flood), method = 'lm', se = FALSE) +
  facet_wrap(~flood, scales = 'free') +
  theme_minimal()
)

# --- Flood bounds -----------------------------------------------------------
flood.start <- CO2.clean %>% group_by(ID, flood) %>% summarise(start = min(as.Date(Date)), .groups = 'drop')
flood.end   <- CO2.clean %>% group_by(ID, flood) %>% summarise(end   = max(as.Date(Date)), .groups = 'drop')
flood.bounds <- left_join(flood.start, flood.end, by = c('ID', 'flood'))

# --- Maximum, duration ------------------------------------------------------
CO2.max      <- maximum(CO2.clean, CO2.daily.max)
CO2.duration <- duration(CO2.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(CO2.clean, CO2.base, CO2, base.CO2)
rise.lm      <- fit_rise(CO2.clean,       CO2.base, CO2, base.CO2)

# Check: recession fit
CO2.clean %>%
  filter(ID == 'OS', count>0) %>%
  ggplot(aes(x = count, y = CO2, color = stage.flood)) +
  geom_point(size = 0.5) +
  geom_point(aes(y = CO2_loess), color = 'blue', alpha = 0.4) +
  geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
  geom_smooth(aes(x = count, y = CO2.daily.max, group = stage.flood),
              method = 'lm', se = FALSE, color = 'darkgreen') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "CO2: recession check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.CO2 <-
  full_join(recession.lm, CO2.duration) %>%
  full_join(rise.lm,      by = c('ID', 'flood')) %>%
  full_join(CO2.max,      by = c('ID', 'flood')) %>%
  full_join(CO2.base,     by = c('ID', 'flood')) %>%
  full_join(flood.bounds, by = c('ID', 'flood')) %>%
  mutate(variable = 'CO2')

write_csv(flood.impacts.CO2, "04_Outputs/flood impacts/CO2.csv")
