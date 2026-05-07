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
  arrange(ID, Date) #%>%
  #mutate(DO = if_else(ID == 'OS' & Date > "2023-03-01" & flood == 4, NA_real_, DO))

# --- Baseline ---------------------------------------------------------------
DO.base <- baseline(DO_flagged, DO.daily.min) %>%
  mutate(base = if_else(ID == 'OS', 4.5, base))

# --- Local loess (span = 0.1) -----------------------------------------------
fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.1, min_rows = 5) {
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
DO.smooth <- smooth(
  DO_flagged %>% fill(flood, .direction = "down"),
  DO) %>%
  rename(DO_loess = DO.daily.min_loess) %>%
  left_join(DO.base)

# Check: smooth fit
DO.smooth %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = Date, y = DO.daily.min)) +
  geom_point(color = 'grey60', size = 0.3) +
  geom_line(aes(y = DO_loess), color = 'blue') +
  geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: smooth check (OS)", y = "DO.daily.min")

# --- Isolate disturbance ----------------------------------------------------
DO.clean <- prep.for.slope.min(DO.smooth, DO_loess, DO_loess)

# Check: clean fit
plot_grid(
  DO.smooth %>%
    filter(ID == 'LF') %>%
    ggplot(aes(x = Date, y = DO)) +
    geom_point(color = 'grey60', size = 0.3) +
    geom_line(aes(y = DO_loess), color = 'blue') +
    geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
    facet_wrap(~flood, scales = 'free') +
    labs(title = "DO: smooth check (OS)", y = "DO.daily.min")
  ,

DO.clean %>%
  filter(ID == 'LF', !is.na(flood)) %>%
  ggplot(aes(x = count, y = DO_loess)) +
  geom_point(aes(color = 'red')) +
  geom_point(aes(y = DO), color = 'gray60') +
  geom_line(aes(y = base)) +
  #geom_smooth(aes(x = count, y = DO.daily.min, group = stage.flood), method = 'lm', se = FALSE) +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: clean check (OS)")
)

# --- Flood bounds -----------------------------------------------------------
flood.start <- DO.clean %>% group_by(ID, flood) %>% summarise(start = min(as.Date(Date)), .groups = 'drop')
flood.end   <- DO.clean %>% group_by(ID, flood) %>% summarise(end   = max(as.Date(Date)), .groups = 'drop')
flood.bounds <- left_join(flood.start, flood.end, by = c('ID', 'flood'))

# --- Minimum, duration ------------------------------------------------------
DO.min      <- minimum(DO.clean, DO)
DO.duration <- duration(DO.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(DO.clean, DO.base, DO, base.DO)
rise.lm      <- fit_rise(DO.clean,       DO.base, DO, base.DO)

# Check: recession fit
DO.clean %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = count, y = DO.daily.min, color = stage.flood)) +
  geom_point(size = 0.5) +
  geom_point(aes(y = DO_loess), color = 'blue', alpha = 0.4) +
  geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
  geom_smooth(aes(x = count, y = DO.daily.min, group = stage.flood),
              method = 'lm', se = FALSE, color = 'darkgreen') +
  facet_wrap(~flood, scales = 'free') +
  labs(title = "DO: recession check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.DO <-
  full_join(recession.lm, DO.duration) %>%
  full_join(rise.lm,       by = c('ID', 'flood')) %>%
  full_join(DO.min,        by = c('ID', 'flood')) %>%
  full_join(DO.base,       by = c('ID', 'flood')) %>%
  full_join(flood.bounds,  by = c('ID', 'flood')) %>%
  mutate(variable = 'DO')

write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO.csv")

# --- FR classification ------------------------------------------------------
class <- read_csv("04_Outputs/flood impacts/depth.csv") %>%
  select(ID, flood, class)

FR.class <- DO.clean %>%
  left_join(class, by = c('ID', 'flood')) %>%
  left_join(SpC) %>%
  arrange(ID, Date) %>%
  fill(SpC, .direction = 'down') %>%
  filter(count > -7 * 24, count < 7 * 24) %>%
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
