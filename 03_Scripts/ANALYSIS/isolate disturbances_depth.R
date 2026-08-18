source("03_Scripts/ANALYSIS/disturbance isolation functions hourly.R")

# --- Data loading -----------------------------------------------------------
h <- master%>%select(Date, ID, depth)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
depth_flagged <- h %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  mutate(date = as.Date(Date)) %>%
  filter(!is.na(depth)) %>%
  group_by(ID, date) %>%
  mutate(depth.daily.max = max(depth, na.rm = TRUE)) %>%
  ungroup()

# --- Baseline ---------------------------------------------------------------
depth.base <- baseline(depth_flagged, depth)

depth.max <- maximum(depth_flagged, depth)

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
depth_flagged <- depth_flagged %>%
  fill(flood, .direction = "down") %>%
  filter(!is.na(depth), !is.na(Date))

depth.smooth <- smooth(depth_flagged, depth) %>%
  left_join(depth.base)

# Check: smooth fit
# depth.smooth %>%
#   filter(ID == 'OS', !is.na(flood)) %>%
#   ggplot(aes(x = Date, y = depth)) +
#   geom_point(color = 'grey60', size = 0.3) +
#   geom_line(aes(y = depth_loess), color = 'blue') +
#   geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
#   facet_wrap(~flood, scales = 'free')

# --- Isolate disturbance (depth increases during floods) --------------------
depth.clean <- prep.max.both(depth.smooth, depth, depth_loess)

# # Check: clean fit
# depth.clean %>%
#   filter(ID == 'LF', !is.na(flood)) %>%
#   ggplot(aes(x = count, y = depth_loess)) +
#   geom_point(color = 'red') +
#   geom_point(aes(y = depth), color = 'grey60') +
#   geom_line(aes(y = base)) +
#   facet_wrap(~flood, scales = 'free') +
#   theme_minimal()
# 
# # Check: smooth vs clean
# depth.smooth %>%
#   filter(ID == 'OS', !is.na(flood)) %>%
#   ggplot(aes(x = Date, y = depth)) +
#   geom_point(color = 'grey60', size = 0.3) +
#   geom_line(aes(y = depth_loess), color = 'blue') +
#   geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
#   facet_wrap(~flood, scales = 'free')

# --- Flood bounds -----------------------------------------------------------
flood.bounds <- flood_dates(depth.smooth, depth_loess, direction = 'max')
#plot_flood_dates(depth.smooth, depth_loess, flood.bounds)

# --- Maximum, duration ------------------------------------------------------
depth.duration <- duration(flood.bounds)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(depth.clean, depth.base, depth, base.depth)
rise.lm      <- fit_rise(depth.clean,       depth.base, depth, base.depth)

# Check: recession fit
# depth.clean %>%
#   filter(ID == 'OS') %>%
#   ggplot(aes(x = count, y = depth, color = stage.flood)) +
#   geom_point(size = 0.5) +
#   geom_point(aes(y = depth_loess), color = 'blue', alpha = 0.4) +
#   geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
#   geom_smooth(aes(x = count, y = depth, group = stage.flood),
#               method = 'lm', se = FALSE, color = 'darkgreen') +
#   facet_wrap(~flood, scales = 'free') +
#   labs(title = "depth: recession check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.depth <-
  full_join(recession.lm, depth.duration) %>%
  full_join(rise.lm,    by = c('ID', 'flood')) %>%
  full_join(depth.max,  by = c('ID', 'flood')) %>%
  full_join(depth.base, by = c('ID', 'flood')) %>%
  mutate(variable = 'depth')

write_csv(flood.impacts.depth, "04_Outputs/flood impacts/depth.csv")

flood.bounds.join<-flood.bounds%>%mutate(keep='Y')

depth_trimmed <- depth.smooth %>%
  left_join(
    flood.bounds.join, by = join_by(ID, flood, between(Date, flood.start, flood.end)))%>%
  filter(keep=='Y') %>%
  select(-keep, -flood.start, -flood.end)%>%
  mutate(variable='depth')%>%
  rename(conc=depth, loess=depth_loess)%>%
  select(Date, ID, flood, conc, loess, base, variable)

write_csv(depth_trimmed, "04_Outputs/flood impacts/depth.flood.df.csv")


