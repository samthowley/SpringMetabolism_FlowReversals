source("03_Scripts/ANALYSIS/disturbance isolation functions daily.R")

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
  mutate(date = as.Date(Date),
         GPP=if_else(ID=='LF' & flood==2 & GPP<1.2, NA_real_, GPP)
         )

# --- Baseline ---------------------------------------------------------------
GPP.base <- baseline(GPP_flagged, GPP)

GPP.min <- minimum(GPP_flagged, GPP)

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


GPP.smooth <- smooth(
  GPP_flagged %>% fill(flood, .direction = "down"),
  GPP) %>%
  left_join(GPP.base)

# --- Isolate disturbance ----------------------------------------------------
GPP.clean <- prep.min.both.daily(GPP.smooth, GPP_loess, GPP, 14)
# 
# GPP.clean %>%
#   filter(ID == 'OS', !is.na(flood)) %>%
#   ggplot(aes(x = count, y = GPP_loess)) +
#   geom_point(color = 'red') +
#   geom_point(aes(y = GPP), color = 'blue') +
#   geom_line(aes(y = base)) +
#   geom_smooth(aes(x = count, y = GPP, group = stage.flood), method = 'lm', se = FALSE) +
#   facet_wrap(~flood, scales = 'free') 
# 
# 
# GPP.smooth %>%
#   filter(ID == 'GB') %>%
#   ggplot(aes(x = Date, y = GPP)) +
#   geom_point(color = 'grey60', size = 0.3) +
#   geom_line(aes(y = GPP_loess), color = 'blue') +
#   geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
#   facet_wrap(~flood, scales = 'free') 


# --- Flood bounds -----------------------------------------------------------

#flood_dates <- function(df, variable)
GPP.dates<-flood_dates(GPP.smooth, GPP_loess, direction="min")
#plot_flood_dates(GPP.smooth, GPP_loess, GPP.dates)

# --- Minimum, duration ------------------------------------------------------
GPP.duration <- duration(GPP.dates)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(GPP.clean, GPP.base, GPP, base.GPP)
rise.lm      <- fit_rise(GPP.clean,       GPP.base, GPP, base.GPP)

# Check: recession fit
# GPP.clean %>%
#   filter(ID == 'ID') %>%
#   ggplot(aes(x = count, y = GPP, color = stage.flood)) +
#   geom_point(size = 0.5) +
#   geom_point(aes(y = GPP_loess), color = 'blue', alpha = 0.4) +
#   geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
#   geom_smooth(aes(x = count, y = GPP, group = stage.flood),
#               method = 'lm', se = FALSE, color = 'darkgreen') +
#   facet_wrap(~flood, scales = 'free') +
#   labs(title = "GPP: recession check (OS)")

# --- Compile outputs --------------------------------------------------------
flood.impacts.GPP <-
  full_join(recession.lm, GPP.duration) %>%
  full_join(rise.lm,  by = c('ID', 'flood')) %>%
  full_join(GPP.min,  by = c('ID', 'flood')) %>%
  full_join(GPP.base, by = c('ID', 'flood')) %>%
  mutate(variable = 'GPP')

write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP.csv")



flood.bounds.join<-flood.bounds%>%mutate(keep='Y')

GPP_trimmed <- GPP.smooth %>%
  left_join(
    flood.bounds.join, by = join_by(ID, flood, between(Date, flood.start, flood.end)))%>%
  filter(keep=='Y') %>%
  select(-keep, -flood.start, -flood.end)%>%
  mutate(variable='GPP')%>%
  rename(conc=GPP, loess=GPP_loess)%>%
  select(Date, ID, flood, conc, loess, base, variable)

write_csv(GPP_trimmed, "04_Outputs/flood impacts/GPP.flood.df.csv")
