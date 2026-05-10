source("03_Scripts/disturbance isolation functions daily.R")
source("03_Scripts/disturbance isolation functions.R")


# --- Data loading -----------------------------------------------------------
ER <- read_csv("04_Outputs/master.metabolism.csv") %>%
  select(Date, ID, ER) %>%
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
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(ER))%>%
  mutate(
         ER=if_else(ID=='ID'& flood == 2 & ER>25, NA_real_, ER)
         )
# --- Baseline ---------------------------------------------------------------
ER.base <- baseline(ER_flagged, ER)

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
ER.smooth <- smooth(
  ER_flagged %>% fill(flood, .direction = "down") %>% filter(!is.na(ER)),
  ER) %>%
  left_join(ER.base)

# Check: smooth fit
# ER.smooth %>%
#   filter(ID == 'OS') %>%
#   ggplot(aes(x = Date, y = ER)) +
#   geom_point(color = 'grey60', size = 0.3) +
#   geom_line(aes(y = ER_loess), color = 'blue') +
#   geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
#   facet_wrap(~flood, scales = 'free') +
#   labs(title = "ER: smooth check (OS)", y = "ER (abs)")

# --- Isolate disturbance (|ER| increases during floods) ---------------------
ER.clean <- rbind(
  
prep.max.both.daily(
  ER.smooth%>%filter(ID!='AM'), 
  ER, ER, gap_days = 14)
,
prep.max.both.daily(
  ER.smooth%>%filter(ID=='AM'), 
  ER_loess, ER_loess, gap_days = 14)%>%
  group_by(ID, flood)%>%
  mutate(
    ER=if_else(ID=='AM' & count>40 & flood==5, NA, ER)
    )
)

# Check: clean fit####
ER.clean %>%
  filter(ID == 'AM', !is.na(flood)) %>%
  ggplot(aes(x = count, y = ER_loess)) +
  geom_point(color = 'red') +
  geom_point(aes(y = ER), color = 'blue') +
  geom_line(aes(y = base)) +
  facet_wrap(~flood, scales = 'free') 


#LF
plot_grid(
ER.clean %>%
  filter(ID == 'LF', !is.na(flood)) %>%
  ggplot(aes(x = Date, y = ER_loess)) +
  geom_point(color = 'red') +
  geom_point(aes(y = ER), color = 'blue') +
  geom_line(aes(y = base)) +
  facet_wrap(~flood, scales = 'free') 
,
  ER.smooth %>%
    filter(ID == 'AM') %>%
    ggplot(aes(x = Date, y = ER)) +
    geom_point(color = 'grey60', size = 0.3) +
    geom_line(aes(y = ER_loess), color = 'blue') +
    geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
    facet_wrap(~flood, scales = 'free') ,
ncol=1
)

# --- Flood bounds -----------------------------------------------------------
flood.start <- ER.clean %>% group_by(ID, flood) %>% summarise(start = min(as.Date(Date)), .groups = 'drop')
flood.end   <- ER.clean %>% group_by(ID, flood) %>% summarise(end   = max(as.Date(Date)), .groups = 'drop')
flood.bounds <- left_join(flood.start, flood.end, by = c('ID', 'flood'))

# --- Maximum, duration ------------------------------------------------------
ER.max      <- maximum(ER.clean, ER)
ER.duration <- duration(ER.clean)

# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(ER.clean, ER.base, ER, base.ER)
rise.lm      <- fit_rise(ER.clean,       ER.base, ER, base.ER)

# Check: recession fit
ER.clean %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = count, y = ER, color = stage.flood)) +
  geom_point() +
  geom_point(aes(y = ER_loess), color = 'blue', alpha = 0.4) +
  geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
  geom_smooth(aes(x = count, y = ER, group = stage.flood),
              method = 'lm', se = FALSE, color = 'darkgreen') +
  facet_wrap(~flood, scales = 'free') 

# --- Compile outputs --------------------------------------------------------
flood.impacts.ER <-
  full_join(recession.lm, ER.duration) %>%
  full_join(rise.lm,      by = c('ID', 'flood')) %>%
  full_join(ER.max,       by = c('ID', 'flood')) %>%
  full_join(ER.base,      by = c('ID', 'flood')) %>%
  full_join(flood.bounds, by = c('ID', 'flood')) %>%
  mutate(variable = 'ER')

write_csv(flood.impacts.ER, "04_Outputs/flood impacts/ER.csv")
