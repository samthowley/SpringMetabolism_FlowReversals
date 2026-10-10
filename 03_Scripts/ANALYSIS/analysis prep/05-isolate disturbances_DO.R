source("03_Scripts/ANALYSIS/00-disturbance isolation functions hourly.R")


# --- Data loading -----------------------------------------------------------
DO  <- master%>% dplyr::select(Date, ID, DO, depth)%>%
  filter(!is.na(Date), !is.na(DO)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(ID, date) %>%
  mutate(
    DO.daily.min = min(DO, na.rm = TRUE),
    )

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  dplyr::select(-start, -end) %>%
  arrange(ID, Date) 

# --- Baseline and minimum ---------------------------------------------------------------
DO.base <- baseline(DO_flagged, DO) %>%
  mutate(base = if_else(ID == 'OS', 4.5, base))

DO.min<- minimum(DO_flagged, DO)

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
  left_join(DO.base)

# --- Isolate disturbance ----------------------------------------------------
DO.clean <- prep.min.both(DO.smooth, DO, DO_loess)#%>%
  #mutate(DO=if_else(ID=='GB' & flood==2 & count>1500, NA, DO))

site="OS"

plot_grid(
  DO.clean %>%
  filter(ID == site, !is.na(flood)) %>%
  ggplot(aes(x = count, y = DO_loess)) +
  geom_point(aes(y = DO), color = 'gray60') +
  geom_point(aes(color = 'red')) +
  geom_line(aes(y = base)) +
  geom_smooth(aes(x = count, y = DO, group = stage.flood), method = 'lm', se = FALSE) +
  facet_wrap(~flood, scales = 'free'),


# Check: clean fit
  DO.smooth %>%
    filter(ID == site, !is.na(flood)) %>%
    ggplot(aes(x = Date, y = DO)) +
    geom_point(color = 'grey60', size = 0.3) +
    geom_line(aes(y = DO_loess), color = 'blue') +
    geom_line(aes(y = base), color = 'red', linetype = 'dashed') +
    facet_wrap(~flood, scales = 'free'),
ncol=2
)

 # --- Flood bounds -----------------------------------------------------------
flood.bounds<-flood_dates(DO.smooth, DO_loess, direction='min')
#plot_flood_dates(DO.smooth, DO_loess, flood.bounds)

DO.duration <- duration(flood.bounds)


DO.cleaner<-DO.clean%>%
  filter(!(ID == "ID" & flood == 4))%>%
  filter(!(ID == "ID" & flood == 6 & count>160))%>%
  filter(!(ID == "IU" & flood == 2))


  
# --- Recession & rise models ------------------------------------------------
recession.lm <- fit_recessions(DO.cleaner, DO.base, DO.daily.min, base.DO)
rise.lm      <- fit_rise(DO.cleaner,       DO.base, DO.daily.min, base.DO)

# Check: recession fit
# DO.clean %>%
#   filter(ID == 'ID', count>0) %>%
#   ggplot(aes(x = count, y = DO, color = stage.flood)) +
#   geom_point(size = 1, alpha=0.5) +
#   geom_line(aes(y = DO_loess), color = 'blue', alpha = 0.4) +
#   geom_line(aes(y = base, color = NULL), color = 'red', linetype = 'dashed') +
#   geom_smooth(aes(x = count, y = DO.daily.min, group = stage.flood),
#               method = 'lm', se = FALSE, color = 'darkgreen') +
#   facet_wrap(~flood, scales = 'free') +theme_minimal()
# 
# --- Compile outputs --------------------------------------------------------
flood.impacts.DO <-
  full_join(recession.lm, DO.duration) %>%
  full_join(rise.lm,  by = c('ID', 'flood')) %>%
  full_join(DO.min,   by = c('ID', 'flood')) %>%
  full_join(DO.base,  by = c('ID', 'flood')) %>%
  mutate(variable = 'DO')

write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO.csv")


flood.bounds.join<-flood.bounds%>%mutate(keep='Y')

DO_trimmed <- DO.smooth %>%
  left_join(
    flood.bounds.join, by = join_by(ID, flood, between(Date, flood.start, flood.end)))%>%
  filter(keep=='Y') %>%
  dplyr::select(-keep, -flood.start, -flood.end)%>%
  mutate(variable='DO')%>%
  rename(conc=DO, loess=DO_loess)%>%
  dplyr::select(Date, ID, flood, conc, loess, base, variable)

write_csv(DO_trimmed, "04_Outputs/flood impacts/DO.flood.df.csv")


# --- Flood classification (FR / BO / HI) ------------------------------------

SpC <- read_csv("02_Clean_data/Chem/SpC.csv")
pH <- read_csv("02_Clean_data/Chem/pH.csv")


flood.class.dates <- DO.clean %>%
  filter(!is.na(flood)) %>%
  dplyr::select(Date, ID, flood, DO, count) %>%
  left_join(
    SpC, by = c("Date", "ID")
  ) %>%
  left_join(
    pH, by = c("Date", "ID")
  )%>%
  group_by(ID, flood) %>%
  #filter(count> -7*24 & count<7*24)%>%
  mutate(
    class.raw= case_when(
      DO>5 & SpC<200 ~'FR',
      DO<3   ~'BO'),
    has_FR = any(class.raw=='FR', na.rm = TRUE),
    has_BO = any(class.raw=='BO', na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(
    class = case_when(
      has_FR ~ "FR",
      has_BO ~ "BO",
      TRUE   ~ "HI"
    ),
    class=if_else(ID=='AM' & flood==6, 'FR', class),
    class=if_else(ID=='LF' & flood==6, 'FR', class),
    class=if_else(ID=='LF' & class=='BO', 'HI', class)
    
  ) #%>%
  

write_csv(flood.class.dates, "04_Outputs/flood impacts/peak dates.csv")

flood.class<-flood.class.dates%>%
  filter(count==0)%>%dplyr::select(ID, flood, class)%>%distinct()

write_csv(flood.class, "04_Outputs/flood impacts/FR_class.csv")
