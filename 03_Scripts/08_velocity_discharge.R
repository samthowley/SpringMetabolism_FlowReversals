rm(list=ls())
library(tidyverse)
library(readxl)
library(measurements)

## ============================================================================
## 08 -- velocity rating curve and discharge, for BOTH metabolism scripts
##
## Created 2026-10-06. Previously 09_one_station.R read a discharge.csv that no
## surviving script produced (its old producer, velocity_discharge_K600_RC.R,
## was deleted), while 10_two_station.R computed velocity and discharge
## privately and never saved them. Two copies of the same quantity, one of them
## unreproducible -- the same pattern that caused the K600 depth bug.
##
## This script is now the single place velocity and discharge are defined.
## The rating curve below is lifted verbatim from 10_two_station.R so the two
## cannot drift; that script now reads this file instead of refitting.
##
##   discharge = w * depth * velocity * 86400     (m3/day, as before)
##
## Writes 02_Clean_data/Chem/discharge.csv  (Date, ID, discharge)  <- 09 reads
##        04_Outputs/velocity_discharge.csv (Date, ID, depth, velocity, discharge)
##                                                                 <- 10 reads
## Negative discharge is kept: at LF and ID it is a real flow reversal, and
## 09_one_station.R does its own filtering.
## ============================================================================

sites <- c("AM", "GB", "ID", "LF", "OS")

## ---- CONFIG: must match 10_two_station.R --------------------------------
VEL_FORM <- "power"    # "power" | "linear"   -- velocity ~ depth
EXCL     <- "strict"   # "base"  | "strict"   -- point-exclusion severity
UNITS    <- "county"   # AM/LF/OS converted ft/s->m/s, GB/ID raw (county-gauge evidence)

recipe <- tibble(ID = sites) %>%
  mutate(units = UNITS, vel_form = VEL_FORM, vel_excl = EXCL)

## ---- inputs -----------------------------------------------------------------
width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>%
  select(ID, w)

depth <- read_csv("02_Clean_data/Chem/depth.csv", col_types = cols(ID = col_character())) %>%
  filter(ID %in% sites)

depth_daily <- depth %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

u_raw <- read_csv("01_Raw_data/u.csv", show_col_types = FALSE) %>%
  mutate(Date = mdy(Date)) %>%
  select(Date, ID, velocity = u) %>%
  filter(ID %in% sites, !is.na(velocity), velocity > 0)

unit_lists <- list(
  all_conv = c('AM', 'GB', 'ID', 'LF', 'OS'),
  county   = c('AM', 'LF', 'OS')          # GB/ID stay raw
)

## LF flow-reversal anchors: velocity goes to 0 at high depth (backflooding).
## Dropped automatically under a power fit (can't log velocity = 0).
lf_flow_reversals <- tribble(
  ~ID,  ~depth, ~velocity,
  "LF",  1.75,   0,
  "LF",  1.90,   0,
  "LF",  2.10,   0
) %>%
  left_join(width, by = "ID") %>%
  mutate(Date = as.Date(NA)) %>%
  select(Date, ID, w, depth, velocity)

## ==================== velocity rating curve, per site ========================
make_vel_cal <- function(site, units, excl) {
  base <- u_raw %>%
    filter(ID == site) %>%
    mutate(velocity = if_else(ID %in% unit_lists[[units]],
                              conv_unit(velocity, 'ft_per_sec', 'm_per_sec'), velocity)) %>%
    left_join(width, by = "ID") %>%
    left_join(depth_daily, by = c("ID", "Date" = "day")) %>%
    filter(!is.na(depth), depth > 0) %>%
    select(Date, ID, w, depth, velocity) %>%
    bind_rows(lf_flow_reversals %>% filter(ID == site)) %>%
    mutate(
      velocity = if_else(ID == 'LF' & velocity > 0.06, NA, velocity),   # base trims
      velocity = if_else(ID == 'OS' & velocity > 0.1,  NA, velocity)
    )
  if (excl == "strict") {
    base <- base %>%
      mutate(
        velocity = if_else(ID == 'AM' & velocity > 0.15, NA, velocity),
        velocity = if_else(ID == 'GB' & velocity > 0.3,  NA, velocity)
      )
  }
  base %>% filter(!is.na(velocity))
}

velocity_RC <- map_dfr(seq_len(nrow(recipe)), function(i) {
  r   <- recipe[i, ]
  cal <- make_vel_cal(r$ID, r$units, r$vel_excl)
  d   <- depth %>% filter(ID == r$ID)
  if (r$vel_form == "linear") {
    m <- lm(velocity ~ depth, data = cal)
    d <- d %>% mutate(velocity = pmax(coef(m)[[1]] + coef(m)[[2]] * depth, 0))   # floored at 0
  } else {
    m <- lm(log(velocity) ~ log(depth), data = cal %>% filter(velocity > 0))
    d <- d %>% mutate(velocity = exp(coef(m)[[1]]) * depth^unname(coef(m)[[2]]))
  }
  d %>% select(Date, ID, depth, velocity)
})

## ==================== discharge ==============================================
velocity_discharge <- velocity_RC %>%
  left_join(width, by = "ID") %>%
  mutate(discharge = w * depth * velocity * 86400) %>%     # m3/day
  select(Date, ID, depth, velocity, discharge)

write_csv(velocity_discharge, "04_Outputs/velocity_discharge.csv")
write_csv(velocity_discharge %>% select(Date, ID, discharge),
          "02_Clean_data/Chem/discharge.csv")

## ==================== check ==================================================
cat("\n== velocity ~ depth fits used ==\n")
walk(sites, function(s) {
  cal <- make_vel_cal(s, UNITS, EXCL) %>% filter(velocity > 0)
  m   <- lm(log(velocity) ~ log(depth), data = cal)
  cat(sprintf("  %s  n=%2d   velocity = %.4f * depth^%+.3f   (R2 %.2f)\n",
              s, nrow(cal), exp(coef(m)[[1]]), coef(m)[[2]], summary(m)$r.squared))
})

cat("\n== discharge written (m3/day) ==\n")
print(as.data.frame(velocity_discharge %>%
  summarise(n = n(),
            median = round(median(discharge, na.rm = TRUE)),
            min    = round(min(discharge, na.rm = TRUE)),
            max    = round(max(discharge, na.rm = TRUE)),
            .by = ID) %>% arrange(ID)), row.names = FALSE)

velocity_discharge %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  ggplot(aes(depth, velocity)) +
  geom_line(colour = "#d95f02", linewidth = 0.8) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = paste0("Velocity rating curve used by scripts 09 and 10 (",
                      VEL_FORM, ", ", EXCL, " exclusions)"),
       x = "depth (m)", y = "velocity (m/s)") +
  theme_bw(base_size = 10)
