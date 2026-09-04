rm(list=ls())
library(tidyverse)
library(readxl)
library(dataRetrieval)
library(measurements)

##data#############
sites <- c("AM", "GB", "ID", "LF", "OS")
outdir <- "04_Outputs"
cfs_to_m3day <- 0.0283168 * 86400

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% select(ID, w)
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

depth_daily <- depth %>% 
  mutate(Date = as.Date(Date)) %>% 
  group_by(ID, Date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

u <- read_csv("01_Raw_data/u.csv")%>%
  mutate(Date=mdy(Date))%>%
  select(Date, ID, u)%>%
  rename(velocity=u)

velocity <-u %>%
  filter(ID %in% sites, !is.na(velocity), velocity > 0) %>%
  left_join(width, by = "ID") %>%
  left_join(depth_daily) %>%
  filter(!is.na(depth), depth > 0) %>%
  select(Date, ID, w, depth, velocity)

velocity <- velocity %>%
  mutate(velocity = if_else(ID %in% c('AM','LF','OS', 'GB', 'ID'), conv_unit(velocity, 'ft_per_sec', 'm_per_sec'), velocity))


lf_flow_reversals <- tribble(
  ~ID,   ~depth, ~velocity,
  "LF",   1.75,   0,
  "LF",   1.90,   0,
  "LF",   2.10,   0
) %>%
  left_join(width, by = "ID") %>%
  mutate(Date = as.Date(NA)) %>%
  select(Date, ID, w, depth, velocity)

field_measurments <- bind_rows(velocity, lf_flow_reversals)%>%
  mutate(
    # velocity=if_else(ID=='AM' & velocity>0.15, NA, velocity),
    # velocity=if_else(ID=='GB' & velocity>0.3, NA, velocity),
    velocity=if_else(ID=='LF' & velocity>0.06, NA, velocity),
    ## OS: two shallow-depth points (0.19-0.21 m/s converted) sit 4-10x above
    ## the other 16 OS measurements; with them in, OS discharge runs 1.5x the
    ## county gauge median (02323200) and the fitted line hits zero at 1.5 m
    ## so high-depth months collapse to ~0 while the gauge shows 30-50k m3/day.
    ## Trimmed -> discharge lands at 0.84x county median, 100% in range.
    velocity=if_else(ID=='OS' & velocity>0.1, NA, velocity)
    )%>%
  filter(!is.na(velocity))

## ==================== 1. depth-velocity RC: linear, decreasing ====================

velocity_fits <- map(sites, function(s) lm(velocity ~ depth, data = field_measurments %>% filter(ID == s))) %>%
  set_names(sites)

velocity_RC <- depth %>%
  rowwise() %>%
  mutate(velocity = predict(velocity_fits[[ID]], newdata = data.frame(depth = depth))) %>%
  ungroup()%>%
  mutate(velocity=if_else(velocity<0, 0, velocity))


ggplot() +
  geom_line(data = velocity_RC, aes(x = depth, y = velocity), alpha = 0.7, color='red') +
  geom_point(data = field_measurments, aes(x = depth, y = velocity), alpha = 0.7) +
  facet_wrap(~ID, scales = "free") +
  labs(title = "Depth-velocity RC: linear, decreasing", x = "depth (m)", y = "velocity (m/s)") +
  theme_bw(base_size = 12)


write_csv(velocity_RC, "04_Outputs/velocity_RC.csv")


velocity_RC.edit<-velocity_RC%>%
  rename(velocity.RC=velocity)%>%
  mutate(Date=as.Date(Date))%>%
  summarise(velocity.RC=mean(velocity.RC, na.rm=T), .by=c(ID, Date))%>%
  select(Date, ID, velocity.RC)


field_measurments<-field_measurments%>%left_join(velocity_RC.edit, by=c('ID', 'Date'))%>%
  mutate(discharge=depth*w*velocity.RC*86400)

## ==================== 1b. depth-velocity RC: M8 -- plain power law, no breakpoint ====================

fit_velocity_M8 <- function(df) {
  df <- df %>% filter(velocity > 0) %>% arrange(depth)
  m <- lm(log(velocity) ~ log(depth), data = df)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  list(a = a, b = b)
}
velocity_fits_M8 <- map(sites, function(s) fit_velocity_M8(field_measurments %>% filter(ID == s))) %>%
  set_names(sites)

velocity_RC_power <- depth %>%
  rowwise() %>%
  mutate(velocity = with(velocity_fits_M8[[ID]], a * depth^b)) %>%
  ungroup()

ggplot() +
  geom_line(data = velocity_RC_power, aes(x = depth, y = velocity), alpha = 0.7, color = 'red') +
  geom_point(data = field_measurments, aes(x = depth, y = velocity), alpha = 0.7) +
  #scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(title = "Depth-velocity RC: M8, plain power law, no breakpoint", x = "depth (m, log)", y = "velocity (m/s, log)") +
  theme_bw(base_size = 12)

write_csv(velocity_RC_power, "04_Outputs/velocity_RC_power.csv")
range(velocity_RC_power$velocity, na.rm=T)
# ==================== 3a. clean rC_k600.xlsx ====================

k600_sheet_names <- excel_sheets("04_Outputs/rC_k600.xlsx")
k600_raw_dirty <- map_dfr(k600_sheet_names, function(s) read_excel("04_Outputs/rC_k600.xlsx", sheet = s))

k600_clean <- k600_raw_dirty %>%
  rename(Date=day)%>%
  select(Date, ID, rep, CO2, CO2_enviro, depth, k600_1.day) %>%  # drop stray ...10-...15 cols
  filter(!is.na(ID)) %>%                                                            # drop blank trailing rows
  filter(!is.na(k600_1.day)) 
 

k600s.raw <- k600_clean %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(velocity_RC, by = c('ID', 'Date')) %>%
  mutate(
    k600_1.day=ifelse(ID=='GB' & k600_1.day>20, NA, k600_1.day),
    #k600_1.day=ifelse(ID=='ID' & velocity>0.3, NA, k600_1.day),
    
    )%>%
  filter(k600_1.day<15, !is.na(k600_1.day))


ggplot() +
  geom_point(data = k600s.raw, aes(x = velocity, y = k600_1.day), alpha = 0.7) +
  scale_x_log10() + scale_y_log10() +
  geom_hline(yintercept = 15)+
  facet_wrap(~ID, scales = "free") +
  theme_bw(base_size = 12)


## ---- method 1: linear ----
k600_fits_linear <- map(sites, function(s) lm(k600_1.day ~ velocity, data = k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ---- method 2: M6, statistical breakpoint (power law below bp, flat above) ----
find_breakpoint_k600 <- function(df) {
  df <- df %>% arrange(velocity)
  cand <- unique(df$velocity)
  cand <- cand[cand > sort(df$velocity)[4] & cand < max(df$velocity)]
  map_dfr(cand, function(bp) {
    left <- df %>% filter(velocity <= bp); right <- df %>% filter(velocity > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600_1.day) ~ log(velocity), data = left)
    flatval <- exp(predict(m, newdata = data.frame(velocity = bp)))
    sse <- sum((left$k600_1.day - exp(predict(m)))^2) + sum((right$k600_1.day - flatval)^2)
    tibble(bp = bp, sse = sse)
  })
}

## velocity==0 rows (AM's linear velocity RC floors to exactly 0 at high depth
## -- see problem 1) break log(velocity) in the M6/M8 fits below, same as the
## LF flow-reversal points did in section 1b -- filtered out here for the
## same reason.
bp_search_k600 <- map_dfr(sites, function(s) find_breakpoint_k600(k600s.raw %>% filter(ID == s, velocity > 0)) %>%
                            mutate(ID = s))
best_bp_k600 <- bp_search_k600 %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()

fit_M6 <- function(df, bp) {
  left <- df %>% filter(velocity <= bp)
  m <- lm(log(k600_1.day) ~ log(velocity), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  lo <- min(df$velocity)
  list(a = a, b = b, bp = bp, lo = lo, flatval = a * bp^b, flatval_lo = a * lo^b)
}
k600_fits_M6 <- map(sites, function(s) fit_M6(k600s.raw %>% filter(ID == s, velocity > 0), best_bp_k600$bp[best_bp_k600$ID == s])) %>%
  set_names(sites)

## ---- method 3: M8, plain power law, no breakpoint ----

fit_M8 <- function(df) {
  m <- lm(log(k600_1.day) ~ log(velocity), data = df)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  lo <- min(df$velocity)
  list(a = a, b = b, lo = lo, flatval_lo = a * lo^b)
}
k600_fits_M8 <- map(sites, function(s) fit_M8(k600s.raw %>% filter(ID == s, velocity > 0))) %>%
  set_names(sites)

## ---- apply all three to the velocity RC ----
K600_RC <- velocity_RC %>%
  rowwise() %>%
  mutate(
    K600_linear = predict(k600_fits_linear[[ID]], newdata = data.frame(velocity = velocity)),
    K600_M6 = with(k600_fits_M6[[ID]], case_when(
      is.na(velocity) ~ NA_real_,
      velocity < lo ~ flatval_lo,
      velocity <= bp ~ a * velocity^b,
      TRUE ~ flatval
    )),
    K600_M8 = with(k600_fits_M8[[ID]], if_else(velocity < lo, flatval_lo, a * velocity^b))
  ) %>%
  ungroup()

K600_RC_long <- K600_RC %>%
  pivot_longer(c(K600_linear, K600_M6, K600_M8), names_to = "method", values_to = "K600_1.d_daily")


## ---- fit-quality plot: raw calibration points + smooth predicted curves,
##      evaluated over the CALIBRATION data's own velocity range ----

# curve_grid_k600 <- map_dfr(sites, function(s) {
#   # velocity > 0 -- same reason as the fits above: a velocity==0 row makes
#   # rng[1]=0, and log(0)=-Inf breaks seq() ("'from' must be a finite number")
#   rng <- range(k600s.raw$velocity[k600s.raw$ID == s & k600s.raw$velocity > 0], na.rm = TRUE)
#   tibble(ID = s, velocity = exp(seq(log(rng[1]), log(rng[2]), length.out = 300)))
# })
# 
# pred_k600 <- curve_grid_k600 %>%
#   rowwise() %>%
#   mutate(
#     K600_M6 = with(k600_fits_M6[[ID]], case_when(
#       velocity < lo ~ flatval_lo,
#       velocity <= bp ~ a * velocity^b,
#       TRUE ~ flatval
#     )),
#     K600_M8 = with(k600_fits_M8[[ID]], if_else(velocity < lo, flatval_lo, a * velocity^b))
#   ) %>%
#   ungroup() %>%
#   pivot_longer(c(K600_M6, K600_M8), names_to = "method", values_to = "K600_1.d_daily")
# 
# bp_lines_k600 <- best_bp_k600 %>% transmute(ID, bp)

# ggplot() +
#   geom_point(data = k600s.raw, aes(x = velocity, y = k600_1.day), alpha = 0.7) +
#   geom_line(data = pred_k600, aes(x = velocity, y = K600_1.d_daily, color = method), linewidth = 0.8) +
#   geom_vline(data = bp_lines_k600, aes(xintercept = bp), linetype = "dashed", color = "grey40") +
#   scale_x_log10() + scale_y_log10() +
#   facet_wrap(~ID, scales = "free") +
#   labs(title = "K600-velocity RC: M6 (breakpoint) vs. M8 (power), fit over calibration range",
#        subtitle = "Dashed = M6 breakpoint velocity",
#        x = "velocity (m/s, log)", y = "K600 (1/day, log)") +
#   theme_bw(base_size = 12)


write_csv(K600_RC_long, "04_Outputs/K600_RC_velocity.csv")

## ==================== 3c. K600-depth RC (for comparison with K600-Q) ====================

## ---- method 1: linear ----
k600_fits_linear_depth <- map(sites, function(s) lm(k600_1.day ~ depth.x, data = k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ---- method 2: M6, statistical breakpoint (power law below bp, flat above) ----
find_breakpoint_k600_depth <- function(df) {
  df <- df %>% arrange(depth.x)
  cand <- unique(df$depth.x)
  cand <- cand[cand > sort(df$depth.x)[4] & cand < max(df$depth.x)]
  map_dfr(cand, function(bp) {
    left <- df %>% filter(depth.x <= bp); right <- df %>% filter(depth.x > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600_1.day) ~ log(depth.x), data = left)
    flatval <- exp(predict(m, newdata = data.frame(depth.x = bp)))
    sse <- sum((left$k600_1.day - exp(predict(m)))^2) + sum((right$k600_1.day - flatval)^2)
    tibble(bp = bp, sse = sse)
  })
}
bp_search_k600_depth <- map_dfr(sites, function(s) find_breakpoint_k600_depth(k600s.raw %>% filter(ID == s)) %>% mutate(ID = s))
best_bp_k600_depth <- bp_search_k600_depth %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()

fit_M6_depth <- function(df, bp) {
  left <- df %>% filter(depth.x <= bp)
  m <- lm(log(k600_1.day) ~ log(depth.x), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  lo <- min(df$depth.x)
  list(a = a, b = b, bp = bp, lo = lo, flatval = a * bp^b, flatval_lo = a * lo^b)
}
k600_fits_M6_depth <- map(sites, function(s) fit_M6_depth(k600s.raw %>% filter(ID == s), best_bp_k600_depth$bp[best_bp_k600_depth$ID == s])) %>%
  set_names(sites)

## ---- method 3: M8, plain power law, no breakpoint ----
fit_M8_depth <- function(df) {
  m <- lm(log(k600_1.day) ~ log(depth.x), data = df)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  lo <- min(df$depth.x)
  list(a = a, b = b, lo = lo, flatval_lo = a * lo^b)
}
k600_fits_M8_depth <- map(sites, function(s) fit_M8_depth(k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)


K600_RC_depth <- depth %>%
  rowwise() %>%
  mutate(
    K600_linear = predict(k600_fits_linear_depth[[ID]], newdata = data.frame(depth.x = depth)),
    K600_M6 = with(k600_fits_M6_depth[[ID]], case_when(
      is.na(depth) ~ NA_real_,
      depth < lo ~ flatval_lo,
      depth <= bp ~ a * depth^b,
      TRUE ~ flatval
    )),
    K600_M8 = with(k600_fits_M8_depth[[ID]], if_else(depth < lo, flatval_lo, a * depth^b))
  ) %>%
  ungroup()

K600_RC_depth_long <- K600_RC_depth %>%
  pivot_longer(c(K600_linear, K600_M6, K600_M8), names_to = "method", values_to = "K600_1.d_daily")

## ---- fit-quality plot, same calibration-range-only approach as the K600-Q plot ----
curve_grid_k600_depth <- map_dfr(sites, function(s) {
  rng <- range(k600s.raw$depth.x[k600s.raw$ID == s], na.rm = TRUE)
  tibble(ID = s, depth.x = exp(seq(log(rng[1]), log(rng[2]), length.out = 300)))
})

pred_k600_depth <- curve_grid_k600_depth %>%
  rowwise() %>%
  mutate(
    K600_M6 = with(k600_fits_M6_depth[[ID]], case_when(
      depth.x < lo ~ flatval_lo,
      depth.x <= bp ~ a * depth.x^b,
      TRUE ~ flatval
    )),
    K600_M8 = with(k600_fits_M8_depth[[ID]], if_else(depth.x < lo, flatval_lo, a * depth.x^b))
  ) %>%
  ungroup() %>%
  pivot_longer(c(K600_M6, K600_M8), names_to = "method", values_to = "K600_1.d_daily")

bp_lines_k600_depth <- best_bp_k600_depth %>% transmute(ID, bp)

ggplot() +
  geom_point(data = k600s.raw, aes(x = depth.x, y = k600_1.day), alpha = 0.7) +
  geom_line(data = pred_k600_depth, aes(x = depth.x, y = K600_1.d_daily, color = method), linewidth = 0.8) +
  geom_vline(data = bp_lines_k600_depth, aes(xintercept = bp), linetype = "dashed", color = "grey40") +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(title = "K600-depth RC: M6 (breakpoint) vs. M8 (power), fit over calibration range",
       subtitle = "Dashed = M6 breakpoint depth",
       x = "depth (m, log)", y = "K600 (1/day, log)") +
  theme_bw(base_size = 12)

write_csv(K600_RC_depth_long, "04_Outputs/K600_RC_depth.csv")




