rm(list=ls())
library(tidyverse)
library(readxl)
library(dataRetrieval)

##data#############
sites <- c("AM", "GB", "ID", "LF")
outdir <- "04_Outputs/Power Function RC"
cfs_to_m3day <- 0.0283168 * 86400

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ") %>% select(ID, w)
depth <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>% filter(ID %in% sites)

depth_daily <- depth %>% mutate(date = as.Date(Date)) %>% group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

sheet_names <- excel_sheets("04_Outputs/velocity.xlsx")

field_measurments <- map_dfr(sheet_names, function(s) read_excel("04_Outputs/velocity.xlsx", sheet = s)) %>%
  filter(ID %in% sites, !is.na(depth), !is.na(u), u > 0, depth > 0) %>%
  mutate(u=if_else(ID=='GB' & depth>1, NA, u))%>%
  left_join(width, by = "ID") %>%
  mutate(velocity = u) %>%
  select(Date, ID, w, depth, velocity)

field_measurments <- field_measurments %>%
  mutate(velocity = if_else(ID %in% c('AM','LF','OS', 'ID'), conv_unit(velocity, 'ft_per_sec', 'm_per_sec'), velocity))


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


velocity_RC.edit<-velocity_RC%>%
  rename(velocity.RC=velocity)%>%
  mutate(Date=as.Date(Date))%>%
  summarise(velocity.RC=mean(velocity.RC, na.rm=T), .by=c(ID, Date))


field_measurments<-field_measurments%>%left_join(velocity_RC.edit, by=c('ID', 'Date'))%>%
  mutate(discharge=depth*w*velocity.RC*86400)
## ==================== 2. depth-discharge RC: 2 segments, increasing then decreasing ====================

find_breakpoint <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < sort(df$depth, decreasing = TRUE)[4]]
  map_dfr(cand, function(bp) {
    # hinge regression instead of two independent lm()s -- forces the two
    # segments to meet exactly at bp instead of allowing a vertical jump there
    m <- lm(discharge ~ depth + I(pmax(depth - bp, 0)), data = df)
    tibble(bp = bp, sse = sum(resid(m)^2))
  })
}

bp_search <- map_dfr(sites, function(s) find_breakpoint(field_measurments %>% filter(ID == s)) %>% mutate(ID = s))
best_bp <- bp_search %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()


fit_two_segment <- function(df, bp) {
  # same hinge form as find_breakpoint -- b_right is the slope AFTER bp
  # (b1+b2), and because both pieces come from one continuous model, they
  # meet exactly at bp with no vertical jump
  m <- lm(discharge ~ depth + I(pmax(depth - bp, 0)), data = df)
  b0 <- coef(m)[1]; b1 <- coef(m)[2]; b2 <- coef(m)[3]
  list(bp = bp, a_left = b0, b_left = b1, b_right = b1 + b2)
}
discharge_fits <- map(sites, function(s) fit_two_segment(field_measurments %>% filter(ID == s), best_bp$bp[best_bp$ID == s])) %>%
    set_names(sites)

predict_discharge <- function(d, fit) {
  if_else(d <= fit$bp,
          fit$a_left + fit$b_left * d,
          fit$a_left + fit$b_left * fit$bp + fit$b_right * (d - fit$bp))
}

discharge_RC <- depth %>%
  rowwise() %>%
  mutate(discharge = predict_discharge(depth, discharge_fits[[ID]])) %>%
  ungroup()%>%
  mutate(discharge=if_else(discharge<0, 0, discharge))


ggplot() +
  geom_point(data = field_measurments, aes(x = depth, y = discharge), alpha = 0.7) +
  geom_point(data = field_measurments, aes(x = depth, y = velocity.RC), alpha = 0.7) +
  geom_line(data = discharge_RC %>% arrange(ID, depth), aes(x = depth, y = discharge), color = "red", linewidth = 0.8) +

  facet_wrap(~ID, scales = "free") +
  labs(title = "Depth-discharge RC: 2 segments (breakpoint dashed)", x = "depth (m)", y = "discharge (m3/day)") +
  theme_bw(base_size = 12)

discharge_RC.edit<-discharge_RC%>%
  rename(discharge.RC=discharge)%>%
  mutate(Date=as.Date(Date))%>%
  summarise(discharge.RC=mean(discharge.RC, na.rm=T), .by=c(ID, Date))


field_measurments<-field_measurments%>%left_join(discharge_RC.edit, by=c('ID', 'Date'))%>%
  mutate(discharge=depth*w*discharge.RC)

## ==================== 3a. clean rC_k600.xlsx ====================

k600_sheet_names <- excel_sheets("04_Outputs/rC_k600.xlsx")
k600_raw_dirty <- map_dfr(k600_sheet_names, function(s) read_excel("04_Outputs/rC_k600.xlsx", sheet = s))

k600_clean <- k600_raw_dirty %>%
  select(Date, ID, rep, Temp_C, CO2, CO2_enviro, depth, k600_1.day, KCO2_m.day) %>%  # drop stray ...10-...15 cols
  filter(!is.na(ID)) %>%                                                            # drop blank trailing rows
  filter(!is.na(k600_1.day)) %>%
  filter(k600_1.day <= 30)                                                          # same plausibility ceiling as 01_fit_breakpoint_K600.R -- published K600 tops out ~26

## ==================== 3b. K600-discharge RC: linear, M6, M8 ====================

k600s.raw <- k600_clean %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(discharge_RC.edit, by = c('ID', 'Date')) 

## ---- method 1: linear ----
k600_fits_linear <- map(sites, function(s) lm(k600_1.day ~ depth, data = k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ---- method 2: M6, statistical breakpoint (power law below bp, flat above) ----
find_breakpoint_k600 <- function(df) {
  df <- df %>% arrange(depth)
  cand <- unique(df$depth)
  cand <- cand[cand > sort(df$depth)[4] & cand < max(df$depth)]
  map_dfr(cand, function(bp) {
    left <- df %>% filter(depth <= bp); right <- df %>% filter(depth > bp)
    if (nrow(left) < 4 || nrow(right) < 1) return(NULL)
    m <- lm(log(k600_1.day) ~ log(depth), data = left)
    flatval <- exp(predict(m, newdata = data.frame(depth = bp)))
    sse <- sum((left$k600_1.day - exp(predict(m)))^2) + sum((right$k600_1.day - flatval)^2)
    tibble(bp = bp, sse = sse)
  })
}
bp_search_k600 <- map_dfr(sites, function(s) find_breakpoint_k600(k600s.raw %>% filter(ID == s)) %>% mutate(ID = s))
best_bp_k600 <- bp_search_k600 %>% group_by(ID) %>% slice_min(sse, n = 1, with_ties = FALSE) %>% ungroup()

fit_M6 <- function(df, bp) {
  left <- df %>% filter(depth <= bp)
  m <- lm(log(k600_1.day) ~ log(depth), data = left)
  a <- exp(coef(m)[1]); b <- unname(coef(m)[2])
  list(a = a, b = b, bp = bp, flatval = a * bp^b)
}
k600_fits_M6 <- map(sites, function(s) fit_M6(k600s.raw %>% filter(ID == s), best_bp_k600$bp[best_bp_k600$ID == s])) %>%
  set_names(sites)

## ---- method 3: M8, plain power law, no breakpoint ----
k600_fits_M8 <- map(sites, function(s) lm(log(k600_1.day) ~ log(depth), data = k600s.raw %>% filter(ID == s))) %>%
  set_names(sites)

## ---- apply all three to the discharge RC ----
K600_RC <- discharge_RC %>%
  rowwise() %>%
  mutate(
    K600_linear = predict(k600_fits_linear[[ID]], newdata = data.frame(depth = depth)),
    K600_M6 = with(k600_fits_M6[[ID]], if_else(depth <= bp, a * depth^b, flatval)),
    K600_M8 = exp(predict(k600_fits_M8[[ID]], newdata = data.frame(depth = depth)))
  ) %>%
  ungroup()

K600_RC_long <- K600_RC %>%
  pivot_longer(c(K600_linear, K600_M6, K600_M8), names_to = "method", values_to = "K600_1.d_daily")


unique(K600_RC_long$method)
ggplot() +
  geom_point(data = k600s.raw, aes(x = depth, y = k600_1.day), alpha = 0.7) +
  geom_line(data = K600_RC_long %>%
              arrange(ID, depth)%>%
              filter(method!="K600_linear"),
            aes(x = depth, y = K600_1.d_daily, color = method), linewidth = 0.8) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(title = "K600-depth RC: linear vs. M6 (breakpoint) vs. M8 (power)", x = "depth (m, log)", y = "K600 (1/day, log)") +
  theme_bw(base_size = 12)

## ==================== 4. write the interpolated RCs out ====================

write_csv(velocity_RC %>% select(Date, ID, depth, velocity), file.path(outdir, "38_velocity_RC.csv"))
write_csv(discharge_RC %>% select(Date, ID, depth, discharge), file.path(outdir, "38_discharge_RC.csv"))
write_csv(K600_RC %>% select(Date, ID, depth, discharge, K600_linear, K600_M6, K600_M8), file.path(outdir, "38_K600_RC.csv"))

cat("\nWrote 38_velocity_RC.csv, 38_discharge_RC.csv, 38_K600_RC.csv to", outdir, "\n")
