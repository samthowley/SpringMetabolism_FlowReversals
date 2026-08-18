# =============================================================================
# Exploratory Figures & Tables: Hypotheses 1–3
# Spring Metabolism – Flow Reversals
# =============================================================================

# Install segmented if needed (breakpoint analysis)
if (!requireNamespace("segmented", quietly = TRUE)) install.packages("segmented")

library(tidyverse)
library(patchwork)
library(segmented)
library(flextable)
library(officer)
library(lubridate)

# segmented loads MASS which masks dplyr::select – restore it
select <- dplyr::select

setwd("C:/SpringMetabolism_FlowReversals")

# ── AESTHETICS ────────────────────────────────────────────────────────────────

site_colors <- c(AM = "#E41A1C", GB = "#377EB8", ID = "#4DAF4A",
                 LF = "#984EA3", OS = "#FF7F00", IU = "#A65628")

class_colors <- c(BO = "#74C476", FR = "#2171B5", HI = "#CB181D")

site_shapes <- c(AM = 16, GB = 17, ID = 15, LF = 18, OS = 8, IU = 3)

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom"
    )
}

# ── DATA LOADING ──────────────────────────────────────────────────────────────

metab <- read_csv("04_Outputs/master.metabolism.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

chem_hourly <- read_csv("02_Clean_data/master_chem1.csv", show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

velocity_hourly <- read_csv("02_Clean_data/Chem/velocity.csv",
                            show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

discharge_hourly <- read_csv("02_Clean_data/Chem/discharge.csv",
                             show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

flood_periods <- read_csv("01_Raw_data/flood.periods.csv",
                          show_col_types = FALSE) %>%
  mutate(start = as.POSIXct(start, tz = "UTC"),
         end   = as.POSIXct(end,   tz = "UTC"))

flood_class <- read_csv("04_Outputs/flood impacts/FR.class.csv",
                        show_col_types = FALSE)

# ── FLOOD RESPONSE (combined impact summaries) ────────────────────────────────
# Each file has one row per flood event per variable; pH and SpC lack rise/recess

read_impact <- function(path) {
  d <- read_csv(path, show_col_types = FALSE)
  # Normalise min/max into a single "extreme" column
  if ("minimum" %in% names(d) & !"maximum" %in% names(d))
    d <- rename(d, extreme = minimum)
  if ("maximum" %in% names(d) & !"minimum" %in% names(d))
    d <- rename(d, extreme = maximum)
  d
}

flood_response <- map_dfr(
  c("04_Outputs/flood impacts/GPP.csv",
    "04_Outputs/flood impacts/ER.csv",
    "04_Outputs/flood impacts/DO.csv",
    "04_Outputs/flood impacts/CO2.csv",
    "04_Outputs/flood impacts/depth.csv",
    "04_Outputs/flood impacts/pH.csv",
    "04_Outputs/flood impacts/SpC.csv"),
  read_impact
) %>%
  left_join(flood_class, by = c("ID", "flood")) %>%
  filter(!is.na(flood))   # drop non-flood summary rows

# ── FLOOD TIMESERIES (hourly/daily obs during flood periods) ──────────────────

flood_df <- map_dfr(
  list.files("04_Outputs/flood impacts", pattern = "flood\\.df\\.csv$",
             full.names = TRUE),
  read_csv, show_col_types = FALSE
) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC")) %>%
  left_join(flood_class, by = c("ID", "flood"))

# ── DAILY AVERAGES ────────────────────────────────────────────────────────────

chem_daily <- chem_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(DO  = mean(DO,  na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE),
            pH  = mean(pH,  na.rm = TRUE),
            SpC = mean(SpC, na.rm = TRUE),
            .groups = "drop") %>%
  rename(Date = day)

vel_daily <- velocity_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(velocity = mean(velocity, na.rm = TRUE), .groups = "drop") %>%
  rename(Date = day)

dis_daily <- discharge_hourly %>%
  mutate(day = as.Date(Date)) %>%
  group_by(ID, day) %>%
  summarise(discharge = mean(discharge, na.rm = TRUE), .groups = "drop") %>%
  rename(Date = day)

# Master daily dataset
master <- metab %>%
  left_join(chem_daily,  by = c("Date", "ID")) %>%
  left_join(vel_daily,   by = c("Date", "ID")) %>%
  left_join(dis_daily,   by = c("Date", "ID"))

# Tag hourly chemistry rows with their flood class
tag_flood_class <- function(df) {
  df <- df %>% mutate(flood_num = NA_integer_, class = NA_character_)
  for (i in seq_len(nrow(flood_periods))) {
    rows <- which(df$ID == flood_periods$ID[i] &
                    df$Date >= flood_periods$start[i] &
                    df$Date <= flood_periods$end[i])
    if (length(rows) == 0) next
    df$flood_num[rows] <- flood_periods$flood[i]
    cl <- flood_class$class[flood_class$ID   == flood_periods$ID[i] &
                              flood_class$flood == flood_periods$flood[i]]
    if (length(cl) > 0) df$class[rows] <- cl[1]
  }
  df
}

chem_flood <- tag_flood_class(chem_hourly) %>% filter(!is.na(class))


# =============================================================================
# HYPOTHESIS 1
# =============================================================================

# ── H1 Fig 1: Scatter plots of GPP, ER, DO, CO2 ~ depth ─────────────────────

master_long <- master %>%
  pivot_longer(cols = c(GPP, ER, DO, CO2),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(depth), !is.na(value))

 ggplot(master_long, aes(x = depth, y = value, color = ID)) +
  geom_point(alpha = 0.35, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~variable, scales = "free_y", nrow = 2) +
  scale_color_manual(values = site_colors) +
  labs(x = "Depth (m)", y = NULL, color = "Site",
       title = "H1: Response variables ~ depth by site") +
  theme_spring()

# Extract OLS slope per site × variable and export
slopes_depth <- master_long %>%
  group_by(ID, variable) %>%
  filter(sum(!is.na(value) & !is.na(depth)) > 5) %>%
  summarise(
    n         = n(),
    slope     = coef(lm(value ~ depth))[["depth"]],
    intercept = coef(lm(value ~ depth))[["(Intercept)"]],
    r2        = summary(lm(value ~ depth))$r.squared,
    p_val     = summary(lm(value ~ depth))$coefficients["depth", 4],
    .groups   = "drop"
  )

write_csv(slopes_depth, "04_Outputs/slopes_variable_depth.csv")

ggplot(slopes_depth, aes(x = ID, y = slope, fill = ID)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = site_colors) +
  labs(x = "Site", y = "Slope (unit per m depth)",
       title = "H1: OLS slope of response ~ depth by site") +
  theme_spring() + theme(legend.position = "none")

ggsave("05_Figures/H1_fig1b_depth_slopes.png", fig1b,
       width = 9, height = 6, dpi = 300)


# ── H1 Fig 2: Breakpoint analysis ────────────────────────────────────────────
# Package: segmented (Muggeo 2003)
# Fits a piecewise linear regression with one estimated breakpoint per site×var.

# Pre-compute segmented fits and extract predictions + breakpoints
seg_preds <- list()
seg_bps   <- list()
bp_slopes <- list()

for (var in c("GPP", "ER", "DO", "CO2")) {
  dat_v <- master %>%
    transmute(Date, ID, depth, value = .data[[var]]) %>%
    filter(!is.na(depth), !is.na(value))

  for (site in unique(dat_v$ID)) {
    sub <- filter(dat_v, ID == site) %>% arrange(depth)
    if (nrow(sub) < 25) next

    lm_fit  <- lm(value ~ depth, data = sub)
    seg_fit <- tryCatch(
      segmented(lm_fit, seg.Z = ~depth, npsi = 1,
                control = seg.control(it.max = 50, n.boot = 0)),
      error = function(e) NULL
    )
    if (is.null(seg_fit)) next

    px <- seq(min(sub$depth), max(sub$depth), length.out = 300)
    py <- predict(seg_fit, newdata = data.frame(depth = px))
    seg_preds[[paste(var, site)]] <- tibble(variable = var, ID = site,
                                            depth = px, fitted = py)

    bp_val <- seg_fit$psi[, "Est."]
    seg_bps[[paste(var, site)]] <- tibble(variable = var, ID = site,
                                          breakpoint = bp_val)

    sl <- slope(seg_fit)$depth
    if (is.null(sl) || nrow(sl) < 2) next
    # Column name for SE varies across segmented versions
    se_col <- intersect(c("St.Err", "Std.Err"), colnames(sl))
    se_col <- if (length(se_col) > 0) se_col[1] else NULL
    bp_slopes[[paste(var, site)]] <- tibble(
      variable   = var, ID = site, breakpoint = bp_val,
      seg1_slope = sl[1, "Est."],
      seg1_se    = if (!is.null(se_col)) sl[1, se_col] else NA_real_,
      seg2_slope = sl[2, "Est."],
      seg2_se    = if (!is.null(se_col)) sl[2, se_col] else NA_real_
    )
  }
}

seg_pred_all  <- bind_rows(seg_preds)
seg_bp_all    <- bind_rows(seg_bps)
bp_slopes_df  <- bind_rows(bp_slopes)
#write_csv(bp_slopes_df, "04_Outputs/breakpoint_slopes.csv")

# Plot all four variables in one figure; facet by variable × site
ggplot(filter(master_long, variable %in% c("GPP","ER","DO","CO2")),
               aes(x = depth, y = value)) +
  geom_point(aes(color = ID), alpha = 0.25, size = 0.6) +
  # Segmented fits
  geom_line(data = seg_pred_all,
            mapping = aes(x = depth, y = fitted),
            color = "black", linewidth = 0.9,
            inherit.aes = FALSE) +
  # Breakpoint verticals
  geom_vline(data = seg_bp_all,
             aes(xintercept = breakpoint),
             linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  facet_grid(variable ~ ID, scales = "free") +
  scale_color_manual(values = site_colors) +
  labs(x = "Depth (m)", y = NULL, color = "Site",
       title = "H1: Breakpoint analysis – response ~ depth",
       subtitle = "Dashed red line = estimated breakpoint; black line = segmented fit") +
  theme_spring() +
  theme(legend.position = "none", axis.text.x = element_text(size = 7))

# ggsave("05_Figures/H1_fig2_breakpoints.png", fig2,
#        width = 14, height = 10, dpi = 300)

# Slope comparison plot: seg1 vs seg2 per site
bp_slopes_long <- bp_slopes_df %>%
  pivot_longer(cols = c(seg1_slope, seg2_slope),
               names_to = "segment", values_to = "slope") %>%
  mutate(segment = recode(segment,
                          seg1_slope = "Segment 1 (< BP)",
                          seg2_slope = "Segment 2 (> BP)"))

ggplot(bp_slopes_long, aes(x = ID, y = slope, fill = segment)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Site", y = "Slope", fill = "Segment",
       title = "H1: Slopes per breakpoint segment by site") +
  theme_spring()


# ── H1 Fig 3: Boxplots across flood classes (flood_df timeseries) ────────────

# Summarise to one mean per flood event (point = one flood; shape = site)
flood_event_means <- flood_df %>%
  filter(variable %in% c("GPP", "ER", "CO2", "DO"), !is.na(class)) %>%
  group_by(ID, flood, variable, class) %>%
  summarise(mean_val = mean(conc, na.rm = TRUE), .groups = "drop")

ggplot(flood_event_means,
               aes(x = class, y = mean_val, fill = class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values  = class_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = "Flood class", y = "Mean value during flood",
       fill = "Class", shape = "Site",
       title = "H1: Response variables across flood classes") +
  theme_spring()



# ── H1 Fig 4: Rise slope boxplots across sites ───────────────────────────────

rise_data <- flood_response %>%
  filter(!is.na(rise.slope),
         variable %in% c("GPP", "ER", "DO", "CO2", "depth"))

ggplot(rise_data, aes(x = ID, y = rise.slope, fill = ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values  = site_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = "Site", y = "Rise slope",
       title = "H1: Flood onset rate (rise slope) by site") +
  theme_spring() + theme(legend.position = "none")

ggsave("05_Figures/H1_fig4_rise_slope.png", fig4,
       width = 12, height = 8, dpi = 300)


# ── H1 Fig 5: Time between consecutive floods per site ───────────────────────

inter_flood <- flood_response %>%
  filter(variable == "depth", !is.na(flood), !is.na(flood.start),
         !is.na(flood.end)) %>%
  mutate(flood.start = as.Date(flood.start),
         flood.end   = as.Date(flood.end)) %>%
  arrange(ID, flood.start) %>%
  group_by(ID) %>%
  mutate(prev_end         = lag(flood.end),
         inter_flood_days = as.numeric(flood.start - prev_end)) %>%
  filter(!is.na(inter_flood_days), inter_flood_days >= 0) %>%
  ungroup()

ggplot(inter_flood, aes(x = ID, y = inter_flood_days, fill = ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  scale_fill_manual(values  = site_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = "Site", y = "Days between floods",
       title = "H1: Inter-flood interval by site") +
  theme_spring() + theme(legend.position = "none")


# ── H1 Fig 6: Time lag between variable peaks and peak flood depth ────────────

peak_dates <- flood_response %>%
  filter(!is.na(flood), !is.na(peak.Date)) %>%
  mutate(peak.Date = as.POSIXct(peak.Date, tz = "UTC")) %>%
  select(ID, flood, variable, peak.Date)

depth_peaks <- peak_dates %>%
  filter(variable == "depth") %>%
  select(ID, flood, depth_peak = peak.Date)

lag_df <- peak_dates %>%
  filter(variable != "depth") %>%
  left_join(depth_peaks, by = c("ID", "flood")) %>%
  filter(!is.na(depth_peak)) %>%
  mutate(lag_days = as.numeric(difftime(peak.Date, depth_peak,
                                        units = "days"))) %>%
  left_join(flood_class, by = c("ID", "flood"))

ggplot(lag_df, aes(x = variable, y = lag_days)) +
  geom_boxplot(aes(fill = variable), outlier.shape = NA,
               alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID, color = class), width = 0.15,
              size = 2.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = class_colors) +
  scale_shape_manual(values = site_shapes) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variable", y = "Lag relative to peak depth (days)",
       shape = "Site", color = "Class",
       title = "H1: Time lag of variable peak relative to peak flood depth") +
  guides(fill = "none") +
  theme_spring()


# =============================================================================
# HYPOTHESIS 2
# =============================================================================

# ── H2 Fig 7: By-site boxplots of duration, recovery time, recess.slope ──────

h2_depth <- flood_response %>%
  filter(variable == "depth", !is.na(flood),
         !is.na(flood.end), !is.na(peak.Date)) %>%
  mutate(
    peak_date     = as.Date(as.POSIXct(peak.Date, tz = "UTC")),
    flood_end     = as.Date(flood.end),
    recovery_days = as.numeric(flood_end - peak_date)
  )

ggplot(filter(h2_depth, !is.na(duration)),
              aes(x = ID, y = duration, fill = ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  scale_fill_manual(values = site_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = NULL, y = "Flood duration (days)") +
  theme_spring() + theme(legend.position = "none")

ggplot(filter(h2_depth, !is.na(recovery_days), recovery_days >= 0),
              aes(x = ID, y = recovery_days, fill = ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  scale_fill_manual(values = site_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = NULL, y = "Recovery time: peak → end (days)") +
  theme_spring() + theme(legend.position = "none")

ggplot(filter(h2_depth, !is.na(recess.slope)),
              aes(x = ID, y = recess.slope, fill = ID)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.5) +
  geom_jitter(aes(shape = ID), width = 0.15, size = 2.5, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = site_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = "Site", y = "Recession slope (m/day)") +
  theme_spring() + theme(legend.position = "none")



# ── H2 Fig 8: Orientation plot – recess.slope (x) vs r2.recess (y) ───────────

orient_data <- flood_response %>%
  filter(!is.na(recess.slope), !is.na(r2.recess), !is.na(class))

ggplot(orient_data,
               aes(x = recess.slope, y = r2.recess,
                   color = class, shape = ID)) +
  geom_point(size = 3, alpha = 0.85) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(values = class_colors) +
  scale_shape_manual(values = site_shapes) +
  labs(x = "Recession slope", y = expression(R^2 ~~ recession),
       color = "Class", shape = "Site",
       title = "H2: Recession quality by flood class") +
  theme_spring()



# =============================================================================
# HYPOTHESIS 3
# =============================================================================

# ── H3 Fig 9: Autocorrelation plots per spring ───────────────────────────────

acf_all <- map_dfr(unique(metab$ID), function(s) {
  sub <- metab %>% filter(ID == s) %>% arrange(Date)
  if (nrow(sub) < 30) return(NULL)

  nep_ts <- sub$GPP + sub$ER
  n_obs  <- sum(!is.na(nep_ts))

  bind_rows(
    tibble(ID = s, variable = "GPP",
           lag = acf(sub$GPP, lag.max = 30, plot = FALSE,
                     na.action = na.pass)$lag[,,1],
           acf_val = acf(sub$GPP, lag.max = 30, plot = FALSE,
                         na.action = na.pass)$acf[,,1],
           n = n_obs),
    tibble(ID = s, variable = "ER",
           lag = acf(sub$ER, lag.max = 30, plot = FALSE,
                     na.action = na.pass)$lag[,,1],
           acf_val = acf(sub$ER, lag.max = 30, plot = FALSE,
                         na.action = na.pass)$acf[,,1],
           n = n_obs),
    tibble(ID = s, variable = "NEP",
           lag = acf(nep_ts, lag.max = 30, plot = FALSE,
                     na.action = na.pass)$lag[,,1],
           acf_val = acf(nep_ts, lag.max = 30, plot = FALSE,
                         na.action = na.pass)$acf[,,1],
           n = n_obs)
  )
}) %>%
  mutate(ci = qnorm(0.975) / sqrt(n))

ggplot(filter(acf_all, lag > 0),
               aes(x = lag, y = acf_val)) +
  geom_segment(aes(xend = lag, yend = 0, color = variable), linewidth = 0.9) +
  geom_hline(aes(yintercept =  ci), linetype = "dashed",
             color = "steelblue", linewidth = 0.5) +
  geom_hline(aes(yintercept = -ci), linetype = "dashed",
             color = "steelblue", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  facet_grid(variable ~ ID) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Lag (days)", y = "Autocorrelation",
       title = "H3: Autocorrelation of GPP, ER, and NEP by spring") +
  theme_spring() + theme(legend.position = "none")

# ── H3 Fig 10: pH vs DO & SpC vs DO during flood periods ─────────────────────

ggplot(filter(chem_flood, !is.na(pH), !is.na(DO)),
               aes(x = pH, y = DO, color = class)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = class_colors) +
  labs(x = "pH", y = "DO (mg/L)", color = "Class",
       title = "pH vs DO") +
  theme_spring()

ggplot(filter(chem_flood, !is.na(SpC), !is.na(DO)),
               aes(x = SpC, y = DO, color = class)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = class_colors) +
  labs(x = expression("SpC (" * mu * "S cm"^-1 * ")"), y = "DO (mg/L)",
       color = "Class", title = "SpC vs DO") +
  theme_spring()

# =============================================================================
# TABLE 1: Site average characteristics
# =============================================================================

tbl1_means <- master %>%
  group_by(ID) %>%
  summarise(
    GPP      = round(mean(GPP,             na.rm = TRUE), 2),
    ER       = round(mean(ER,              na.rm = TRUE), 2),
    DO       = round(mean(DO,              na.rm = TRUE), 2),
    CO2      = round(mean(CO2 / 1000,      na.rm = TRUE), 1),
    depth    = round(mean(depth,           na.rm = TRUE), 2),
    K600     = round(mean(K600,            na.rm = TRUE), 1),
    velocity = round(mean(velocity,        na.rm = TRUE), 3),
    Q        = round(mean(discharge / 1000, na.rm = TRUE), 1),
    .groups  = "drop"
  )

tbl1_sd <- master %>%
  group_by(ID) %>%
  summarise(
    sd.GPP      = round(sd(GPP,             na.rm = TRUE), 2),
    sd.ER       = round(sd(ER,              na.rm = TRUE), 2),
    sd.DO       = round(sd(DO,              na.rm = TRUE), 2),
    sd.CO2      = round(sd(CO2 / 1000,      na.rm = TRUE), 1),
    sd.depth    = round(sd(depth,           na.rm = TRUE), 2),
    sd.K600     = round(sd(K600,            na.rm = TRUE), 1),
    sd.velocity = round(sd(velocity,        na.rm = TRUE), 3),
    sd.Q        = round(sd(discharge / 1000, na.rm = TRUE), 1),
    .groups     = "drop"
  )

tbl1_wide <- left_join(tbl1_means, tbl1_sd, by = "ID") %>%
  mutate(
    GPP      = paste(GPP,      "±", sd.GPP),
    ER       = paste(ER,       "±", sd.ER),
    DO       = paste(DO,       "±", sd.DO),
    CO2      = paste(CO2,      "±", sd.CO2),
    depth    = paste(depth,    "±", sd.depth),
    K600     = paste(K600,     "±", sd.K600),
    velocity = paste(velocity, "±", sd.velocity),
    Q        = paste(Q,        "±", sd.Q)
  ) %>%
  select(ID, GPP, ER, DO, CO2, depth, K600, velocity, Q)

var_labels_t1 <- c(
  GPP      = "GPP (g O₂ m⁻² d⁻¹)",
  ER       = "ER (g O₂ m⁻² d⁻¹)",
  DO       = "DO (mg L⁻¹)",
  CO2      = "pCO₂ (×10³ ppm)",
  depth    = "Depth (m)",
  K600     = "k₆₀₀ (d⁻¹)",
  velocity = "Velocity (m s⁻¹)",
  Q        = "Discharge (L s⁻¹)"
)

n_sites <- n_distinct(tbl1_wide$ID)

table1_long <- tbl1_wide %>%
  pivot_longer(-ID, names_to = "Variable", values_to = "value") %>%
  pivot_wider(names_from = ID, values_from = value) %>%
  mutate(Variable = recode(Variable, !!!var_labels_t1),
         Variable = factor(Variable, levels = unname(var_labels_t1))) %>%
  arrange(Variable) %>%
  mutate(Variable = as.character(Variable))

ft1 <- flextable(table1_long) %>%
  set_header_labels(Variable = "") %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(j = 1, align = "left",   part = "all") %>%
  align(j = seq(2, n_sites + 1), align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  width(j = 1, width = 2.8) %>%
  width(j = seq(2, n_sites + 1), width = 1.1) %>%
  height_all(height = 0.25) %>%
  add_header_lines("Table 1. Mean ± SD of site characteristics for the full period of record.") %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines("Values are mean ± SD of daily values.") %>%
  italic(part = "footer") %>%
  align(part = "footer", align = "left") %>%
  fontsize(part = "footer", size = 10)

save_as_docx(ft1, path = "04_Outputs/Table1_site_characteristics.docx")


# =============================================================================
# TABLE 2: Flood impacts per event
# =============================================================================

# Build one row per flood using depth as the source for dates/duration;
# join extreme values for each other variable.

get_extreme <- function(var_name) {
  flood_response %>%
    filter(variable == var_name, !is.na(flood)) %>%
    select(ID, flood, !!var_name := extreme)
}

tbl2 <- flood_response %>%
  filter(variable == "depth", !is.na(flood)) %>%
  select(ID, flood, class, Date = flood.start, duration) %>%
  left_join(get_extreme("GPP"),  by = c("ID", "flood")) %>%
  left_join(get_extreme("ER"),   by = c("ID", "flood")) %>%
  left_join(get_extreme("DO"),   by = c("ID", "flood")) %>%
  left_join(get_extreme("CO2"),  by = c("ID", "flood")) %>%
  left_join(get_extreme("depth"), by = c("ID", "flood")) %>%
  left_join(get_extreme("pH"),   by = c("ID", "flood")) %>%
  left_join(get_extreme("SpC"),  by = c("ID", "flood")) %>%
  # Site-average K600, velocity, discharge as context columns
  left_join(
    metab %>% group_by(ID) %>%
      summarise(K600 = round(mean(K600, na.rm = TRUE), 1), .groups = "drop"),
    by = "ID"
  ) %>%
  left_join(
    vel_daily %>% group_by(ID) %>%
      summarise(velocity = round(mean(velocity, na.rm = TRUE), 3),
                .groups = "drop"),
    by = "ID"
  ) %>%
  left_join(
    dis_daily %>% group_by(ID) %>%
      summarise(discharge = round(mean(discharge / 1000, na.rm = TRUE), 1),
                .groups = "drop"),
    by = "ID"
  ) %>%
  mutate(across(c(GPP, ER, DO, depth, pH), ~round(., 2)),
         CO2 = round(CO2, 0),
         SpC = round(SpC, 1)) %>%
  arrange(ID, Date) %>%
  select(ID, Date, class, duration, DO, CO2, GPP, ER, depth,
         K600, discharge, velocity, SpC, pH)

ft2 <- flextable(tbl2) %>%
  set_header_labels(
    ID       = "Site",
    Date     = "Start date",
    class    = "Class",
    duration = "Duration\n(days)",
    DO       = "DO\n(mg/L)",
    CO2      = "CO₂\n(ppm)",
    GPP      = "GPP\n(g O₂/m²/d)",
    ER       = "ER\n(g O₂/m²/d)",
    depth    = "Peak depth\n(m)",
    K600     = "k₆₀₀\n(d⁻¹)",
    discharge = "Q\n(L/s)",
    velocity  = "u\n(m/s)",
    SpC      = "SpC\n(μS/cm)",
    pH       = "pH"
  ) %>%
  font(fontname = "Aptos", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
  autofit() %>%
  add_header_lines("Table 2. Flood event characteristics by site.") %>%
  bold(part = "header", i = 1) %>%
  align(part = "header", i = 1, align = "left") %>%
  add_footer_lines(
    "Extreme values: minimum (GPP, ER, DO, pH, SpC) or maximum (CO₂, depth) during each flood event. K600, velocity, and discharge are site-period averages."
  ) %>%
  italic(part = "footer") %>%
  fontsize(part = "footer", size = 9)

