library(tidyverse)
library(segmented)
library(cowplot)
select <- dplyr::select

#plot settings########
class_colors <- c(BO = "#A65628", FR = "black", HI = "#2171B5", baseline='lightblue')

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom"
    )
}

#call in data########
chem_hourly <- read_csv("02_Clean_data/master_chem1.csv", show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

metab <- read_csv("04_Outputs/combined metabolism methods.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

peak_dates.file <- "04_Outputs/flood impacts/peak dates.csv"
if (file.exists(peak_dates.file)) {
  peak_dates <- read_csv(peak_dates.file, show_col_types = FALSE)
} else {
  message("peak dates.csv not found: plots will not be coloured by flood class")
  peak_dates <- tibble(ID = character(),
                       Date = as.POSIXct(character(), tz = "UTC"),
                       class = character())
}

# H1: how do GPP, ER, DO and CO2 change with stage?
#   GPP, ER (|ER|) = daily value; DO, CO2 = daily diel range (max - min)
# Each site x variable gets a linear fit (0 breakpoints), a 1 breakpoint fit and
# a 2 breakpoint fit; the simplest model that improves the fit enough is kept.
# Output: 04_Outputs/breakpoints.csv (one row per site x variable)

#parameters########
min.day.frac    <- 0.9   # a day needs this fraction of the site's usual readings to get a diel range
floor.frac      <- 0.10  # GPP counts as halted when the top segment mean is below this fraction of the site's 95th percentile
fit_criterion   <- "adjR2"   # "adjR2" (default) | "AIC" | "BIC"
adj_r2_min_gain <- 0.02      # minimum adj-R2 improvement to prefer the more complex model
aic_min_gain    <- 2         # minimum AIC reduction to prefer the more complex model
min.seg.frac    <- 0.05      # each segment needs at least this fraction of the site's days ...
min.seg.n       <- 10        # ... and at least this many

#daily data########
# same CO2 > 600 filter as isolate disturbances_CO2.R
daily.chem <- chem_hourly %>%
  mutate(
    Date = as.Date(Date),
    CO2  = if_else(CO2 > 600, CO2, NA_real_)
  ) %>%
  group_by(ID, Date) %>%
  summarise(
    n.DO        = sum(!is.na(DO)),
    n.CO2       = sum(!is.na(CO2)),
    DO.diurnal  = if (n.DO == 0)  NA_real_ else max(DO,  na.rm = TRUE) - min(DO,  na.rm = TRUE),
    CO2.diurnal = if (n.CO2 == 0) NA_real_ else max(CO2, na.rm = TRUE) - min(CO2, na.rm = TRUE),
    depth       = mean(depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ID) %>%
  mutate(
    DO.diurnal  = if_else(n.DO  >= min.day.frac * median(n.DO[n.DO > 0]),   DO.diurnal,  NA_real_),
    CO2.diurnal = if_else(n.CO2 >= min.day.frac * median(n.CO2[n.CO2 > 0]), CO2.diurnal, NA_real_)
  ) %>%
  ungroup() %>%
  select(-n.DO, -n.CO2)

df <- daily.chem %>%
  left_join(
    metab %>%
      distinct(ID, Date, .keep_all = TRUE) %>%
      transmute(ID, Date, GPP, ER = abs(ER)),
    by = c("Date", "ID"),
    relationship = "one-to-one"
  ) %>%
  rename(DO = DO.diurnal, CO2 = CO2.diurnal) %>%
  arrange(ID, Date)

# Join diagnostics
df %>%
  group_by(ID) %>%
  summarise(n_rows  = n(),
            n_depth = sum(!is.na(depth)),
            n_DO    = sum(!is.na(DO)),
            n_CO2   = sum(!is.na(CO2)),
            n_GPP   = sum(!is.na(GPP)),
            n_ER    = sum(!is.na(ER)),
            .groups = "drop") %>%
  print()

master_long <- df %>%
  pivot_longer(cols = c(GPP, ER, DO, CO2),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(depth)) %>%
  left_join(peak_dates, by = join_by(ID, Date), relationship = "many-to-many") %>%
  arrange(ID, depth)%>%
  group_by(ID)%>%
  mutate(
    class=if_else(is.na(class), 'baseline', class),
    variable = factor(variable, levels = c("DO", "CO2", "GPP", "ER")),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS'))
  )
unique(master_long$ID)


#helpers########

# Adjusted R2 for a segmented or lm object
get_adj_r2 <- function(fit) summary(fit)$adj.r.squared

# TRUE if every segment holds enough observations (blocks tiny end segments and
# two breakpoints sitting right next to each other)
seg_ok <- function(fit, depth_vec) {
  bp   <- sort(fit$psi[, "Est."])
  n_in <- as.numeric(table(cut(depth_vec, c(-Inf, bp, Inf))))
  all(n_in >= max(min.seg.n, min.seg.frac * length(depth_vec)))
}

# Segmented fit with n_bp breakpoints. Most days sit near baseline stage, so
# one set of starting values can land on a poor solution: try several starting
# sets (depth quantiles and evenly spaced depths) and keep the lowest AIC.
# Returns NULL if nothing converges.
try_seg <- function(lm_fit, depth_vec, n_bp) {
  cand <- sort(unique(c(as.numeric(quantile(depth_vec, c(0.2, 0.4, 0.6, 0.8, 0.9))),
                        seq(min(depth_vec), max(depth_vec), length.out = 5)[2:4])))
  starts <- if (n_bp == 1) as.list(cand) else combn(cand, n_bp, simplify = FALSE)
  best <- NULL
  for (st in starts) {
    fit <- tryCatch(
      suppressWarnings(
        segmented(lm_fit, seg.Z = ~depth, psi = list(depth = st),
                  control = seg.control(it.max = 50, n.boot = 0))),
      error = function(e) NULL
    )
    if (is.null(fit) || !inherits(fit, "segmented")) next
    if (any(is.na(fit$psi[, "Est."])) || !seg_ok(fit, depth_vec)) next
    if (is.null(best) || AIC(fit) < AIC(best)) best <- fit
  }
  best
}

# Start from the linear fit (0 bp); take the 1 bp fit only if it beats it, then
# take the 2 bp fit only if it beats whatever is currently kept
choose_model <- function(fit0, fit1, fit2) {
  best <- fit0
  for (cand in list(fit1, fit2)) {
    if (is.null(cand)) next
    better <- switch(fit_criterion,
      adjR2 = (get_adj_r2(cand) - get_adj_r2(best)) >= adj_r2_min_gain,
      AIC   = (AIC(best) - AIC(cand)) >= aic_min_gain,
      BIC   = BIC(cand) < BIC(best)
    )
    if (isTRUE(better)) best <- cand
  }
  best
}


#segmented fits########
seg_preds <- list()
seg_bps   <- list()
bp_slopes <- list()
bp_summ   <- list()

for (var in c("GPP", "ER", "DO", "CO2")) {
  dat_v <- df %>%
    transmute(Date, ID, depth, value = .data[[var]]) %>%
    filter(!is.na(depth), !is.na(value))

  for (site in unique(dat_v$ID)) {
    sub <- filter(dat_v, ID == site) %>% arrange(depth)
    if (nrow(sub) < 25) {
      message(sprintf("SKIP  %s x %s: only %d observations (need >= 25)", var, site, nrow(sub)))
      next
    }

    lm_fit <- lm(value ~ depth, data = sub)
    seg1   <- try_seg(lm_fit, sub$depth, 1)
    seg2   <- try_seg(lm_fit, sub$depth, 2)
    best   <- choose_model(lm_fit, seg1, seg2)

    key <- paste(var, site)

    # Breakpoints (none when the linear fit wins) and segment slopes
    if (inherits(best, "segmented")) {
      bp_val <- sort(best$psi[, "Est."])
      sl     <- slope(best)$depth
      sl_est <- sl[, "Est."]
      se_col <- grep("^St(d)?\\.? ?Err", colnames(sl), value = TRUE)[1]
      sl_se  <- if (!is.na(se_col)) sl[, se_col] else rep(NA_real_, nrow(sl))
    } else {
      bp_val <- numeric(0)
      cf     <- summary(best)$coefficients
      sl_est <- cf["depth", "Estimate"]
      sl_se  <- cf["depth", "Std. Error"]
    }

    # Predictions along depth range
    px <- seq(min(sub$depth), max(sub$depth), length.out = 300)
    py <- predict(best, newdata = data.frame(depth = px))
    seg_preds[[key]] <- tibble(variable = var, ID = site, depth = px, fitted = py)

    if (length(bp_val) > 0) {
      seg_bps[[key]] <- tibble(variable = var, ID = site, breakpoint = bp_val)
    }

    lower_bounds <- c(min(sub$depth), bp_val)
    upper_bounds <- c(bp_val, max(sub$depth))

    bp_slopes[[key]] <- tibble(
      variable      = var,
      ID            = site,
      n_breakpoints = length(bp_val),
      adj_r2        = get_adj_r2(best),
      segment       = seq_along(sl_est),
      seg_lower     = lower_bounds,
      seg_upper     = upper_bounds,
      slope         = as.numeric(sl_est),
      slope_se      = as.numeric(sl_se)
    )

    # Pattern: direction of each segment (flat = 95% CI of the slope includes 0)
    flat <- abs(sl_est) < 1.96 * sl_se
    flat[is.na(flat)] <- FALSE
    dir <- if_else(flat, "flat", if_else(sl_est > 0, "up", "down"))
    n_seg <- length(dir)

    # First interior breakpoint where the slope changes sign (peak or trough)
    turn.stage <- NA_real_
    turn.type  <- NA_character_
    if (n_seg > 1) {
      for (i in seq_len(n_seg - 1)) {
        if (dir[i] == "up" & dir[i + 1] == "down") { turn.stage <- bp_val[i]; turn.type <- "peak";   break }
        if (dir[i] == "down" & dir[i + 1] == "up") { turn.stage <- bp_val[i]; turn.type <- "trough"; break }
      }
    }

    # Halting (GPP only): ends down then flat, and the top segment sits near zero
    top.level     <- mean(sub$value[sub$depth >= max(lower_bounds)])
    top.level.rel <- top.level / as.numeric(quantile(sub$value, 0.95))
    halted <- var == "GPP" && n_seg > 1 &&
      dir[n_seg] == "flat" && dir[n_seg - 1] == "down" &&
      isTRUE(top.level.rel <= floor.frac)

    bp_summ[[key]] <- tibble(
      variable        = var,
      ID              = site,
      metric          = if_else(var %in% c("DO", "CO2"), "diel range", "daily value"),
      n.obs           = nrow(sub),
      depth.min       = min(sub$depth),
      depth.max       = max(sub$depth),
      n.breakpoints   = length(bp_val),
      pattern         = paste(dir, collapse = "-"),
      bp1             = if (length(bp_val) >= 1) bp_val[1] else NA_real_,
      bp2             = if (length(bp_val) >= 2) bp_val[2] else NA_real_,
      slope1          = as.numeric(sl_est)[1],
      slope1.se       = as.numeric(sl_se)[1],
      slope2          = if (n_seg >= 2) as.numeric(sl_est)[2] else NA_real_,
      slope2.se       = if (n_seg >= 2) as.numeric(sl_se)[2]  else NA_real_,
      slope3          = if (n_seg >= 3) as.numeric(sl_est)[3] else NA_real_,
      slope3.se       = if (n_seg >= 3) as.numeric(sl_se)[3]  else NA_real_,
      adj.r2          = get_adj_r2(best),
      turn.stage      = turn.stage,
      turn.type       = turn.type,
      top.level.rel   = top.level.rel,
      halted          = if (var == "GPP") halted else NA,
      halting.stage   = if (var == "GPP" && isTRUE(halted)) max(bp_val) else NA_real_
    )
  }
}

id_levels  <- levels(master_long$ID)
var_levels <- levels(master_long$variable)

seg_pred_all <- bind_rows(seg_preds) %>%
  mutate(ID       = factor(ID,       levels = id_levels),
         variable = factor(variable, levels = var_levels))

seg_bp_all <- bind_rows(seg_bps) %>%
  mutate(ID       = factor(ID,       levels = id_levels),
         variable = factor(variable, levels = var_levels))

bp_slopes_df <- bind_rows(bp_slopes)

breakpoints.out <- bind_rows(bp_summ) %>%
  mutate(
    variable = factor(variable, levels = var_levels),
    ID       = factor(ID,       levels = id_levels)
  ) %>%
  arrange(variable, ID) %>%
  mutate(variable = as.character(variable), ID = as.character(ID))

write_csv(breakpoints.out, "04_Outputs/breakpoints.csv")

breakpoints.out %>% select(variable, ID, n.breakpoints, pattern, bp1, bp2, turn.type, halted, halting.stage)


#dominant flood class per segment########
# Ordinal encoding: baseline=1, BO=2, HI=3, FR=4
# Weighted mean ordinal (by n distinct days per class in segment depth range),
# then rounded back to the nearest class label.
class_ord_map <- c(baseline = 1, BO = 2, HI = 3, FR = 4)
ord_to_class  <- c("1" = "baseline", "2" = "BO", "3" = "HI", "4" = "FR")

# One row per site x date x depth, with class already filled down
depth_class <- master_long %>%
  distinct(ID, Date, depth, class) %>%
  filter(!is.na(class)) %>%
  mutate(class_num = class_ord_map[class])

seg_class <- bp_slopes_df %>%
  select(variable, ID, segment, seg_lower, seg_upper) %>%
  mutate(ID = factor(ID, levels = id_levels)) %>%
  left_join(depth_class, by = "ID", relationship = "many-to-many") %>%
  filter(depth >= seg_lower, depth <= seg_upper, !is.na(class_num)) %>%
  distinct(variable, ID, segment, Date, class, class_num) %>%
  group_by(variable, ID, segment, class, class_num) %>%
  summarise(n_days = n(), .groups = "drop") %>%
  group_by(variable, ID, segment) %>%
  summarise(mean_class_num = sum(class_num * n_days) / sum(n_days),
            .groups = "drop") %>%
  mutate(seg_class = factor(ord_to_class[as.character(round(mean_class_num))],
                            levels = names(class_ord_map)))

bp_slopes_df <- bp_slopes_df %>%
  mutate(ID = factor(ID, levels = id_levels)) %>%
  left_join(seg_class %>% select(variable, ID, segment, seg_class),
            by = c("variable", "ID", "segment"))


#breakpoint plot########


a <- master_long %>%
    mutate(class = factor(class, levels = c("baseline", "HI", "BO", "FR")))%>%
  ggplot(aes(x = depth, y = value)) +
  geom_point(aes(color = class), size = 0.6) +
  geom_line(data = seg_pred_all,
            mapping = aes(x = depth, y = fitted),
            color = "black", linewidth = 0.9,
            inherit.aes = FALSE) +
  geom_vline(data = seg_bp_all,
             aes(xintercept = breakpoint),
             linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  facet_wrap(vars(variable, ID), scales = "free",
             ncol = n_distinct(master_long$ID)) +
  scale_color_manual(values = class_colors, na.value = "grey70") +
  labs(x = "Depth (m)", y = NULL, color = "Class") +
  theme_spring() +
  theme(axis.text.x = element_text(size = 7),
        legend.position = 'right')


#slope scatter plot#
# Each point = one segment; colour = dominant flood class for that segment's
# depth range; label = segment number (shallow -> deep)
b <- bp_slopes_df %>%
  mutate(variable = factor(variable, levels = var_levels)) %>%
  ggplot(aes(x = ID, y = slope, color = seg_class, group = ID)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(color = "grey75", linewidth = 0.5) +
  geom_errorbar(aes(ymin = slope - slope_se, ymax = slope + slope_se,
                    color = seg_class),
                width = 0.15, linewidth = 0.4, na.rm = TRUE) +
  geom_text(aes(label = segment, color = seg_class),
            fontface = "bold", size = 4, show.legend = FALSE) +
  scale_color_manual(values = class_colors, na.value = "grey70",
                     name = "Dominant flood\nclass (shallow -> deep)") +
  # inverse hyperbolic sine: linear near 0, log-like for large slopes, keeps the sign
  scale_y_continuous(transform = scales::transform_asinh(),
                     breaks = c(-1e5, -1e4, -1e3, -100, -10, -1, 0, 1, 10, 100, 1e3, 1e4, 1e5),
                     labels = scales::label_comma(drop0trailing = TRUE)) +
  facet_wrap(~variable, scales = "free_y", nrow=1)  +
  theme_spring() +
  theme(legend.position = "right")


plot_grid(a,b, ncol=1, rel_heights = c(1, 0.35))
