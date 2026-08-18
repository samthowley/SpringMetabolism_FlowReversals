#source('03_Scripts/ANALYSIS/analysis prep.R')

#library(tidyverse)
#library(patchwork)
#library(segmented)
select <- dplyr::select

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom"
    )
}

df <- left_join(
  chem_hourly %>%
    select(ID, Date, DO, CO2, depth) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarise(DO    = mean(DO,    na.rm = TRUE),
              CO2   = mean(CO2,   na.rm = TRUE),
              depth = mean(depth, na.rm = TRUE),
              .groups = "drop"),
  metab %>% rename(Date = Date) %>% select(-depth, -K600) %>%
    distinct(ID, Date, .keep_all = TRUE) %>%
    mutate(NEP = GPP + ER),
  by = c("Date", "ID"),
  relationship = "one-to-one"
) %>% arrange(ID, Date)

# ── Join diagnostics (remove once IU is confirmed working) ───────────────────
message("IDs in chem_hourly : ", paste(sort(unique(chem_hourly$ID)), collapse = ", "))
message("IDs in metab       : ", paste(sort(unique(metab$ID)),        collapse = ", "))
message("IDs in df          : ", paste(sort(unique(df$ID)),           collapse = ", "))
df %>%
  group_by(ID) %>%
  summarise(n_rows  = n(),
            n_depth = sum(!is.na(depth)),
            n_CO2   = sum(!is.na(CO2)),
            n_GPP   = sum(!is.na(GPP)),
            n_ER    = sum(!is.na(ER)),
            .groups = "drop") %>%
  print()


master_long <- df %>%
  pivot_longer(cols = c(GPP, ER, NEP, DO, CO2),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(depth)) %>%
  left_join(peak_dates, by = join_by(ID, Date), relationship = "many-to-many") %>%
  arrange(ID, depth)%>%
  group_by(ID)%>%
  #fill(class, .direction = 'down')%>%
  mutate(
    class=if_else(is.na(class), 'baseline', class),
    variable = factor(variable, levels = c("depth", "DO", "CO2", "GPP", "ER", "NEP")),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS'))
  )
unique(master_long$ID)

# ── Model-selection parameters ────────────────────────────────────────────────
# fit_criterion: "adjR2" (default) | "AIC" | "BIC"
#   adjR2 — 3 bp wins only when adj-R² improves by >= adj_r2_min_gain
#   AIC   — 3 bp wins when ΔAIC > aic_min_gain (conventional threshold = 2)
#   BIC   — 3 bp wins when ΔBIC > 0 (BIC penalises complexity more strongly)
fit_criterion   <- "adjR2"
adj_r2_min_gain <- 0.02   # minimum adj-R² improvement to prefer 3 bp over 2 bp
aic_min_gain    <- 2      # minimum AIC *reduction* to prefer 3 bp over 2 bp


# ── Helpers ───────────────────────────────────────────────────────────────────

# Evenly-spaced interior quantiles used as breakpoint starting positions
bp_starts <- function(x, n) as.numeric(quantile(x, seq(1/(n+1), n/(n+1), length.out = n)))

# Adjusted R² for a segmented or lm object
get_adj_r2 <- function(fit) summary(fit)$adj.r.squared

# Attempt a segmented fit with n_bp breakpoints; returns NULL on failure
try_seg <- function(lm_fit, depth_vec, n_bp) {
  tryCatch(
    segmented(lm_fit, seg.Z = ~depth,
              psi     = list(depth = bp_starts(depth_vec, n_bp)),
              control = seg.control(it.max = 50, n.boot = 0)),
    error = function(e) NULL
  )
}

# Return whichever of fit1 (1 bp) / fit2 (2 bp) wins under the chosen criterion
choose_model <- function(fit1, fit2) {
  if (is.null(fit1) && is.null(fit2)) return(NULL)
  if (is.null(fit2)) return(fit1)
  if (is.null(fit1)) return(fit2)

  switch(fit_criterion,
    adjR2 = {
      if ((get_adj_r2(fit2) - get_adj_r2(fit1)) >= adj_r2_min_gain) fit2 else fit1
    },
    AIC = {
      if ((AIC(fit1) - AIC(fit2)) >= aic_min_gain) fit2 else fit1
    },
    BIC = {
      if (BIC(fit2) < BIC(fit1)) fit2 else fit1
    }
  )
}


# ── Pre-compute segmented fits ────────────────────────────────────────────────
seg_preds <- list()
seg_bps   <- list()
bp_slopes <- list()

for (var in c("GPP", "ER", "NEP", "DO", "CO2")) {
  dat_v <- df %>%
    transmute(Date, ID, depth, value = .data[[var]]) %>%
    filter(!is.na(depth), !is.na(value))

  for (site in unique(dat_v$ID)) {
    sub <- filter(dat_v, ID == site) %>% arrange(depth)
    if (nrow(sub) < 25) {
      message(sprintf("SKIP  %s × %s — only %d observations (need ≥ 25)", var, site, nrow(sub)))
      next
    }

    lm_fit <- lm(value ~ depth, data = sub)
    seg1   <- try_seg(lm_fit, sub$depth, 1)
    seg2   <- try_seg(lm_fit, sub$depth, 2)

    best <- choose_model(seg1, seg2)
    if (is.null(best)) {
      message(sprintf("SKIP  %s × %s — both segmented fits failed to converge", var, site))
      next
    }

    key   <- paste(var, site)
    bp_val <- best$psi[, "Est."]   # length = n_bp (2 or 3)

    # Predictions along depth range
    px <- seq(min(sub$depth), max(sub$depth), length.out = 300)
    py <- predict(best, newdata = data.frame(depth = px))
    seg_preds[[key]] <- tibble(variable = var, ID = site, depth = px, fitted = py)

    # Breakpoints (one row per breakpoint)
    seg_bps[[key]] <- tibble(variable = var, ID = site, breakpoint = bp_val)

    # Slopes — one row per segment; n_segments = n_bp + 1
    sl     <- slope(best)$depth
    se_col <- intersect(c("St.Err", "Std.Err"), colnames(sl))
    se_col <- if (length(se_col) > 0) se_col[1] else NULL

    if (!is.null(sl) && nrow(sl) >= 1) {
      bp_sorted    <- sort(bp_val)
      lower_bounds <- c(min(sub$depth), bp_sorted)
      upper_bounds <- c(bp_sorted, max(sub$depth))

      bp_slopes[[key]] <- tibble(
        variable      = var,
        ID            = site,
        n_breakpoints = length(bp_val),
        adj_r2        = get_adj_r2(best),
        segment       = seq_len(nrow(sl)),
        seg_lower     = lower_bounds,
        seg_upper     = upper_bounds,
        slope         = sl[, "Est."],
        slope_se      = if (!is.null(se_col)) sl[, se_col] else NA_real_
      )
    }
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

# ── Dominant flood class per segment ─────────────────────────────────────────
# Ordinal encoding: baseline=1, BO=2, HI=3, FR=4
# Weighted mean ordinal (by n distinct days per class in segment depth range),
# then rounded back to the nearest class label.
class_ord_map <- c(baseline = 1, BO = 2, HI = 3, FR = 4)
ord_to_class  <- c("1" = "baseline", "2" = "BO", "3" = "HI", "4" = "FR")

# One row per site × date × depth, with class already filled down
depth_class <- master_long %>%
  distinct(ID, Date, depth, class) %>%
  filter(!is.na(class)) %>%
  mutate(class_num = class_ord_map[class])

# Join every segment to all site observations, filter to depth range,
# count distinct days per class, compute weighted mean ordinal
seg_class <- bp_slopes_df %>%
  select(variable, ID, segment, seg_lower, seg_upper) %>%
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
  left_join(seg_class %>% select(variable, ID, segment, seg_class),
            by = c("variable", "ID", "segment"))

# write_csv(bp_slopes_df, "04_Outputs/breakpoint_slopes.csv")

# ── Breakpoint plot ───────────────────────────────────────────────────────────
(a <- master_long %>%
  filter(variable %in% c("GPP", "ER", "NEP", "DO", "CO2")) %>%
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
        legend.position = 'right'))

unique(master_long$class)
# ggsave("05_Figures/H1_fig2_breakpoints.png", fig2,
#        width = 14, height = 10, dpi = 300)


# ── Slope scatter plot ────────────────────────────────────────────────────────
# Each point = one segment; colour = dominant flood class for that segment's
# depth range (weighted mean ordinal, rounded); shape = segment order
# Grey lines connect segments within the same site to show slope progression
criterion_caption <- switch(fit_criterion,
  adjR2 = paste0("adjR² (min gain = ", adj_r2_min_gain, ")"),
  AIC   = paste0("AIC (min reduction = ", aic_min_gain, ")"),
  BIC   = "BIC"
)

b <- bp_slopes_df %>%
    mutate(
      variable = factor(variable, levels = c("depth", "DO", "CO2", "NEP", "GPP", "ER")),
      ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS'))
    )%>%
  ggplot(aes(x = ID, y = slope, color = seg_class, group = ID)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # lines connecting segments within a site
  geom_line(color = "grey75", linewidth = 0.5) +
  # error bars drawn before text so text sits on top
  geom_errorbar(aes(ymin = slope - slope_se, ymax = slope + slope_se,
                    color = seg_class),
                width = 0.15, linewidth = 0.4, na.rm = TRUE) +
  # segment number as the point marker; shallow -> deep reads 1 → n
  geom_text(aes(label = segment, color = seg_class),
            fontface = "bold", size = 4, show.legend = FALSE) +
  scale_color_manual(values = class_colors, na.value = "grey70",
                     name = "Dominant flood\nclass (shallow -> deep)") +
  facet_wrap(~variable, scales = "free_y", nrow=1)  +
  theme_spring() +
  theme(legend.position = "right")


plot_grid(a,b, ncol=1, rel_heights = c(1, 0.35))
