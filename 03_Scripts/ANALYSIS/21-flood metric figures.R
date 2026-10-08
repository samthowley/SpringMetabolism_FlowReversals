source('03_Scripts/ANALYSIS/00-test helpers.R')

# Simple figures of the flood impact metrics (one row per site x flood x variable).
# Reads 04_Outputs/flood impacts/flood_metrics.csv through the test helpers.
#
# Fig 1  scatter: metric vs site vulnerability score, one panel per metric x variable (as in H2)
# Fig 2  boxplots: metric by flood class
# Fig 3  boxplots: metric by site, sites ordered by vulnerability
#
# Outputs (04_Outputs/tests/): fig_scatter_vulnerability.png, fig_box_class.png, fig_box_site.png

#parameters########
drop.unrecovered <- TRUE   # duration-type metrics are censored when the variable never recovered
metrics     <- c("abs.percent.change", "duration", "severity", "time.to.recover")
log.metrics <- c("duration", "severity", "time.to.recover")   # plotted on the log scale (non-positive values dropped)
censored    <- c("duration", "severity", "time.to.recover")

#long format########
figs <- flood.metrics %>%
  select(ID, flood, variable, class, vulnerable.score, flood.recovered, all_of(metrics)) %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
  mutate(
    value = if_else(drop.unrecovered & metric %in% censored & flood.recovered %in% FALSE,
                    NA_real_, value),
    log.scale = metric %in% log.metrics,
    value = case_when(log.scale & value > 0 ~ log(pmax(value, 1e-9)),
                      log.scale ~ NA_real_,
                      TRUE ~ value),
    metric.label = factor(if_else(log.scale, paste0("log(", metric, ")"), metric),
                          levels = if_else(metrics %in% log.metrics, paste0("log(", metrics, ")"), metrics))
  ) %>%
  filter(!is.na(value))

#Fig 1: scatter vs vulnerability########
p.scatter <- figs %>%
  ggplot(aes(x = vulnerable.score, y = value)) +
  geom_jitter(aes(color = class), width = 0.12, alpha = 0.6, size = 1.8) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.6, alpha = 0.15) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  scale_x_continuous(breaks = 1:6, labels = site.order) +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Site (low to high vulnerability)", y = NULL, color = "Flood class") +
  theme_spring()
ggsave(paste0(out.dir, "fig_scatter_vulnerability.png"), p.scatter, width = 10, height = 9, dpi = 200)

#Fig 2: boxplots by flood class########
p.class <- figs %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Flood class", y = NULL, color = "Flood class") +
  theme_spring()
ggsave(paste0(out.dir, "fig_box_class.png"), p.class, width = 10, height = 9, dpi = 200)

#Fig 3: boxplots by site, ordered by vulnerability########
p.site <- figs %>%
  ggplot(aes(x = ID, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Site (low to high vulnerability)", y = NULL, color = "Flood class") +
  theme_spring()
ggsave(paste0(out.dir, "fig_box_site.png"), p.site, width = 10, height = 9, dpi = 200)

p.scatter
p.class
p.site
