source('03_Scripts/ANALYSIS/17-test helpers.R')

# H2: less-disturbed springs tolerate floods better (smaller metabolic impact)
# Reads 04_Outputs/flood impacts/flood_metrics.csv (one row per site x flood x variable).
#
# Part A  metric ~ vulnerable.score + h.percent.change + (1|ID), one model per variable x metric
#         vulnerability is one number per site, so there is no site term. Only 6 sites:
#         read the estimates and CIs, not the p-values alone.
# Part B  site-mean check: rank correlation of site mean metric with vulnerability
# Part C  headline: are GPP / ER less impacted than DO / CO2?
#         abs.percent.change ~ variable + (1|ID) + (1|flood.id)
#
# Outputs (04_Outputs/tests/): H2_vulnerability_models.csv, H2_site_rank_cor.csv,
#   H2_headline_means.csv, H2_headline_contrasts.csv, H2_vs_vulnerability.png, H2_headline.png

#parameters########
drop.unrecovered <- TRUE   # duration-type metrics are censored when the variable never recovered
metrics     <- c("abs.percent.change", "duration", "duration.rel", "severity", "time.to.recover")
log.metrics <- c("duration", "duration.rel", "severity", "time.to.recover")   # modelled on the log scale
censored    <- c("duration", "duration.rel", "severity", "time.to.recover")

#long format########
h2 <- flood.metrics %>%
  select(ID, flood, flood.id, variable, class, vulnerable.score, h.percent.change,
         flood.recovered, all_of(metrics)) %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
  mutate(
    value = if_else(drop.unrecovered & metric %in% censored & flood.recovered %in% FALSE,
                    NA_real_, value),
    # log-scale metrics: non-positive values cannot be logged
    log.scale = metric %in% log.metrics,
    value = case_when(log.scale & value > 0 ~ log(value),
                      log.scale ~ NA_real_,
                      TRUE ~ value),
    metric.label = if_else(log.scale, paste0("log(", metric, ")"), metric)
  ) %>%
  filter(!is.na(value))

#Part A: vulnerability models########
fits <- h2 %>%
  group_by(variable, metric, metric.label) %>%
  group_split()

h2.models <- map_dfr(fits, function(d) {
  v <- as.character(d$variable[1]); m <- d$metric[1]
  message("H2 model: ", v, " ", m)
  fit <- safe_lmer(value ~ vulnerable.score + h.percent.change + (1|ID), data = d)
  tidy_lmer(fit) %>%
    mutate(variable = v, metric = m, metric.label = d$metric.label[1], .before = 1)
})

# adjust the vulnerability p-values across the 4 variables within each metric
h2.models <- h2.models %>%
  group_by(metric, term) %>%
  mutate(p.holm = p.adjust(p, method = "holm")) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels = var.order)) %>%
  arrange(metric, variable, term)

write_csv(h2.models, paste0(out.dir, "H2_vulnerability_models.csv"))

message("\nH2, effect of vulnerability (one step up the score) on each metric:")
print(h2.models %>% filter(term == "vulnerable.score") %>%
        select(variable, metric.label, estimate, ci.low, ci.high, p, p.holm, n, n.sites, singular),
      n = Inf, width = Inf)

#Part B: site-mean rank correlation########
site.means <- h2 %>%
  group_by(variable, metric.label, ID, vulnerable.score) %>%
  summarise(site.mean = mean(value), n.floods = n(), .groups = "drop")

h2.rank <- site.means %>%
  group_by(variable, metric.label) %>%
  group_modify(~{
    if (n_distinct(.x$ID) < 4) return(tibble(rho = NA_real_, p = NA_real_, n.sites = n_distinct(.x$ID)))
    ct <- suppressWarnings(cor.test(.x$vulnerable.score, .x$site.mean, method = "spearman"))
    tibble(rho = unname(ct$estimate), p = ct$p.value, n.sites = nrow(.x))
  }) %>%
  ungroup()

write_csv(h2.rank, paste0(out.dir, "H2_site_rank_cor.csv"))

#Part C: headline, regime (GPP, ER) vs raw signal (DO, CO2)########
head.d <- flood.metrics %>%
  select(ID, flood, flood.id, variable, abs.percent.change) %>%
  drop_na()

m.head <- safe_lmer(abs.percent.change ~ variable + (1|ID) + (1|flood.id), data = head.d)

if (!is.null(m.head)) {
  print(anova(m.head))
  em <- emmeans(m.head, ~variable)
  lv <- levels(head.d$variable)

  head.means <- as_tibble(summary(em, infer = c(TRUE, TRUE)))
  write_csv(head.means, paste0(out.dir, "H2_headline_means.csv"))

  # expectation: GPP / ER less impacted, so regime minus raw signal is negative
  regime.coef <- setNames(ifelse(lv %in% c("GPP", "ER"), 0.5, -0.5), lv)
  head.contrasts <- bind_rows(
    as_tibble(summary(contrast(em, list("regime - raw signal" = regime.coef)), infer = c(TRUE, TRUE))),
    as_tibble(summary(pairs(em), infer = c(TRUE, TRUE)))
  )
  write_csv(head.contrasts, paste0(out.dir, "H2_headline_contrasts.csv"))

  message("\nH2 headline, mean absolute % change by variable:")
  print(head.means, width = Inf)
  message("\nContrasts (regime - raw signal < 0 means GPP / ER less impacted):")
  print(head.contrasts, width = Inf)
}

#figures########
p.vuln <- h2 %>%
  filter(metric %in% c("abs.percent.change", "duration", "severity")) %>%
  ggplot(aes(x = vulnerable.score, y = value)) +
  geom_jitter(aes(color = ID), width = 0.12, alpha = 0.6, size = 1.8) +
  stat_summary(fun = mean, geom = "point", shape = 95, size = 8, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.6, alpha = 0.15) +
  scale_color_manual(values = site_colors) +
  scale_x_continuous(breaks = 1:6, labels = site.order) +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Site (low to high vulnerability)", y = NULL, color = "Site") +
  theme_spring()
ggsave(paste0(out.dir, "H2_vs_vulnerability.png"), p.vuln, width = 10, height = 7.5, dpi = 200)

p.head <- head.d %>%
  ggplot(aes(x = variable, y = abs.percent.change)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = ID), width = 0.15, alpha = 0.7, size = 2) +
  scale_color_manual(values = site_colors) +
  labs(x = NULL, y = "Absolute % change from baseline", color = "Site") +
  theme_spring()
ggsave(paste0(out.dir, "H2_headline.png"), p.head, width = 6, height = 4, dpi = 200)

plot_grid(p.head, p.vuln, ncol = 1, rel_heights = c(0.4, 1))
