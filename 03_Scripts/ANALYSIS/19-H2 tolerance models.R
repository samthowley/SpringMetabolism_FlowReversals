source('03_Scripts/ANALYSIS/00-test helpers.R')

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
# Nothing is written to disk: results print in the console, plots in the Plots pane, tables in the Viewer.

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

  # expectation: GPP / ER less impacted, so regime minus raw signal is negative
  regime.coef <- setNames(ifelse(lv %in% c("GPP", "ER"), 0.5, -0.5), lv)
  head.contrasts <- bind_rows(
    as_tibble(summary(contrast(em, list("regime - raw signal" = regime.coef)), infer = c(TRUE, TRUE))),
    as_tibble(summary(pairs(em), infer = c(TRUE, TRUE)))
  )

  message("\nH2 headline, mean absolute % change by variable:")
  print(head.means, width = Inf)
  message("\nContrasts (regime - raw signal < 0 means GPP / ER less impacted):")
  print(head.contrasts, width = Inf)
}

#tables########
# flextables for each part (they show in the RStudio Viewer).
# Model terms are in their own columns; bold p = below 0.05.
library(flextable)
set_flextable_defaults(font.size = 8.5, padding = 2)

fmt_p   <- function(p) case_when(is.na(p) ~ "", p < 0.001 ~ "<0.001", TRUE ~ formatC(p, format = "f", digits = 3))
fmt_num <- function(x) ifelse(is.na(x), "", formatC(x, format = "fg", digits = 3))
fmt_est <- function(e, lo, hi) ifelse(is.na(e), "", paste0(fmt_num(e), " [", fmt_num(lo), ", ", fmt_num(hi), "]"))
bold_sig <- function(ft, tab, pcol, key) bold(ft, i = which(tab[[pcol]] < 0.05), j = key)

h2.tables <- list()

#Part A: vulnerability models (one row per metric x variable)
term.key <- c("(Intercept)" = "int", "vulnerable.score" = "vuln", "h.percent.change" = "stage")

tabA <- h2.models %>%
  mutate(term = unname(term.key[term])) %>%
  select(metric.label, variable, term, estimate, ci.low, ci.high, p, n, n.sites, singular) %>%
  pivot_wider(id_cols = c(metric.label, variable, n, n.sites, singular),
              names_from = term, values_from = c(estimate, ci.low, ci.high, p),
              names_glue = "{.value}.{term}") %>%
  left_join(h2.models %>% filter(term == "vulnerable.score") %>%
              select(metric.label, variable, p.holm.vuln = p.holm),
            by = c("metric.label", "variable")) %>%
  arrange(factor(metric.label, levels = unique(h2.models$metric.label)), variable)

tabA.fmt <- tabA %>%
  transmute(
    metric.label, variable,
    int.est   = fmt_est(estimate.int,   ci.low.int,   ci.high.int),   int.p   = fmt_p(p.int),
    vuln.est  = fmt_est(estimate.vuln,  ci.low.vuln,  ci.high.vuln),  vuln.p  = fmt_p(p.vuln),
    vuln.holm = fmt_p(p.holm.vuln),
    stage.est = fmt_est(estimate.stage, ci.low.stage, ci.high.stage), stage.p = fmt_p(p.stage),
    n, n.sites, singular = if_else(singular, "yes", ""))

ft.A <- tabA.fmt %>%
  flextable() %>%
  set_header_labels(metric.label = "Metric", variable = "Variable",
                    int.est = "Estimate [95% CI]", int.p = "p",
                    vuln.est = "Estimate [95% CI]", vuln.p = "p", vuln.holm = "p (Holm)",
                    stage.est = "Estimate [95% CI]", stage.p = "p",
                    n = "Floods", n.sites = "Sites", singular = "Singular fit") %>%
  add_header_row(values = c("", "Intercept", "Vulnerability score (per step)", "Stage change (per 1%)", "Model"),
                 colwidths = c(2, 2, 3, 2, 3)) %>%
  merge_v(j = "metric.label") %>%
  valign(j = "metric.label", valign = "top", part = "body") %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = c("metric.label", "variable"), align = "left", part = "all") %>%
  bold(part = "header") %>%
  bold_sig(tabA, "p.int", "int.p") %>%
  bold_sig(tabA, "p.vuln", "vuln.p") %>%
  bold_sig(tabA, "p.holm.vuln", "vuln.holm") %>%
  bold_sig(tabA, "p.stage", "stage.p") %>%
  add_footer_lines("metric ~ vulnerable.score + h.percent.change + (1|ID). Duration-type metrics are on the log scale. Holm adjusts the vulnerability p across the 4 variables within a metric. Only 6 sites: read the CI, not just the p.") %>%
  autofit()
h2.tables[[length(h2.tables) + 1]] <- ft.A %>% set_caption(caption = "Table H2-A. Effect of site vulnerability and stage change on flood impact metrics") %>% fit_to_width(max_width = 9.5)

#Part B: site-mean rank correlation (metric rows, variable column groups)
tabB <- h2.rank %>%
  mutate(variable = as.character(variable)) %>%
  pivot_wider(id_cols = metric.label, names_from = variable,
              values_from = c(rho, p), names_glue = "{variable}.{.value}")

tabB.fmt <- tabB %>% select(metric.label)
for (v in var.order) {
  tabB.fmt[[paste0(v, ".rho")]] <- fmt_num(tabB[[paste0(v, ".rho")]])
  tabB.fmt[[paste0(v, ".p")]]   <- fmt_p(tabB[[paste0(v, ".p")]])
}

ft.B <- tabB.fmt %>%
  flextable() %>%
  set_header_labels(metric.label = "Metric",
                    GPP.rho = "rho", GPP.p = "p", ER.rho = "rho", ER.p = "p",
                    DO.rho = "rho", DO.p = "p", CO2.rho = "rho", CO2.p = "p") %>%
  add_header_row(values = c("", var.order), colwidths = c(1, 2, 2, 2, 2)) %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = "metric.label", align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_lines("Spearman rank correlation of the site mean metric with the vulnerability score (n = 6 sites; exact p).") %>%
  autofit()
for (v in var.order) ft.B <- bold_sig(ft.B, tabB, paste0(v, ".p"), paste0(v, ".p"))
h2.tables[[length(h2.tables) + 1]] <- ft.B %>% set_caption(caption = "Table H2-B. Site-mean rank correlation with vulnerability") %>% fit_to_width(max_width = 9.5)

#Part C: headline, regime vs raw signal
if (!is.null(m.head)) {
  an <- as.data.frame(anova(m.head))
  tabC0 <- tibble(term = "Variable (GPP, ER, DO, CO2)", F = an$`F value`, NumDF = an$NumDF,
                  DenDF = an$DenDF, p = an$`Pr(>F)`)
  ft.C0 <- tabC0 %>%
    mutate(F = fmt_num(F), NumDF = fmt_num(NumDF), DenDF = fmt_num(DenDF), p = fmt_p(p)) %>%
    flextable() %>%
    set_header_labels(term = "Term", F = "F", NumDF = "Num. df", DenDF = "Den. df", p = "p") %>%
    theme_booktabs() %>% align(align = "center", part = "all") %>%
    align(j = "term", align = "left", part = "all") %>% bold(part = "header") %>%
    bold(i = which(tabC0$p < 0.05), j = "p") %>%
    add_footer_lines("abs.percent.change ~ variable + (1|ID) + (1|flood.id); Satterthwaite df.") %>%
    autofit()
  h2.tables[[length(h2.tables) + 1]] <- ft.C0 %>% set_caption(caption = "Table H2-C1. Do the four variables differ in impact?") %>% fit_to_width(max_width = 9.5)

  ft.C1 <- head.means %>%
    transmute(variable, mean = fmt_num(emmean), SE = fmt_num(SE), df = fmt_num(df),
              ci = paste0("[", fmt_num(lower.CL), ", ", fmt_num(upper.CL), "]")) %>%
    flextable() %>%
    set_header_labels(variable = "Variable", mean = "Mean |% change|", SE = "SE", df = "df", ci = "95% CI") %>%
    theme_booktabs() %>% align(align = "center", part = "all") %>%
    align(j = "variable", align = "left", part = "all") %>% bold(part = "header") %>%
    autofit()
  h2.tables[[length(h2.tables) + 1]] <- ft.C1 %>% set_caption(caption = "Table H2-C2. Mean absolute % change from baseline by variable") %>% fit_to_width(max_width = 9.5)

  ft.C2 <- head.contrasts %>%
    transmute(contrast, estimate = fmt_num(estimate), SE = fmt_num(SE), df = fmt_num(df),
              ci = paste0("[", fmt_num(lower.CL), ", ", fmt_num(upper.CL), "]"), p = fmt_p(p.value)) %>%
    flextable() %>%
    set_header_labels(contrast = "Contrast", estimate = "Estimate", SE = "SE", df = "df",
                      ci = "95% CI", p = "p") %>%
    theme_booktabs() %>% align(align = "center", part = "all") %>%
    align(j = "contrast", align = "left", part = "all") %>% bold(part = "header") %>%
    bold(i = which(head.contrasts$p.value < 0.05), j = "p") %>%
    add_footer_lines("Regime - raw signal = mean of GPP and ER minus mean of DO and CO2 (negative = regime less impacted). Pairwise rows are Tukey-adjusted.") %>%
    autofit()
  h2.tables[[length(h2.tables) + 1]] <- ft.C2 %>% set_caption(caption = "Table H2-C3. Regime vs raw signal and pairwise contrasts") %>% fit_to_width(max_width = 9.5)
}

# show all the tables on one page in the RStudio Viewer
htmltools::html_print(htmltools::tagList(lapply(h2.tables, flextable::htmltools_value)))
