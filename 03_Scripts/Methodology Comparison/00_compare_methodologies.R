rm(list = ls())
source("03_Scripts/Methodology Comparison/_engine_two_station.R")

outdir <- "04_Outputs/Methodology Comparison"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

## ---- the six uniform methodologies ------------------------------------------
methods <- tribble(
  ~label,                       ~vel_form, ~k600_src,   ~k600_form,
  "M1_velPower_K600power",      "power",   "RC",        "power",
  "M2_velPower_K600linear",     "power",   "RC",        "linear",
  "M3_velPower_K600freewater",  "power",   "freewater",  NA,
  "M4_velLinear_K600power",     "linear",  "RC",        "power",
  "M5_velLinear_K600linear",    "linear",  "RC",        "linear",
  "M6_velLinear_K600freewater", "linear",  "freewater",  NA
)

runs <- map(seq_len(nrow(methods)), function(i) {
  m <- methods[i, ]
  message("running ", m$label, " ...")
  run_two_station(uniform_recipe(vel_form  = m$vel_form,
                                 k600_src  = m$k600_src,
                                 k600_form = m$k600_form,
                                 k600_pred = "depth",
                                 excl      = "base"),
                  m$label)
})
names(runs) <- methods$label

## ---- benchmark: the per-site tuned recipe (Scratch Pad/19) ------------------
## Not a uniform methodology -- each site got its own best combination. Included
## so the uniform runs have something to be measured against.
recipe_final <- tribble(
  ~ID,  ~units,   ~vel_form, ~vel_excl, ~k600_src,   ~k600_pred, ~k600_form, ~k600_excl,
  "AM", "county", "linear",  "strict",  "RC",        "depth",    "linear",   "strict",
  "GB", "county", "power",   "strict",  "RC",        "velocity", "M6",       "base",
  "LF", "county", "power",   "base",    "RC",        "depth",    "linear",   "base",
  "ID", "county", "power",   "base",    "freewater",  NA,         NA,        NA,
  "OS", "county", "linear",  "base",    "RC",        "depth",    "M8",       "base"
)
message("running PERSITE_FINAL (benchmark) ...")
runs[["PERSITE_FINAL"]] <- run_two_station(recipe_final, "PERSITE_FINAL")

## ---- score everything -------------------------------------------------------
daily_all <- map_dfr(runs, "daily")
scores    <- score_methodology(daily_all) %>%
  mutate(methodology = factor(methodology, levels = c(methods$label, "PERSITE_FINAL"))) %>%
  arrange(ID, methodology)

## the headline table, one row per site x methodology
success_table <- scores %>%
  select(ID, methodology, n_days, pct_in_range, pct_reach, pct_both,
         K600_mean, GPP_mean, ER_mean, K600_med, GPP_med, ER_med)

print(success_table, n = Inf)

## a compact all-site roll-up: how does each methodology do overall?
overall <- daily_all %>%
  mutate(methodology = factor(methodology, levels = c(methods$label, "PERSITE_FINAL"))) %>%
  score_methodology() %>%
  group_by(methodology) %>%
  summarise(across(c(pct_in_range, pct_reach, pct_both), ~round(mean(.x), 1)),
            .groups = "drop") %>%
  arrange(desc(pct_both))

## unweighted mean across the 5 sites -- each site counts once regardless of
## how many days it contributed, so a long record can't dominate the ranking.
print(overall)

## ---- MATCHED-DAYS table (read this one before drawing conclusions) ----------
## The free-water methodologies only produce K600 on days the Stan runs
## converged (Rhat < 1.05), so M3/M6 cover far fewer days than M1/M2/M4/M5 --
## at GB, 41 days vs 716. Percentages off different day sets are not comparable:
## a methodology can look good simply by being scored on easier days.
## This rescores every methodology on ONLY the days all six can produce.
common_days <- daily_all %>%
  filter(methodology %in% methods$label) %>%
  distinct(methodology, ID, date) %>%
  count(ID, date) %>%
  filter(n == nrow(methods)) %>%
  select(ID, date)

matched_table <- daily_all %>%
  semi_join(common_days, by = c("ID", "date")) %>%
  mutate(methodology = factor(methodology, levels = c(methods$label, "PERSITE_FINAL"))) %>%
  score_methodology() %>%
  select(ID, methodology, n_days, pct_in_range, pct_reach, pct_both,
         K600_mean, GPP_mean, ER_mean, K600_med, GPP_med, ER_med) %>%
  arrange(ID, methodology)

cat("\n==== MATCHED DAYS (same days for every methodology) ====\n")
print(matched_table, n = Inf)


## ---- plots ------------------------------------------------------------------
## 1. the success metric itself, site x methodology
success_table %>%
  select(ID, methodology, pct_in_range, pct_reach, pct_both) %>%
  pivot_longer(starts_with("pct_"), names_to = "metric", values_to = "pct") %>%
  mutate(metric = factor(metric, levels = c("pct_in_range", "pct_reach", "pct_both"),
                         labels = c("in plausible range", "reach test passes", "both")),
         ID = factor(ID, levels = sites)) %>%
  ggplot(aes(x = methodology, y = pct, fill = metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~ID, ncol = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Methodology success by site",
       subtitle = paste0("plausible = GPP [", GPP_RANGE[1], ",", GPP_RANGE[2],
                         "] and ER [", ER_RANGE[1], ",", ER_RANGE[2], "]"),
       x = NULL, y = "% of days") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

## 2. the GPP/ER time series under every methodology, one panel per site
daily_all %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  ggplot(aes(x = date, y = GPP, color = methodology)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = GPP_RANGE[1], ymax = GPP_RANGE[2], fill = "grey50", alpha = 0.15) +
  geom_point(size = 0.4, alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol = 1) +
  labs(title = "GPP under each methodology", x = NULL, y = "GPP (g O2 m-2 d-1)") +
  theme_bw(base_size = 9)+
  theme(legend.position = "bottom")

daily_all %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  ggplot(aes(x = date, y = ER, color = methodology)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = ER_RANGE[1], ymax = ER_RANGE[2], fill = "grey50", alpha = 0.15) +
  geom_point(size = 0.4, alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol = 1) +
  labs(title = "ER under each methodology", x = NULL, y = "ER (g O2 m-2 d-1)") +
  theme_bw(base_size = 9)+
  theme(legend.position = "bottom")

## 3. what K600 each methodology actually hands the mass balance
daily_all %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  ggplot(aes(x = methodology, y = K600, fill = methodology)) +
  geom_boxplot(outlier.size = 0.4) +
  facet_wrap(~ID, scales = "free_y", ncol = 1) +
  labs(title = "K600 delivered by each methodology", x = NULL, y = "K600 (1/day)") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none")

