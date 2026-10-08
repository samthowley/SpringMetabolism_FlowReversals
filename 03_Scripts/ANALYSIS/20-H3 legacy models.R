source('03_Scripts/ANALYSIS/17-test helpers.R')

# H3: backwater floods leave lasting (legacy) metabolic effects
# Reads 04_Outputs/flood impacts/flood_metrics.csv (one row per site x flood x variable).
#
# Part 1  is there a legacy?
#   1a  offset.pct.impact ~ 1 + (1|ID): is the post-flood window still shifted vs the pre-flood window?
#       offset.pct.impact > 0 = still shifted in the flood-response direction (GPP, DO lower; CO2, ER higher)
#   1b  offset.pct.impact ~ class + h.percent.change + (1|ID): each class mean vs 0, FR vs the others
#       (class is partly built from DO, so read the DO rows with care)
#   1c  supporting: recovery R2, lag behind the depth flood (end.lag, peak.lag), flood.recovered
# Part 2  only if part 1 finds a legacy (you expected none):
#   offset.pct.impact ~ vulnerable.score + (1|ID): larger at the most disturbed sites?
#
# Outputs (04_Outputs/tests/): H3_offset_overall.csv, H3_offset_by_class.csv, H3_class_contrasts.csv,
#   H3_recovery_r2.csv, H3_lags.csv, H3_unrecovered.csv, H3_part2_vulnerability.csv (if run),
#   H3_offset.png, H3_verdict.txt

#parameters########
alpha        <- 0.05
min.n.recess <- 4       # recession R2 needs at least this many days
force.part2  <- FALSE   # TRUE runs part 2 even if part 1 finds no legacy

h3 <- flood.metrics

#Part 1a: overall offset########
intercept_test <- function(d, y) {
  d <- d %>% mutate(y = .data[[y]])
  fit <- safe_lmer(y ~ 1 + (1|ID), data = d, min.n = 6)
  out <- tidy_lmer(fit) %>% select(-term)
  w <- d %>% filter(!is.na(y))
  wil <- if (nrow(w) >= 6) suppressWarnings(wilcox.test(w$y, mu = 0)$p.value) else NA_real_
  if (nrow(out) == 0) out <- tibble(estimate = NA_real_, n = nrow(w))
  out %>% mutate(median = median(w$y), wilcoxon.p = wil)
}

h3.overall <- h3 %>%
  group_by(variable) %>%
  group_modify(~intercept_test(.x, "offset.pct.impact")) %>%
  ungroup() %>%
  mutate(p.holm = p.adjust(p, method = "holm"))

write_csv(h3.overall, paste0(out.dir, "H3_offset_overall.csv"))
message("\nH3 1a, mean % offset still in the flood-response direction (0 = fully recovered):")
print(h3.overall, width = Inf)

#Part 1b: by class########
h3.class <- list(); h3.contrast <- list()

for (v in var.order) {
  d <- h3 %>% filter(variable == v) %>% mutate(class = droplevels(class))
  if (n_distinct(d$class[!is.na(d$offset.pct.impact)]) < 2) next
  message("H3 1b: ", v)
  fit <- safe_lmer(offset.pct.impact ~ class + h.percent.change + (1|ID), data = d, min.n = 8)
  if (is.null(fit)) next

  em <- emmeans(fit, ~class)   # class means at the average stage change
  h3.class[[v]] <- as_tibble(summary(em, infer = c(TRUE, TRUE))) %>%   # tests each mean against 0
    mutate(variable = v, .before = 1)

  lv <- levels(d$class)
  if ("FR" %in% lv && length(lv) > 1) {
    coef <- setNames(ifelse(lv == "FR", 1, -1/(length(lv) - 1)), lv)
    h3.contrast[[v]] <- as_tibble(summary(contrast(em, list("FR - others" = coef)), infer = c(TRUE, TRUE))) %>%
      mutate(variable = v, .before = 1)
  }
}

h3.class    <- bind_rows(h3.class)
h3.contrast <- bind_rows(h3.contrast)
write_csv(h3.class, paste0(out.dir, "H3_offset_by_class.csv"))
write_csv(h3.contrast, paste0(out.dir, "H3_class_contrasts.csv"))
message("\nH3 1b, class means vs 0:"); print(h3.class, width = Inf)
message("\nH3 1b, FR vs other classes:"); print(h3.contrast, width = Inf)

#Part 1c: supporting metrics########
# recovery R2 by class
h3.r2 <- h3 %>%
  filter(n.recess >= min.n.recess) %>%
  group_by(variable, class) %>%
  summarise(n = sum(!is.na(r2.recess)), median.r2 = median(r2.recess, na.rm = TRUE),
            mean.recess.slope.z = mean(recess.slope.z, na.rm = TRUE), .groups = "drop")
write_csv(h3.r2, paste0(out.dir, "H3_recovery_r2.csv"))

# lag behind the depth flood (days; > 0 = the variable recovers / peaks after depth does)
h3.lags <- map_dfr(c("end.lag", "peak.lag"), function(y) {
  h3 %>%
    group_by(variable) %>%
    group_modify(~intercept_test(.x, y)) %>%
    ungroup() %>%
    mutate(lag = y, .before = 1)
})
write_csv(h3.lags, paste0(out.dir, "H3_lags.csv"))
message("\nH3 1c, lags behind depth (days):"); print(h3.lags %>% select(lag, variable, estimate, ci.low, ci.high, median, p, wilcoxon.p, n), n = Inf, width = Inf)

# floods that never recovered
h3.unrec <- h3 %>%
  count(variable, ID, flood.recovered) %>%
  pivot_wider(names_from = flood.recovered, values_from = n, values_fill = 0, names_prefix = "recovered_")
write_csv(h3.unrec, paste0(out.dir, "H3_unrecovered.csv"))

#verdict: is there a legacy?########
overall.hit <- h3.overall %>% filter(!is.na(p.holm), p.holm < alpha, estimate > 0)
class.hit   <- if (nrow(h3.class) > 0) h3.class %>% filter(!is.na(p.value), p.value < alpha, emmean > 0) else tibble()
legacy.found <- nrow(overall.hit) > 0 || nrow(class.hit) > 0

verdict <- paste0(
  "H3 part 1: ", if (legacy.found) "evidence of a legacy offset" else "no evidence of a legacy offset",
  " (alpha = ", alpha, "; overall tests Holm-adjusted across variables, class-mean tests unadjusted).\n",
  "Variables with an overall offset: ", if (nrow(overall.hit) > 0) paste(overall.hit$variable, collapse = ", ") else "none", "\n",
  "Class means above 0: ", if (nrow(class.hit) > 0) paste(class.hit$variable, class.hit$class, collapse = "; ") else "none", "\n",
  "Part 2 ", if (legacy.found || force.part2) "run" else "skipped (set force.part2 <- TRUE to run it anyway)", ".\n")
writeLines(verdict, paste0(out.dir, "H3_verdict.txt"))
message("\n", verdict)

#Part 2: is the legacy larger at the most disturbed sites?########
if (legacy.found || force.part2) {
  h3.part2 <- map_dfr(var.order, function(v) {
    fit <- safe_lmer(offset.pct.impact ~ vulnerable.score + (1|ID), data = h3 %>% filter(variable == v), min.n = 8)
    tidy_lmer(fit) %>% filter(term == "vulnerable.score") %>% mutate(variable = v, .before = 1)
  }) %>% mutate(p.holm = p.adjust(p, method = "holm"))
  write_csv(h3.part2, paste0(out.dir, "H3_part2_vulnerability.csv"))
  message("\nH3 part 2:"); print(h3.part2, width = Inf)
}

#figure########
p.off <- h3 %>%
  filter(!is.na(offset.pct.impact)) %>%
  ggplot(aes(x = class, y = offset.pct.impact)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_jitter(aes(color = class), width = 0.15, alpha = 0.7, size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5) +
  scale_color_manual(values = class_colors, guide = "none") +
  facet_wrap(~variable, nrow = 1, scales = "free_y") +
  labs(x = "Flood class", y = "Post-flood offset (% of pre-flood mean)\n+ = still shifted in the flood direction") +
  theme_spring()
ggsave(paste0(out.dir, "H3_offset.png"), p.off, width = 10, height = 3.8, dpi = 200)
p.off
