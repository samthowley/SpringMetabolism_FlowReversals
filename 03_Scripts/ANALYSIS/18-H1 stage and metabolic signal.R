source('03_Scripts/ANALYSIS/00-test helpers.R')

# H1: the metabolic signal becomes less productive with increasing stage, then halts
# Reads 04_Outputs/breakpoints.csv (one row per site x variable, from script 11).
# The breakpoint fits are the test: this script summarises them across sites.
# With 6 sites, results are counts and a site table, not p-values you can lean on.
#
# Nothing is written to disk: results print in the console, plots in the Plots pane, tables in the Viewer.

#end behaviour at the top of the stage range########
# the last segment is the one at the highest stage: slope3 if it exists, else slope2, else slope1
h1.sites <- breakpoints %>%
  mutate(
    final.slope = coalesce(slope3, slope2, slope1),
    final.se    = coalesce(slope3.se, slope2.se, slope1.se),
    first.slope = slope1,
    first.se    = slope1.se,
    # flat = 95% CI includes 0 (same rule as script 11)
    final.dir = case_when(
      is.na(final.slope) ~ NA_character_,
      final.slope < -1.96 * final.se ~ "down",
      final.slope >  1.96 * final.se ~ "up",
      TRUE ~ "flat"),
    first.dir = case_when(
      is.na(first.slope) ~ NA_character_,
      first.slope < -1.96 * first.se ~ "down",
      first.slope >  1.96 * first.se ~ "up",
      TRUE ~ "flat"),
    # stage as a fraction of the site's observed range, so sites are comparable
    bp1.rel  = (bp1 - depth.min) / (depth.max - depth.min),
    bp2.rel  = (bp2 - depth.min) / (depth.max - depth.min),
    turn.rel = (turn.stage - depth.min) / (depth.max - depth.min),
    halt.rel = (halting.stage - depth.min) / (depth.max - depth.min)
  ) %>%
  arrange(variable, vulnerable.score)


#summary by variable########
h1.summary <- h1.sites %>%
  group_by(variable) %>%
  summarise(
    n.sites           = n(),
    n.linear          = sum(n.breakpoints == 0),
    n.one.bp          = sum(n.breakpoints == 1),
    n.two.bp          = sum(n.breakpoints == 2),
    n.decline.at.top  = sum(final.dir == "down", na.rm = TRUE),   # still falling at the highest stage
    n.flat.at.top     = sum(final.dir == "flat", na.rm = TRUE),
    n.rise.at.top     = sum(final.dir == "up", na.rm = TRUE),
    n.rise.first      = sum(first.dir == "up", na.rm = TRUE),      # initial increase (river-water contact?)
    n.with.turn       = sum(!is.na(turn.stage)),
    median.turn.rel   = median(turn.rel, na.rm = TRUE),
    n.halted          = sum(halted %in% TRUE),
    median.halt.stage = median(halting.stage, na.rm = TRUE),
    median.adj.r2     = median(adj.r2, na.rm = TRUE),
    .groups = "drop"
  )

# GPP: do sites decline with stage? exact sign test on the number of sites falling at the top
# (6 sites at most, so the smallest possible p is 0.03: treat as a count, not strong evidence)
gpp <- h1.sites %>% filter(variable == "GPP", !is.na(final.dir))
if (nrow(gpp) > 0) {
  n.dec <- sum(gpp$final.dir == "down")
  sign.p <- binom.test(n.dec, nrow(gpp), p = 0.5, alternative = "greater")$p.value
  h1.summary <- h1.summary %>%
    mutate(gpp.sign.test.p = if_else(variable == "GPP", sign.p, NA_real_))
}


#what H1 says for GPP########
message("\nH1, GPP by site (ordered by vulnerability):")
print(h1.sites %>% filter(variable == "GPP") %>%
        select(ID, pattern, n.breakpoints, turn.stage, turn.type, halted, halting.stage), n = Inf)
message("\nH1 summary by variable:")
print(h1.summary, width = Inf)

#figure: where are the thresholds?########
# stage of each breakpoint by site, as a fraction of that site's stage range
thresh <- h1.sites %>%
  select(variable, ID, bp1.rel, bp2.rel, halted, halt.rel) %>%
  pivot_longer(c(bp1.rel, bp2.rel), names_to = "bp", values_to = "stage.rel") %>%
  filter(!is.na(stage.rel)) %>%
  mutate(bp = recode(bp, bp1.rel = "breakpoint 1", bp2.rel = "breakpoint 2"),
         halting = (halted %in% TRUE) & coalesce(abs(stage.rel - halt.rel) < 1e-9, FALSE))

p.thresh <- ggplot(thresh, aes(x = ID, y = stage.rel)) +
  geom_point(aes(shape = bp, fill = halting), size = 3.2, color = "black", stroke = 0.7) +
  scale_shape_manual(values = c("breakpoint 1" = 21, "breakpoint 2" = 24), name = NULL) +
  scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "black"),
                    name = "GPP halting\nstage", labels = c("no", "yes")) +
  scale_x_discrete(limits = site.order, drop = FALSE) +
  facet_wrap(~variable, nrow = 1) +
  labs(x = "Site (low to high vulnerability)", y = "Breakpoint stage (fraction of site range)") +
  theme_spring()

print(p.thresh)
