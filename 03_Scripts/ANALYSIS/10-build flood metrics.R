source('03_Scripts/ANALYSIS/09-analysis prep.R')

# One tidy table of flood metrics: one row per site x flood x variable
# (GPP, ER, DO, CO2). Feeds the H2 and H3 models in the second round.
#
# Before running: re-run the isolate disturbances_*.R scripts (GPP, ER, DO, CO2,
# depth) so flood.recovered, n.rise and n.recess exist in the flood impact csvs,
# then re-run analysis prep.R (sourced above).
#
# Variable definitions (daily, same as the flood response):
#   GPP, ER = daily value (ER is |ER|), DO = daily minimum, CO2 = daily maximum
# Rise / recession slopes are per sample step (hourly DO, CO2; daily GPP, ER),
# so they are z-scored within variable and only comparable within a variable.

#parameters########
window.n   <- 7                    # days in the pre / post flood windows
min.window <- ceiling(window.n/2)  # minimum days with data in a window

#daily series for the pre / post windows########
daily.vars <- bind_rows(
  metab%>%
    distinct(ID, Date, .keep_all = T)%>%
    transmute(ID, Date, GPP, ER=abs(ER))%>%
    pivot_longer(cols = c(GPP, ER), names_to = 'variable', values_to = 'value'),
  chem_hourly%>%
    mutate(Date=as.Date(Date))%>%
    filter(!is.na(DO))%>%
    group_by(ID, Date)%>%
    summarise(value=min(DO), .groups='drop')%>%
    mutate(variable='DO'),
  chem_hourly%>%
    mutate(Date=as.Date(Date))%>%
    filter(!is.na(CO2), CO2>600)%>%
    group_by(ID, Date)%>%
    summarise(value=max(CO2), .groups='drop')%>%
    mutate(variable='CO2'),
  chem_hourly%>%
    mutate(Date=as.Date(Date))%>%
    filter(!is.na(depth))%>%
    group_by(ID, Date)%>%
    summarise(value=mean(depth), .groups='drop')%>%
    mutate(variable='depth')
)%>%
  filter(!is.na(value))

#depth flood windows########
# anchored on the depth flood so pre and post sit at comparable stage, and any
# metabolic lag behind depth counts as legacy
depth.windows <- flood.response%>%
  filter(variable=='depth', !is.na(flood.start), !is.na(flood.end))%>%
  transmute(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    dep.start=as.Date(flood.start),
    dep.end=as.Date(flood.end)
  )%>%
  arrange(ID, dep.start)%>%
  group_by(ID)%>%
  mutate(
    gap.prev=as.numeric(dep.start-lag(dep.end)),
    gap.next=as.numeric(lead(dep.start)-dep.end)
  )%>%
  ungroup()%>%
  mutate(
    pre.start=dep.start-window.n,
    pre.end=dep.start-1,
    post.start=dep.end+1,
    post.end=dep.end+window.n,
    # a window is dropped if it runs into the neighbouring flood
    pre.ok=is.na(gap.prev) | gap.prev>window.n,
    post.ok=is.na(gap.next) | gap.next>window.n
  )

window.means <- depth.windows%>%
  inner_join(daily.vars, by='ID', relationship='many-to-many')%>%
  mutate(
    window=case_when(
      pre.ok & Date>=pre.start & Date<=pre.end ~ 'pre',
      post.ok & Date>=post.start & Date<=post.end ~ 'post'
    )
  )%>%
  filter(!is.na(window))%>%
  group_by(ID, flood, variable, window)%>%
  summarise(mean=mean(value), n=n(), .groups='drop')%>%
  pivot_wider(names_from=window, values_from=c(mean, n), names_sep='.')

for (col in c('mean.pre', 'mean.post', 'n.pre', 'n.post')) {
  if (!col %in% names(window.means)) window.means[[col]] <- NA_real_
}

depth.window <- window.means%>%
  filter(variable=='depth')%>%
  transmute(ID, flood, depth.pre=mean.pre, depth.post=mean.post,
            depth.diff=mean.post-mean.pre)

# offset = post-flood mean minus pre-flood mean (not the flood baseline, which is
# built from post-flood days)
# offset.pct.impact: + = still shifted in the direction of the flood response
# (DO falls during floods, CO2 rises; GPP and ER each fall in some floods and rise in
# others, so the sign comes from response.dir per flood)
response.dir <- flood.response%>%
  filter(variable!='depth')%>%
  transmute(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    variable=as.character(variable),
    response.dir
  )

offset <- window.means%>%
  filter(variable!='depth')%>%
  left_join(response.dir, by=c('ID', 'flood', 'variable'))%>%
  mutate(
    response.dir=coalesce(response.dir, if_else(variable %in% c('GPP', 'DO'), 'decrease', 'increase')),
    pre.mean=if_else(coalesce(n.pre, 0)>=min.window, mean.pre, NA_real_),
    post.mean=if_else(coalesce(n.post, 0)>=min.window, mean.post, NA_real_),
    offset=post.mean-pre.mean,
    offset.pct=if_else(pre.mean>0, offset/pre.mean*100, NA_real_),
    offset.pct.impact=offset.pct*if_else(response.dir=='decrease', -1, 1)
  )%>%
  select(ID, flood, variable, n.pre, n.post, pre.mean, post.mean, offset,
         offset.pct, offset.pct.impact)%>%
  left_join(depth.window, by=c('ID', 'flood'))%>%
  left_join(depth.windows%>%select(ID, flood, gap.prev, gap.next), by=c('ID', 'flood'))

#depth reference########
depth.ref <- flood.response%>%
  filter(variable=='depth')%>%
  transmute(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    duration.depth=duration,
    peak.Date.depth=as.Date(peak.Date),
    flood.end.depth=as.Date(flood.end)
  )

#new columns from the edited functions file########
for (col in c('flood.recovered', 'n.rise', 'n.recess')) {
  if (!col %in% names(flood.response)) {
    message(col, ' not found: re-run the isolate disturbances_*.R scripts')
    flood.response[[col]] <- NA
  }
}

#metrics########
# expected direction of the rise to peak (-1 = the variable falls, +1 = it rises);
# the recession is the reverse. Comes from response.dir, so GPP is -1 in most
# floods and +1 in the five GPP-increase floods (GPPmax script)
flood.metrics <- flood.response%>%
  filter(variable!='depth')%>%
  mutate(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    variable=as.character(variable),
    exp.dir=if_else(response.dir=='decrease', -1, 1),
    rise.ok=sign(rise.slope)==exp.dir,
    recess.ok=sign(recess.slope)==-exp.dir,
    rise.slope=if_else(rise.ok, rise.slope, NA_real_),
    r2.rise=if_else(rise.ok, r2.rise, NA_real_),
    recess.slope=if_else(recess.ok, recess.slope, NA_real_),
    r2.recess=if_else(recess.ok, r2.recess, NA_real_)
  )%>%
  group_by(variable, response.dir)%>%   # GPP rises in some floods and falls in others: z-score each separately
  mutate(
    rise.slope.z=as.numeric(scale(rise.slope)),
    recess.slope.z=as.numeric(scale(recess.slope))
  )%>%
  ungroup()%>%
  left_join(depth.ref, by=c('ID', 'flood'))%>%
  mutate(
    metric=case_when(
      variable=='DO' ~ 'daily minimum',
      variable=='CO2' ~ 'daily maximum',
      TRUE ~ 'daily value'),
    class=as.character(class),
    percent.change=reponse.percent.change,
    abs.percent.change=abs(percent.change),
    duration.rel=duration/duration.depth,
    severity=abs.percent.change*duration,
    peak.lag=as.numeric(as.Date(peak.Date)-peak.Date.depth),
    end.lag=as.numeric(as.Date(flood.end)-flood.end.depth)
  )%>%
  left_join(offset, by=c('ID', 'flood', 'variable'))%>%
  select(
    ID, flood, variable, response.dir, metric, class, vulnerable.score, h.percent.change,
    flood.start, flood.end, peak.Date, base, peak.response,
    percent.change, abs.percent.change, duration, duration.depth, duration.rel,
    severity,
    time2peak, time.to.recover, peak.lag, end.lag, flood.recovered,
    rise.slope, rise.slope.z, r2.rise, n.rise,
    recess.slope, recess.slope.z, r2.recess, n.recess,
    depth.pre, depth.post, depth.diff, gap.prev, gap.next, n.pre, n.post,
    pre.mean, post.mean, offset, offset.pct, offset.pct.impact
  )%>%
  arrange(variable, ID, flood)

write_csv(flood.metrics, "04_Outputs/flood impacts/flood_metrics.csv")

#checks########
flood.metrics%>%count(variable, ID)%>%pivot_wider(names_from = ID, values_from = n)
flood.metrics%>%count(variable, flood.recovered)
flood.metrics%>%
  group_by(variable)%>%
  summarise(
    n.floods=n(),
    n.offset=sum(!is.na(offset)),
    median.depth.diff=median(depth.diff, na.rm=T),
    median.gap.next=median(gap.next, na.rm=T))


#rise vs fall comparison########
# (was script 12) Rise = flood start to peak, fall = peak to recovery.
# Built from flood.metrics above, so slopes with the wrong sign are already
# masked together with their R2, and the z-scores are within variable and response.dir.
phase_colors <- c(Rise = "#d6604d", Fall = "#4393c3")

metric_labels <- c(
  slope.z = "Slope (z-scored)",
  r2      = "R²"
)

rise_fall <- bind_rows(

  flood.metrics %>%
    filter(time2peak > 0) %>%
    transmute(ID, flood, variable, class, vulnerable.score,
              slope.z = rise.slope.z, r2 = r2.rise, phase = "Rise"),

  flood.metrics %>%
    filter(time.to.recover > 0) %>%
    transmute(ID, flood, variable, class, vulnerable.score,
              slope.z = recess.slope.z, r2 = r2.recess, phase = "Fall")

) %>%
  mutate(
    phase    = factor(phase,    levels = c("Rise", "Fall")),
    variable = factor(variable, levels = c("DO", "CO2", "GPP", "ER")),
    ID       = factor(ID,       levels = c("IU", "ID", "GB", "LF", "AM", "OS")),
    class    = factor(class,    levels = c("HI", "BO", "FR"))
  )

rise_fall_long <- rise_fall %>%
  pivot_longer(
    cols      = c(slope.z, r2),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = factor(metric, levels = c("slope.z", "r2")))

# Table 1: mean slope.z and mean R2 by site and phase
rise_fall_slope_by_site <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(ID, variable, phase) %>%
  summarise(mean_slope_z = mean(slope.z, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = c("variable", "phase"), values_from = "mean_slope_z")

print(rise_fall_slope_by_site)

rise_fall_r2_by_site <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(ID, variable, phase) %>%
  summarise(mean_r2 = mean(r2, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = c("variable", "phase"), values_from = "mean_r2")

print(rise_fall_r2_by_site)

# Table 2: mean slope.z and R2 by flood class and phase
rise_fall_by_class <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(class, variable, phase) %>%
  summarise(
    mean_slope_z = mean(slope.z, na.rm = TRUE),
    mean_r2      = mean(r2,      na.rm = TRUE),
    .groups = "drop"
  )

print(rise_fall_by_class)

# Figure 1: rise vs fall by flood class
rise_fall_long %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = class, y = value, color = phase)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_point(
    aes(shape = ID),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 1.5, alpha = 0.7
  ) +
  scale_color_manual(name = "Phase", values = phase_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_spring() +
  facet_grid(metric ~ variable, scales = "free",
             labeller = labeller(metric = metric_labels)) +
  theme(axis.title = element_blank())

# Figure 2: rise vs fall by site
rise_fall_long %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = ID, y = value, color = phase)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_point(
    aes(shape = ID),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 1.5, alpha = 0.7
  ) +
  scale_color_manual(name = "Phase", values = phase_colors) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_spring() +
  facet_grid(metric ~ variable, scales = "free",
             labeller = labeller(metric = metric_labels)) +
  labs(x = "Increasing Flood Vulnerability") +
  theme(axis.title.y = element_blank())

# Figure 3: slope vs R2, rise vs fall, colored by class
rise_fall %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = slope.z, y = r2, color = class, shape = phase)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(name = "Class", values = class_colors) +
  scale_shape_manual(name = "Phase", values = c(Rise = 16, Fall = 1)) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "gray", linewidth = 1) +
  annotate(
    "text", x = -Inf, y = 0.37, label = "Displays Hysteresis",
    hjust = -0.02, color = "gray40", size = 3, fontface = "italic"
  ) +
  facet_wrap(~ variable, scales = "free") +
  theme_spring() +
  labs(
    x = "Slope (z-scored)",
    y = "R²"
  )
