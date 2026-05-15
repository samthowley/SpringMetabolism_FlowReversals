source('03_Scripts/ANALYSIS/analysis prep.R')


# Table 1: Mean recovery metrics by site
recovery_by_site <- flood.response %>%
  filter(time.to.recover > 0, !is.na(class), recess.slope > -10, recess.slope < 4) %>%
  group_by(ID, variable) %>%
  summarise(
    mean_time_to_recover = mean(time.to.recover, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(
    names_from = "variable",
    values_from = "mean_time_to_recover"
  )

print(recovery_by_site)


recovery_by_site <- flood.response %>%
  filter(time.to.recover > 0, !is.na(class), recess.slope > -10, recess.slope < 4) %>%
  group_by(ID, variable) %>%
  summarise(
    mean_time_to_recover = mean(time.to.recover, na.rm = TRUE),
    mean_recess_slope_z  = mean(recess.slope.z,  na.rm = TRUE),
    mean_r2_recess       = mean(r2.recess,        na.rm = TRUE),
    .groups = "drop"
  )

print(recovery_by_site)

# Table 2: Mean recovery metrics by flood class
recovery_by_class <- flood.response %>%
  filter(time.to.recover > 0, !is.na(class), recess.slope > -10, recess.slope < 4) %>%
  group_by(class, variable) %>%
  summarise(
    mean_time_to_recover = mean(time.to.recover, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(
    names_from = "class",
    values_from = "mean_time_to_recover"
  )

print(recovery_by_class)

recovery_by_class <- flood.response %>%
  filter(time.to.recover > 0, !is.na(class), recess.slope > -10, recess.slope < 4) %>%
  group_by(class, variable) %>%
  summarise(
    mean_time_to_recover = mean(time.to.recover, na.rm = TRUE),
    mean_recess_slope_z  = mean(recess.slope.z,  na.rm = TRUE),
    mean_r2_recess       = mean(r2.recess,        na.rm = TRUE),
    .groups = "drop"
  )


#Recovery##########

time.series.daily<-
  time.series%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date, variable)%>%
  mutate(
         conc=mean(conc, na.rm=T))%>%
  distinct(Date, ID, variable, flood, .keep_all = T)


o<-time.series.daily %>%
  filter(
    Date > peak.Date, !is.na(flood)) %>%
  ggplot(aes(x = flood.count, y = perc.change, group = flood, color = class)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "Class", values = class_colors) +
  new_scale_color() +
  stat_poly_line(se = FALSE, aes(color = class)) +
  # stat_poly_eq(
  #   aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = " ~~ "),
  #       color = class),
  #   formula = y ~ x, parse = TRUE,
  #   size = 4, label.x = "right", label.y = "top", vstep = 0.08
  # ) +
  facet_wrap(~ variable + ID, scales = "free") +
  labs(
    x = "Days since peak",
    y = "Percent change from Base (%)") +
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring()




#linearity#########

recovery_labels <- c(
  r2.recess = "R² Recession",
  recess.slope.z = "Recession Slope (z)",
  time.to.recover = "Time to Recover"
)

flood.response%>%
  filter(time.to.recover>0, !is.na(class), recess.slope> -10, recess.slope< 4)%>%
  pivot_longer(
    cols=c('r2.recess', 'recess.slope.z'),
    names_to = 'recovery',
    values_to = 'metric'
  )%>%
  mutate(
    recovery= factor(recovery, levels =c('time.to.recover', 'recess.slope.z', 'r2.recess')))%>%
  ggplot(aes(x=class, y=metric, color=class))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+
  scale_color_manual(name = "Class", values = class_colors) +
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  theme_spring()+
  facet_grid(recovery ~ variable, scales = 'free',
             labeller = labeller(recovery = recovery_labels))+
  theme(axis.title = element_blank())


flood.response%>%
  filter(time.to.recover>0, !is.na(class), recess.slope> -10, recess.slope< 4)%>%
  pivot_longer(
    cols=c('r2.recess', 'recess.slope.z'),
    names_to = 'recovery',
    values_to = 'metric'
  )%>%
  mutate(
    recovery= factor(recovery, levels =c('time.to.recover', 'recess.slope.z', 'r2.recess')))%>%
  ggplot(aes(x=ID, y=metric))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID, color=class))+
  scale_color_manual(name = "Class", values = class_colors) +
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  theme_spring()+
  facet_grid(recovery ~ variable, scales = 'free',
             labeller = labeller(recovery = recovery_labels))+
  theme(axis.title = element_blank())


flood.response%>%
  filter(!is.na(class))%>%
  ggplot(aes(x=recess.slope, y=r2.recess, color=class, shape=ID))+
  geom_point(size=3)+
  facet_wrap(~variable, scales = "free")+
  scale_color_manual(name = "Class", values = class_colors) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color='gray', size=1)+
  annotate(
    "text", x = -Inf, y = 0.37, label = "Displays Hysteresis", 
    hjust = -0.02, color = "gray40", size = 3, fontface = "italic")+
  facet_wrap(~variable, scales = "free")+
  theme_spring()+
  labs(
    x="Recession Slope",
    y="R² Recession"
  )

avg<-flood.response%>%
  group_by(variable, vulnerable.score)%>%
  summarise(r2.recess=mean(r2.recess, na.rm=T))

rC <- lmList(r2.recess ~ vulnerable.score | variable, data = avg)

peak.lm.pvals <- data.frame(
  variable = names(rC),
  p.val = sapply(rC, function(m) {
    if (inherits(m, "lm")) {
      coef(summary(m))["vulnerable.score", "Pr(>|t|)"]
    } else {
      NA
    }
  })
) %>%
  mutate(
    variable = factor(variable, levels = levels(flood.response$variable)),
    p.label = ifelse(is.na(p.val), "p = NA",
                     ifelse(p.val < 0.001, "p < 0.001",
                            paste0("p = ", formatC(p.val, format = "f", digits = 3))))
  )


# Flood end timing relative to depth ########

plot_grid(
  analysis.long%>%
    filter(ID=='AM', !is.na(flood))%>%
    ggplot(aes(x=Date,y=conc, color=variable))+
    geom_point(alpha=0.5, size=0.3)+
    geom_smooth(method='loess', se=F,span=0.1)+
    geom_vline(aes(xintercept=flood.end), linetype='dashed', alpha=0.7)+
    theme_spring()+
    facet_grid(variable ~ flood, scales = "free")+
    labs(y="Metabolic Impact",
         title="AM")
  ,
  analysis.long%>%
    filter(ID=='LF', !is.na(flood))%>%
    ggplot(aes(x=Date,y=conc, color=variable))+
    geom_point(alpha=0.5, size=0.3)+
    geom_smooth(method='loess', se=F,span=0.1)+
    geom_vline(aes(xintercept=flood.end), linetype='dashed', alpha=0.7)+
    theme_spring()+
    facet_grid(variable ~ flood, scales = "free")+
    labs(y="Metabolic Impact",
         title="LF"),
  ncol=1)

depth.end.date <- flood.response %>%
  filter(variable == 'depth') %>%
  select(flood, ID, flood.end) %>%
  rename(depth.end = flood.end)

end.diff <- left_join(flood.response, depth.end.date) %>%
  mutate(
    end.diff = as.numeric(difftime(flood.end, depth.end, units = "days")),
    variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
    ID       = factor(ID,       levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS')),
    class    = factor(class,    levels = c("HI", "BO", "FR"))
  ) %>%
  filter(variable != 'depth')

a <- ggplot(end.diff %>% filter(!is.na(class), end.diff>-500, end.diff<200), aes(x = variable, y = end.diff, color = class)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray") +
  scale_color_manual(values = class_colors, na.value = "grey70") +
  annotate(
    "text", x = -Inf, y =  Inf, label = "recovered after depth",
    hjust = -0.02, vjust = 1.5, color = "black", size = 3, fontface = "italic") +
  annotate(
    "text", x = -Inf, y = -Inf, label = "recovered before depth",
    hjust = -0.02, vjust = -0.5, color = "black", size = 3, fontface = "italic") +
  theme_spring() +
  labs(x = '', y = "Days between depth flood.end") +
  theme(legend.position = 'none')

avg.end <- end.diff%>% filter(!is.na(class), end.diff>-500, end.diff<200) %>%
  group_by(variable, vulnerable.score) %>%
  summarise(end.diff = mean(end.diff, na.rm = T))

rC.end <- lmList(end.diff ~ vulnerable.score | variable, data = avg.end)

end.lm.pvals <- data.frame(
  variable = names(rC.end),
  p.val = sapply(rC.end, function(m) {
    if (inherits(m, "lm")) {
      coef(summary(m))["vulnerable.score", "Pr(>|t|)"]
    } else {
      NA
    }
  })
) %>%
  mutate(
    variable = factor(variable, levels = levels(end.diff$variable)),
    p.label  = ifelse(is.na(p.val), "p = NA",
                      ifelse(p.val < 0.001, "p < 0.001",
                             paste0("p = ", formatC(p.val, format = "f", digits = 3))))
  )

b <- ggplot(end.diff %>% filter(!is.na(class), end.diff>-500, end.diff<200), aes(x = ID, y = end.diff)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID, color = class)) +
  scale_color_manual(values = class_colors, na.value = "grey70") +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray") +
  geom_text(
    data = end.lm.pvals,
    aes(x = Inf, y = Inf, label = p.label),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 1.5,
    size = 3
  ) +
  annotate(
    "text", x = -Inf, y =  Inf, label = "recovered after depth",
    hjust = -0.02, vjust = 1.5, color = "black", size = 3, fontface = "italic") +
  annotate(
    "text", x = -Inf, y = -Inf, label = "recovered before depth",
    hjust = -0.02, vjust = -0.5, color = "black", size = 3, fontface = "italic") +
  theme_spring() +
  labs(x = 'Increasing Flood Vulnerability', y = "Days between depth flood.end") +
  facet_wrap(~ variable)

plot_grid(a, b, ncol = 1, rel_heights = c(0.5, 1))


