source('03_Scripts/ANALYSIS/analysis prep.R')


# Rise ########

time.series.daily <-
  time.series %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date, variable) %>%
  mutate(conc = mean(conc, na.rm = T)) %>%
  distinct(Date, ID, variable, flood, .keep_all = T)


time.series.daily %>%
  filter(Date < peak.Date, !is.na(flood)) %>%
  ggplot(aes(x = flood.count, y = perc.change, group = flood, color = class)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = "Class", values = class_colors) +
  new_scale_color() +
  stat_poly_line(se = FALSE, aes(color = class)) +
  facet_wrap(~ variable + ID, scales = "free") +
  labs(
    x = "Days before peak",
    y = "Percent change from Base (%)") +
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring()


rise_labels <- c(
  r2.rise      = "R² Rise",
  rise.slope.z = "Rise Slope (z)",
  time2peak    = "Time to Peak"
)


# linearity ########
flood.response %>%
  mutate(
    rise.slope   = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, r2.rise)
  ) %>%
  filter(time2peak > 0, !is.na(class)) %>%
  pivot_longer(
    cols      = c('r2.rise', 'rise.slope.z'),
    names_to  = 'rise',
    values_to = 'metric'
  ) %>%
  mutate(
    rise = factor(rise, levels = c('time2peak', 'rise.slope.z', 'r2.rise'))
  ) %>%
  ggplot(aes(x = class, y = metric, color = class)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID)) +
  scale_color_manual(name = "Class", values = class_colors) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray") +
  theme_spring() +
  facet_grid(rise ~ variable, scales = 'free',
             labeller = labeller(rise = rise_labels)) +
  theme(axis.title = element_blank())


flood.response %>%
  mutate(
    rise.slope   = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, r2.rise)
  ) %>%
  filter(time2peak > 0, !is.na(class)) %>%
  pivot_longer(
    cols      = c('r2.rise', 'rise.slope.z'),
    names_to  = 'rise',
    values_to = 'metric'
  ) %>%
  mutate(
    rise = factor(rise, levels = c('time2peak', 'rise.slope.z', 'r2.rise'))
  ) %>%
  ggplot(aes(x = ID, y = metric)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID, color = class)) +
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring() +
  facet_grid(rise ~ variable, scales = 'free',
             labeller = labeller(rise = rise_labels)) +
  theme(axis.title = element_blank())


flood.response %>%
  mutate(
    rise.slope   = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, r2.rise),

    rise.slope   = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, rise.slope),
    r2.rise      = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, r2.rise)
  ) %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = rise.slope, y = r2.rise, color = class, shape = ID)) +
  geom_point(size = 3) +
  facet_wrap(~ variable, scales = "free") +
  scale_color_manual(name = "Class", values = class_colors) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = 'gray', size = 1) +
  annotate(
    "text", x = -Inf, y = 0.37, label = "Displays Hysteresis",
    hjust = -0.02, color = "gray40", size = 3, fontface = "italic") +
  theme_spring() +
  labs(
    x = "Rise Slope",
    y = "R² Rise"
  )


avg <- flood.response %>%
  group_by(variable, vulnerable.score) %>%
  summarise(r2.rise = mean(r2.rise, na.rm = T))

rC <- lmList(r2.rise ~ vulnerable.score | variable, data = avg)

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
    p.label  = ifelse(is.na(p.val), "p = NA",
                      ifelse(p.val < 0.001, "p < 0.001",
                             paste0("p = ", formatC(p.val, format = "f", digits = 3))))
  )
