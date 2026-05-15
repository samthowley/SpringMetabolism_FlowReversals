source('03_Scripts/ANALYSIS/analysis prep.R')


phase_colors <- c(Rise = "#d6604d", Fall = "#4393c3")

metric_labels <- c(
  slope.z = "Slope (z-scored)",
  r2      = "R²"
)


# Build combined rise / fall dataset ########

rise_fall <- bind_rows(

  flood.response %>%
    filter(time2peak > 0) %>%
    mutate(
      rise.slope = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, rise.slope),
      r2.rise    = if_else(variable == 'ER'  & rise.slope < 0, NA_real_, r2.rise),
      rise.slope = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, rise.slope),
      r2.rise    = if_else(variable == 'CO2' & rise.slope < 0, NA_real_, r2.rise),
      rise.slope = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, rise.slope),
      r2.rise    = if_else(variable == 'GPP' & rise.slope > 0, NA_real_, r2.rise),
      rise.slope = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, rise.slope),
      r2.rise    = if_else(variable == 'DO'  & rise.slope > 0, NA_real_, r2.rise)
    ) %>%
    select(ID, flood, variable, class, vulnerable.score, rise.slope.z, r2.rise) %>%
    rename(slope.z = rise.slope.z, r2 = r2.rise) %>%
    mutate(phase = "Rise"),

  flood.response %>%
    filter(time.to.recover > 0) %>%
    select(ID, flood, variable, class, vulnerable.score, recess.slope.z, r2.recess) %>%
    rename(slope.z = recess.slope.z, r2 = r2.recess) %>%
    mutate(phase = "Fall")

) %>%
  mutate(
    phase    = factor(phase,    levels = c("Rise", "Fall")),
    variable = factor(variable, levels = c("depth", "DO", "CO2", "GPP", "ER")),
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


# Tables ########

# Table 1: Mean slope.z and R² by site and phase
rise_fall_by_site <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(ID, variable, phase) %>%
  summarise(
    mean_slope_z = mean(slope.z, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(
    names_from = c("variable", "phase"),
    values_from = "mean_slope_z"
  )

print(rise_fall_by_site)


rise_fall_by_site <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(ID, variable, phase) %>%
  summarise(
    mean_r2      = mean(r2,      na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(
    names_from = c("variable", "phase"),
    values_from = "mean_r2"
  )

print(rise_fall_by_site)

# Table 2: Mean slope.z and R² by flood class and phase
rise_fall_by_class <- rise_fall %>%
  filter(!is.na(class)) %>%
  group_by(class, variable, phase) %>%
  summarise(
    mean_slope_z = mean(slope.z, na.rm = TRUE),
    mean_r2      = mean(r2,      na.rm = TRUE),
    .groups = "drop"
  )

print(rise_fall_by_class)


# Figures ########

# Figure 1: Rise vs Fall by flood class
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


# Figure 2: Rise vs Fall by site
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


# Figure 3: Slope vs R² — rise vs fall, colored by class
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
