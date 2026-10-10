source('03_Scripts/ANALYSIS/09-analysis prep.R')

# H1: how do GPP, ER, DO and CO2 change with stage?
# (Replaces the breakpoint fits that were here.) Fit one straight line (value ~ depth) to each
# flood class at each site. Days are split by the class of the flood they sit in:
#   FR, HI, BO = days inside a flood of that class
#   baseline   = days outside any flood
# All four variables are daily values vs daily mean depth.
#   DO and CO2 are daily means (CO2 readings <= 600 are dropped first)
#   |ER| is used, as elsewhere
# Slope = change in the variable per 1 m of depth.
# Nothing is written to disk. Slopes are printed, plots show in RStudio.

#parameters########
min.day.frac <- 0.9    # a day needs this fraction of the site's usual readings to get a daily mean DO or CO2
min.days     <- 10     # a site x class needs at least this many days to get a slope

#daily data########
daily.chem <- chem_hourly %>%
  mutate(
    Date = as.Date(Date),
    CO2  = if_else(CO2 > 600, CO2, NA_real_)
  ) %>%
  filter(DO<7)%>%
  group_by(ID, Date) %>%
  summarise(
    n.DO  = sum(!is.na(DO)),
    n.CO2 = sum(!is.na(CO2)),
    DO    = mean(DO,  na.rm = TRUE),
    CO2   = mean(CO2, na.rm = TRUE),
    depth = mean(depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ID) %>%
  mutate(
    DO  = if_else(n.DO  >= min.day.frac * median(n.DO[n.DO > 0]),   DO,  NA_real_),
    CO2 = if_else(n.CO2 >= min.day.frac * median(n.CO2[n.CO2 > 0]), CO2, NA_real_)
  ) %>%
  ungroup() %>%
  select(-n.DO, -n.CO2)

#class of each day########
stage.long <- daily.chem %>%
  left_join(
    metab %>%
      distinct(ID, Date, .keep_all = TRUE) %>%
      transmute(ID, Date, GPP, ER = abs(ER)),
    by = c("Date", "ID"),
    relationship = "one-to-one"
  ) %>%
  left_join(floods %>% select(ID, flood, start, end),
            join_by(ID, between(Date, start, end))) %>%
  left_join(flood_class %>% select(ID, flood, class), by = c("ID", "flood")) %>%
  mutate(class = if_else(is.na(flood), "baseline", class)) %>%   # flood days with no class drop out below
  pivot_longer(cols = c(GPP, ER, DO, CO2), names_to = "variable", values_to = "value") %>%
  filter(!is.na(depth), !is.na(value), !is.na(class)) %>%
  mutate(
    variable = factor(variable, levels = c("GPP", "ER", "DO", "CO2")),
    ID       = factor(ID, levels = c("IU", "ID", "GB", "LF", "AM", "OS")),
    class    = factor(class, levels = c("baseline", "HI", "BO", "FR"))
  )

#slope per site x class x variable########
slopes <- stage.long %>%
  group_by(variable, ID, class) %>%
  filter(n() >= min.days, n_distinct(depth) >= 3) %>%
  group_modify(~{
    fit <- lm(value ~ depth, data = .x)
    cf  <- summary(fit)$coefficients["depth", ]
    ci  <- confint(fit)["depth", ]
    tibble(
      n.days   = nrow(.x),
      n.floods = n_distinct(.x$flood, na.rm = TRUE),
      depth.min = min(.x$depth),
      depth.max = max(.x$depth),
      slope    = unname(cf["Estimate"]),
      ci.low   = unname(ci[1]),
      ci.high  = unname(ci[2]),
      p        = unname(cf["Pr(>|t|)"]),
      r2       = summary(fit)$r.squared,
      pct.per.m = unname(cf["Estimate"]) / mean(.x$value) * 100,   # slope as % of the class mean per metre
      direction = case_when(ci.low > 0 ~ "up", ci.high < 0 ~ "down", TRUE ~ "flat")   # flat = 95% CI includes 0
    )
  }) %>%
  ungroup()


# quick read: direction of the slope by site and class (n floods in brackets)
slopes %>%
  mutate(cell = paste0(direction, " (", n.floods, ")")) %>%
  select(variable, ID, class, cell) %>%
  pivot_wider(names_from = class, values_from = cell) %>%
  arrange(variable, ID) %>%
  print(n = Inf)

#plots########
# points and the fitted line for each class (only classes that got a slope)
fitted.keys <- slopes %>% select(variable, ID, class)
plot.data   <- stage.long %>% semi_join(fitted.keys, by = c("variable", "ID", "class"))

p.scatter <- ggplot(plot.data, aes(x = depth, y = value, color = class)) +
  geom_point(size = 0.6, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(vars(variable, ID), scales = "free", ncol = n_distinct(plot.data$ID)) +
  scale_color_manual(values = class_colors) +
  labs(x = "Depth (m)", y = NULL, color = "Flood class") +
  theme_spring() +
  theme(axis.text.x = element_text(size = 7), legend.position = "right")
p.scatter

# slopes with 95% CI, by site
p.slopes <- ggplot(slopes, aes(x = ID, y = slope, color = class)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci.low, ymax = ci.high), position = position_dodge(width = 0.6)) +
  facet_wrap(~variable, scales = "free_y", nrow = 1) +
  scale_color_manual(values = class_colors) +
  labs(x = "Site (low to high vulnerability)", y = "Slope (change per m of depth)", color = "Flood class") +
  theme_spring() +
  theme(legend.position = "right")
p.slopes
