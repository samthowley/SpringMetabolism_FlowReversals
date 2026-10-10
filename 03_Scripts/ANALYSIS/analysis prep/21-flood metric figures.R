source('03_Scripts/ANALYSIS/00-test helpers.R')

# Outputs (04_Outputs/tests/): fig_scatter_vulnerability.png, fig_box_class.png, fig_box_site.png
class_colors <- c(BO = "#A65628", FR = "black", HI = "#2171B5", baseline='lightblue')
flood.metrics$class <- factor(flood.metrics$class, levels = c("baseline", "HI", "BO", "FR"))


#parameters########
drop.unrecovered <- TRUE   # duration-type metrics are censored when the variable never recovered
metrics     <- c("abs.percent.change", "duration", "severity", "time.to.recover")
log.metrics <- c("duration", "severity", "time.to.recover")   # plotted on the log scale (non-positive values dropped)
censored    <- c("duration", "severity", "time.to.recover")

#H1: flood focus################

#is there a change in the magnitude of the response with flood class? (boxplots)
flood.metrics%>%select(ID, class, peak.response, base, variable)%>%
  filter(!is.na(peak.response))%>%
  pivot_longer(
    c(peak.response, base),
    names_to = "test", 
    values_to = "value")%>%
  mutate(
    class=if_else(test=="base", "baseline", class),
    class = fct_relevel(class, "baseline", "HI", "BO", "FR")
  )%>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class, shape=ID), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Flood class", y = NULL, color = "Flood class")+
  theme_bw()



flood.metrics%>%select(ID, class, variable, duration, time.to.recover)%>%
  filter(time.to.recover>0, !is.na(class))%>%
  mutate(
    class = fct_relevel(class, "baseline", "HI", "BO", "FR"),
    duration.recovery=duration/time.to.recover
  )%>%
  ggplot(aes(x = class, y = time.to.recover)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class, shape=ID), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_wrap(~variable) +
  labs(x = "Flood class", y = NULL, color = "Flood class")+
  theme_bw()




#when i check whether more vulnerable sites have more extreme responses, 
# I need to check that there is an interaction with flood class


#Fig 1: scatter vs vulnerability########
flood.metrics%>%select(ID, class,percent.change, variable)%>%
  filter(!is.na(class))%>%
  mutate(
    class = fct_relevel(class, "HI", "BO", "FR")
  )%>%
  ggplot(aes(x = class, y = percent.change)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class, shape=ID), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Flood class", y = NULL, color = "Flood class")+
  theme_bw()

flood.metrics%>%select(ID, class,percent.change, variable)%>%
  filter(!is.na(class))%>%
  mutate(
    ID = fct_relevel(ID, "IU", "ID", "GB", "LF", "AM", "OS")
  )%>%
  ggplot(aes(x = ID, y = percent.change)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class, shape=ID), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Flood class", y = NULL, color = "Flood class")+
  theme_bw()

figs %>%
  ggplot(aes(x = vulnerable.score, y = value)) +
  geom_jitter(aes(color = class), width = 0.12, alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.6, alpha = 0.15) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  scale_x_continuous(breaks = 1:6, labels = site.order) +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Site (low to high vulnerability)", y = NULL, color = "Flood class") +
  theme_spring()
#ggsave(paste0(out.dir, "fig_scatter_vulnerability.png"), p.scatter, width = 10, height = 9, dpi = 200)

#Fig 2: boxplots by flood class########
names(flood.metrics)



figs %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Flood class", y = NULL, color = "Flood class") +
  theme_spring()
#ggsave(paste0(out.dir, "fig_box_class.png"), p.class, width = 10, height = 9, dpi = 200)

#Fig 3: boxplots by site, ordered by vulnerability########
p.site <- figs %>%
  ggplot(aes(x = ID, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "grey30") +
  geom_jitter(aes(color = class), width = 0.15, alpha = 0.7, size = 1.8) +
  scale_color_manual(values = class_colors, breaks = names(class_colors), na.value = "grey60") +
  facet_grid(metric.label ~ variable, scales = "free_y") +
  labs(x = "Site (low to high vulnerability)", y = NULL, color = "Flood class") +
  theme_spring()
#ggsave(paste0(out.dir, "fig_box_site.png"), p.site, width = 10, height = 9, dpi = 200)

p.scatter
p.class
p.site
