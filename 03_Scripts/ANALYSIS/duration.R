source('03_Scripts/ANALYSIS/analysis prep.R')


# Table 1: Mean duration by flood
dur_by_flood <- flood.response %>%
  filter(duration < 200, !is.na(class)) %>%
  group_by(class, variable) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_duration)

print(dur_by_flood)

# Table 2: Mean duration by site
dur_by_site <- flood.response %>%
  filter(duration < 200, !is.na(class)) %>%
  group_by(ID, variable) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_duration)

print(dur_by_site)


flood.response %>%
  filter(duration<200, !is.na(class))%>%
  ggplot(aes(x=ID, y=duration))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID, color=class))+
  scale_color_manual(values = class_colors)+
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  theme_spring()+
  facet_wrap(~variable, scales='free')+
  ylab("Flood Duration (days)")+
  theme(axis.title.x = element_blank())


flood.response %>%
  filter(duration<200, !is.na(class))%>%
  ggplot(aes(x=class, y=duration))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID, color=class))+
  scale_color_manual(values = class_colors)+
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  theme_spring()+
  facet_wrap(~variable, scales='free')+
  ylab("Flood Duration (days)")+
  theme(axis.title.x = element_blank())



time2peak<-flood.response %>%
  mutate(
    time2peak=as.numeric(difftime(peak.Date, flood.start, units="days")),
    time2recover=as.numeric(difftime(flood.end,peak.Date, units="days")),
  )
  

plot_grid(
time2peak %>%
  filter(time2peak>0, time2peak<200, !is.na(class))%>%
  filter(time.to.recover>0) %>%
  ggplot(aes(x = time2peak, y = time2recover, color = class, shape = ID)) +
  geom_point(size = 3) +
  scale_color_manual(values = class_colors)+
  facet_wrap(~variable, scales = "free", nrow=1) +
  theme_spring() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    y="Start to Peak (days)",
    x=" Peak to End (days)"
  )+
  annotate(
    "text", x = -Inf, y = 0.37, label = "Flood Impacts lagged", 
    hjust = -0.02, color = "gray40", size = 3, fontface = "italic")+
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")
,
time2peak %>%
  filter(time2peak > 0, time2peak < 200, !is.na(class)) %>%
  ggplot(aes(x = ID, y = time2peak / time2recover)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID, color = class)) +
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring() +
  facet_wrap(~variable, scales = "free") +
  labs(
    x = " ",
    y = "Start to Peak (days) / Peak to End (days)"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = 'gray', linewidth = 1) +
  annotate(
    "text", x = Inf, y = Inf, label = "Recovery Time< Flood Rise",
    hjust = 1.02, vjust = 1.5, color = "gray40", size = 3, fontface = "italic")  ,
ncol=1,
rel_heights=c(0.45, 1)
)





#end of flood##############

depth.date<-flood.response%>%filter(variable=='depth')%>%
  select(flood, ID, flood.end)%>%
  rename(depth.end=flood.end)

delay.end<-left_join(flood.response, depth.date)%>%
  mutate(
    end.diff=as.numeric(difftime(flood.end, depth.end, units="days")),
    variable = factor(variable, levels = c("depth", "DO", "CO2", 'GPP', 'ER')),
    ID = factor(ID, levels = c("IU", "ID", "GB", 'LF', 'AM', 'OS')),
    class = factor(class, levels = c("HI", "BO", "FR"))
  )%>%
  filter(variable != 'depth')


plot_grid(
  analysis.long%>%
    filter(ID=='ID', !is.na(flood))%>%
    ggplot(aes(x=Date,y=conc, color=variable))+
    geom_point(alpha=0.5, size=0.3)+
    geom_smooth(method='loess', se=F,span=0.1)+
    geom_vline(aes(xintercept=flood.end), linetype='dashed', alpha=0.7)+
    theme_spring()+
    facet_grid(variable ~ flood, scales = "free")+
    labs(y="Metabolic Impact",
         title="ID")
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

label_df <- data.frame(
  variable = unique(delay.end$variable),
  x = Inf, y = -120,
  label = "Duration of Rise = Recovery Time"
)

delay.end %>% 
  filter(!is.na(class), end.diff > -500, end.diff < 400, variable != 'depth') %>%
  ggplot(aes(x = ID, y = end.diff)) +
  geom_boxplot() +
  geom_jitter(aes(shape = ID, color = class)) +
  scale_color_manual(values = class_colors, na.value = "grey70") +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray") +
  geom_text(data = label_df, aes(x = x, y = y, label = label),
            hjust = 1.02, color = "black", size = 3, fontface = "italic",
            inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  theme_spring() +
  labs(x = 'Increasing Flood Vulnerability',
       y = "Days between depth maximum") +
  facet_wrap(~variable, scales = 'free')
