source('03_Scripts/ANALYSIS/analysis prep.R')


# Table 1: Mean percent change by site
perc_by_site <- flood.response %>%
  filter(!is.na(class)) %>%
  group_by(ID, variable) %>%
  summarise(mean_perc_change = mean(reponse.percent.change, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_perc_change)

print(perc_by_site)

# Table 2: Mean percent change by flood class
perc_by_class <- flood.response %>%
  filter(!is.na(class)) %>%
  group_by(class, variable) %>%
  summarise(mean_perc_change = mean(reponse.percent.change, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_perc_change)

print(perc_by_class)


time.series.daily<-
  time.series%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date, variable)%>%
  mutate(
    conc=mean(conc, na.rm=T))%>%
  distinct(Date, ID, variable, flood, .keep_all = T)

time.series.daily %>%
  filter(!is.na(class),
         variable !='depth')%>%
  ggplot(aes(x = h.percent.change, y = perc.change, group=flood, color = class)) +
  geom_point(alpha=0.5, size=0.3) +
  scale_color_manual(name = "Class", values = class_colors) +
  geom_smooth(method = 'lm', se = FALSE)+
  facet_grid(variable ~ ID, scales = "free") +
  labs(
    x = "Stage Percent change from Base (%)", 
    y = "Percent change from Base (%)"
  ) +
  theme_spring()

plot_grid(
flood.response %>%
  filter(variable != 'depth', !is.na(class)) %>%
  ggplot(aes(x = h.percent.change, y = reponse.percent.change, color=class, shape=ID)) +
  geom_point(size=3) +
  scale_color_manual(name = "Class", values = class_colors) +
  #geom_smooth(method='loess', aes(color = flood), se=F,span=0.3)+
  # new_scale_color() +
  # stat_poly_line(color = 'black', se = F) +
  # stat_poly_eq(
  #   aes(label = paste(..eq.label.., ..rr.value.label.., sep = " ~~ ")), color='black',
  #   formula = y ~ x, parse = TRUE,
  #   size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  # )+
  facet_wrap(~variable, scales = "free", nrow=1) +
  labs(
    x = "Stage Percent change from Base (%)", 
    y = "Percent change from Base (%)"
  ) +
  theme_spring()+theme(legend.position = "none")
,

flood.response%>%
  filter(!is.na(class))%>%
  ggplot(aes(x=class, y= reponse.percent.change ,color=class))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+  
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring()+
  facet_wrap(~variable, scales = "free")+
  labs(
    x =  " ", 
    y = "Percent change from Base (%)"
  ),
ncol=1, rel_heights = c(0.6,1)
)


flood.response%>%
  filter(!is.na(class))%>%
  ggplot(aes(x=ID, y= reponse.percent.change))+
  geom_boxplot(aes())+
  geom_jitter(aes(shape=ID, color=class))+  
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring()+
  facet_wrap(~variable, scales = "free")+
  labs(
    x =  " ", 
    y = "Percent change from Base (%)")
