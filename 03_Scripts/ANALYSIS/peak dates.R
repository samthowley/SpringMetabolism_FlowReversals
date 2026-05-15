source('03_Scripts/ANALYSIS/analysis prep.R')

depth.date<-flood.response%>%filter(variable=='depth')%>%
  select(flood, ID, peak.Date)%>%
  rename(depth.Date=peak.Date)

peak.diff<-left_join(flood.response, depth.date)%>%
  mutate(
    peak.diff=as.numeric(difftime(peak.Date, depth.Date, units="days")),
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
  geom_vline(aes(xintercept=peak.Date), linetype='dashed', alpha=0.7)+
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
  geom_vline(aes(xintercept=peak.Date), linetype='dashed', alpha=0.7)+
  theme_spring()+
  facet_grid(variable ~ flood, scales = "free")+
  labs(y="Metabolic Impact",
       title="LF"),
ncol=1)

a<-ggplot(peak.diff%>% filter(peak.diff<100, !is.na(class)), aes(x=variable, y=peak.diff, color=class))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  scale_color_manual(values = class_colors, na.value = "grey70") +
  annotate(
    "text", x = -Inf, y = 100, label = "peaked after depth maximum",
    hjust = -0.02, color = "black", size = 3, fontface = "italic")+
  annotate(
    "text", x = -Inf, y = -50, label = "peaked before depth maximum",
    hjust = -0.02, color = "black", size = 3, fontface = "italic")+
  theme_spring()+
  labs(
    x='',
    y="Days between depth maximum"
  )+theme(legend.position = 'none')

avg<-peak.diff%>%
  group_by(variable, vulnerable.score)%>%
  summarise(peak.diff=mean(peak.diff, na.rm=T))

rC <- lmList(peak.diff ~ vulnerable.score | variable, data=avg)

peak.lm.pvals <- data.frame(
  variable = names(rC),
  p.val = sapply(rC, function(m) coef(summary(m))["vulnerable.score", "Pr(>|t|)"])
) %>%
  mutate(
    variable = factor(variable, levels = levels(peak.diff$variable)),
    p.label = ifelse(p.val < 0.001, "p < 0.001",
                     paste0("p = ", formatC(p.val, format = "f", digits = 3)))
  )

b<-ggplot(peak.diff%>% filter(!is.na(class)), aes(x=ID, y=peak.diff))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID, color=class))+
  scale_color_manual(values = class_colors, na.value = "grey70") +
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
  geom_text(
    data = peak.lm.pvals,
    aes(x = Inf, y = Inf, label = p.label),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = 1.5,
    size = 3
  )+
  annotate(
    "text", x = -Inf, y = 100, label = "peaked after depth maximum",
    hjust = -0.02, color = "black", size = 3, fontface = "italic")+
  annotate(
    "text", x = -Inf, y = -50, label = "peaked before depth maximum",
    hjust = -0.02, color = "black", size = 3, fontface = "italic")+
  theme_spring()+
  labs(
    x='Increasing Flood Vulnerability',
    y="Days between depth maximum"
  )+
  facet_wrap(~variable)


plot_grid(a, b, ncol=1, rel_heights = c(0.5, 1))


peak.diff%>%
  group_by(ID, variable)%>%
  summarize(peak.diff=mean(peak.diff, na.rm=T)
  )%>%
  pivot_wider(
    names_from = 'variable' ,
    values_from = 'peak.diff'
  )
