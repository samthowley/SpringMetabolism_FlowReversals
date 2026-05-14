source('03_Scripts/ANALYSIS/analysis prep.R')

depth.date<-flood.response%>%filter(variable=='depth')%>%
  select(flood, ID, peak.Date)%>%
  rename(depth.Date=peak.Date)

peak.diff<-left_join(flood.response, depth.date)%>%
  mutate(peak.diff=as.numeric(difftime(peak.Date, depth.Date, units="days")))%>%
  filter(variable != 'depth')

analysis.long%>%
  filter(ID=='OS', !is.na(flood))%>%
  ggplot(aes(x=Date,y=conc, color=variable))+
  geom_point(alpha=0.5, size=0.3)+
  geom_smooth(method='loess', se=F,span=0.1)+
  geom_vline(aes(xintercept=peak.Date), linetype='dashed', alpha=0.7)+
  theme_spring()+
  facet_grid(variable ~ flood, scales = "free")




ggplot(peak.diff, aes(x=variable, y=peak.diff, color=variable))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+
  geom_hline(yintercept = 0, linetype='dashed', color="gray")+
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
  )

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

ggplot(peak.diff, aes(x=ID, y=peak.diff, color=variable))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+
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
