source('03_Scripts/ANALYSIS/analysis prep.R')

#percent change################
time.series %>%
  #filter(ID=='AM')%>%
  ggplot(aes(x = h.percent.change, y = perc.change)) +
  geom_point(aes(color = class), shape=1) +
  scale_color_manual(name = "Class", values = class_colors) +
  #geom_smooth(method='loess', aes(color = flood), se=F,span=0.3)+
  # new_scale_color() +
  # stat_poly_line(color = 'black', se = F) +
  # stat_poly_eq(
  #   aes(label = paste(..eq.label.., ..rr.value.label.., sep = " ~~ ")), color='black',
  #   formula = y ~ x, parse = TRUE,
  #   size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  # )+
  facet_grid(variable ~ ID, scales = "free") +
  labs(
     x = "Stage Percent change from Base (%)", 
     y = "Percent change from Base (%)"
    ) +
  theme_spring()


flood.response %>%
  filter(variable != 'depth') %>%
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
  facet_wrap(~variable, scales = "free") +
  labs(
    x = "Stage Percent change from Base (%)", 
    y = "Percent change from Base (%)"
  ) +
  theme_spring()


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
  )

#difference in peaks########

#Recovery##########
time.series %>%
  filter(
    Date > peak.Date, !is.na(flood)) %>%
  ggplot(aes(x = flood.count, y = perc.change, group = flood)) +
  geom_point(aes(color = class)) +
  scale_color_manual(name = "Class", values = class_colors) +
  
  new_scale_color() +
  stat_poly_line(color = 'black', se = F) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.value.label.., sep = " ~~ ")), color='black',
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  )+
  facet_grid(variable ~ ID, scales = "free") +
  labs(
    x = "Days since peak", 
    y = "Percent change from Base (%)") +
  theme_spring()


flood.response%>%
  ggplot(aes(x=recess.slope, y=r2.recess, color=flood, shape=ID))+
  geom_point(size=3)+
  facet_wrap(~variable, scales = "free")+
  geom_hline(yintercept = 0.4, linetype = "dashed", color='gray', size=1)+
  annotate(
    "text", x = -Inf, y = 0.37, label = "Displays Hysteresis", 
           hjust = -0.02, color = "gray40", size = 3, fontface = "italic")+
  facet_wrap(~variable, scales = "free")+
  theme_spring()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ggplotly()


time.series%>%
  filter(ID=='OS')%>%
  ggplot(aes(x=Date,y=conc, color=variable))+
  geom_point(aes(),shape=1)+
  geom_smooth(method='loess', se=F,span=0.3)+
  theme_spring()+
  facet_grid(variable ~ flood, scales = "free")
  

#test flooding severity with recovery time vs duration, time between, and percent change#######

#time to recover vs duration
flood.response %>%
  filter(time.to.recover>0) %>%
  ggplot(aes(x = time.to.recover, y = duration, color = class, shape = ID)) +
  geom_point(size = 3) +
  facet_wrap(~variable, scales = "free") +
  theme_spring() +
  #scale_color_manual(name = "Class", values = class_colors) +
  scale_y_log10()+
  labs(x = "Time to recover (days)", y = "Flood duration (days)") +
  annotate("text", x = -Inf, y =  Inf, label = "Long flood, short recovery", hjust = -0.1, vjust =  1.5) +
  annotate("text", x =  Inf, y =  Inf, label = "Long flood, long recovery", hjust =  1.1, vjust =  1.5) +
  annotate("text", x = -Inf, y = -Inf, label = "Short flood, short recovery", hjust = -0.1, vjust = -0.5) +
  annotate("text", x =  Inf, y = -Inf, label = "Short flood, long recovery", hjust =  1.1, vjust = -0.5) 
names(flood.response)



flood.response%>%
  filter(!is.na(class))%>%
  ggplot(aes(x=class, y= time.to.recover/duration ,color=class))+
  geom_boxplot()+
  geom_jitter(aes(shape=ID))+  
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring()+
  facet_wrap(~variable, scales = "free")+
  labs(
    x =  " ", 
    y = "Recovery time / Flood duration"
  )+
  geom_hline(yintercept = 1, linetype = "dashed", color='gray', size=1)


#duration vs percent change
flood.response %>%
  filter(time.to.recover>0) %>%
  ggplot(aes(x = duration, y = reponse.percent.change, color = class, shape = ID)) +
  geom_point(size = 3) +
  facet_wrap(~variable, scales = "free") +
  theme_spring() +
  #scale_color_manual(name = "Class", values = class_colors) +
  labs(x = "Time to recover (days)", y = "Flood duration (days)") +
  annotate("text", x = -Inf, y =  Inf, label = "Long flood, short recovery", hjust = -0.1, vjust =  1.5) +
  annotate("text", x =  Inf, y =  Inf, label = "Long flood, long recovery", hjust =  1.1, vjust =  1.5) +
  annotate("text", x = -Inf, y = -Inf, label = "Short flood, short recovery", hjust = -0.1, vjust = -0.5) +
  annotate("text", x =  Inf, y = -Inf, label = "Short flood, long recovery", hjust =  1.1, vjust = -0.5) 
names(flood.response)

#test flooding vulnerability with time between vs recovery time and percent change#######

flood.response %>%
  filter(time.to.recover>0) %>%
  ggplot(aes(x = time.to.recover, y = duration, color = class, shape = ID)) +
  geom_point(size = 3) +
  facet_wrap(~variable, scales = "free") +
  scale_color_manual(name = "Class", values = class_colors) +
  theme_spring() +
  #scale_color_manual(name = "Class", values = class_colors) +
  scale_y_log10()+
  labs(x = "Time to recover (days)", y = "Flood duration (days)") +
  annotate("text", x = -Inf, y =  Inf, label = "Long flood, short recovery", hjust = -0.1, vjust =  1.5) +
  annotate("text", x =  Inf, y =  Inf, label = "Long flood, long recovery", hjust =  1.1, vjust =  1.5) +
  annotate("text", x = -Inf, y = -Inf, label = "Short flood, short recovery", hjust = -0.1, vjust = -0.5) +
  annotate("text", x =  Inf, y = -Inf, label = "Short flood, long recovery", hjust =  1.1, vjust = -0.5) 
names(flood.response)



flood.response%>%
  filter(!is.na(class), days.since.last.flood>0)%>%
  ggplot(aes(x=days.since.last.flood, y=time.to.recover, color=class, shape=ID))+
  geom_point(size = 3) +
  scale_color_manual(name = "Class", values = class_colors) +
  facet_wrap(~variable, scales = "free") +
  theme_spring() +
  #scale_color_manual(name = "Class", values = class_colors) +
  labs(x = "Days Since Last Flood (days)", y = "Recovery Time (days)") 


flood.response%>%
  filter(!is.na(class), days.since.last.flood>0)%>%
  ggplot(aes(x=days.since.last.flood, y=reponse.percent.change, color=class, shape=ID))+
  geom_point(size = 3) +
  scale_color_manual(name = "Class", values = class_colors) +
  facet_wrap(~variable, scales = "free") +
  theme_spring() +
  #scale_color_manual(name = "Class", values = class_colors) +
  labs(x = "Days Since Last Flood (days)", 
       y = "Percent change from Base (%)") 



