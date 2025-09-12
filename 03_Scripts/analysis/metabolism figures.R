cols <- c(
  "sup.GPP"  = "darkgreen",    
  "inf.GPP"   = "green",    
  "sup.ER"  = "darkred",    
  "inf.ER" = "red"
)

ggplot(master_scatter, aes(x = depth, y = prod, color = interaction(SpC.med, type), group = interaction(SpC.med, type))) +
  geom_point(size = 1) +
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4,label.x.npc = "right",label.y.npc = 0.017,vstep=0.2) +
  scale_colour_manual(name = "", values = cols) +
  ylab(flux) + xlab(h) + theme_sam_insideplots + facet_wrap(~ID, scales = 'free')




ggplot(master_scatter, aes(x = SpC, y = prod))+
  geom_point(size = 1) +
  ylab(flux) + xlab(h) + theme_sam_insideplots + facet_wrap(~ID, scales = 'free')+
  theme(legend.position = 'bottom')
