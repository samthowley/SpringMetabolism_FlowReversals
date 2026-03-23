source("03_Scripts/disturbance isolation functions.R")


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

#Oberseving flood effects###########
master_metabolism <- read_csv("04_Outputs/master.metabolism.csv")
floods <- read_csv("01_Raw_data/flood.periods.csv")
class <- read_csv("04_Outputs/flood impacts/depth.csv")%>%
  mutate(Date=as.Date(Date))

met_flagged <- master_metabolism %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  left_join(class)%>%
  drop_na(ER)



met_flagged%>%
  filter(ID=='AM')%>%
  ggplot(aes(x = depth, color=SpC)) +
  #geom_point(aes(y = GPP)) +
  geom_point(aes(y = ER)) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()



