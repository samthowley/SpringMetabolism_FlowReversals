rm(list=ls())
source("03_Scripts/disturbance isolation functions.R")

depth <- read_csv("02_Clean_data/Chem/depth.csv")
floods <- read_csv("01_Raw_data/flood.periods.csv")

stage_flagged <- depth %>%
  full_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end)%>%
  arrange(ID, Date)



depth.smooth<-smooth(stage_flagged, depth)
depth.base<-baseline(stage_flagged, depth)
h.count<-count(depth.smooth, depth)

depth.peak<-h.count%>% filter(count==0)
depth.compare<-flood.base_compare(depth.peak, depth.base, depth)

depth.trim<-trim(h.count, depth.base, depth_loess, base.depth)%>%filter(count>0)
recession.lm<-lm(depth.trim)

depth.time.btwn<-time.btwn(stage_flagged)







(a<-ggplot(compare, 
           aes(x = Date, y=percent.change.depth, colour = flood))+
    geom_point(size=1)+
    facet_wrap(~ID, scales='free'))


depth.base<-baseline(depth.smooth, depth)



