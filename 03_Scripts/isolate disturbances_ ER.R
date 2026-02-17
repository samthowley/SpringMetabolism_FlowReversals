source("03_Scripts/disturbance isolation functions.R")

ER <- read_csv("04_Outputs/master.metabolism.csv")%>%
  select(Date, ID, ER)%>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv")%>%
      mutate(Date=as.Date(Date))%>%
      group_by(ID, Date)%>%
      summarise(depth=mean(depth, na.rm=T))
  )


floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(start=as.Date(start), end=as.Date(end))


ER_flagged <- ER %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  filter(!is.na(ER))%>%
  mutate(date=Date)


ER.base<-baseline(ER_flagged, ER)


ER.smooth<-smooth(ER_flagged, ER)


all_out <- run_bp_all_sites(ER_flagged, site_col = ID, y = ER, x = depth, breaks = 2)
brk.slopes<-all_out$segments%>%select(ID, seg_start, seg_end, slope)


ER.impact <- ER.smooth %>%
  left_join(
    brk.slopes, by = join_by(ID, between(depth, seg_start, seg_end))
  )%>%  
  select(-seg_start, -seg_end)%>%
  mutate(
    impact=
      case_when(
        slope<0 ~ 'dec',
        slope>0 ~ 'inc')
  )%>%
  group_by(ID, flood) %>%
  mutate(
    max_height = which.max(replace(depth, is.na(depth), -Inf)), 
    maximum = case_when(
      row_number() == max_height ~ 0))%>%
  filter(maximum==0)%>%
  select(Date, ID, flood, impact)%>%
  rename(impact.sum=impact)%>%select(-Date)

ER.parse<-left_join(ER.smooth, ER.impact, by=c('flood', 'ID'))

#RR periods########

ER.dec<-ER.parse%>%filter(impact.sum=='dec')%>%
  mutate(flood=if_else(ID=='OS' & flood==3 & Date>'2023-07-19', NA, flood))

ER.dec.count<-count.min(ER.dec, ER_loess)

ER.dec.prep<-prep.by.slope_decreases(ER.dec.count, ER_loess)%>%
  mutate(
    remove = if_else(abs(count)<5, "keep", remove),
    ID.flood=paste0(ID, ".", flood),
    remove = if_else(ER_loess< -20, "keep", remove),
    # remove = if_else(ID=='LF' & flood==3, "keep", remove),
    # remove = if_else(ID=='OS' & flood==3, "keep", remove)
    
    )

#requires trimming:

site<-'OS'
flood.num<-1
plot_grid(
  
  ER.dec.prep%>% 
    #filter(ID==site, flood==flood.num)%>%
    ggplot(aes(x=count, y=ER))+
    geom_point(aes(y=ER_loess, color=remove), size=3)+
    geom_point(color='red')+
    geom_smooth(method = lm, aes(group=stage, y=ER))+
    facet_wrap(~ID.flood, scales='free')+
    theme(legend.position = 'bottom')
  
  ,
  
  ER.dec.prep%>% 
    filter(ID==site, flood==flood.num)%>%
    ggplot(aes(x=count, y=depth))+
    geom_point()+
    facet_wrap(~flood, scales='free', nrow=1)
  
  ,
  nrow=2,
  rel_heights = c(1, 0.4)
)

unique(ER.dec.prep$ID.flood)


ER.dec.trim<-trim(ER.dec.prep)%>%
  mutate(
    ER=if_else(ID=='LF' & flood==5 & Date>'2024-05-15', NA, ER),
    ER=if_else(ID=='AM' & flood==2 & Date<'2023-01-22', NA, ER)
    )
  

ER.dec.duration<-duration(ER.dec.trim)

ER.dec.min<-minimum(ER.dec.trim, ER)


recession.lm.dec<-fit_recessions(ER.dec.trim, ER.base, ER_loess, base) 
rise.lm.dec<-fit_rise(ER.dec.trim, ER.base, ER_loess, base) 

flood.impacts.ER.dec<-
  full_join(recession.lm.dec,
            ER.dec.duration)%>%
  full_join(rise.lm.dec, by=c('ID', 'flood'))%>%
  full_join(ER.dec.min, by=c('ID', 'flood'))%>%
  full_join(ER.base, by=c('ID', 'flood'))


#high stage periods######

ER.inc<-ER.smooth%>%filter(impact=='inc')

ER.inc.count<-count.max(ER.inc, ER_loess)

ER.inc.prep<-
  prep.by.slope_increases(ER.inc.count, ER_loess)%>%
  mutate(
    remove = if_else(abs(count)<5, "keep", remove),
    remove=if_else(ID=='AM' & flood==5 & count> -30 & count<0, 'keep', remove)
  )

ER.inc.trim<-trim(ER.inc.prep)

ER.inc.trim%>% 
  filter(ID=='OS')%>%
  ggplot(aes(x=count, y=ER))+
  geom_point(aes(y=ER_loess), size=3)+
  geom_point(color='red')+
  geom_smooth(method = lm, aes(group=stage, y=ER))+
  facet_wrap(~flood, scales='free', nrow=1)


ER.inc.duration<-duration(ER.inc.trim)

ER.inc.max<-maximum(ER.inc.trim, ER)

recession.lm.inc<-fit_recessions(ER.inc.trim, ER.base, ER, base) 
rise.lm.inc<-fit_rise(ER.inc.trim, ER.base, ER, base) 

flood.impacts.ER.inc<-
  full_join(recession.lm.inc,
            ER.inc.duration)%>%
  full_join(rise.lm.inc, by=c('ID', 'flood'))%>%
  full_join(ER.inc.max, by=c('ID', 'flood'))%>%
  full_join(ER.base, by=c('ID', 'flood'))


#combine########

flood.impacts.ER<-
  rbind(flood.impacts.ER.dec, flood.impacts.ER.inc)
  
write_csv(flood.impacts.ER, "04_Outputs/flood impacts/ER")



  