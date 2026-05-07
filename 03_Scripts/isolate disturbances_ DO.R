source("03_Scripts/disturbance isolation functions.R")

DO <- read_csv("02_Clean_data/Chem/DO.csv")
h <- read_csv("02_Clean_data/Chem/depth.csv")
SpC <- read_csv("02_Clean_data/Chem/SpC.csv")

DO<-full_join(DO, h)%>%
  filter(!is.na(Date), !is.na(DO))%>% 
  mutate(
    date=as.Date(Date)
  )%>%
  group_by(ID, date)%>%
  mutate(
    DO.daily.min=min(DO, na.rm=T)
  )%>%left_join(read_csv("02_Clean_data/Chem/SpC.csv"))

floods <- read_csv("01_Raw_data/flood.periods.csv")

DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  
  select(-start, -end)%>%
  arrange(ID, Date)



DO%>%
  ggplot(aes(x=Date, y=DO))+
  geom_point()+
  facet_wrap(~ID, scales='free')



DO.base<-baseline(DO_flagged, DO)

DO.smooth<-smooth(DO_flagged, DO.daily.min)%>%
  rename(DO_loess= DO.daily.min_loess)

DO.count <- DO.smooth %>%
  left_join(DO.base, by = c("ID", "flood")) %>%
  count.min(DO) %>%
  filter(is.na(flood) | is.na(base) | DO < base) %>%
  trim_post_gap(gap_days = 1.5)


DO.prep<-prep.by.slope_decreases(DO.count, DO.daily.min)%>%
  mutate(
    remove=if_else(ID %in% c('ID', 'GB') & DO_loess<5, 'keep', remove),
    remove=if_else(ID %in% c('AM', 'LF', 'OS') & DO_loess<3.5, 'keep', remove),
    # DO=if_else(ID=='LF' & flood=='4' & count>700, NA, DO),
    # DO=if_else(ID=='ID' & flood=='1' & count>500, NA, DO),
  )%>%
  drop_na(DO)

class <- read_csv("04_Outputs/flood impacts/depth.csv")%>%select(ID, flood, class)
  
DO.trim<-trim(DO.prep)%>%left_join(class, by=c('ID', 'flood'))


DO.trim%>%
  filter(ID=='ID', !is.na(flood))%>%
  ggplot(aes(x=count, y=DO, color=as.factor(remove)))+
  geom_point(aes(y=DO_loess), color='red')+
  geom_point(size=0.5)+
  facet_wrap(~flood, scales='free')

FR.class<-DO.trim%>% 
  left_join(SpC)%>%
  arrange(ID, Date)%>%
  fill(SpC, .direction = 'down')%>%
  filter(count>-7*24, count<7*24)%>%
  mutate(
    class=if_else(class=='RR' & SpC<200 & DO>4, "FR", class),
    class=if_else(class=='RR', "BO", class)
  )%>%
    group_by(ID, flood) %>%
    mutate(
      max_height = which.max(replace(DO, is.na(DO), -Inf)), 
      minimum = case_when(
        row_number() == max_height ~ 0))%>%
    filter(minimum==0)%>%
  select(ID, flood, class)

unique(FR.class$class)
write_csv(FR.class, "04_Outputs/FR.class.csv")



DO.min<-minimum(DO.trim,  DO)

DO.duration<-duration(DO.trim)

recession.lm<-fit_recessions(DO.trim, DO.base, DO, base.DO) 
rise.lm<-fit_rise(DO.trim, DO.base, DO, base.DO) 


flood.impacts.DO<-
  full_join(recession.lm,DO.duration)%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(DO.min, by=c('ID', 'flood'))%>%
  full_join(DO.base, by=c('ID', 'flood'))%>%
  mutate(variable='DO')


write_csv(flood.impacts.DO, "04_Outputs/flood impacts/DO.csv")



