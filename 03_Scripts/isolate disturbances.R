#call in data########

#might be worth seeing if GB has anymore DO data
source("03_Scripts/disturbance isolation functions.R")

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(2, 11, 7, 4, 1)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  mutate(
    min=minute(Date),
    depth=if_else(ID=='GB' & Date>'2024-07-01', NA, depth)
    )%>%
  filter(
    Date> '2022-01-01', 
    ID %in% c('GB', 'AM', 'LF', 'OS', 'ID', 'IU'),
    min==0
    )%>%
  select(-min)


chem <- master %>%
  arrange(Date) %>%
  filter(!is.na(depth))%>%
  mutate(
    Temp_C = fahrenheit.to.celsius(Temp),
    Temp_K = Temp_C + 273.15,
    exp    = 2400 * ((1/Temp_K) - (1/298.15)),
    KH     = 0.034 * 2.178^exp,
    CO2.mg.L = CO2 / 10^6 * KH * 44.01 * 10^3,
    day    = as.Date(Date)
  ) %>%
  group_by(ID)%>%
  mutate(
    t_num = as.numeric(Date),
    depth_smooth = predict(loess(depth ~ t_num, span = 0.1)))


slopes <- chem %>%
  filter(!is.na(depth_smooth), !is.na(Date))%>%
  arrange(ID, Date) %>%
  group_by(ID)%>%
  mutate(
    norm=depth_smooth/mean(depth_smooth, na.rm=T),
    date      = as.Date(Date),
    # 3-day index starting at the first date in your data
    day_index = as.integer(date - min(date)),
    block3    = day_index %/% 3) %>%ungroup()%>%
  group_by(block3, ID) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    slope      = {
      x <- as.numeric(Date)      # seconds since origin
      y <- norm
      coef(lm(y ~ x))[2] * 86400     # convert to units per day
    },
    .groups = "drop")


isolate <- chem %>%
  mutate(day=as.Date(Date))%>%
  left_join(
    slopes, by = join_by(ID, between(day, start_date, end_date))
  ) %>%
  mutate(
    abs.slope=abs(slope),
    )%>%
  select(-start_date, -end_date, -day)%>%
  arrange(ID, Date)%>%filter(abs.slope>0.01)#%>%filter(abs.slope>0.01)


iso_flagged <- isolate %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(
    slope_pos = slope > 0,
    run_id    = consecutive_id(slope_pos)
  ) %>%
  ungroup()


# summarize each run
runs <- iso_flagged %>%
  group_by(ID, run_id, slope_pos) %>%
  summarise(
    run_start = min(Date),
    run_end   = max(Date),
    .groups = "drop"
  ) %>%
  arrange(ID, run_start) %>%
  group_by(ID) %>%
  mutate(
    next_pos   = lead(slope_pos),
    next_end   = lead(run_end),
    next_runid = lead(run_id)
  ) %>%
  ungroup()


# define flood periods: positive run followed immediately by negative run
flood.periods <- runs %>%
  filter(slope_pos == TRUE, next_pos == FALSE) %>%
  group_by(ID) %>%
  mutate(flood = row_number()) %>%   # flood event number per ID
  ungroup() %>%
  transmute(
    ID,
    flood,
    start = run_start,
    end   = next_end
  )

write_csv(flood.periods, "01_Raw_data/flood.periods.csv")

#flood impacts by stage############
source("03_Scripts/disturbance isolation functions.R")

floods <- read_csv("01_Raw_data/flood.periods.csv")

stage_flagged <- master %>%
  full_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end)%>%
  arrange(ID, Date)


ggplot(stage_flagged, aes(x=Date, y=depth, color=as.factor(flood)))+
  geom_point(size=0.5)+
  facet_wrap(~ID, scales='free')


depth.base<-baseline(stage_flagged, depth)

fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.3, min_rows = 5) {
  y_name <- rlang::as_name(rlang::enquo(y_var))
  x_name <- rlang::as_name(rlang::enquo(x_var))
  g_name <- rlang::as_name(rlang::enquo(group_var))
  
  split_list <- split(df, df[[g_name]])
  
  lapply(split_list, function(.x) {
    # Remove NAs pairwise for this group/var
    complete_cases <- complete.cases(.x[[y_name]], .x[[x_name]])
    .x_clean <- .x[complete_cases, ]
    
    if (nrow(.x_clean) < min_rows) {
      message("Skip group with only ", nrow(.x_clean), " complete cases (min: ", min_rows, ")")
      return(NULL)
    }
    
    fit <- loess(.x_clean[[y_name]] ~ .x_clean[[x_name]], span = span)
    
    # Predict on full original rows (fills NA with NA)
    .x %>%
      mutate(!!paste0(y_name, "_loess") := predict(fit, newdata = .x[[x_name]]))
  }) %>%
    compact() %>%
    bind_rows()
}
depth.smooth<-smooth(stage_flagged, depth)

depth.count<-count.max(depth.smooth, depth_loess)

depth.max<-maximum(depth.count, depth)

depth.compare<-flood.base_compare(depth.max, depth.base, maximum)

prep<- depth.smooth %>% 
  mutate(
    date=as.Date(Date),
    flooded=case_when(
      !is.na(flood)~'flooded',
      TRUE~'norm')
  )%>%
  fill(flood, .direction = 'down')


prep%>%
  filter(ID=='LF', !is.na(flood))%>%
ggplot(aes(x=Date, y=depth, color=as.factor(flood)))+
  geom_point(size=0.5)+
  facet_wrap(~flood, scales='free')



time.btwn<- prep %>%
  filter(flooded=='norm')%>%
  group_by(ID, flood)%>%
  mutate(
    date=as.Date(Date),
    time.btwn=n_distinct(date)
  )%>% 
  summarise(
    time.btwn=max(time.btwn)
  )

depth.duration<- duration(prep)


recession.lm<-fit_recessions(depth.count, depth.base, depth, base.depth) 
rise.lm<-fit_rise(depth.count, depth.base, depth, base.depth) 

flood.impacts.depth<-
  full_join(recession.lm,depth.duration)%>%
  full_join(depth.compare, by=c('ID', 'flood'))%>%
  full_join(rise.lm, by=c('ID', 'flood'))%>%
  full_join(depth.max, by=c('ID', 'flood'))%>%
  full_join(depth.base, by=c('ID', 'flood'))%>%
  full_join(time.btwn, by=c('ID', 'flood'))


#SpC and pH###################

SpC <- read_csv("02_Clean_data/Chem/SpC.csv")%>%
  full_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    flood=if_else(ID=='LF' & Date>'2024-05-22', 16, flood),
    flood=if_else(ID=='OS' & Date>'2024-05-21', 12, flood),
    #SpC=if_else(ID=='AM' & flood==3 & SpC<350, NA, SpC),
    #SpC=if_else(ID=='AM' & flood==3 & SpC<350, NA, SpC)
    
  )%>%filter(!is.na(SpC))

spc.base<-baseline(SpC, SpC)
spc.count<-count.min(SpC, SpC)
spc.min<-spc.count%>% filter(count==0)%>%select(ID, flood, SpC)

pH <- read_csv("02_Clean_data/Chem/pH.csv")%>%full_join(
  floods, by = join_by(ID, between(Date, start, end))
) %>%
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  mutate(
    flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    flood=if_else(ID=='LF' & Date>'2024-05-22', 16, flood),
    flood=if_else(ID=='OS' & Date>'2024-05-21', 12, flood),
  )%>%filter(!is.na(pH))

ph.base<-baseline(pH, pH)
ph.count<-count.min(pH, pH)
ph.min<-ph.count%>% filter(count==0)%>%select(ID, flood, pH)

# (a<-ggplot(ph.count %>% filter(ID=='AM'),
#            aes(x = Date, y=pH, color=as.factor(flood))) +
#     geom_point()+
#     facet_wrap(~flood, scales='free'))
# ggplotly(a)


stage<-flood.impacts.depth%>%
  full_join(spc.min)%>%
  full_join(ph.min)%>%
  mutate(
    class=case_when(
      pH<7 ~'RR',
      SpC<100 ~ 'RR',
      TRUE~'HI'
    )
  )

write_csv(stage, "04_Outputs/flood impacts/depth.csv")

#figures##########


floods.day <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(
    start=as.Date(start), end =as.Date(end)
  )

SpC.avg<-SpC%>%
  mutate(Date=as.Date(Date))%>%
  group_by(Date, ID)%>%
  mutate(SpC=mean(SpC, na.rm=T))

metabolism <- read_csv("04_Outputs/master.metabolism.csv")%>%
  full_join(
    floods.day, by = join_by(ID, between(Date, start, end))
  )%>%
  left_join(SpC.avg)


metabolism%>%
  filter(ID=='AM')%>%
  ggplot(aes(x=depth, y=ER, color=SpC))+
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~ID, scales='free')


  

p1 <- stage_flagged %>%
  filter(ID == 'OS', flood==2) %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = DO), color = 'red', alpha = 0.3) +
  geom_point(aes(y = CO2/10^3), color = 'black', alpha = 0.3) +
  # geom_smooth(aes(y = DO), method = 'loess', color = 'red') +
  # geom_smooth(aes(y = CO2/10^3), method = 'loess', color = 'black') +
  scale_y_continuous(
    name = "DO mg/L",
    sec.axis = sec_axis(~ . * 10^3, name = "CO2 ppm")
  ) +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')

p2 <- stage_flagged %>%
  filter(ID == 'OS', flood==2) %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = depth), color = 'blue', alpha = 0.3) +
  geom_point(aes(y = SpC/300), color = 'purple', alpha = 0.3) +
  # geom_smooth(aes(y = depth), method = 'loess', color = 'blue') +
  # geom_smooth(aes(y = SpC/100), method = 'loess', color = 'purple') +
  scale_y_continuous(
    name = "depth (m)",
    sec.axis = sec_axis(~ . * 300, name = "SpC")
  ) +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')

p3 <- one %>%
  filter(ID == 'OS', flood==2) %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = ER/-2), color = 'darkred') +
  geom_point(aes(y = GPP), color = 'darkgreen') +
  geom_smooth(aes(y = ER/-2), method = 'loess', color = 'darkred', alpha = 0.3, se=F) +
  geom_smooth(aes(y = GPP), method = 'loess',color = 'darkgreen', alpha = 0.3, se=F) +
  
  scale_y_continuous(
    name = "GPP",
    sec.axis = sec_axis(~ . * -2, name = "ER")
  ) +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')

p4 <- stage_flagged %>%
  filter(ID == 'OS', flood==2) %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = pH), color = 'orange', alpha = 0.3) +
  ylab('pH')+
  # geom_smooth(aes(y = depth), method = 'loess', color = 'blue') +
  # geom_smooth(aes(y = SpC/100), method = 'loess', color = 'purple') +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')

plot_grid(p1, p2, p4,ncol = 1)
library(cowplot)



stage_flagged %>%
  filter(ID == 'OS') %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = depth), color = 'blue', alpha = 0.3) +
  geom_point(aes(y = SpC/300), color = 'purple', alpha = 0.3) +
  # geom_smooth(aes(y = depth), method = 'loess', color = 'blue') +
  # geom_smooth(aes(y = SpC/100), method = 'loess', color = 'purple') +
  scale_y_continuous(
    name = "depth (m)",
    sec.axis = sec_axis(~ . * 300, name = "SpC")
  ) +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')


stage_flagged %>%
  filter(ID == 'OS', flood==2) %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = DO), color = 'red', alpha = 0.3) +
  geom_point(aes(y = CO2/10^3), color = 'black', alpha = 0.3) +
  # geom_smooth(aes(y = DO), method = 'loess', color = 'red') +
  # geom_smooth(aes(y = CO2/10^3), method = 'loess', color = 'black') +
  scale_y_continuous(
    name = "DO mg/L",
    sec.axis = sec_axis(~ . * 10^3, name = "CO2 ppm")
  ) +
  facet_wrap(~ID+flood, scales = 'free') +
  theme_minimal()+
  theme(legend.position = 'right')
