library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
library(mmand)
library(weathermetrics)

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  filter(Date> '2022-01-01', ID %in% c('GB', 'AM', 'LF', 'OS', 'ID'))


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
    depth_smooth = predict(loess(depth ~ t_num, span = 0.05)))

slopes <- chem %>%
  filter(!is.na(depth_smooth), !is.na(Date))%>%
  arrange(ID, Date) %>%
  group_by(ID)%>%
  mutate(
    norm=depth_smooth/mean(depth_smooth, na.rm=T),
    date      = as.Date(Date),
    day_index = as.numeric(date - min(date)) + 1,
    block3    = ((day_index - 1) %/% 3) + 1   # 3‑day group index
  ) %>%ungroup()%>%
  group_by(block3, ID) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    slope      = {
      x <- as.numeric(Date)      # seconds since origin
      y <- norm
      coef(lm(y ~ x))[2] * 86400     # convert to units per day
    },
    .groups = "drop"
  )


isolate <- chem %>%
  mutate(day=as.Date(Date))%>%
  left_join(
    slopes, by = join_by(ID, between(day, start_date, end_date))
  ) %>%
  mutate(
    abs.slope=abs(slope),
    slope=slope,
    flooded=case_when(
      slope>0.0145~'Y',
      TRUE~'N')
    )%>%
  select(-start_date, -end_date, -day)%>%
  arrange(ID, Date)%>%filter(abs.slope>0.0145)

find.floods <- isolate %>%
  arrange(ID, Date) %>%
  mutate(slope_pos = slope > 0) %>%
  group_by(ID) %>%
  mutate(
    pos_id = consecutive_id(slope_pos)  # Positional only!
  ) %>%
  ungroup() %>%
  mutate(flood = if_else(slope_pos, pos_id, NA_integer_)) %>%
  select(-slope_pos)%>%
  fill(flood, .direction = 'down')


(a<-ggplot(find.floods, 
           aes(x = Date, y=depth_smooth, color=as.factor(flood)))+
    geom_point(shape=1)+
    facet_wrap(~ID, scales='free'))

ggplotly(a)

flood.periods<-find.floods%>% 
  group_by(ID, flood)%>%
  summarise(
    start=min(Date, na.rm=T),
    end=max(Date, na.rm=T)
  )
#select(Date, ID, depth, flood)

write_csv(flood.periods, "01_Raw_data/flood.periods.csv")











floods <- read_csv("01_Raw_data/flood periods.xlsx", sheet='stage') %>%
  mutate(
    start = mdy(start), 
    end   = mdy(end))%>%
  select(ID, start, end, flood.event)

stage_flagged <- chem %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end)%>%
  group_by(day, ID)%>%
  arrange(ID, Date)

plot_grid(
  ggplot(stage_flagged_long%>% filter(!is.na(flood.event), ID=='AM'), 
         aes(x = Date, y=value, color=variable, group=interaction(variable))) +
    geom_point()+
    geom_smooth(method='loess', aes(group=interaction(variable)), color='black')+
    scale_y_continuous(
      name = "mg/L",
      sec.axis = sec_axis(~ . /20, name = "stage (m)"))+
    facet_wrap(~flood.event, scales='free'),
  # ,
  # 
  # ggplot(stage_flagged_long%>% filter(ID=='LF',!is.na(flood.event)), 
  #        aes(x = Date, y=SpC)) +
  #   geom_point()+
  #   facet_wrap(~flood.event, scales='free')
  # ,
  nrow=1)



#Functions########
##baseline periods#####
undisturbed <- stage_flagged %>%
  fill(flood.event, .direction='updown')%>%
  filter(flooded == FALSE) %>%
  group_by(ID, flood.event)%>%
  summarize(
    depth.base=mean(depth, na.rm=T),
    DO.base=max(DO, na.rm=T),
    CO2.base=min(CO2, na.rm=T)
  )

##loess smoothing###########

loess.prep.1 <- stage_flagged %>%
  filter(flooded) %>%
  arrange(flood.event, Date) %>%
  group_by(flood.event, ID) %>%
  mutate(
    t = as.numeric(Date - min(Date)),
    group_ID=paste(ID, flood.event, sep = "_")) %>%   # numeric time for loess
  ungroup()%>%
  filter(!is.na(CO2))

loess.prep.2<-left_join(loess.prep.1, undisturbed)%>%
  arrange(ID, Date)%>%
  fill(depth.base, DO.base, CO2.base, .direction = 'down')

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

CO2<-fit_loess_by_group(loess.prep.2%>%filter(!is.na(CO2)), CO2, t, group_ID)
depth<-fit_loess_by_group(CO2%>%filter(!is.na(depth)), depth, t, group_ID)
all<-fit_loess_by_group(depth%>%filter(!is.na(DO)), DO, t, group_ID)

##t to baseline and peak#########

#return
count.hours<-all%>%
  group_by(ID, flood.event) %>%
  mutate(
    max_height = which.max(replace(depth_loess, is.na(depth_loess), -Inf)), 
    h_count.depth = case_when(
      row_number() < max_height ~ row_number() - max_height,
      row_number() == max_height ~ 0,
      row_number() > max_height ~ row_number() - max_height))

locate.peak<-count.hours%>%
  filter(h_count.depth>0)
  
library(lme4)
rC <- lmList(depth ~ h_count.depth | group_ID, data = locate.peak)
(cf <- coef(rC))

cf_df <- as_tibble(cf) %>%
  mutate(ID = names(rC))%>%
  rename('Intercept'="(Intercept)" , slope="h_count.depth")%>%
  separate(ID, into=c('ID', 'flood.event'), sep="_")%>%
  mutate(flood.event=as.numeric(flood.event))


solve.for.t.return<-left_join(cf_df, undisturbed)%>%
  mutate(hours.return=((depth.base-Intercept)/slope)/24)

#peak
locate.rise<-count.hours%>%
  filter(h_count.depth<0)

rC <- lmList(depth ~ h_count.depth | group_ID, data = locate.rise)
(cf <- coef(rC))

cf_df <- as_tibble(cf) %>%
  mutate(ID = names(rC))%>%
  rename('Intercept'="(Intercept)" , slope="h_count.depth")%>%
  separate(ID, into=c('ID', 'flood.event'), sep="_")%>%
  mutate(flood.event=as.numeric(flood.event))


solve.for.t.peak<-left_join(cf_df, undisturbed)%>%
  mutate(hours.return=(abs(depth.base-Intercept)/slope)/24)

#check
regress.chk<-locate.peak%>%mutate(regression=1)%>%
  select(Date, ID, flood.event, regression)

check.regress<-left_join(stage_flagged, regress.chk)

ggplot(check.regress, 
       aes(x = Date, color=regression)) +
  geom_line(aes(y=depth), size=1)+
  facet_wrap(~ID, scales='free')

##flood water quality############
ggplot(flood.peak%>%filter(ID=='AM'), 
       aes(x = Date, color=peak.flood)) +
  geom_point(aes(y=depth), size=1)+
  facet_wrap(~ID, scales='free')

flood.peak<-count.hours%>%
  mutate(peak.flood=case_when(h_count.depth > -30 & h_count.depth< 30 ~ "peak"))

##time btwn events#########

prep.time.btwn<-stage_flagged%>%
  mutate(condition=case_when(
    flooded==TRUE~"flooded",
    flooded==FALSE~"base"
  ))

time.btwn <- prep.time.btwn %>% 
  arrange(ID, Date) %>% 
  group_by(ID) %>%
  mutate(
    group = cumsum(condition == "flooded"),  # Create a grouping variable that increments at each "baseline"
    time.btwn = unlist(ave(condition, group, FUN = function(x) {
      cumsum(x %in% c("base"))
    }))) %>%ungroup()  %>%
  fill(flood.event, .direction = "updown")%>%
  group_by(ID, condition,flood.event)%>%
  summarize(
    time.btwn=max(as.numeric(time.btwn), na.rm = T)/24
  )%>%filter(condition=='base')
 



