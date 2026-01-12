
DO <- read_csv("02_Clean_data/Chem/DO.csv")

floods <- read_excel("01_Raw_data/flood periods.xlsx", 
                            sheet = "DO")%>%
  mutate(
    start = ymd(start), 
    end   = ymd(end))%>%
  select(ID, start, end, flood.event)


DO_flagged <- DO %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)


ggplot(DO_flagged%>% filter(!is.na(flood.event), ID=='LF'), 
       aes(x = Date, y=DO)) +
  geom_point()+
  geom_smooth(method='loess', color='black')+
  facet_wrap(~flood.event, scales='free')


#loess smoothing###########

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

#t to baseline and peak#########

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

#flood water quality############
ggplot(flood.peak%>%filter(ID=='AM'), 
       aes(x = Date, color=peak.flood)) +
  geom_point(aes(y=depth), size=1)+
  facet_wrap(~ID, scales='free')

flood.peak<-count.hours%>%
  mutate(peak.flood=case_when(h_count.depth > -30 & h_count.depth< 30 ~ "peak"))

#time btwn events#########

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


