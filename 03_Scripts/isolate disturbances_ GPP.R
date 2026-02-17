source("03_Scripts/disturbance isolation functions.R")

GPP <- read_csv("04_Outputs/master.metabolism.csv")%>%
  select(Date, ID, GPP)%>%
  left_join(
    read_csv("02_Clean_data/Chem/depth.csv")%>%
      mutate(Date=as.Date(Date))%>%
      group_by(ID, Date)%>%
      summarise(depth=mean(depth, na.rm=T))
  )


GPP%>% 
  ggplot(aes(x=depth, y=GPP))+
  geom_point()+
  facet_wrap(~ID, scales='free')

floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(start=as.Date(start), end=as.Date(end))


GPP_flagged <- GPP %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  )%>%  # TRUE if matched an interval
  select(-start, -end)%>%
  arrange(ID, Date)%>%
  filter(!is.na(GPP))%>%
  mutate(date=Date)


GPP.base<-baseline(GPP_flagged, GPP)


fit_loess_by_group <- function(df, y_var, x_var = "t", group_var, span = 0.4, min_rows = 5) {
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

GPP.smooth<-smooth(GPP_flagged, GPP)%>%
  mutate(
    impact= case_when(
      ID=='AM' & flood==4~ 'inc',
      ID=='ID' & flood==1~ 'inc',
      ID=='ID' & flood==2~ 'inc',
      TRUE~"dec"
    )
  )

#high stage periods######

GPP.inc<-GPP.smooth%>%filter(impact=='inc')

GPP.inc.count<-count.max(GPP.inc, GPP_loess)

GPP.inc.prep<-prep.by.slope_increases(GPP.inc.count, GPP_loess)%>%
  mutate(remove = if_else(abs(count)<5, "keep", remove),
         remove=if_else(ID=='ID' & flood==2 & abs(count<45), 'keep', remove))

GPP.inc.trim<-trim(GPP.inc.prep)

GPP.inc.trim%>% filter(ID=='ID')%>%
ggplot(aes(x=count, y=GPP))+
  geom_point(aes(y=GPP_loess, color=remove), size=3)+
  geom_point()+
  geom_smooth(method = lm, aes(group=stage, y=GPP))+
  facet_wrap(~flood, scales='free', nrow=1)+
  theme(legend.position = "bottom")


GPP.inc.duration<-duration(GPP.inc.trim)

GPP.inc.max<-maximum(GPP.inc.trim, GPP)

GPP.inc.compare<-flood.base_compare(GPP.inc.max, GPP.base, maximum)

recession.lm.inc<-fit_recessions(GPP.inc.trim, GPP.base, GPP, base) 
rise.lm.inc<-fit_rise(GPP.inc.trim, GPP.base, GPP, base) 

flood.impacts.GPP.inc<-
  full_join(recession.lm.inc,
            GPP.inc.duration)%>%
  full_join(rise.lm.inc, by=c('ID', 'flood'))%>%
  full_join(GPP.inc.max, by=c('ID', 'flood'))%>%
  full_join(GPP.base, by=c('ID', 'flood'))

#RR periods########

GPP.dec<-GPP.smooth%>%filter(impact=='dec')%>%
  mutate(GPP_loess=if_else(ID=='GB' & flood==1 & Date>'2022-09-29', NA, GPP_loess),
         GPP=if_else(ID=='GB' & flood==1 & Date>'2022-09-29', NA, GPP))
         

GPP.dec.count<-count.min(GPP.dec, GPP_loess)

GPP.dec.prep<-prep.by.slope_decreases(GPP.dec.count, GPP_loess)%>%
  mutate(
    remove = if_else(abs(count)<5, "keep", remove),
    remove = if_else(GPP_loess<2, "keep", remove),
    remove = if_else(ID=='OS' & flood==1, "keep", remove)
    )

GPP.dec.trim<-trim(GPP.dec.prep)%>%
  mutate(
    GPP=if_else(ID=='LF' & flood==5 & Date>'2024-05-15', NA, GPP),
    GPP=if_else(ID=='AM' & flood==2 & Date<'2023-01-22', NA, GPP)
    )
  

GPP.dec.trim%>% 
  filter(ID=='OS')%>%
  ggplot(aes(x=Date, y=GPP))+
  geom_point(aes(y=GPP_loess, color=remove), size=3)+
  geom_point()+
  geom_smooth(method = lm, aes(group=stage, y=GPP))+
  facet_wrap(~flood, scales='free')+
  theme(legend.position = "bottom")



GPP.dec.duration<-duration(GPP.dec.trim)

GPP.dec.min<-minimum(GPP.dec.trim, GPP)


recession.lm.dec<-fit_recessions(GPP.dec.trim, GPP.base, GPP_loess, base) 
rise.lm.dec<-fit_rise(GPP.dec.trim, GPP.base, GPP_loess, base) 

flood.impacts.GPP.dec<-
  full_join(recession.lm.dec,
            GPP.dec.duration)%>%
  full_join(rise.lm.dec, by=c('ID', 'flood'))%>%
  full_join(GPP.dec.min, by=c('ID', 'flood'))%>%
  full_join(GPP.base, by=c('ID', 'flood'))
#combine########

flood.impacts.GPP<-
  rbind(flood.impacts.GPP.dec, flood.impacts.GPP.inc)
  
write_csv(flood.impacts.GPP, "04_Outputs/flood impacts/GPP")



  