source("03_Scripts/disturbance isolation functions.R")

floods <- read_csv("01_Raw_data/flood.periods.csv")%>%
  mutate(
    start=as.Date(start), end =as.Date(end)
  )

h <- read_csv("02_Clean_data/Chem/depth.csv")%>%
  mutate(Date=as.Date(Date))%>%
  distinct(ID, Date, .keep_all = T)

class <- read_csv("04_Outputs/flood impacts/stage.csv")%>%
  select(ID, flood, class)%>%
  filter(!is.na(flood))

file.names <- list.files(path="04_Outputs/one station results", pattern=".csv", full.names=TRUE)
onestation.df <- data.frame()
for(fil in file.names){
  df <- read_csv(fil)
  onestation.df <- rbind(onestation.df, df)}

onestation<-onestation.df%>%
  rename(GPP=GPP_daily_mean,
         ER=ER_daily_mean,
         K6001=K600_daily_mean,
         Date=date)%>%
  separate(ID,into = c('ID', 'stage'),sep='_')%>%
  select(-ER_Rhat, -K600_daily_Rhat,-stage)%>%
  arrange(ID, Date)


met <- onestation %>%
  select(ID,Date, GPP, ER)%>%
  left_join(
    floods, by = join_by(ID, between(Date,start,end))
  )%>% 
  filter(!is.na(GPP))%>%select(-start, -end)%>%

  mutate(
    # flood=if_else(ID=='AM' & Date>'2024-05-12', 15, flood),
    # flood=if_else(ID=='LF' & Date>'2024-05-13', 16, flood),
    # flood=if_else(ID=='OS' & Date>'2024-06-05', 12, flood),
    day=Date
  )%>% full_join(class)%>% full_join(h)

# GPP############

GPP<-met%>%select(Date, day, ID, GPP, depth, flood, class)
GPP.base<-baseline(GPP, GPP)

GPP.HI<-GPP%>% filter(class=='HI', !is.na(GPP))
GPP.RR<-GPP%>% filter(class=='RR', !is.na(GPP))

#GPP.trimmed.HI<-trim.greater.than1(GPP.HI, GPP.base, base.GPP, GPP)

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
GPP.smooth.HI<-smooth(GPP.HI, GPP)
GPP.count.HI<-count.max(GPP.smooth.HI, GPP_loess)


GPP.max<-GPP.count.HI%>% filter(count==0)

GPP.compare.HI<-flood.base_compare(GPP.max, GPP.base, GPP)

recession.lm.HI<-fit_recessions.greater1(GPP.count.HI, GPP.base, GPP, base.GPP)
rise.lm.HI<-fit_recessions.less1(GPP.count.HI, GPP.base, GPP, base.GPP)

GPP.HI.impacts<-full_join(GPP.compare.HI, recession.lm.HI)%>%
                            full_join(rise.lm.HI)




#GPP.trimmed.RR<-trim.less.than1(GPP.RR, GPP.base, base.GPP, GPP)
GPP.smooth.RR<-smooth(GPP.RR, GPP)
GPP.count.RR<-count.min(GPP.smooth.RR, GPP_loess)

GPP.max<-GPP.count.RR%>% filter(count==0)

GPP.compare.RR<-flood.base_compare(GPP.max, GPP.base, GPP)
recession.lm.RR<-fit_recessions.greater1(GPP.count.RR, GPP.base, GPP, base.GPP)
rise.lm.RR<-fit_recessions.less1(GPP.count.RR, GPP.base, GPP, base.GPP)

  
GPP.RR.impacts<-full_join(GPP.compare.RR, recession.lm.RR)%>%
  full_join(rise.lm.RR)%>%
  rbind(GPP.HI.impacts)





# ER############

ER<-met%>%select(Date, day, ID, ER, depth, flood, class)
ER.base<-baseline(ER, ER)


ER.HI<-ER%>% filter(class=='HI', !is.na(ER))
ER.RR<-ER%>% filter(class=='RR', !is.na(ER))

#ER.trimmed.HI<-trim.greater.than1(ER.HI, ER.base, base.ER, ER)

ER.smooth.HI<-smooth(ER.HI, ER)
ER.count.HI<-count.min(ER.smooth.HI, ER_loess)

ER.max<-ER.count.HI%>% filter(count==0)

ER.compare.HI<-flood.base_compare(ER.max, ER.base, ER)

recession.lm.HI<-fit_recessions.greater1(ER.count.HI, ER.base, ER, base.ER)
rise.lm.HI<-fit_recessions.less1(ER.count.HI, ER.base, ER, base.ER)

ER.HI.impacts<-full_join(ER.compare.HI, recession.lm.HI)%>%
  full_join(rise.lm.HI)




#ER.trimmed.RR<-trim.less.than1(ER.RR, ER.base, base.ER, ER)
ER.smooth.RR<-smooth(ER.RR, ER)
ER.count.RR<-count.max(ER.smooth.RR, ER_loess)

ER %>%
  filter(ID=='AM') %>%
  #mutate(flood = as.factor(flood)) %>%
  ggplot(aes(x = Date, y = ER)) +
  geom_point(color = "black") +
  geom_point(aes(y=depth*-5),color='pink')+
  #geom_line(aes(y=ER_loess),color='red', size=2, alpha=0.3)+
  
  facet_wrap(~ flood+class, scales = "free")

ggplotly(ER.count.HI %>%
           filter(ID == "GB", !is.na(flood)) %>%
           mutate(flood = as.factor(flood)) %>%
           ggplot(aes(x = count, y = ER)) +
           geom_point(color = "black") +
           geom_point(aes(y=depth*1),color='pink')+
           geom_point(aes(y=ER_loess),color='red', size=2, shape=1)+
           
           facet_wrap(~ flood, scales = "free"))


ggplotly(ER.count.HI %>%
           filter(ID == "AM", !is.na(Date), count>0) %>%
           mutate(flood = as.factor(flood)) %>%
           ggplot(aes(x = Date, y = GPP)) +
           geom_point(color = "black") +
           #geom_point(aes(y=depth*3),color='pink')+
           geom_point(aes(y=GPP_loess),color='red', size=2, shape=1)+
           geom_smooth(method='lm', color='blue', alpha=0.3)+
           theme_minimal()+
           facet_wrap(~ flood, scales = "free"))




ER.max<-ER.count.RR%>% filter(count==0)

ER.compare.RR<-flood.base_compare(ER.max, ER.base, ER)
recession.lm.RR<-fit_recessions.greater1(ER.count.RR, ER.base, ER, base.ER)
rise.lm.RR<-fit_recessions.less1(ER.count.RR, ER.base, ER, base.ER)


ER.RR.impacts<-full_join(ER.compare.RR, recession.lm.RR)%>%
  full_join(rise.lm.RR)%>%
  rbind(ER.HI.impacts)






