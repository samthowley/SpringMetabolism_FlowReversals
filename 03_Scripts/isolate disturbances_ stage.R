rm(list=ls())

file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(1, 2, 4, 7, 10)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

#master<-master %>%  mutate(min = minute(Date)) %>% filter(min==0) %>%select(-min)

chem <- master%>%
  mutate(Temp_C = fahrenheit.to.celsius(Temp),
         Temp_K=Temp_C+273.15,
         exp=2400*((1/Temp_K)-(1/298.15)),
         KH=0.034*2.178^(exp),
         CO2.mg.L=CO2/10^6*KH*44.01*10^3)

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
  arrange(ID, Date)

ggplot(check%>% filter( slope>0.025), aes(x = Date, y=depth, color=slope)) +
  scale_color_gradient(low='blue', high='red')+
  geom_point()+
  facet_wrap(~ID, scales='free')

