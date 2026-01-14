library(plotly)
library(tidyverse)
library(readxl)
library(measurements)
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
    depth_smooth = predict(loess(depth ~ t_num, span = 0.1)))


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
      slope>8.9*10^-3~'Y',
      TRUE~'N')
    )%>%
  select(-start_date, -end_date, -day)%>%
  arrange(ID, Date)%>%filter(abs.slope>0.01)


find.floods <- isolate %>%
  arrange(ID, Date) %>%
  mutate(slope_pos = slope > 0) %>%
  group_by(ID) %>%
  mutate(
    pos_id = consecutive_id(slope_pos)  # Positional only!
  ) %>%
  ungroup() %>%
  mutate(
    flood = if_else(slope_pos, pos_id, NA_integer_)
    ) %>%
  select(-slope_pos)%>%
  fill(flood, .direction = 'down')



flood.periods<-find.floods%>% 
  group_by(ID, flood)%>%
  summarise(
    start=min(Date, na.rm=T),
    end=max(Date, na.rm=T)
  )

write_csv(flood.periods, "01_Raw_data/flood.periods.csv")





