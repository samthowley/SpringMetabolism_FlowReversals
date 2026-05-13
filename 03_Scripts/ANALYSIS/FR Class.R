








FR.class <- DO.clean %>%
  left_join(class, by = c('ID', 'flood')) %>%
  left_join(SpC) %>%
  arrange(ID, Date) %>%
  fill(SpC, .direction = 'down') %>%
  filter(count > -7 * 24, count < 7 * 24) %>%
  mutate(
    class = if_else(class == 'RR' & SpC < 200 & DO > 4, "FR", class),
    class = if_else(class == 'RR', "BO", class)
  ) %>%
  group_by(ID, flood) %>%
  mutate(
    max_height = which.max(replace(DO, is.na(DO), -Inf)),
    minimum    = case_when(row_number() == max_height ~ 0)
  ) %>%
  filter(minimum == 0) %>%
  select(ID, flood, class)

write_csv(FR.class, "04_Outputs/FR.class.csv")