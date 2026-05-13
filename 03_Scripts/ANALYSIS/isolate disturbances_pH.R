source("03_Scripts/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
pH<-master%>%select(Date, ID, depth, pH)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
pH_flagged <- pH %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(pH))

# --- Baseline and minimum ---------------------------------------------------
pH.base <- baseline(pH_flagged, pH)

pH.min <- minimum(pH_flagged, pH)

# --- Compile outputs --------------------------------------------------------
flood.impacts.pH <-
  full_join(pH.min, pH.base, by = c("ID", "flood")) %>%
  mutate(variable = "pH")

write_csv(flood.impacts.pH, "04_Outputs/flood impacts/pH.csv")
