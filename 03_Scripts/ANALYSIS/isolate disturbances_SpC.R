source("03_Scripts/ANALYSIS/disturbance isolation functions.R")

# --- Data loading -----------------------------------------------------------
SpC<-master%>%select(Date, ID, depth, SpC)

floods <- read_csv("01_Raw_data/flood.periods.csv") %>%
  mutate(start = as.Date(start), end = as.Date(end))

# --- Flag flood periods -----------------------------------------------------
SpC_flagged <- SpC %>%
  left_join(
    floods, by = join_by(ID, between(Date, start, end))
  ) %>%
  select(-start, -end) %>%
  arrange(ID, Date) %>%
  filter(!is.na(SpC))

# --- Baseline and minimum ---------------------------------------------------
SpC.base <- baseline(SpC_flagged, SpC)

SpC.min <- minimum(SpC_flagged, SpC)

# --- Compile outputs --------------------------------------------------------
flood.impacts.SpC <-
  full_join(SpC.min, SpC.base, by = c("ID", "flood")) %>%
  mutate(variable = "SpC")

write_csv(flood.impacts.SpC, "04_Outputs/flood impacts/SpC.csv")
