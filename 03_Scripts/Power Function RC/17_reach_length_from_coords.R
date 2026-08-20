rm(list=ls())
library(tidyverse)

# Estimates each site's reach length directly from GPS waypoints instead of
# the values in "01_Raw_data/Depth_length_velocity_width/length width.xlsx"
# (which included a manual override forcing AM's reach to 800m instead of
# 900m -- Samantha flagged this as a past deliberate adjustment made while
# trying to fix GPP/ER, not a surveyed value, so it's being replaced here).
#
# Fill in 01_Raw_data/Depth_length_velocity_width/reach_waypoints_template.csv
# with lat/lon for each site: at minimum seq 1 (upstream/vent) and seq 2
# (downstream station) for a straight-line distance. For a meandering channel,
# add more rows per site (seq 3, 4, ...) tracing the channel -- e.g. points
# digitized off aerial imagery or a GPS track -- and this script will sum the
# consecutive segment distances instead of using the straight-line shortcut,
# which is more accurate for a winding spring run.

waypoints <- read_csv(
  "01_Raw_data/Depth_length_velocity_width/reach_waypoints_template.csv",
  show_col_types = FALSE
) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  arrange(ID, seq)

if (nrow(waypoints) == 0) {
  stop("No coordinates found -- fill in reach_waypoints_template.csv (lat/lon columns) first.")
}

# Haversine great-circle distance (m) between consecutive points -- accurate
# to well under 1% error at these distances (hundreds to low thousands of m),
# far smaller than the uncertainty in the coordinates themselves.
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat/2)^2 + cos(lat1*to_rad) * cos(lat2*to_rad) * sin(dlon/2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

reach_length <- waypoints %>%
  group_by(ID) %>%
  filter(n() >= 2) %>%
  arrange(seq, .by_group = TRUE) %>%
  mutate(
    seg_m = if_else(row_number() == 1, 0,
                     haversine_m(lag(lat), lag(lon), lat, lon))
  ) %>%
  summarise(length_m = sum(seg_m), n_waypoints = n(), .groups = "drop") %>%
  mutate(length_km = length_m / 1000)

cat("=== Reach length from GPS waypoints ===\n")
print(reach_length)

cat("\n=== vs. the values currently used in two station.R / 16_travel_time_correction.R ===\n")
old <- tibble(ID = c("AM","GB","ID","LF"),
              length_m_old = c(800, 350, 2500, 320),  # AM's 800 is the manual override, not the 900 in the excel
              note = c("manually overridden from 900 in length width.xlsx", "", "", ""))
print(left_join(reach_length, old, by = "ID"))

write_csv(reach_length, "04_Outputs/Power Function RC/reach_length_from_coords.csv")
cat("\nWrote 04_Outputs/Power Function RC/reach_length_from_coords.csv\n")
