rm(list=ls())
library(tidyverse)
library(readxl)

# Reach length from real coordinates, replacing the "length width.xlsx"
# values (which included a manual AM override Samantha flagged as a past
# deliberate tweak, not a surveyed value -- see [[day-night-classification-fix]]).
#
# Source: "01_Raw_data/Long Lat.xlsx" (headspring + confluence/downstream-
# sensor coordinates, degrees-minutes-seconds format).
#
# IMPORTANT: headspring-to-confluence is a STRAIGHT LINE. These spring runs
# meander, so straight-line distance underestimates true channel length.
# "01_Raw_data/Depth_length_velocity_width/channel_bends.csv" holds optional
# intermediate waypoints (ID, seq, lat, lon) traced along the actual channel
# between headspring and confluence -- when present for a site, this script
# sums the point-to-point distance through all of them (headspring -> bend 1
# -> bend 2 -> ... -> confluence) instead of using the straight-line shortcut.
# Sites with no bend points yet fall back to straight-line, clearly flagged,
# since that's a real underestimate, not an equivalent alternative.

## ---- parse "30 deg 9'46.74\"N" style DMS strings to decimal degrees ----
parse_dms <- function(x) {
  m <- str_match(x, "([0-9.]+)\\D+([0-9.]+)'([0-9.]+)\"?\\s*([NSEW])")
  deg <- as.numeric(m[,2]); min <- as.numeric(m[,3]); sec <- as.numeric(m[,4]); dir <- m[,5]
  dd <- deg + min/60 + sec/3600
  sign <- if_else(dir %in% c("S","W"), -1, 1)
  dd * sign
}

## ---- Haversine great-circle distance (m) ----
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000; to_rad <- pi/180
  dlat <- (lat2-lat1)*to_rad; dlon <- (lon2-lon1)*to_rad
  a <- sin(dlat/2)^2 + cos(lat1*to_rad)*cos(lat2*to_rad)*sin(dlon/2)^2
  2*R*asin(pmin(1, sqrt(a)))
}

## ---- read headspring/confluence endpoints ----
endpoints_raw <- read_excel("01_Raw_data/Long Lat.xlsx", sheet = "Sheet1")

endpoints <- endpoints_raw %>%
  transmute(
    ID = Site,
    headspring_lat = parse_dms(X.headspring), headspring_lon = parse_dms(Y.headspring),
    confluence_lat = parse_dms(X.Confluence), confluence_lon = parse_dms(Y.Confluence)
  )

cat("=== Parsed headspring/confluence coordinates (decimal degrees) ===\n")
print(endpoints)

straight_line <- endpoints %>%
  mutate(straight_line_m = haversine_m(headspring_lat, headspring_lon, confluence_lat, confluence_lon))

## ---- optional traced-channel bends ----
bends_path <- "01_Raw_data/Depth_length_velocity_width/channel_bends.csv"
bends <- read_csv(bends_path, show_col_types = FALSE)

sites <- endpoints$ID
result <- map_dfr(sites, function(s) {
  site_bends <- bends %>% filter(ID == s)
  ep <- endpoints %>% filter(ID == s)

  if (nrow(site_bends) == 0) {
    sl <- straight_line %>% filter(ID == s) %>% pull(straight_line_m)
    return(tibble(ID = s, length_m = sl, length_km = sl/1000, n_points = 2,
                   method = "straight-line (NO bend points yet -- underestimate)"))
  }

  pts <- bind_rows(
    tibble(seq = -1, lat = ep$headspring_lat, lon = ep$headspring_lon),
    site_bends %>% select(seq, lat, lon),
    tibble(seq = 999999, lat = ep$confluence_lat, lon = ep$confluence_lon)
  ) %>% arrange(seq)

  seg_m <- haversine_m(head(pts$lat,-1), head(pts$lon,-1), tail(pts$lat,-1), tail(pts$lon,-1))
  tot <- sum(seg_m)
  tibble(ID = s, length_m = tot, length_km = tot/1000, n_points = nrow(pts),
         method = "traced channel (headspring -> bends -> confluence)")
})

cat("\n=== Reach length: traced channel (or straight-line fallback) ===\n")
print(result)

cat("\n=== vs. straight-line headspring-confluence (sinuosity check) ===\n")
print(left_join(result, straight_line %>% select(ID, straight_line_m), by = "ID") %>%
        mutate(sinuosity = round(length_m / straight_line_m, 2)))

cat("\n=== vs. values previously used in the pipeline ===\n")
old <- tibble(ID = c("AM","GB","ID","LF"),
              length_m_old = c(800, 350, 2500, 320),
              note_old = c("manually overridden from 900 in length width.xlsx", "", "", ""))
print(left_join(result, old, by = "ID"))

out <- result %>% transmute(ID, length_m, length_km)
write_csv(out, "04_Outputs/Power Function RC/reach_length_from_coords.csv")
cat("\nWrote 04_Outputs/Power Function RC/reach_length_from_coords.csv\n")
cat("(feeds into 18_travel_time_correction_gps_reach.R)\n")
