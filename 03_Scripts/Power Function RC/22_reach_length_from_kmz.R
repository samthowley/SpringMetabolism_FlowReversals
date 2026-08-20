rm(list=ls())
library(tidyverse)

# Reach length from traced channel paths (KMZ, one per site, drawn in Google
# Earth from headspring to confluence following the actual channel bends --
# supersedes 21_reach_length_from_headspring_confluence.R's straight-line
# fallback, and the manually-entered channel_bends.csv approach). Each KMZ
# is a zip containing a doc.kml with a single <LineString><coordinates>
# block: "lon,lat,elev lon,lat,elev ..." in path order.

kmz_dir <- "01_Raw_data/Spring Channel KMZ"
kmz_files <- list.files(kmz_dir, pattern = "\\.kmz$", full.names = TRUE)

haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000; to_rad <- pi/180
  dlat <- (lat2-lat1)*to_rad; dlon <- (lon2-lon1)*to_rad
  a <- sin(dlat/2)^2 + cos(lat1*to_rad)*cos(lat2*to_rad)*sin(dlon/2)^2
  2*R*asin(pmin(1, sqrt(a)))
}

parse_kmz_path <- function(path) {
  id <- tools::file_path_sans_ext(basename(path))
  tmpdir <- tempfile()
  dir.create(tmpdir)
  unzip(path, exdir = tmpdir)
  kml_file <- list.files(tmpdir, pattern = "\\.kml$", full.names = TRUE)[1]
  kml_text <- paste(readLines(kml_file, warn = FALSE), collapse = " ")

  coord_block <- str_match(kml_text, "<coordinates>\\s*(.*?)\\s*</coordinates>")[,2]
  triplets <- str_trim(str_split(coord_block, "\\s+")[[1]])
  triplets <- triplets[triplets != ""]

  parts <- str_split(triplets, ",")
  lon <- map_dbl(parts, ~as.numeric(.x[1]))
  lat <- map_dbl(parts, ~as.numeric(.x[2]))

  tibble(ID = id, seq = seq_along(lon), lat = lat, lon = lon)
}

all_paths <- map_dfr(kmz_files, parse_kmz_path)

cat("=== Points per traced path ===\n")
print(all_paths %>% count(ID, name = "n_points"))

reach_length <- all_paths %>%
  arrange(ID, seq) %>%
  group_by(ID) %>%
  mutate(seg_m = if_else(row_number()==1, 0, haversine_m(lag(lat), lag(lon), lat, lon))) %>%
  summarise(length_m = sum(seg_m), n_points = n(), .groups = "drop") %>%
  mutate(length_km = length_m/1000)

straight_line <- all_paths %>%
  group_by(ID) %>%
  summarise(sl_m = haversine_m(first(lat), first(lon), last(lat), last(lon)), .groups = "drop")

cat("\n=== Traced channel length (KMZ) vs straight-line (sinuosity) ===\n")
print(left_join(reach_length, straight_line, by = "ID") %>%
        mutate(sinuosity = round(length_m / sl_m, 2)))

cat("\n=== vs. values previously used in the pipeline ===\n")
old <- tibble(ID = c("AM","GB","ID","LF"),
              length_m_old = c(800, 350, 2500, 320),
              note_old = c("manually overridden from 900 in length width.xlsx", "", "", ""))
print(left_join(reach_length, old, by = "ID") %>%
        mutate(pct_diff = round(100*(length_m-length_m_old)/length_m_old, 0)))

out <- reach_length %>% select(ID, length_m, length_km)
write_csv(out, "04_Outputs/Power Function RC/reach_length_from_coords.csv")
cat("\nWrote 04_Outputs/Power Function RC/reach_length_from_coords.csv (from traced KMZ paths)\n")
cat("(feeds into 18_travel_time_correction_gps_reach.R)\n")
