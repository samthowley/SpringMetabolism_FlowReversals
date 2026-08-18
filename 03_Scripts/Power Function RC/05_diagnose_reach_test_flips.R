# Why does the two-station / one-station split interleave (two, one, two...)
# instead of a single clean crossover at some depth? Reruns the reach-test
# logic from "one station.R" for the M7 K600 series, WITHOUT dropping the
# excluded ('below') rows, so we can see exactly which test fails and whether
# it's a function of depth or of something else (velocity reversal). The
# choice of K600 methodology barely matters for this diagnostic -- almost all
# exclusions are driven by velocity<0, which doesn't depend on K600 at all.

library(tidyverse)
library(weathermetrics)
library('StreamMetabolism')
library(readxl)

outdir <- "04_Outputs/Power Function RC"
method <- "M7_breakpoint_domain"  # any single method is enough to show the mechanism

width <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "width ")
length_tbl <- read_excel("01_Raw_data/Depth_length_velocity_width/length width.xlsx", sheet = "length ")
area <- left_join(width, length_tbl, by = "ID") %>%
  mutate(area = w * m) %>%
  mutate(m = if_else(ID == "AM", 800, m))

file.names <- list.files(path = "02_Clean_data/Chem", pattern = ".csv", full.names = TRUE)
base_data <- lapply(file.names[c(2, 4, 12)], function(x) read_csv(x, col_types = cols(ID = col_character())))
VentDO <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE)
k600 <- read_csv(file.path(outdir, paste0("K600_", method, ".csv")), col_types = cols(ID = col_character())) %>%
  mutate(Date = as.POSIXct(as.character(Date), tz = "UTC"))

master <- reduce(c(base_data, list(k600)), full_join, by = c("ID", "Date")) %>%
  left_join(area, by = "ID")
master <- full_join(master, VentDO, by = c("ID", "Date")) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  fill(VentDO, VentTemp, K600_1.d_daily, .direction = "downup") %>%
  filter(!ID %in% c("OS", "IU")) %>%
  distinct(ID, Date, .keep_all = TRUE)

discharge <- master %>% mutate(discharge = w * depth * velocity * 86400)

# reach test WITHOUT filtering out the failures this time
# NOTE: the actual pipeline's filter() keeps reach.test %in% c('passes','above')
# -- 'above' (reach too long) is INCLUDED, not excluded. Only 'below' (either
# reach too short, or velocity<0) is dropped. Categorized accordingly here.
reach <- discharge %>%
  mutate(
    reach.km = ((velocity * 86400) / K600_1.d_daily) / 10^3,
    fail_reason = case_when(
      velocity < 0 ~ "EXCLUDED: velocity<0 (reversal)",
      reach.km < 0.4 * km ~ "EXCLUDED: reach too short",
      reach.km > 3 * km ~ "included: 'above' (reach long)",
      TRUE ~ "included: 'passes'"
    )
  ) %>%
  filter(!is.na(depth), !is.na(reach.km))

cat("=== Classification counts by site (hourly rows) ===\n")
print(reach %>% count(ID, fail_reason) %>% arrange(ID, desc(n)))

cat("\n=== Is exclusion (EXCLUDED rows) concentrated at high depth, or scattered? ===\n")
cat("(depth quartile x % of hours excluded, per site)\n")
reach %>%
  group_by(ID) %>%
  mutate(depth_q = ntile(depth, 4)) %>%
  group_by(ID, depth_q) %>%
  summarise(pct_excluded = round(100 * mean(startsWith(fail_reason, "EXCLUDED")), 1),
            depth_range = paste(round(range(depth),2), collapse="-"), .groups="drop") %>%
  print(n = 20)

# For each site: within the depth range where the method's two-station output
# actually has data, what fraction of hours at each depth bin pass vs fail,
# and why?
p <- reach %>%
  filter(ID %in% c("AM","GB","ID","LF")) %>%
  mutate(depth_bin = cut(depth, breaks = 20)) %>%
  ggplot(aes(x = depth, fill = fail_reason)) +
  geom_histogram(bins = 60, position = "fill") +
  facet_wrap(~ID, scales = "free_x") +
  labs(title = "Reach-test outcome by depth (M3_robust K600) -- proportion of hours, not count",
       subtitle = "If failures were purely a function of depth this would be clean horizontal bands; interleaving/streakiness shows it isn't",
       x = "depth (m)", y = "fraction of hours", fill = "outcome") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/04_reach_test_outcome_by_depth.png"), p, width = 11, height = 7, dpi = 150)

# Time series view, all sites, to show the flip-flopping directly
p2 <- reach %>%
  mutate(date = as_date(Date)) %>%
  distinct(ID, date, .keep_all = TRUE) %>%
  ggplot(aes(x = date, y = depth, color = fail_reason)) +
  geom_point(size = 0.6) +
  facet_wrap(~ID, scales = "free_y", ncol = 1) +
  labs(title = "Depth over time, colored by reach-test outcome (one point/day)",
       subtitle = "If exclusion were purely high-depth, red/orange would only appear at the top of each panel",
       x = NULL, y = "depth (m)", color = "outcome") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(outdir, "figures/05_reach_test_timeseries.png"), p2, width = 13, height = 11, dpi = 150)

cat("\nDone -> figures/04_reach_test_outcome_by_depth.png, figures/05_reach_test_timeseries.png\n")
