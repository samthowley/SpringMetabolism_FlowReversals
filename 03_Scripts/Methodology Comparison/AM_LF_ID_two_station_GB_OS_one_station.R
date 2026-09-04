## =============================================================================
## Mixed recipe: two-station GPP/ER for AM, LF, ID only; GB and OS come from
## one-station results instead (no two-station computed for them at all).
##
##   AM, LF : velocity power RC   |  K600 depth-linear RC
##   ID     : velocity power RC   |  K600 free-water (Bayesian one-station)
##   GB, OS : not in the recipe -- excluded from run_two_station() entirely,
##            so they have no two-station row to coalesce against below and
##            fall back to one-station automatically.
##
## Ends with the same "combine one-station + two-station" merge as the bottom
## of Scratch Pad/19_two_station_FINAL.R (coalesce two-station over
## one-station), adapted because the shared engine's `daily` doesn't carry a
## day/night-split K600 column the way that script's own local pipeline did --
## K6002 here is the daily-mean K600 instead of K600_day.
## =============================================================================
rm(list = ls())
source("03_Scripts/Methodology Comparison/_engine_two_station.R")

## ---- recipe: only AM/LF/ID get two-station -----------------------------------
LABEL <- "AMLF_powerVel_linearK600_ID_powerVel_freewaterK600"

recipe <- tribble(
  ~ID,  ~units,   ~vel_form, ~vel_excl, ~k600_src,   ~k600_pred, ~k600_form, ~k600_excl,
  "AM", "county", "power",   "base",    "RC",        "depth",    "linear",   "base",
  "LF", "county", "power",   "base",    "RC",        "depth",    "linear",   "base",
  "ID", "county", "power",   "base",    "freewater", NA,         NA,         "base"
)

## ---- run two-station for AM/LF/ID only ---------------------------------------
res   <- run_two_station(recipe, LABEL)
daily <- res$daily
score <- score_methodology(daily)


## ---- plot: AM/LF/ID two-station only ------------------------------------------
daily %>%
  mutate(ID = factor(ID, levels = c("AM", "LF", "ID"))) %>%
  ggplot(aes(x = date)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = GPP_RANGE[1], ymax = GPP_RANGE[2], fill = "#1b9e77", alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = ER_RANGE[1],  ymax = ER_RANGE[2],  fill = "#d95f02", alpha = 0.08) +
  geom_point(aes(y = GPP, color = "GPP"), size = 0.8) +
  geom_point(aes(y = ER,  color = "ER"),  size = 0.8) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(GPP = "#1b9e77", ER = "#d95f02")) +
  facet_wrap(~ID, scales = "free", ncol = 1) +
  labs(title = LABEL, subtitle = "AM/LF/ID two-station only; shaded = plausible range",
       x = NULL, y = "g O2 m-2 d-1") +
  theme_bw(base_size = 10)

#### COMBINE ONE STATION AND TWO STATION ########################################
## Same merge as the bottom of Scratch Pad/19_two_station_FINAL.R: coalesce
## two-station over one-station. GB and OS never get a "two" row above, so
## they fall back to "one" here -- that's what makes them one-station-only.

file.names <- list.files(path = "04_Outputs/one station results", pattern = ".csv", full.names = TRUE)
onestation.df <- data.frame()
for (fil in file.names) {
  df <- read_csv(fil, show_col_types = FALSE)
  onestation.df <- rbind(onestation.df, df)
}

onestation <- onestation.df %>%
  rename(GPP1 = GPP_daily_mean,
         ER1  = ER_daily_mean,
         K6001 = K600_daily_mean,
         Date = date) %>%
  separate(ID, into = c('ID', 'stage'), sep = '_') %>%
  mutate(GPP1 = if_else(GPP1 < 0, 0, GPP1),
         model = "1") %>%
  select(-ER_Rhat, -K600_daily_Rhat, -stage) %>%
  arrange(ID, Date)

two <- daily %>%
  select(date, ID, depth, GPP, ER, K600) %>%
  rename(GPP2 = GPP, ER2 = ER, K6002 = K600, Date = date) %>%
  mutate(model = "2",
         GPP2 = if_else(GPP2 < 3, NA, GPP2),
         ER2  = if_else(ER2 > -3, NA, ER2)) %>%
  arrange(ID, Date)

two %>%
  filter(!ID %in% c('OS', 'IU')) %>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP2, shape = 'GPP')) +
  geom_point(aes(y = ER2, shape = 'ER'), shape = 1) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()

all.met <- onestation %>%
  full_join(two, by = c("Date", "ID"), suffix = c("_onestation", "_two")) %>%
  mutate(
    # Prioritize "two" over "onestation"
    GPP = coalesce(GPP2, GPP1),
    ER = coalesce(ER2, ER1),

    # Track which dataset was used -- checks BOTH GPP2/ER2 (not GPP2 alone),
    # so a day isn't mislabeled "neither" just because GPP2 got filtered out
    # while ER2 still had a real value (or vice versa). Only two sources now;
    # a day with nothing in either dataset gets NA and is left out entirely.
    source = case_when(
      !is.na(GPP2) | !is.na(ER2) ~ "two",
      !is.na(GPP1) | !is.na(ER1) ~ "one",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(ID, Date)

all.met %>%
  filter(!ID %in% c('OS', 'IU')) %>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP1, color = '1'), alpha = 0.5) +
  geom_point(aes(y = ER1, color = '1'), alpha = 0.5) +

  geom_point(aes(y = ER2, color = '2'), alpha = 0.5) +
  geom_point(aes(y = GPP2, color = '2'), alpha = 0.5) +
  facet_wrap(~ID, scales = "free") +
  theme_minimal()

## ---- extra: melded GPP/ER over time, ALL sites (incl. GB/OS one-station) -----
## NOTE: all.met already has a `depth` column (from AM/LF/ID's two-station
## side via `two`, NA for GB/OS since they were never in it). Joining another
## column also named `depth` collides -- dplyr silently renames BOTH to
## depth.x/depth.y instead of erroring, aes(x = depth) can't find either one,
## and GB/OS render blank because their depth.x is NA for every row. Named
## this one depth_gbos and coalesced below so AM/LF/ID keep the depth they
## already had and GB/OS pick up the one just joined in.
depth_gbos <- read_csv("02_Clean_data/Chem/depth.csv")%>%
  filter(ID %in% c('GB', 'OS'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(depth_gbos = mean(depth, na.rm = TRUE), .groups = "drop")


all.met %>%
  left_join(depth_gbos, by = c("ID", "Date")) %>%
  mutate(depth = coalesce(depth, depth_gbos)) %>%
  filter(ID != 'IU', !is.na(GPP) | !is.na(ER)) %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS"))) %>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP, color = source), size = 1) +
  geom_point(aes(y = ER,  color = source), size = 1, shape = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol = 3) +
  labs(title = paste(LABEL, "-- melded with one-station (GB, OS)"),
       subtitle = "filled = GPP, open = ER; color = which model supplied the day",
       x = NULL, y = "g O2 m-2 d-1", color = "source") +
  theme_bw(base_size = 10)

