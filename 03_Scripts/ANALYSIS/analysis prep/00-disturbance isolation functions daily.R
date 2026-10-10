source("03_Scripts/ANALYSIS/00-disturbance isolation functions.R")

# --- Daily prep functions ----------------------------------------------------
# No trimming: every row in a flood group is kept. These only add
#   within_baseline, threshold, recovered  (recovery bookkeeping)
#   count                                  (0 = peak, <0 pre, >0 post)
#   stage.flood                            ('pre' / 'post')
# Baseline, gap and depth-date trims were removed; the previous versions are in
# archive/00-disturbance isolation functions daily_with-trims.R

prep.min.both.daily <- function(df.smooth, variable, variable_loess) {

  df.recover <- df.smooth %>%
    group_by(ID, flood) %>%
    mutate(
      date            = as.Date(Date),
      within_baseline = {{variable}} / base,
      threshold       = if_else(any(within_baseline < 0.8, na.rm = TRUE), 0.8, 1.0),
      recovered       = if_else(within_baseline >= threshold, "recovered", NA_character_)
    )

  count.min(df.recover, {{variable_loess}}) %>%
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    mutate(stage.flood = if_else(count >= 0, 'post', 'pre')) %>%
    ungroup()
}

prep.max.both.daily <- function(df.smooth, variable, variable_loess) {

  df.recover <- df.smooth %>%
    group_by(ID, flood) %>%
    mutate(
      date            = as.Date(Date),
      within_baseline = {{variable}} / base,
      threshold       = if_else(any(within_baseline > 1.2, na.rm = TRUE), 1.2, 1.0),
      recovered       = if_else(within_baseline <= threshold, "recovered", NA_character_)
    )

  count.max(df.recover, {{variable_loess}}) %>%
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    mutate(stage.flood = if_else(count >= 0, 'post', 'pre')) %>%
    ungroup()
}
