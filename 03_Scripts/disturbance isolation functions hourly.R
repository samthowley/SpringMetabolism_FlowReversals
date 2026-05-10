source("03_Scripts/disturbance isolation functions.R")

# Second-pass gap trim — for hourly data only.
# Removes disconnected pre/post segments created by the baseline filter.
# Gap detection runs on the already-filtered output, so it catches calendar-day
# gaps produced when above-baseline rows are removed (e.g. DO briefly recovering
# then dipping again). Not appropriate for daily data where baseline crossings
# are a natural part of the recession/rise trajectory.
trim_gaps <- function(df, gap_days = 7) {
  df %>%
    group_by(ID, flood) %>%
    mutate(
      tg_gap_d    = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      tg_gap_post = stage.flood == 'post' & !is.na(tg_gap_d) & tg_gap_d > gap_days,
      tg_after    = cumsum(coalesce(tg_gap_post, FALSE)) > 0,
      tg_gap_pre  = stage.flood == 'pre'  & !is.na(tg_gap_d) & tg_gap_d > gap_days,
      tg_before   = rev(cumsum(rev(coalesce(tg_gap_pre, FALSE)))) > 0
    ) %>%
    filter(!(tg_after  & stage.flood == 'post'),
           !(tg_before & stage.flood == 'pre')) %>%
    select(-tg_gap_d, -tg_gap_post, -tg_after, -tg_gap_pre, -tg_before) %>%
    ungroup()
}

# --- Hourly prep functions ---------------------------------------------------
# First-pass gap detection runs on the FULL data (before baseline filter) so
# that rows temporarily above baseline do not create artificial gaps.
# trim_gaps() then does a second pass on the filtered output to catch islands
# of below-baseline data separated by stretches where DO recovered above base.

prep.min.both <- function(df.smooth, variable, variable_loess) {

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
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > 7,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0,
      gap_in_pre      = stage.flood == 'pre'  & !is.na(days_since_last) & days_since_last > 7,
      before_last_gap = rev(cumsum(rev(coalesce(gap_in_pre, FALSE)))) > 0
    ) %>%
    filter(
      {{variable}} < base,
      !(after_first_gap & stage.flood == 'post'),
      !(before_last_gap & stage.flood == 'pre')
    ) %>%
    ungroup() %>%
    trim_gaps()
}

prep.max.both <- function(df.smooth, variable, variable_loess) {

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
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > 7,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0,
      gap_in_pre      = stage.flood == 'pre'  & !is.na(days_since_last) & days_since_last > 7,
      before_last_gap = rev(cumsum(rev(coalesce(gap_in_pre, FALSE)))) > 0
    ) %>%
    filter(
      !(after_first_gap & stage.flood == 'post'),
      !(before_last_gap & stage.flood == 'pre')
    ) %>%
    ungroup() %>%
    trim_gaps()
}
