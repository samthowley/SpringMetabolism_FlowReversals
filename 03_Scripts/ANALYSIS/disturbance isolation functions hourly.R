source("03_Scripts/ANALYSIS/disturbance isolation functions.R")

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

prep.min.both <- function(df.smooth, variable, variable_loess) {

  df.recover <- df.smooth %>%
    group_by(ID, flood) %>%
    mutate(
      date            = as.Date(Date),
      within_baseline = {{variable}} / base,
      threshold       = if_else(any(within_baseline < 0.8, na.rm = TRUE), 0.8, 1.0),
      recovered       = if_else(within_baseline >= threshold, "recovered", NA_character_)
    )

  count.min(df.recover, {{variable}}) %>%
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
      {{variable_loess}} < base,
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
  
  count.max(df.recover, {{variable}}) %>%
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
      {{variable_loess}} > base,
      !(after_first_gap & stage.flood == 'post'),
      !(before_last_gap & stage.flood == 'pre')
    ) %>%
    ungroup() %>%
    trim_gaps()
}

