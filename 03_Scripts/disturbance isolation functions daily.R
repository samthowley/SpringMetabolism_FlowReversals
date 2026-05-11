source("03_Scripts/disturbance isolation functions.R")

# --- Daily prep functions ----------------------------------------------------
# Gap detection runs on the FULL data (before baseline filter). No second-pass
# trim_gaps: for daily variables (ER, GPP) the recession/rise naturally
# terminates at baseline, so the baseline filter itself acts as the endpoint.
# gap_days controls the minimum missing-data gap that triggers trimming.

prep.min.both.daily <- function(df.smooth, variable, variable_loess, gap_days) {

  df.recover <- df.smooth %>%
    group_by(ID, flood) %>%
    mutate(
      date            = as.Date(Date),
      within_baseline = {{variable}} / base,
      threshold       = if_else(any(within_baseline < 0.8, na.rm = TRUE), 0.8, 1.0),
      recovered       = if_else(within_baseline >= threshold, "recovered", NA_character_)
    )

  clean<-count.min(df.recover, {{variable_loess}}) %>%
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    filter(
      {{variable}} < base)%>%
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > gap_days,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0,
      gap_in_pre      = stage.flood == 'pre'  & !is.na(days_since_last) & days_since_last > gap_days,
      before_last_gap = rev(cumsum(rev(coalesce(gap_in_pre, FALSE)))) > 0
    ) %>%
    filter(
      #{{variable}} < base,
      !(after_first_gap & stage.flood == 'post'),
      !(before_last_gap & stage.flood == 'pre')
    ) %>%
    ungroup()
}

# --- Flood date extraction (daily / loess variable) -------------------------
# Uses the loess-smoothed variable directly. Appropriate for daily data (GPP,
# ER) where each row is already one observation per day.
# flood.start = first of 3 consecutive rows where loess < base*threshold (min)
#               or loess > base*threshold (max) in the pre-peak period.
# flood.end   = first of 3 consecutive rows where loess >= base*threshold (min)
#               or loess <= base*threshold (max) in the post-peak period.

flood_dates <- function(df, variable, direction = c('min', 'max')) {

  direction <- match.arg(direction)

  count.df <- if (direction == 'min') count.min(df, {{variable}}) else count.max(df, {{variable}})

  df_aug <- count.df %>%
    filter(!is.na(flood)) %>%
    group_by(ID, flood) %>%
    mutate(
      stage.flood = if_else(count >= 0, 'post', 'pre'),
      .wb         = {{variable}} / base,
      .threshold  = if (direction == 'min')
                      if_else(any(.wb < 0.8, na.rm = TRUE), 0.8, 1.0)
                    else
                      if_else(any(.wb > 1.2, na.rm = TRUE), 1.2, 1.0)
    ) %>%
    ungroup()

  if (direction == 'min') {

    flood.start <- df_aug %>%
      filter(stage.flood == 'pre') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb < .threshold)) %>%
      filter(crit) %>%
      summarise(flood.start = min(as.Date(Date)), .groups = 'drop')

    flood.end <- df_aug %>%
      filter(stage.flood == 'post') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb >= .threshold)) %>%
      summarise(
        flood.end = if (any(crit, na.rm = TRUE))
                      min(as.Date(Date[crit]), na.rm = TRUE)
                    else
                      max(as.Date(Date)) + 1L,
        .groups = 'drop'
      )

  } else {

    flood.start <- df_aug %>%
      filter(stage.flood == 'pre') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb > .threshold)) %>%
      filter(crit) %>%
      summarise(flood.start = min(as.Date(Date)), .groups = 'drop')

    flood.end <- df_aug %>%
      filter(stage.flood == 'post') %>%
      group_by(ID, flood) %>%
      mutate(crit = first_of_three(.wb <= .threshold)) %>%
      summarise(
        flood.end = if (any(crit, na.rm = TRUE))
                      min(as.Date(Date[crit]), na.rm = TRUE)
                    else
                      max(as.Date(Date)) + 1L,
        .groups = 'drop'
      )
  }

  full_join(flood.start, flood.end, by = c('ID', 'flood'))
}

prep.max.both.daily <- function(df.smooth, variable, variable_loess, gap_days = 14) {

  df.recover <- df.smooth %>%
    group_by(ID, flood) %>%
    mutate(
      date            = as.Date(Date),
      within_baseline = {{variable}} / base,
      threshold       = if_else(any(within_baseline > 1.2, na.rm = TRUE), 1.2, 1.0),
      recovered       = if_else(within_baseline <= threshold, "recovered", NA_character_)
    )

  clean<-count.max(df.recover, {{variable_loess}}) %>%
    
    arrange(ID, flood, Date) %>%
    group_by(ID, flood) %>%
    filter(
      {{variable}} > base)%>%
    mutate(
      stage.flood     = if_else(count >= 0, 'post', 'pre'),
      days_since_last = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
      gap_in_post     = stage.flood == 'post' & !is.na(days_since_last) & days_since_last > gap_days,
      after_first_gap = cumsum(coalesce(gap_in_post, FALSE)) > 0,
      gap_in_pre      = stage.flood == 'pre'  & !is.na(days_since_last) & days_since_last > gap_days,
      before_last_gap = rev(cumsum(rev(coalesce(gap_in_pre, FALSE)))) > 0
    ) %>%
    filter(
      !(after_first_gap & stage.flood == 'post'),
      !(before_last_gap & stage.flood == 'pre')
    ) %>%
    ungroup()
}
