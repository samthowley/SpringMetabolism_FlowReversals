# Model checks: are the residuals close enough to normal?
# Part 1 refits the main model of each test on the raw scale (the 'before').
# Part 2 (bottom) refits them with transformed responses: log for Tests 1, 3, 4 and 5, asinh for Test 2.
# Tests 2-5 use the flood class as a number (HI 1, BO 2, FR 3), as in analysis.R. Test 5 is the flood-level model (one row per flood, within-site exposure).
# analysis.R has since adopted the log for recovery rate (Test 4) and for peak % change in GPP and CO2 (Test 3); the rest of part 2 shows why the others were not changed.
# Parts 1 and 2 only refit and save. The 'view saved checks' section at the bottom loads the saved residuals and opens
# one flextable per part in the Viewer and one figure per test in the Plots pane (Q-Q plot + histogram).
# The residuals of each part are saved as an .rds file in the saved results folder (R format, not a csv), so the refits only happen the first time
# or when the data, the model code or drop.unrecovered change. To force a refit, add 'checks_part1' or 'checks_part2' to `refresh` (set in analysis.R).
# To look at saved checks in a fresh session without refitting: run the libraries, parameters and helpers at the top, then the last section.
# Needs analysis.R to have been run once.
library(tidyverse)
library(flextable)
library(cowplot)

#parameters########
skew.max       <- 1     # |skewness| above this is flagged
kurt.max       <- 2     # excess kurtosis above this is flagged (heavy tails)
shapiro.max.n  <- 200   # Shapiro p is only flagged at or below this many rows (above it, it rejects everything)

#helpers########
if (!exists('cache.dir')) cache.dir <- '04_Outputs/saved results'

# run the refits of one part, or load their saved residuals if nothing they depend on has changed
cached_resid <- function(name, expr) {
  expr <- substitute(expr)
  sig <- rlang::hash(list(
    data = list(daily, daily.flood, flood.level, flood.exposure), drop.unrecovered = drop.unrecovered,
    code = c(deparse(expr, control = NULL), unlist(lapply(list(lme_daily, lmer_flood, get_resid, finite_rows, collector), deparse, control = NULL)))))
  file <- file.path(cache.dir, paste0(name, '.rds'))
  if (isTRUE(get0('use.cache', ifnotfound = TRUE)) && !name %in% get0('refresh', ifnotfound = character(0)) && file.exists(file)) {
    saved <- tryCatch(readRDS(file), error = function(e) NULL)
    if (!is.null(saved) && identical(saved$sig, sig)) {
      message(name, ': loaded saved residuals from ', format(saved$time, '%d %b %Y %H:%M'))
      return(saved$res)
    }
    message(name, ': data or code changed since the saved run, refitting')
  }
  res <- eval(expr, parent.frame())
  dir.create(cache.dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(sig = sig, time = Sys.time(), res = res), file)
  message(name, ': saved to ', file)
  res
}

# collects fitted models; key = the untransformed response name, used to line up part 1 and part 2
collector <- function() {
  items <- list()
  list(
    add = function(test, v, resp, d, fit.fun, key = resp) {
      m <- tryCatch(fit.fun(d), error = function(e) { message('  fit failed (', test, ' ', v, ', ', resp, '): ', conditionMessage(e)); NULL })
      # a daily (lme) model that failed is retried with other AR(1) starting values (lme_retry comes from analysis.R)
      f <- environment(fit.fun)$f
      if (is.null(m) && !is.null(f) && length(lme4::findbars(f)) == 0 && exists('lme_retry')) {
        message('  retrying with other AR(1) starting values')
        m <- tryCatch(lme_retry(f)(d), error = function(e) { message('  retry failed too: ', conditionMessage(e)); NULL })
      }
      if (!is.null(m)) items[[length(items) + 1]] <<- list(test = test, variable = v, response = resp, key = key, m = m)
    },
    get = function() items)
}

# keep rows where the transformed response is finite and say how many were dropped
finite_rows <- function(d, label) {
  bad <- sum(!is.finite(d$y))
  if (bad > 0) message('  ', label, ': dropped ', bad, ' row(s) where the transformed response is not finite')
  d %>% filter(is.finite(y))
}

# daily models: normalized residuals (AR(1) removed); flood-level models: residuals divided by their SD
get_resid <- function(items) {
  map_dfr(items, function(f) {
    m <- f$m
    r <- if (inherits(m, 'lme')) as.numeric(resid(m, type = 'normalized')) else as.numeric(resid(m)) / sigma(m)
    tibble(resid = r, test = f$test, variable = f$variable, response = f$response, key = f$key)
  }) %>% mutate(variable = factor(variable, levels = var.order), response = fct_inorder(response))
}

skew <- function(x) { x <- x - mean(x); mean(x^3) / mean(x^2)^1.5 }
kurt <- function(x) { x <- x - mean(x); mean(x^4) / mean(x^2)^2 - 3 }

get_checks <- function(resid.df) {
  resid.df %>%
    filter(is.finite(resid)) %>%
    group_by(test, variable, response, key) %>%
    summarise(n = n(), skew = skew(resid), kurt = kurt(resid),
              shapiro.p = if (n() >= 3) shapiro.test(if (n() > 5000) sample(resid, 5000) else resid)$p.value else NA_real_,
              .groups = 'drop') %>%
    rowwise() %>%
    mutate(Flags = paste(c(
      if (abs(skew) > skew.max) 'Skewed',
      if (kurt > kurt.max) 'Heavy tails',
      if (n <= shapiro.max.n && !is.na(shapiro.p) && shapiro.p < 0.01) 'Not normal (Shapiro)'), collapse = '; ')) %>%
    ungroup()
}

pt.col   <- '#52514e'   # points and bars: neutral
line.col <- '#2a78d6'   # reference line / curve: the one accent
base.theme <- theme_bw(base_size = 9) + theme(panel.grid.minor = element_blank(), strip.background = element_blank(), strip.text = element_text(face = 'bold'))

# one figure per test: Q-Q plot on the left, histogram with the normal curve on the right
plot_checks <- function(resid.df, tag = '') {
  for (tt in unique(resid.df$test)) {
    d <- resid.df %>% filter(test == tt, is.finite(resid))
    fac  <- facet_wrap(vars(variable, response), scales = 'free', ncol = n_distinct(d$response), labeller = labeller(.multi_line = FALSE))
    many <- nrow(d) > 1000
    p.qq <- ggplot(d, aes(sample = resid)) +
      geom_qq_line(colour = line.col, linewidth = 0.6) +
      geom_qq(colour = pt.col, alpha = if (many) 0.2 else 0.6, size = 0.9) +
      fac + labs(title = paste0('Normal Q-Q', tag), x = 'Normal quantiles', y = 'Standardized residual') + base.theme
    p.hist <- ggplot(d, aes(resid)) +
      geom_histogram(aes(y = after_stat(density)), bins = if (many) 40 else 12, fill = 'grey70', colour = 'white', linewidth = 0.2) +
      stat_function(fun = dnorm, colour = line.col, linewidth = 0.7) +
      fac + labs(title = paste0('Histogram, blue = normal curve', tag), x = 'Standardized residual', y = 'Density') + base.theme
    print(plot_grid(p.qq, p.hist, nrow = 1))
  }
}


#data from analysis.R (only needed for the refits)########
if (!exists('flood.exposure') || !exists('r2_fit') || !exists('lme_retry') || !exists('alone_tests') || !'class.num' %in% names(flood.level)) {   # older sessions (no numeric class at the flood level, no flood.exposure) need analysis.R sourced again
  show.tables <- FALSE     # keeps analysis.R from opening its five tables
  source('03_Scripts/ANALYSIS/analysis.R')
  rm(show.tables)
}


#part 1: models on the raw scale########
resid.1 <- cached_resid('checks_part1', {
f1 <- collector()

# Test 1
for (v in var.order) {
  message('Part 1, Test 1: ', v)
  f1$add('Test 1', v, 'Value', daily %>% filter(variable == v), lme_daily(value ~ class.num))
}

# Test 2
for (v in var.order) {
  message('Part 1, Test 2: ', v)
  d <- daily.flood %>% filter(variable == v) %>% drop_na(diff, vulnerable.score, class.num, rise.c)
  f1$add('Test 2', v, 'Daily difference', d, lme_daily(diff ~ vulnerable.score + class.num + rise.c))
}

# Test 3
for (v in var.order) {
  message('Part 1, Test 3: ', v)
  d  <- flood.level %>% filter(variable == v)
  dd <- d %>% filter(duration > 0, if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)
  for (s in list(list('Peak % change', 'abs.percent.change', d), list('Log duration', 'log(duration)', dd))) {
    dat <- s[[3]] %>% drop_na(vulnerable.score, class.num, rise.c) %>% filter(is.finite(eval(parse(text = s[[2]]))))
    f1$add('Test 3', v, s[[1]], dat, lmer_flood(as.formula(paste(s[[2]], '~ vulnerable.score + class.num + rise.c + (1 | ID)'))))
  }
}

# Test 4
for (v in c('DO', 'CO2')) {
  message('Part 1, Test 4: ', v)
  d <- flood.level %>% filter(variable == v)
  for (s in list(list('Recovery rate', 'recovery.rate', '+ abs.percent.change', d),
                 list('Lag behind depth (d)', 'end.lag', '', d %>% filter(if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)),
                 list('Logit R2', 'r2.logit', '', d))) {
    dat <- s[[4]] %>% drop_na(all_of(c(s[[2]], 'vulnerable.score', 'class.num', 'rise.c')))
    if (s[[3]] != '') dat <- dat %>% drop_na(abs.percent.change)
    f1$add('Test 4', v, s[[1]], dat, lmer_flood(as.formula(paste(s[[2]], '~ vulnerable.score + class.num', s[[3]], '+ rise.c + (1 | ID)'))))
  }
}

# Test 5
for (v in var.order) {
  message('Part 1, Test 5: ', v)
  d <- flood.exposure %>% filter(variable == v) %>% drop_na(pct.abs, diff.abs, class.num, exposure.w) %>% filter(is.finite(pct.abs))
  f1$add('Test 5', v, 'Mean abs % change', d, lmer_flood(pct.abs ~ exposure.w + class.num + (1 | ID)))
  f1$add('Test 5', v, 'Mean abs difference', d, lmer_flood(diff.abs ~ exposure.w + class.num + (1 | ID)))
}

get_resid(f1$get())
})


#part 2: transformed responses########
# log: Tests 1, 3, 4, 5.  asinh: Test 2 (signed, so it cannot be logged).
# Not changed, so not refit: log duration and logit R2 are already on a transformed scale, and lag behind depth is signed (many floods end before depth does)
# asinh(x) behaves like x near 0 and like log(2x) for large |x|, so it compresses CO2 (differences in the thousands) far more than DO (about +-1)
resid.2 <- cached_resid('checks_part2', {
f2 <- collector()

# Test 1: log(value)
for (v in var.order) {
  message('Part 2, Test 1: ', v)
  d <- finite_rows(daily %>% filter(variable == v) %>% mutate(y = log(value)), paste('Test 1', v))
  f2$add('Test 1', v, 'log(Value)', d, lme_daily(y ~ class.num), key = 'Value')
}

# Test 2: asinh(flood minus baseline)
for (v in var.order) {
  message('Part 2, Test 2: ', v)
  d <- daily.flood %>% filter(variable == v) %>% drop_na(diff, vulnerable.score, class.num, rise.c) %>% mutate(y = asinh(diff))
  f2$add('Test 2', v, 'asinh(Daily difference)', d, lme_daily(y ~ vulnerable.score + class.num + rise.c), key = 'Daily difference')
}

# Test 3: log(peak % change)  (duration is already logged in part 1)
for (v in var.order) {
  message('Part 2, Test 3: ', v)
  d <- flood.level %>% filter(variable == v) %>% drop_na(vulnerable.score, class.num, rise.c) %>% mutate(y = log(abs.percent.change)) %>% finite_rows(paste('Test 3', v))
  f2$add('Test 3', v, 'log(Peak % change)', d, lmer_flood(y ~ vulnerable.score + class.num + rise.c + (1 | ID)), key = 'Peak % change')
}

# Test 4: log(recovery rate)
for (v in c('DO', 'CO2')) {
  message('Part 2, Test 4: ', v)
  d <- flood.level %>% filter(variable == v) %>% drop_na(recovery.rate, abs.percent.change, vulnerable.score, class.num, rise.c) %>%
    mutate(y = log(recovery.rate)) %>% finite_rows(paste('Test 4', v))
  f2$add('Test 4', v, 'log(Recovery rate)', d, lmer_flood(y ~ vulnerable.score + class.num + abs.percent.change + rise.c + (1 | ID)), key = 'Recovery rate')
}

# Test 5: log of both absolute responses
for (v in var.order) {
  message('Part 2, Test 5: ', v)
  d <- flood.exposure %>% filter(variable == v) %>% drop_na(pct.abs, diff.abs, class.num, exposure.w) %>% filter(is.finite(pct.abs))
  f2$add('Test 5', v, 'log(Mean abs % change)', d %>% mutate(y = log(pct.abs)) %>% finite_rows(paste('Test 5', v)), lmer_flood(y ~ exposure.w + class.num + (1 | ID)), key = 'Mean abs % change')
  f2$add('Test 5', v, 'log(Mean abs difference)', d %>% mutate(y = log(diff.abs)) %>% finite_rows(paste('Test 5', v)), lmer_flood(y ~ exposure.w + class.num + (1 | ID)), key = 'Mean abs difference')
}

get_resid(f2$get())
})


#view saved checks########
# loads the saved residuals (no refitting): tables open in the Viewer, plots in the Plots pane
if (!exists('var.order')) var.order <- c('GPP', 'ER', 'DO', 'CO2')
if (!exists('fmt_p')) fmt_p <- function(p) if_else(is.na(p), '', if_else(p < 0.001, '<0.001', sprintf('%.3f', p)))
fmt2 <- function(x) if_else(is.na(x), '', sprintf('%.2f', x))
load_saved <- function(name) {
  f <- file.path(cache.dir, paste0(name, '.rds'))
  if (file.exists(f)) readRDS(f)$res else { message(name, ': no saved residuals yet, run its part above'); NULL }
}
saved.1 <- load_saved('checks_part1')
saved.2 <- load_saved('checks_part2')

# part 1: raw scale
if (!is.null(saved.1)) {
  checks.1 <- get_checks(saved.1)
  checks.1 %>%
    transmute(Test = test, Variable = as.character(variable), Response = as.character(response), n = as.character(n),
              Skew = fmt2(skew), `Excess kurtosis` = fmt2(kurt), `Shapiro p` = fmt_p(shapiro.p), Flags) %>%
    flextable() %>% merge_v(j = c('Test', 'Variable')) %>% valign(valign = 'top', part = 'body') %>% bold(j = 'Flags') %>% autofit() %>%
    print()
  plot_checks(saved.1)
}

# part 2: transformed, with the raw-scale skew and kurtosis alongside
if (!is.null(saved.2)) {
  checks.2 <- get_checks(saved.2)
  before <- if (!is.null(saved.1)) checks.1 %>% transmute(test, variable, key = response, skew.0 = skew, kurt.0 = kurt)
            else tibble(test = character(), variable = factor(character(), levels = var.order), key = character(), skew.0 = double(), kurt.0 = double())
  checks.2 %>%
    left_join(before, by = c('test', 'variable', 'key')) %>%
    transmute(Test = test, Variable = as.character(variable), Response = as.character(response), n = as.character(n),
              `Skew before` = fmt2(skew.0), `Skew after` = fmt2(skew),
              `Kurtosis before` = fmt2(kurt.0), `Kurtosis after` = fmt2(kurt), `Flags after` = Flags) %>%
    flextable() %>% merge_v(j = c('Test', 'Variable')) %>% valign(valign = 'top', part = 'body') %>% bold(j = 'Flags after') %>% autofit() %>%
    print()
  plot_checks(saved.2, ' (transformed)')
}
