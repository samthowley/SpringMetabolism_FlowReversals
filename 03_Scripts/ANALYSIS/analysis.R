library(tidyverse)
library(flextable)
source('03_Scripts/ANALYSIS/analysis prep/09-analysis prep.R')

#flood_metrics <- read_csv("04_Outputs/flood impacts/flood_metrics.csv")

#parameters########
min.day.frac <- 0.6    # a day needs this fraction of the site's usual readings to get a daily mean DO or CO2
min.days     <- 10     # a site x class needs at least this many days to get a slope

alpha            <- 0.05   # only terms with p < alpha go on to the permutation test
n.perm           <- 999    # random shuffles per tested term (vulnerability uses every site ordering, so no random draws)
n.boot           <- 999    # bootstrap resamples for the Test 5 intervals (part of the saved-results check, so changing it reruns Test 5)
site.icc.min     <- 0.05   # site share of the variance: below this shuffle across all floods, otherwise within site
drop.unrecovered <- TRUE   # lag and duration models only: drop floods that never returned to threshold
exposure.per     <- 100    # days per exposure unit in Tests 3b and 5: the slope is per this many days of earlier flood exposure. Only rescales the slope and its CI; p-values do not change (try 150 or 10 if a different unit reads better)

# saved results: each test's raw results are saved as an .rds file (R format, not a csv) and loaded on later runs
# a test reruns by itself if the data, the parameters above or the model code change
use.cache <- TRUE                        # FALSE = ignore saved results and rerun everything
refresh   <- character(0)                # force a rerun of some tests, e.g. c('t2', 't3')
cache.dir <- '04_Outputs/saved results'  # created on first run


#daily data########
daily.chem <- chem_hourly %>%
  mutate(
    Date = as.Date(Date),
    CO2  = if_else(CO2 > 600, CO2, NA_real_)
  ) %>%
  filter(DO<7)%>%
  group_by(ID, Date) %>%
  summarise(
    n.DO  = sum(!is.na(DO)),
    n.CO2 = sum(!is.na(CO2)),
    DO    = mean(DO,  na.rm = TRUE),
    CO2   = mean(CO2, na.rm = TRUE),
    depth = mean(depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ID) %>%
  mutate(
    DO  = if_else(n.DO  >= min.day.frac * median(n.DO[n.DO > 0]),   DO,  NA_real_),
    CO2 = if_else(n.CO2 >= min.day.frac * median(n.CO2[n.CO2 > 0]), CO2, NA_real_)
  ) %>%
  ungroup() %>%
  select(-n.DO, -n.CO2)

#class of each day########
stage.long <- daily.chem %>%
  left_join(
    metab %>%
      distinct(ID, Date, .keep_all = TRUE) %>%
      transmute(ID, Date, GPP, ER = abs(ER)),
    by = c("Date", "ID"),
    relationship = "one-to-one"
  ) %>%
  left_join(floods %>% select(ID, flood, start, end),
            join_by(ID, between(Date, start, end)))%>%
  mutate(
    flood=as.factor(flood)
  )%>%
  pivot_longer(cols = c(GPP, ER, DO, CO2), names_to = "variable", values_to = "value")%>%
  left_join(flood.response, by = c("ID", "flood", "variable"))%>%
  mutate(
    flood=if_else(is.na(flood), 'base', flood),
    class=if_else(flood=='base', 'base', as.character(class))   # was is.na(flood), which is never true after the line above
  )


#model data########
var.order <- c('GPP', 'ER', 'DO', 'CO2')
vuln.lookup <- setNames(vulnerability$vulnerable.score, vulnerability$ID)

# one row per flood: class, depth rise (% above baseline depth), depth rise centered on the site mean
flood.info <- flood_class %>%
  left_join(
    flood.response %>%
      filter(variable == 'depth') %>%
      transmute(ID = as.character(ID), flood = as.numeric(as.character(flood)),
                h.percent.change, flood.end.depth = as.Date(flood.end)),
    by = c('ID', 'flood')) %>%
  left_join(floods %>% select(ID, flood, start), by = c('ID', 'flood')) %>%
  group_by(ID) %>%
  mutate(rise.c = h.percent.change - mean(h.percent.change, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(class = factor(class, levels = c('HI', 'BO', 'FR')),
         vulnerable.score = vuln.lookup[ID],
         flood.end.depth = if_else(flood.end.depth < start, as.Date(NA), flood.end.depth)) %>%   # drops a depth end date that falls before its own flood (LF 4)
  select(-start)

# days in flood at the site before each flood (all classes, depth flood periods)
exposure <- floods %>%
  arrange(ID, flood) %>%
  group_by(ID) %>%
  mutate(exposure100 = lag(cumsum(as.numeric(end - start)), default = 0) / 100) %>%   # per 100 days
  ungroup() %>%
  select(ID, flood, exposure100)

# daily table: one row per site x day x variable; flood = 'base' outside flood periods
daily <- stage.long %>%
  transmute(
    ID, Date, variable, value, base,
    t         = as.numeric(Date - as.Date('2022-01-01')),
    flood     = as.character(flood),
    flood.num = suppressWarnings(as.numeric(flood))
  ) %>%
  left_join(flood.info %>% select(ID, flood, class, rise.c) %>% rename(flood.num = flood),
            by = c('ID', 'flood.num')) %>%
  left_join(exposure %>% rename(flood.num = flood), by = c('ID', 'flood.num')) %>%
  mutate(
    vulnerable.score = vuln.lookup[ID],
    period   = factor(if_else(flood == 'base', 'base', as.character(class)), levels = c('base', 'HI', 'BO', 'FR')),
    class.num = as.numeric(period) - 1,                 # base 0, HI 1, BO 2, FR 3
    variable = factor(variable, levels = var.order)
  ) %>%
  filter(!is.na(value), !is.na(period))

daily.flood <- daily %>%
  filter(flood != 'base', !is.na(base)) %>%
  mutate(
    diff        = value - base,                        # flood minus baseline: negative = lower during the flood
    pct.abs     = abs((value - base) / base * 100),
    diff.abs    = abs(value - base)
  )

# flood table: one row per site x flood x variable
flood.level <- flood.response %>%
  filter(variable != 'depth') %>%
  mutate(
    ID = as.character(ID), flood = as.numeric(as.character(flood)), variable = as.character(variable),
    exp.dir = if_else(response.dir == 'decrease', -1, 1),
    recess.ok = sign(recess.slope) == -exp.dir,                     # a recovery slope with the wrong sign is dropped with its R2
    recess.slope = if_else(recess.ok, recess.slope, NA_real_),
    r2.recess = if_else(recess.ok, r2.recess, NA_real_)
  ) %>%
  left_join(flood.info %>% select(ID, flood, rise.c, flood.end.depth), by = c('ID', 'flood')) %>%
  transmute(
    ID, flood, variable, class = factor(as.character(class), levels = c('HI', 'BO', 'FR')),
    class.num = as.numeric(class),                              # HI 1, BO 2, FR 3 (same coding as Test 1, without the baseline 0)
    vulnerable.score = vuln.lookup[ID], rise.c,
    abs.percent.change = abs(reponse.percent.change),          # peak
    duration, flood.recovered,
    recovery.rate = abs(recess.slope),                          # per sample step (hourly for DO and CO2)
    r2.logit = qlogis(pmin(pmax(r2.recess, 0.001), 0.999)),
    end.lag = as.numeric(as.Date(flood.end) - flood.end.depth),
    n.recess
  ) %>%
  mutate(variable = factor(variable, levels = var.order))


#helpers########
tidy_fit <- function(m) {
  if (inherits(m, 'lme')) {
    tt <- as.data.frame(summary(m)$tTable)
    out <- tibble(term = rownames(tt), estimate = tt$Value, se = tt$Std.Error,
                  df = tt$DF, t = tt$`t-value`, p = tt$`p-value`)
  } else {
    co <- as.data.frame(summary(m)$coefficients)
    out <- tibble(term = rownames(co), estimate = co$Estimate, se = co$`Std. Error`,
                  df = co$df, t = co$`t value`, p = co$`Pr(>|t|)`)
  }
  out %>% mutate(ci.low = estimate - qt(0.975, df) * se, ci.high = estimate + qt(0.975, df) * se)
}

# site share of the total variance (random intercepts + residual)
site_icc <- function(m) {
  if (inherits(m, 'lme')) {
    v <- suppressWarnings(as.numeric(nlme::VarCorr(m)[, 'Variance']))
    v <- v[!is.na(v)]
    v[1] / sum(v)
  } else {
    vc <- as.data.frame(lme4::VarCorr(m))
    vc$vcov[vc$grp == 'ID'] / sum(vc$vcov)
  }
}

# R2 of a mixed model (Nakagawa): fixed = variance of the fixed-effect predictions / total variance,
# total = (fixed + all random intercepts) / total variance. Total variance = fixed + random + residual
r2_fit <- function(m) {
  tryCatch({
    if (inherits(m, 'lme')) {
      vf <- var(as.numeric(predict(m, level = 0)))
      v  <- suppressWarnings(as.numeric(nlme::VarCorr(m)[, 'Variance'])); v <- v[!is.na(v)]
      vr <- sum(v[-length(v)]); ve <- v[length(v)]
    } else {
      vf <- var(as.numeric(predict(m, re.form = NA)))
      vc <- as.data.frame(lme4::VarCorr(m))
      vr <- sum(vc$vcov[vc$grp != 'Residual']); ve <- vc$vcov[vc$grp == 'Residual']
    }
    c(marg = vf / (vf + vr + ve), cond = (vf + vr) / (vf + vr + ve))
  }, error = function(e) c(marg = NA_real_, cond = NA_real_))
}

# daily models: random intercepts for site and flood within site, continuous-time AR(1) within flood
# rho is only supplied for the permutation refits (fixed at the observed AR(1) value: same t-values, ~6x faster)
lme_daily <- function(f) {
  force(f)
  function(d, rho = NULL) {
    cor.str <- if (is.null(rho)) nlme::corCAR1(form = ~ t | ID / flood)
               else nlme::corCAR1(value = rho, form = ~ t | ID / flood, fixed = TRUE)
    nlme::lme(f, random = ~ 1 | ID / flood, correlation = cor.str,
              data = d, na.action = na.omit,
              control = nlme::lmeControl(maxIter = 200, msMaxIter = 200))
  }
}

# retry for a daily model that failed to fit: same model, other starting values for the AR(1) term (0.2 is the default start)
# the default optimizer is kept on purpose: opt = 'optim' converged to a different, degenerate fit (rho = 1) on the same data
lme_retry <- function(f) {
  force(f)
  function(d, rho = NULL) {
    if (!is.null(rho)) return(lme_daily(f)(d, rho))
    for (start in c(0.5, 0.8, 0.3, 0.9)) {
      m <- tryCatch(
        nlme::lme(f, random = ~ 1 | ID / flood, correlation = nlme::corCAR1(value = start, form = ~ t | ID / flood),
                  data = d, na.action = na.omit, control = nlme::lmeControl(maxIter = 200, msMaxIter = 200)),
        error = function(e) NULL)
      if (!is.null(m)) { message('  retry converged with AR(1) start ', start); return(m) }
    }
    stop('no AR(1) starting value converged')
  }
}

# flood-level models: random intercept for site
lmer_flood <- function(f) {
  force(f)
  function(d, rho = NULL) suppressMessages(lmerTest::lmer(f, data = d))
}

all_perms <- function(n) {   # every ordering of 1:n, one per row
  if (n == 1) return(matrix(1L, 1, 1))
  sub <- all_perms(n - 1)
  do.call(rbind, lapply(seq_len(n), function(i) cbind(i, matrix(setdiff(seq_len(n), i)[sub], nrow(sub)))))
}

# shuffle one flood-level column among floods (within site or across all floods)
shuffle_col <- function(d, col, scheme) {
  fl <- d %>% distinct(ID, flood, .data[[col]])
  shuf <- function(x) x[sample.int(length(x))]
  fl <- if (scheme == 'within site') fl %>% group_by(ID) %>% mutate(across(all_of(col), shuf)) %>% ungroup()
        else fl %>% mutate(across(all_of(col), shuf))
  d %>% select(-all_of(col)) %>% left_join(fl, by = c('ID', 'flood'))
}

# permutation p for one term: shuffle whole floods (or whole sites for vulnerability), refit the same model
perm_p <- function(fit.fun, d, term, t.obs, scheme, rho = NULL) {
  if (term == 'vulnerable.score') {
    sites <- sort(unique(d$ID))
    ords  <- all_perms(length(sites))
    sc    <- vuln.lookup[sites]
    t.star <- map_dbl(seq_len(nrow(ords)), function(i) {
      d2 <- d; d2$vulnerable.score <- sc[ords[i, ]][match(d2$ID, sites)]
      m <- tryCatch(fit.fun(d2, rho), error = function(e) NULL)
      if (is.null(m)) NA_real_ else tidy_fit(m)$t[tidy_fit(m)$term == term]
    })
    ok <- !is.na(t.star)
    return(mean(abs(t.star[ok]) >= abs(t.obs) - 1e-9))               # exact: the observed ordering is one of them
  }
  cols <- intersect(c('class.num', 'rise.c', 'abs.percent.change', 'exposure.w', 'exposure100'), names(d))
  col  <- cols[map_lgl(cols, ~ startsWith(term, .x))][1]
  if (is.na(col)) stop('no column to shuffle for term ', term)
  t.star <- map_dbl(seq_len(n.perm), function(i) {
    m <- tryCatch(fit.fun(shuffle_col(d, col, scheme), rho), error = function(e) NULL)
    if (is.null(m)) NA_real_ else tidy_fit(m)$t[tidy_fit(m)$term == term]
  })
  ok <- !is.na(t.star)
  (1 + sum(abs(t.star[ok]) >= abs(t.obs) - 1e-9)) / (1 + sum(ok))
}

# fit, tidy, and permute only the terms with p < alpha
analyse <- function(fit.fun, d, perm = TRUE) {
  m <- tryCatch(fit.fun(d), error = function(e) { message('  fit failed: ', conditionMessage(e)); NULL })
  if (is.null(m)) return(NULL)
  tab <- tidy_fit(m) %>% filter(term != '(Intercept)')
  icc <- site_icc(m)
  scheme <- if (!is.na(icc) && icc < site.icc.min) 'all floods' else 'within site'
  tab$p.perm <- NA_real_
  tab$scheme <- NA_character_
  rho <- if (inherits(m, 'lme')) unname(coef(m$modelStruct$corStruct, unconstrained = FALSE)) else NULL
  if (perm) for (i in which(tab$p < alpha)) {
    message('  permuting ', tab$term[i], ' (', if (tab$term[i] == 'vulnerable.score') 'all site orderings' else scheme, ')')
    tab$p.perm[i] <- perm_p(fit.fun, d, tab$term[i], tab$t[i], scheme, rho)
    tab$scheme[i] <- if (tab$term[i] == 'vulnerable.score') 'all site orderings' else scheme
  }
  r2 <- r2_fit(m)
  tab %>% mutate(site.var = icc, r2.marg = r2[['marg']], r2.cond = r2[['cond']], n = if (inherits(m, 'lme')) m$dims$N else nobs(m),
                 floods = n_distinct(paste(d$ID, d$flood)[d$flood != 'base']), sites = n_distinct(d$ID))
}

# the same daily model, with other AR(1) starting values if the default start fails (used by the one-predictor-at-a-time models)
lme_daily_retry <- function(f) {
  force(f)
  function(d, rho = NULL) {
    if (!is.null(rho)) return(lme_daily(f)(d, rho))
    tryCatch(lme_daily(f)(d), error = function(e) lme_retry(f)(d))
  }
}

# one-predictor-at-a-time tests: for every fixed term of the full model f, refit with only that term (same response, same random
# effects, same rows) and run the same fit and permutation test. One row per term; make_ft puts these next to the combined model
# make = lme_daily_retry (daily models) or lmer_flood (flood-level models); perm = FALSE skips the permutation test
alone_tests <- function(f, make, d, perm = TRUE) {
  terms.f <- attr(terms(lme4::nobars(f)), 'term.labels')
  bars <- lme4::findbars(f)
  re <- if (length(bars)) paste0('(', vapply(bars, function(b) paste(deparse(b), collapse = ''), ''), ')') else NULL
  map_dfr(terms.f, function(tm) {
    message('  alone: ', tm)
    f1  <- as.formula(paste(paste(deparse(f[[2]]), collapse = ''), '~', paste(c(tm, re), collapse = ' + ')))
    out <- analyse(make(f1), d, perm = perm)
    if (is.null(out)) return(NULL)
    out %>% filter(term == tm) %>%
      transmute(term, alone.estimate = estimate, alone.ci.low = ci.low, alone.ci.high = ci.high,
                alone.p = p, alone.perm = p.perm, alone.scheme = scheme)
  })
}

term.labels <- c(class.num = 'Per class step (HI 1, BO 2, FR 3)',
                 vulnerable.score = 'Vulnerability (per step)',
                 rise.c = 'Depth rise (centered, %)', abs.percent.change = 'Peak % change',
                 exposure100 = 'Exposure (per 100 d)',
                 exposure.w = paste0('Exposure within site (per ', exposure.per, ' d)'))

term.labels.t1 <- replace(term.labels, 'class.num', 'Per class step (base 0, HI 1, BO 2, FR 3)')   # Test 1 also has the baseline

# label the model; one that failed to fit (a 'fit failed' message is printed) is skipped instead of stopping the run
add_model <- function(x, label) if (is.null(x)) NULL else mutate(x, model = label)

fmt_p   <- function(p) if_else(is.na(p), '', if_else(p < 0.001, '<0.001', sprintf('%.3f', p)))
fmt_num <- function(x) formatC(x, digits = 3, format = 'g')

# results table -> flextable; the only caption is the title (test number and its question)
# R2 fixed = share of the variance explained by the fixed effects, R2 total = fixed + random intercepts; one value per model (shown on its first row)
# Slope = change in the response per unit of the term (class step, point of vulnerability, % of depth rise, ...)
# 'Single predictor' rows (add_singles, below) are the same response with only that predictor in the model; they follow the combined model
make_ft <- function(res, cols = c('Variable', 'Model', 'Term', 'Slope', '95% CI', 'p', 'Perm. p', 'Scheme', 'Site var %', 'R2 fixed', 'R2 total', 'n', 'Floods'),
                    labels = term.labels, title = NULL) {
  if (!'Model' %in% names(res)) res$Model <- if ('model' %in% names(res)) res$model else ''
  if ('boot.low' %in% names(res)) res$`Boot 95% CI` <- if_else(is.na(res$boot.low), '', paste(fmt_num(res$boot.low), 'to', fmt_num(res$boot.high)))
  res %>%
    mutate(
      Variable = as.character(variable), Term = coalesce(labels[term], term),
      Slope = fmt_num(estimate), `95% CI` = paste(fmt_num(ci.low), 'to', fmt_num(ci.high)),
      p.raw = p, p = fmt_p(p), `Perm. p` = fmt_p(p.perm), Scheme = coalesce(scheme, ''),
      `Site var %` = if_else(is.na(site.var), '', sprintf('%.0f', 100 * site.var)),
      n = if_else(is.na(n), '', as.character(n)), Floods = if_else(is.na(floods), '', as.character(floods)),
      `R2 fixed` = if_else(is.na(r2.marg), '', sprintf('%.2f', r2.marg)), `R2 total` = if_else(is.na(r2.cond), '', sprintf('%.2f', r2.cond))
    ) %>%
    group_by(Variable, Model) %>%
    mutate(across(c(`R2 fixed`, `R2 total`), ~ if_else(row_number() == 1, .x, ''))) %>%
    ungroup() %>%
    select(any_of(c(cols, 'p.raw'))) %>%
    flextable(col_keys = intersect(cols, names(.))) %>%
    merge_v(j = intersect(c('Variable', 'Model', 'Response'), cols)) %>%
    valign(valign = 'top', part = 'body') %>%
    bold(i = ~ p.raw < 0.05, j = intersect(c('Term', 'Slope', '95% CI', 'p', 'Perm. p'), cols)) -> ft
  if (any(grepl('Single predictor', res$Model))) ft <- add_footer_lines(ft, 'Single predictor = the same response with only that predictor in the model (n, R2 and site share are not kept for these rows).')
  ft <- autofit(ft)
  if (!is.null(title)) ft <- set_caption(ft, caption = title)   # table title: test number and its question
  ft
}

# run a test, or load its saved result if nothing it depends on has changed
# the table formatting (make_ft) is outside this, so changing how tables look never triggers a rerun
cached <- function(name, data, expr, deps = list()) {   # deps = extra functions this test uses, added to the signature
  expr <- substitute(expr)
  helpers <- list(tidy_fit, site_icc, r2_fit, lme_daily, lme_retry, lmer_flood, all_perms, shuffle_col, perm_p, analyse)
  sig <- rlang::hash(c(list(
    params = list(alpha, n.perm, n.boot, site.icc.min, drop.unrecovered, min.day.frac, min.days),
    data   = data,
    code   = c(deparse(expr, control = NULL), unlist(lapply(helpers, deparse, control = NULL)))
  ), if (length(deps)) list(deps = unlist(lapply(deps, deparse, control = NULL)))))
  file <- file.path(cache.dir, paste0(name, '.rds'))
  if (use.cache && !name %in% refresh && file.exists(file)) {
    saved <- tryCatch(readRDS(file), error = function(e) NULL)
    if (!is.null(saved) && identical(saved$sig, sig)) {
      message(name, ': loaded saved results from ', format(saved$time, '%d %b %Y %H:%M'))
      return(saved$res)
    }
    message(name, ': data, parameters or code changed since the saved run, rerunning')
  }
  set.seed(42)   # same shuffles every time a test is run, whatever order the tests ran in
  res <- eval(expr, parent.frame())
  dir.create(cache.dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(sig = sig, time = Sys.time(), res = res), file)
  message(name, ': saved to ', file)
  res
}

#Test 1: baseline vs flood class########
# value ~ class.num (base 0, HI 1, BO 2, FR 3): the slope is the average change per class step
# random site and flood within site; AR(1) within flood
t1 <- cached('t1', daily, map_dfr(var.order, function(v) {
  message('Test 1: ', v)
  d <- daily %>% filter(variable == v)
  out <- analyse(lme_daily(value ~ class.num), d, perm = FALSE)
  if (is.null(out)) return(NULL)

  # permutation: dif = a flood's mean minus its site's baseline mean. The slope is dif regressed on class.num
  # through the origin (baseline is 0 by construction). Random sign flips of dif give the null; class labels
  # are not shuffled because baseline is not tied to a flood
  fl <- d %>% group_by(ID) %>% mutate(base.mean = mean(value[period == 'base'])) %>% ungroup() %>%
    filter(period != 'base') %>% group_by(ID, flood, class.num) %>%
    summarise(dif = mean(value) - first(base.mean), .groups = 'drop')
  for (i in which(out$p < alpha)) {
    obs   <- sum(fl$class.num * fl$dif) / sum(fl$class.num^2)
    flips <- matrix(sample(c(-1, 1), n.perm * nrow(fl), replace = TRUE), n.perm)
    star  <- as.vector(flips %*% (fl$class.num * fl$dif)) / sum(fl$class.num^2)
    out$p.perm[i] <- (1 + sum(abs(star) >= abs(obs) - 1e-9)) / (1 + n.perm)
    out$scheme[i] <- 'sign flip'
  }
  out %>% mutate(variable = v)
}))


#Test 2: daily severity (flood minus baseline)########
# same floods in both models so they can be compared; second model adds depth rise
t2 <- cached('t2', daily.flood, map_dfr(var.order, function(v) {
  d <- daily.flood %>% filter(variable == v) %>% drop_na(diff, vulnerable.score, class.num, rise.c)
  bind_rows(
    {message('Test 2: ', v, ' without depth'); analyse(lme_daily(diff ~ vulnerable.score + class.num), d) %>% add_model('No depth')},
    {message('Test 2: ', v, ' with depth');    analyse(lme_daily(diff ~ vulnerable.score + class.num + rise.c), d) %>% add_model('With depth')}
  ) %>% mutate(variable = v)
}))


# a Test 2 model that failed to fit above (a 'fit failed' message was printed) is retried with other AR(1) starting values
# the retry only runs when a variable x model is missing from t2; its results are saved separately (t2_retry.rds)
t2.missing <- expand_grid(variable = var.order, model = c('No depth', 'With depth')) %>%
  anti_join(t2 %>% distinct(variable, model), by = c('variable', 'model'))
if (nrow(t2.missing) > 0) {
  t2.retry <- cached('t2_retry', list(daily.flood, t2.missing), pmap_dfr(t2.missing, function(variable, model) {
    v <- variable
    message('Test 2 retry: ', v, ' ', model)
    d <- daily.flood %>% filter(variable == v) %>% drop_na(diff, vulnerable.score, class.num, rise.c)
    f <- if (model == 'No depth') diff ~ vulnerable.score + class.num else diff ~ vulnerable.score + class.num + rise.c
    out <- analyse(lme_retry(f), d)
    if (is.null(out)) NULL else out %>% add_model(model) %>% mutate(variable = v)
  }))
  t2 <- bind_rows(t2, t2.retry)
}


# Test 2, one predictor at a time (vulnerability, class, depth rise each alone); joined to the Test 2 table in the viewer
t2_alone <- cached('t2_alone', daily.flood, map_dfr(var.order, function(v) {
  message('Test 2 alone: ', v)
  d <- daily.flood %>% filter(variable == v) %>% drop_na(diff, vulnerable.score, class.num, rise.c)
  alone_tests(diff ~ vulnerable.score + class.num + rise.c, lme_daily_retry, d) %>% mutate(variable = v)
}), deps = list(alone_tests, lme_daily_retry))


#Test 3: flood severity (one row per flood)########
# peak % change and log duration, separate models; duration is the variable's own response duration
# peak % change is logged for GPP and CO2 (their residuals were skewed); ER and DO stay on the raw scale (edit the variable names in log.peak below to change this)
t3 <- cached('t3', flood.level, map_dfr(var.order, function(v) {
  d <- flood.level %>% filter(variable == v)
  dd <- d %>% filter(duration > 0, if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)
  message('Test 3: ', v, ' (duration models drop ', nrow(d %>% filter(duration > 0)) - nrow(dd), ' floods)')
  log.peak <- v %in% c('GPP', 'CO2')
  map_dfr(list(
    list(resp = if (log.peak) 'Log peak % change' else 'Peak % change', y = if (log.peak) 'log(abs.percent.change)' else 'abs.percent.change', data = d),
    list(resp = 'Log duration',  y = 'log(duration)',      data = dd)),
    function(s) {
      s$data <- s$data %>% drop_na(vulnerable.score, class.num, rise.c) %>% filter(is.finite(eval(parse(text = s$y))))
      bind_rows(
        analyse(lmer_flood(as.formula(paste(s$y, '~ vulnerable.score + class.num + (1 | ID)'))), s$data) %>% add_model('No depth'),
        analyse(lmer_flood(as.formula(paste(s$y, '~ vulnerable.score + class.num + rise.c + (1 | ID)'))), s$data) %>% add_model('With depth')
      ) %>% mutate(Response = s$resp)
    }) %>% mutate(variable = v)
}))


# Test 3, one predictor at a time
t3_alone <- cached('t3_alone', flood.level, map_dfr(var.order, function(v) {
  message('Test 3 alone: ', v)
  d  <- flood.level %>% filter(variable == v)
  dd <- d %>% filter(duration > 0, if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)
  log.peak <- v %in% c('GPP', 'CO2')
  map_dfr(list(
    list(resp = if (log.peak) 'Log peak % change' else 'Peak % change', y = if (log.peak) 'log(abs.percent.change)' else 'abs.percent.change', data = d),
    list(resp = 'Log duration',  y = 'log(duration)',      data = dd)),
    function(s) {
      s$data <- s$data %>% drop_na(vulnerable.score, class.num, rise.c) %>% filter(is.finite(eval(parse(text = s$y))))
      alone_tests(as.formula(paste(s$y, '~ vulnerable.score + class.num + rise.c + (1 | ID)')), lmer_flood, s$data) %>% mutate(Response = s$resp)
    }) %>% mutate(variable = v)
}), deps = list(alone_tests))


#Test 3b: flood severity with prior flood exposure########
# the same responses as Test 3, with prior exposure at the site (days spent in earlier depth floods, in units of exposure.per days,
# centered on the site mean = a within-site effect) in place of depth rise. Depth rise is left out so the model stays at three predictors
flood.level.exp <- flood.level %>%
  left_join(exposure, by = c('ID', 'flood')) %>%
  mutate(exposure.u = exposure100 * 100 / exposure.per)

# rows for a Test 3b model: complete cases, finite response, exposure centered within site
prep_3b <- function(dat, y) {
  dat %>% drop_na(vulnerable.score, class.num, exposure.u) %>% filter(is.finite(eval(parse(text = y)))) %>%
    group_by(ID) %>% mutate(exposure.w = exposure.u - mean(exposure.u)) %>% ungroup()
}

t3b <- cached('t3b', list(flood.level.exp, exposure.per), map_dfr(var.order, function(v) {
  d  <- flood.level.exp %>% filter(variable == v)
  dd <- d %>% filter(duration > 0, if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)
  message('Test 3b: ', v)
  log.peak <- v %in% c('GPP', 'CO2')
  map_dfr(list(
    list(resp = if (log.peak) 'Log peak % change' else 'Peak % change', y = if (log.peak) 'log(abs.percent.change)' else 'abs.percent.change', data = d),
    list(resp = 'Log duration',  y = 'log(duration)',      data = dd)),
    function(s) {
      s$data <- prep_3b(s$data, s$y)
      analyse(lmer_flood(as.formula(paste(s$y, '~ vulnerable.score + class.num + exposure.w + (1 | ID)'))), s$data) %>%
        add_model('With exposure') %>% mutate(Response = s$resp)
    }) %>% mutate(variable = v)
}), deps = list(prep_3b))

t3b_alone <- cached('t3b_alone', list(flood.level.exp, exposure.per), map_dfr(var.order, function(v) {
  message('Test 3b alone: ', v)
  d  <- flood.level.exp %>% filter(variable == v)
  dd <- d %>% filter(duration > 0, if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)
  log.peak <- v %in% c('GPP', 'CO2')
  map_dfr(list(
    list(resp = if (log.peak) 'Log peak % change' else 'Peak % change', y = if (log.peak) 'log(abs.percent.change)' else 'abs.percent.change', data = d),
    list(resp = 'Log duration',  y = 'log(duration)',      data = dd)),
    function(s) {
      s$data <- prep_3b(s$data, s$y)
      alone_tests(as.formula(paste(s$y, '~ vulnerable.score + class.num + exposure.w + (1 | ID)')), lmer_flood, s$data) %>% mutate(Response = s$resp)
    }) %>% mutate(variable = v)
}), deps = list(alone_tests, prep_3b))


#Test 4: recovery (DO and CO2 only)########
# recovery rate is logged
t4 <- cached('t4', flood.level, map_dfr(c('DO', 'CO2'), function(v) {
  d <- flood.level %>% filter(variable == v)
  message('Test 4: ', v, ' (smallest n.recess = ', min(d$n.recess, na.rm = TRUE), ')')
  specs <- list(
    list(resp = 'Log recovery rate', y = 'log(recovery.rate)', col = 'recovery.rate', extra = '+ abs.percent.change', data = d),
    list(resp = 'Lag behind depth (d)', y = 'end.lag', col = 'end.lag', extra = '', data = d %>% filter(if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)),
    list(resp = 'Logit R2', y = 'r2.logit', col = 'r2.logit', extra = '', data = d))
  map_dfr(specs, function(s) {
    s$data <- s$data %>% drop_na(all_of(c(s$col, 'vulnerable.score', 'class.num', 'rise.c'))) %>% filter(is.finite(eval(parse(text = s$y))))
    if (s$extra != '') s$data <- s$data %>% drop_na(abs.percent.change)
    bind_rows(
      analyse(lmer_flood(as.formula(paste(s$y, '~ vulnerable.score + class.num', s$extra, '+ (1 | ID)'))), s$data) %>% add_model('No depth'),
      analyse(lmer_flood(as.formula(paste(s$y, '~ vulnerable.score + class.num', s$extra, '+ rise.c + (1 | ID)'))), s$data) %>% add_model('With depth')
    ) %>% mutate(Response = s$resp)
  }) %>% mutate(variable = v)
}))


# Test 4, one predictor at a time
t4_alone <- cached('t4_alone', flood.level, map_dfr(c('DO', 'CO2'), function(v) {
  message('Test 4 alone: ', v)
  d <- flood.level %>% filter(variable == v)
  specs <- list(
    list(resp = 'Log recovery rate', y = 'log(recovery.rate)', col = 'recovery.rate', extra = '+ abs.percent.change', data = d),
    list(resp = 'Lag behind depth (d)', y = 'end.lag', col = 'end.lag', extra = '', data = d %>% filter(if (drop.unrecovered) flood.recovered %in% TRUE else TRUE)),
    list(resp = 'Logit R2', y = 'r2.logit', col = 'r2.logit', extra = '', data = d))
  map_dfr(specs, function(s) {
    s$data <- s$data %>% drop_na(all_of(c(s$col, 'vulnerable.score', 'class.num', 'rise.c'))) %>% filter(is.finite(eval(parse(text = s$y))))
    if (s$extra != '') s$data <- s$data %>% drop_na(abs.percent.change)
    alone_tests(as.formula(paste(s$y, '~ vulnerable.score + class.num', s$extra, '+ rise.c + (1 | ID)')), lmer_flood, s$data) %>% mutate(Response = s$resp)
  }) %>% mutate(variable = v)
}), deps = list(alone_tests))


#Test 5: cumulative exposure (exploratory, no permutation)########
# one value per flood: the mean abs % change and mean abs difference over the flood's days, both logged
# exposure.w = prior exposure at the site (days spent in earlier depth floods, in units of exposure.per days) minus the site's mean, so the term is a within-site effect
# lmer with a site random intercept; the Boot 95% CI is a site-stratified flood bootstrap (BCa) shown next to the model CI
flood.exposure <- daily.flood %>%
  group_by(ID, flood, variable, class.num, exposure100) %>%
  summarise(pct.abs = mean(pct.abs[is.finite(pct.abs)]), diff.abs = mean(diff.abs), days = n(), .groups = 'drop') %>%
  group_by(ID, variable) %>%
  mutate(exposure.u = exposure100 * 100 / exposure.per, exposure.w = exposure.u - mean(exposure.u)) %>%
  ungroup()

# bootstrap CI for the fixed effects of a flood-level lmer: resample floods with replacement within each site, refit, BCa interval
# a replicate where a term drops out leaves it missing; a term with fewer than half its replicates gets no interval
boot_ci <- function(f, d, B = n.boot) {
  est <- function(dd) {
    m <- tryCatch(suppressWarnings(suppressMessages(lme4::lmer(f, data = dd))), error = function(e) NULL)
    if (is.null(m)) NULL else lme4::fixef(m)
  }
  th <- est(d)
  if (is.null(th)) return(NULL)
  full <- function(b) { out <- setNames(rep(NA_real_, length(th)), names(th)); if (!is.null(b)) out[intersect(names(b), names(th))] <- b[intersect(names(b), names(th))]; out }
  by.site <- split(seq_len(nrow(d)), d$ID)
  star <- t(replicate(B, full(est(d[unlist(lapply(by.site, function(ix) ix[sample.int(length(ix), replace = TRUE)])), , drop = FALSE]))))
  jack <- t(sapply(seq_len(nrow(d)), function(i) full(est(d[-i, , drop = FALSE]))))
  map_dfr(names(th), function(tm) {
    s <- star[, tm]; s <- s[is.finite(s)]
    if (length(s) < B / 2) return(tibble(term = tm, boot.low = NA_real_, boot.high = NA_real_))
    z0 <- qnorm(mean(s < th[tm]) + 0.5 * mean(s == th[tm]))
    jj <- jack[, tm]; jj <- jj[is.finite(jj)]
    a  <- if (length(jj) > 2) { dj <- mean(jj) - jj; sum(dj^3) / (6 * sum(dj^2)^1.5) } else NA_real_
    zq <- qnorm(c(0.025, 0.975))
    probs <- if (is.finite(z0) && is.finite(a)) pnorm(z0 + (z0 + zq) / (1 - a * (z0 + zq))) else c(0.025, 0.975)   # plain percentile if BCa is not defined
    q <- unname(quantile(s, probs))
    tibble(term = tm, boot.low = q[1], boot.high = q[2])
  })
}

analyse_boot <- function(f, d) {
  out <- analyse(lmer_flood(f), d, perm = FALSE)
  if (is.null(out)) return(NULL)
  out %>% left_join(boot_ci(f, d), by = 'term')
}

t5 <- cached('t5', flood.exposure, map_dfr(var.order, function(v) {
  message('Test 5: ', v)
  d <- flood.exposure %>% filter(variable == v) %>% drop_na(class.num, exposure.w)
  bind_rows(
    analyse_boot(y ~ exposure.w + class.num + (1 | ID), d %>% mutate(y = log(pct.abs))  %>% filter(is.finite(y))) %>% add_model('Log mean abs % change'),
    analyse_boot(y ~ exposure.w + class.num + (1 | ID), d %>% mutate(y = log(diff.abs)) %>% filter(is.finite(y))) %>% add_model('Log mean abs difference')
  ) %>% mutate(variable = v)
}))


# Test 5, one predictor at a time (no permutation, like Test 5)
t5_alone <- cached('t5_alone', flood.exposure, map_dfr(var.order, function(v) {
  message('Test 5 alone: ', v)
  d <- flood.exposure %>% filter(variable == v) %>% drop_na(class.num, exposure.w)
  bind_rows(
    alone_tests(y ~ exposure.w + class.num + (1 | ID), lmer_flood, d %>% mutate(y = log(pct.abs))  %>% filter(is.finite(y)), perm = FALSE) %>% mutate(Response = 'Log mean abs % change'),
    alone_tests(y ~ exposure.w + class.num + (1 | ID), lmer_flood, d %>% mutate(y = log(diff.abs)) %>% filter(is.finite(y)), perm = FALSE) %>% mutate(Response = 'Log mean abs difference')
  ) %>% mutate(variable = v)
}), deps = list(alone_tests))


#View saved results########
# reads the saved results from disk (no refitting) and opens each table in the Viewer pane (the tab next to Plots)
# the Viewer's back arrow flips between tables; print(tabs$test1) (test1 to test5) brings one back later
res <- map(set_names(c('t1', 't2', 't3', 't3b', 't4', 't5', 't2_alone', 't3_alone', 't3b_alone', 't4_alone', 't5_alone')), function(n) {
  f <- file.path(cache.dir, paste0(n, '.rds'))
  if (file.exists(f)) readRDS(f)$res else { message(n, ': no saved results yet, run its test above'); NULL }
})
cols.model <- c('Variable', 'Model', 'Term', 'Slope', '95% CI', 'p', 'Perm. p', 'Scheme', 'Site var %', 'R2 fixed', 'R2 total', 'n')

# put the one-predictor-at-a-time rows ('Single predictor') right after the combined model(s) of the same variable (and response)
# group.cols = 'variable' or c('variable', 'Response'); a missing alone table leaves the main table unchanged
add_singles <- function(main, alone, group.cols) {
  if (is.null(main) || is.null(alone)) return(main)
  single <- alone %>%
    mutate(estimate = alone.estimate, ci.low = alone.ci.low, ci.high = alone.ci.high, p = alone.p, p.perm = alone.perm,
           scheme = alone.scheme, model = 'Single predictor') %>%
    select(all_of(group.cols), term, model, estimate, ci.low, ci.high, p, p.perm, scheme)
  key <- function(x) do.call(paste, c(unname(as.list(x[group.cols])), sep = '|'))
  out <- bind_rows(mutate(main, .blk = 1L), mutate(single, .blk = 2L))
  out$.g <- match(key(out), unique(key(main)))
  out$.r <- seq_len(nrow(out))
  out %>% arrange(.g, .blk, .r) %>% select(-.g, -.blk, -.r)
}

tabs <- list()
if (!is.null(res$t1)) tabs$test1 <- make_ft(res$t1, c('Variable', 'Term', 'Slope', '95% CI', 'p', 'Perm. p', 'R2 fixed', 'R2 total', 'n', 'Floods'), term.labels.t1,
    title = 'Test 1: Do GPP, ER, DO and CO2 change from baseline with each step up in flood class (HI, BO, FR)?')
if (!is.null(res$t2)) {
  retry2 <- file.path(cache.dir, 't2_retry.rds')
  if (file.exists(retry2)) res$t2 <- bind_rows(res$t2, readRDS(retry2)$res %>% anti_join(res$t2, by = c('variable', 'model'))) %>% arrange(factor(variable, levels = var.order))
  tabs$test2 <- make_ft(add_singles(res$t2, res$t2_alone, 'variable'), title = 'Test 2: Is the daily difference from baseline (flood minus baseline) related to site vulnerability and flood class, with and without depth rise?')
}
if (!is.null(res$t3)) tabs$test3 <- make_ft(add_singles(res$t3, res$t3_alone, c('variable', 'Response')) %>% mutate(Model = paste(Response, '-', model)), cols.model,
    title = 'Test 3: Do site vulnerability and flood class predict how large (peak % change) and how long (duration) each flood response is?')
if (!is.null(res$t3b)) tabs$test3b <- make_ft(add_singles(res$t3b, res$t3b_alone, c('variable', 'Response')) %>% mutate(Model = paste(Response, '-', model)), cols.model,
    title = 'Test 3b: Does prior flood exposure at a site add to what vulnerability and class explain about overall severity (peak % change, duration)?')
if (!is.null(res$t4)) tabs$test4 <- make_ft(add_singles(res$t4, res$t4_alone, c('variable', 'Response')) %>% mutate(Model = paste(Response, '-', model)), cols.model,
    title = 'Test 4: Do site vulnerability and flood class predict how fast, how completely and how late DO and CO2 recover after a flood?')
if (!is.null(res$t5)) tabs$test5 <- make_ft(add_singles(res$t5 %>% mutate(Response = model, model = 'Combined'), res$t5_alone, c('variable', 'Response')) %>% mutate(Model = paste(Response, '-', model)),
    c('Variable', 'Model', 'Term', 'Slope', '95% CI', 'Boot 95% CI', 'p', 'Site var %', 'R2 fixed', 'R2 total', 'n', 'Floods'),
    title = 'Test 5: Does prior flood exposure at a site (days in earlier floods) change the size of the response, after accounting for flood class?')

if (!exists('show.tables') || isTRUE(show.tables)) for (nm in names(tabs)) print(tabs[[nm]])   # 'model checks.R' turns this off
