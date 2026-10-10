# Figures for the five tests. Plots only: they open in the RStudio Plots pane (back arrow flips between them).
# Nothing is saved to disk. analysis.R is not edited; its objects are reused.
library(tidyverse)
source('03_Scripts/ANALYSIS/analysis prep/09-analysis prep.R')

#flood_metrics <- read_csv("04_Outputs/flood impacts/flood_metrics.csv")

#parameters########
min.day.frac <- 0.6    # a day needs this fraction of the site's usual readings to get a daily mean DO or CO2
min.days     <- 10     # a site x class needs at least this many days to get a slope

alpha            <- 0.05   # only terms with p < alpha go on to the permutation test
n.perm           <- 999    # random shuffles per tested term (vulnerability uses every site ordering, so no random draws)
site.icc.min     <- 0.05   # site share of the variance: below this shuffle across all floods, otherwise within site
drop.unrecovered <- TRUE   # lag and duration models only: drop floods that never returned to threshold

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



#objects from analysis.R########
if (!(exists('flood.level') && exists('daily'))) {
  show.tables <- FALSE     # keeps analysis.R from opening its five tables
  source('03_Scripts/ANALYSIS/analysis.R')
  rm(show.tables)
}


#style########
site.order  <- names(sort(vuln.lookup))                    # IU, ID, GB, LF, AM, OS (low to high vulnerability)
class.col   <- c(HI = '#2a78d6', BO = '#eb6834', FR = '#1baf7a')
period.col  <- c(base = '#9a9a96', class.col)              # baseline is grey
class.shape <- c(HI = 21, BO = 24, FR = 22)                # circle, triangle, square: all take a fill, so open = never recovered
site.col    <- setNames(c('#9f93e8', '#857acf', '#6d62b6', '#554b9e', '#3f3386', '#2b1b6e'), site.order)   # one violet hue, light to dark
var.label   <- as_labeller(c(GPP = 'GPP', ER = '|ER|', DO = 'DO', CO2 = 'CO2'))

theme_fig <- theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line(colour = 'grey92', linewidth = 0.25),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(colour = 'grey70', linewidth = 0.3),
        axis.ticks       = element_line(colour = 'grey70', linewidth = 0.3),
        strip.background = element_blank(),
        strip.text       = element_text(face = 'bold'),
        plot.subtitle    = element_text(colour = 'grey35'),
        legend.key.height = unit(0.9, 'lines'))

# direct n label in the top-left corner of each panel
n.label <- function(d, ...) geom_text(data = d, aes(label = label), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
                                      size = 2.4, colour = 'grey35', inherit.aes = FALSE, ...)

# site (colour) and class (shape) legends shared by Figures 3 to 6
scale.site  <- list(scale_colour_manual(values = site.col, name = 'Site (low to high vulnerability)', drop = FALSE),
                    scale_fill_manual(values = site.col, na.value = NA, guide = 'none', drop = FALSE),
                    scale_shape_manual(values = class.shape, name = 'Flood class', drop = FALSE),
                    guides(colour = guide_legend(order = 1, nrow = 1),
                           shape  = guide_legend(order = 2, nrow = 1, override.aes = list(fill = 'grey45', colour = 'grey45'))))

# flood-level data: sites in vulnerability order, open symbol = flood never returned to threshold
# (same rule as analysis.R: flood.recovered %in% TRUE, so NA counts as not recovered)
prep.flood <- function(d) d %>%
  mutate(ID = factor(as.character(ID), levels = site.order),
         class = factor(as.character(class), levels = names(class.col)),
         recovered = flood.recovered %in% TRUE,
         fill.id = if_else(recovered, ID, factor(NA, levels = site.order)))


#Figure 1. Test 1: raw daily values by period (baseline is its own box)########
d1 <- daily %>%
  mutate(period = factor(as.character(period), levels = names(period.col)))
n1 <- d1 %>% count(variable, period) %>% mutate(label = paste0('n=', n))

fig1 <- ggplot(d1, aes(period, value, fill = period)) +
  geom_boxplot(width = 0.65, linewidth = 0.3, colour = 'grey30', alpha = 0.85,
               outlier.size = 0.4, outlier.alpha = 0.25, outlier.stroke = 0) +
  geom_text(data = n1, aes(y = Inf, label = label), vjust = 1.4, size = 2.4, colour = 'grey35') +
  scale_fill_manual(values = period.col, guide = 'none') +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.12))) +
  facet_wrap(~variable, scales = 'free_y', labeller = var.label) +
  labs(title = 'Test 1: raw daily values by period', subtitle = 'n = days. Baseline = days outside flood periods.',
       x = NULL, y = 'Daily value') +
  theme_fig
print(fig1)


#Figure 2. Test 2: flood minus baseline, by site and class########
d2 <- daily.flood %>%
  filter(!is.na(diff), !is.na(class)) %>%
  mutate(ID = factor(as.character(ID), levels = site.order),
         class = factor(as.character(class), levels = names(class.col)))
n2 <- d2 %>% count(variable, ID, class) %>% mutate(label = as.character(n))

dodge2 <- position_dodge2(preserve = 'single')
fig2 <- ggplot(d2, aes(ID, diff, fill = class)) +
  geom_hline(yintercept = 0, colour = 'grey55', linewidth = 0.3) +
  geom_boxplot(position = dodge2, linewidth = 0.3, colour = 'grey30', alpha = 0.85,
               outlier.size = 0.4, outlier.alpha = 0.25, outlier.stroke = 0) +
  geom_text(data = n2, aes(y = Inf, label = label, group = class), position = position_dodge2(width = 0.9, preserve = 'single'),
            vjust = 1.4, size = 2.2, colour = 'grey35') +
  scale_fill_manual(values = class.col, name = 'Flood class', drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.12))) +
  facet_wrap(~variable, scales = 'free_y', labeller = var.label) +
  labs(title = 'Test 2: flood minus baseline, by site', subtitle = 'Numbers = n days per box. Below 0 = lower during the flood than at baseline.',
       x = 'Site (low to high vulnerability)', y = 'Flood minus baseline') +
  theme_fig + theme(legend.position = 'bottom')
print(fig2)


#Figure 3. Test 3: severity (peak % change vs duration)########
d3.all <- flood.level %>% prep.flood()
d3 <- d3.all %>% filter(!is.na(abs.percent.change), !is.na(duration), duration > 0)
if (nrow(d3) < nrow(d3.all)) message('Figure 3: ', nrow(d3.all) - nrow(d3), ' flood(s) left off (missing peak or duration <= 0, which cannot sit on a log axis)')
n3 <- d3 %>% group_by(variable) %>% summarise(label = paste0('n=', n(), ' (', sum(!recovered), ' open)'), .groups = 'drop')

fig3 <- ggplot(d3, aes(duration, abs.percent.change, colour = ID, fill = fill.id, shape = class)) +
  geom_point(size = 1.9, stroke = 0.5) +
  n.label(n3) +
  scale.site +
  scale_x_log10(breaks = scales::breaks_log(n = 5), labels = scales::label_number(drop0trailing = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.08))) +
  facet_wrap(~variable, scales = 'free', labeller = var.label) +
  labs(title = 'Test 3: severity', subtitle = 'Open symbol = never returned to threshold (duration is the end of the record; the duration models drop these).',
       x = 'Response duration (days, log scale)', y = 'Peak absolute % change') +
  theme_fig + theme(legend.position = 'bottom')
print(fig3)


#Figure 4. Test 4: recovery rate vs fit (DO and CO2)########
d4 <- flood.level %>% filter(variable %in% c('DO', 'CO2')) %>% prep.flood() %>%
  filter(!is.na(recovery.rate), !is.na(r2.logit)) %>%
  mutate(fill.id = ID)
n4 <- d4 %>% group_by(variable) %>% summarise(label = paste0('n=', n()), .groups = 'drop')

fig4 <- ggplot(d4, aes(recovery.rate, r2.logit, colour = ID, fill = fill.id, shape = class)) +
  geom_point(size = 1.9, stroke = 0.5) +
  n.label(n4) +
  scale.site +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.08))) +
  facet_wrap(~variable, scales = 'free', labeller = var.label) +
  labs(title = 'Test 4: recovery', subtitle = 'Rate is per sample step (hourly), so compare within a panel only. R2 is on the logit scale.',
       x = 'Recovery rate (absolute slope)', y = 'Recovery R2 (logit)') +
  theme_fig + theme(legend.position = 'bottom')
print(fig4)


#Figure 5. Test 4 lag: day the variable ended vs day the depth flood ended (DO and CO2)########
var.end <- flood.response %>%
  filter(variable %in% c('DO', 'CO2')) %>%
  transmute(ID = as.character(ID), flood = as.numeric(as.character(flood)), variable = as.character(variable),
            var.end = as.Date(flood.end))
d5.all <- flood.level %>% filter(variable %in% c('DO', 'CO2')) %>%
  mutate(variable = as.character(variable)) %>%
  left_join(var.end, by = c('ID', 'flood', 'variable')) %>%
  left_join(flood.info %>% select(ID, flood, flood.end.depth), by = c('ID', 'flood')) %>%
  mutate(variable = factor(variable, levels = var.order)) %>%
  prep.flood()
d5 <- d5.all %>% filter(!is.na(var.end), !is.na(flood.end.depth))
if (nrow(d5) < nrow(d5.all)) message('Figure 5: ', nrow(d5.all) - nrow(d5), ' flood(s) left off (no end date for the depth flood or the variable)')
n5 <- d5 %>% group_by(variable) %>% summarise(label = paste0('n=', n(), ' (', sum(!recovered), ' open)'), .groups = 'drop')

# the 1:1 line also sets one shared range for x and y inside each panel
lim5 <- d5 %>% group_by(variable) %>%
  summarise(lo = min(c(flood.end.depth, var.end)), hi = max(c(flood.end.depth, var.end)), .groups = 'drop') %>%
  pivot_longer(c(lo, hi), values_to = 'day')

fig5 <- ggplot(d5, aes(flood.end.depth, var.end, colour = ID, fill = fill.id, shape = class)) +
  geom_line(data = lim5, aes(day, day, group = variable), colour = 'grey45', linewidth = 0.3, linetype = 'dashed', inherit.aes = FALSE) +
  geom_point(size = 1.9, stroke = 0.5) +
  n.label(n5) +
  scale.site +
  scale_x_date(breaks = scales::breaks_pretty(4), date_labels = '%b %Y') +
  scale_y_date(breaks = scales::breaks_pretty(4), date_labels = '%b %Y') +
  facet_wrap(~variable, scales = 'free', labeller = var.label) +
  labs(title = 'Test 4: lag behind the depth flood', subtitle = 'Above the dashed 1:1 line = variable ends after the depth flood. Open symbol = never returned to threshold (end of record).',
       x = 'Day the depth flood ended', y = 'Day the variable ended') +
  theme_fig + theme(legend.position = 'bottom')
print(fig5)


#Figure 6. Test 5 (exploratory): cumulative exposure vs peak % change########
exposure.flood <- daily %>% filter(!is.na(flood.num)) %>% distinct(ID, flood = flood.num, exposure100)
d6 <- flood.level %>%
  left_join(exposure.flood, by = c('ID', 'flood')) %>%
  prep.flood() %>%                       # after the join, which turns ID into plain text
  filter(!is.na(abs.percent.change), !is.na(exposure100)) %>%
  mutate(fill.id = ID) %>%
  arrange(ID, exposure100)
n6 <- d6 %>% group_by(variable) %>% summarise(label = paste0('n=', n()), .groups = 'drop')

fig6 <- ggplot(d6, aes(exposure100, abs.percent.change, colour = ID, fill = fill.id, shape = class)) +
  geom_smooth(aes(exposure100, abs.percent.change, group = 1), method = 'lm', formula = y ~ x, se = FALSE,
              colour = 'grey45', linewidth = 0.4, linetype = 'dashed', inherit.aes = FALSE) +
  geom_line(aes(group = ID), linewidth = 0.3, alpha = 0.6) +
  geom_point(size = 1.9, stroke = 0.5) +
  n.label(n6) +
  scale.site +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.08))) +
  facet_wrap(~variable, scales = 'free', labeller = var.label) +
  labs(title = 'Test 5 (exploratory): cumulative exposure', subtitle = 'Lines join each site\'s floods in order. Dashed grey = pooled linear fit, no interval.',
       x = 'Exposure before the flood (per 100 days in flood)', y = 'Peak absolute % change') +
  theme_fig + theme(legend.position = 'bottom')
print(fig6)
