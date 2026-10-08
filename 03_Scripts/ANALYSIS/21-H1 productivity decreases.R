source('03_Scripts/ANALYSIS/09-analysis prep.R')
library(flextable)

# H1: backwater floods decrease spring productivity
#
# Test: paired t-test, one pair per flood
#   baseline   = the flood's baseline value (base, from the isolate disturbances scripts)
#   flood.mean = mean daily value across the flood period (flood.start to flood.end for that variable)
# Run for each flood class (FR, HI, BO) and for all floods together.
# The flood is the unit (not the day), so n = number of floods.
# One-sided: tests whether the flood mean is LOWER than baseline.

#parameters########
h1.vars     <- 'GPP'     # add 'ER' (|ER|), 'DO' or 'CO2' to run the same test on them
alternative <- 'less'    # 'less' = flood < baseline; 'two.sided' to test any change
min.floods  <- 3         # minimum floods in a group to run the test

#one row per flood########
h1.pairs <- time.series%>%
  filter(variable %in% h1.vars, !is.na(conc), !is.na(base), !is.na(class))%>%
  group_by(ID, flood, variable, class)%>%
  summarise(baseline=first(base), flood.mean=mean(conc), n.days=n(), .groups='drop')%>%
  mutate(class=as.character(class))

# add an 'All floods' group
h1.pairs <- bind_rows(h1.pairs, h1.pairs%>%mutate(class='All floods'))%>%
  mutate(class=factor(class, levels=c('All floods', 'FR', 'HI', 'BO')))

#paired t-test########
h1.test <- h1.pairs%>%
  group_by(variable, class)%>%
  group_modify(~{
    if (nrow(.x) < min.floods) return(tibble(n.floods=nrow(.x)))
    tt <- t.test(.x$flood.mean, .x$baseline, paired=TRUE, alternative=alternative)
    tibble(
      n.floods=nrow(.x),
      baseline.mean=mean(.x$baseline),
      flood.mean=mean(.x$flood.mean),
      mean.diff=unname(tt$estimate),            # flood - baseline
      pct.change=mean.diff/baseline.mean*100,
      t=unname(tt$statistic),
      df=unname(tt$parameter),
      p=tt$p.value
    )
  })%>%
  ungroup()

h1.test

#table########
h1.table <- h1.test%>%
  mutate(across(c(variable, class), as.character))%>%
  flextable()%>%
  set_header_labels(variable='Variable', class='Flood class', n.floods='Floods (n)',
                    baseline.mean='Baseline mean', flood.mean='Flood mean',
                    mean.diff='Difference (flood - baseline)', pct.change='% change',
                    t='t', df='df', p='p')%>%
  colformat_double(j=c('baseline.mean', 'flood.mean', 'mean.diff', 'pct.change', 't', 'df'), digits=2)%>%
  colformat_double(j='p', digits=3)%>%
  set_caption('Table H1: paired t-test, flood mean vs baseline')%>%
  autofit()

htmltools::html_print(htmltools::tagList(flextable::htmltools_value(h1.table)))

#boxplot########
h1.plot <- h1.pairs%>%
  pivot_longer(c(baseline, flood.mean), names_to='period', values_to='value')%>%
  mutate(
    period=factor(recode(period, flood.mean='flood'), levels=c('baseline', 'flood')),
    fill.key=if_else(period=='baseline', 'baseline', as.character(class))
  )

ggplot(h1.plot, aes(x=period, y=value))+
  geom_line(aes(group=interaction(ID, flood)), color='grey60', alpha=0.6)+   # one line per flood
  geom_boxplot(aes(fill=fill.key), outlier.shape=NA, alpha=0.6, width=0.55)+
  geom_point(aes(color=ID), size=1.8)+
  scale_fill_manual(values=c(class_colors, 'All floods'='grey50'), guide='none')+
  scale_color_manual(values=site_colors)+
  facet_grid(variable~class, scales='free_y')+
  labs(x=NULL, y='Mean daily value', color='Site')+
  theme_spring()
