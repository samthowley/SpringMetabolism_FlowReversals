source('03_Scripts/ANALYSIS/analysis prep/09-analysis prep.R')

# One tidy table of flood metrics: one row per site x flood x variable
# (GPP, ER, DO, CO2). Feeds the H2 models (script 19).
#
# Before running: re-run the isolate disturbances_*.R scripts (GPP, ER, DO, CO2,
# depth) so flood.recovered, n.rise and n.recess exist in the flood impact csvs,
# then re-run analysis prep.R (sourced above).
#
# Variable definitions (daily, same as the flood response):
#   GPP, ER = daily value (ER is |ER|), DO = daily minimum, CO2 = daily maximum
# Rise / recession slopes are per sample step (hourly DO, CO2; daily GPP, ER),
# so they are z-scored within variable and only comparable within a variable.

#depth reference########
depth.ref <- flood.response%>%
  filter(variable=='depth')%>%
  transmute(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    duration.depth=duration,
    peak.Date.depth=as.Date(peak.Date),
    flood.end.depth=as.Date(flood.end)
  )

#new columns from the edited functions file########
for (col in c('flood.recovered', 'n.rise', 'n.recess')) {
  if (!col %in% names(flood.response)) {
    message(col, ' not found: re-run the isolate disturbances_*.R scripts')
    flood.response[[col]] <- NA
  }
}

#metrics########
# expected direction of the rise to peak (-1 = the variable falls, +1 = it rises);
# the recession is the reverse. Comes from response.dir, so GPP is -1 in most
# floods and +1 in the five GPP-increase floods (GPPmax script)
flood.metrics <- flood.response%>%
  filter(variable!='depth')%>%
  mutate(
    ID=as.character(ID),
    flood=as.numeric(as.character(flood)),
    variable=as.character(variable),
    exp.dir=if_else(response.dir=='decrease', -1, 1),
    rise.ok=sign(rise.slope)==exp.dir,
    recess.ok=sign(recess.slope)==-exp.dir,
    rise.slope=if_else(rise.ok, rise.slope, NA_real_),
    r2.rise=if_else(rise.ok, r2.rise, NA_real_),
    recess.slope=if_else(recess.ok, recess.slope, NA_real_),
    r2.recess=if_else(recess.ok, r2.recess, NA_real_)
  )%>%
  group_by(variable, response.dir)%>%   # GPP rises in some floods and falls in others: z-score each separately
  mutate(
    rise.slope.z=as.numeric(scale(rise.slope)),
    recess.slope.z=as.numeric(scale(recess.slope))
  )%>%
  ungroup()%>%
  left_join(depth.ref, by=c('ID', 'flood'))%>%
  mutate(
    metric=case_when(
      variable=='DO' ~ 'daily minimum',
      variable=='CO2' ~ 'daily maximum',
      TRUE ~ 'daily value'),
    class=as.character(class),
    percent.change=reponse.percent.change,
    abs.percent.change=abs(percent.change),
    duration.rel=duration/duration.depth,
    severity=abs.percent.change*duration,
    peak.lag=as.numeric(as.Date(peak.Date)-peak.Date.depth),
    end.lag=as.numeric(as.Date(flood.end)-flood.end.depth)
  )%>%
  select(
    ID, flood, variable, response.dir, metric, class, vulnerable.score, 
    
    h.percent.change,
    
    flood.start, flood.end, peak.Date, 
    
    base, peak.response,
    
    percent.change, abs.percent.change, duration, duration.depth,
    
    time2peak, time.to.recover, flood.recovered,
    recess.slope , r2.recess, 
  )%>%
  arrange(variable, ID, flood)

write_csv(flood.metrics, "04_Outputs/flood impacts/flood_metrics.csv")

