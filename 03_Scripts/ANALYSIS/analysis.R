source("03_Scripts/ANALYSIS/disturbance isolation functions.R")

SpC<-read_csv("04_Outputs/flood impacts/SpC.csv")%>%
  select(ID, flood, minimum)%>%
  rename(SpC=minimum)
pH<-read_csv("04_Outputs/flood impacts/pH.csv")%>%
  select(ID, flood, minimum)%>%
  rename(pH=minimum)

flood.class<-full_join(SpC, pH)%>%
  filter(!is.na(flood))%>%
  left_join(read_csv("04_Outputs/flood impacts/FR.class.csv"))

vulnerability <- data.frame(
  ID = c('IU', 'ID', 'GB', 'LF', 'AM', 'OS'),
  vulnerable.score = c(1, 2, 3, 4, 5, 6))


h.percent.change<-depth%>%
  mutate(h.percent.change=(maximum-base)/base*100)%>%
  select(ID, flood, h.percent.change)%>%
  left_join(flood.class)%>%left_join(vulnerability)


GPP <- read_csv("04_Outputs/flood impacts/GPP.csv")
ER <- read_csv("04_Outputs/flood impacts/ER.csv")
DO <- read_csv("04_Outputs/flood impacts/DO.csv")
CO2 <- read_csv("04_Outputs/flood impacts/CO2.csv")
depth <- read_csv("04_Outputs/flood impacts/depth.csv")

declined<-rbind(GPP, DO)%>%rename(peak.response=minimum)
increased<-rbind(ER, CO2,depth)%>%rename(peak.response=maximum)

flood.response<-rbind(declined, increased)%>%
  left_join(h.percent.change)%>%
  mutate(reponse.percent.change=(peak.response-base)/base*100)

flood.response%>%
  filter(variable=='GPP')%>%
  ggplot(aes(x=h.percent.change, y=recess.slope, color=r2.recess))+
  geom_point(size=2)+
  facet_wrap(~ID, scales='free')


GPP_flood_df <- read_csv("04_Outputs/flood impacts/GPP.flood.df.csv")
ER_flood_df  <- read_csv("04_Outputs/flood impacts/ER.flood.df.csv")
DO_flood_df  <- read_csv("04_Outputs/flood impacts/DO.flood.df.csv")
CO2_flood_df <- read_csv("04_Outputs/flood impacts/CO2.flood.df.csv")
depth_flood_df <- read_csv("04_Outputs/flood impacts/depth.flood.df.csv")

h.per.change.timeseries <- depth_flood_df%>%
  mutate(h.percent.change=(conc-base)/base*100)%>%
  select(ID, flood, Date, h.percent.change)

time.series <- rbind(GPP_flood_df, ER_flood_df, DO_flood_df, CO2_flood_df,depth_flood_df)%>%
  left_join(h.per.change.timeseries)%>%
  left_join(flood.class)


time.series%>%
  filter(variable=='ER')%>%
  ggplot(aes(x=h.percent.change, y=conc, color=class))+
  geom_point(size=1)+
  facet_wrap(~ID, scales='free')


time.series%>%
  filter(flood==1, variable %in% c('GPP', 'DO'))%>%
  mutate(loess.per.change=(loess-base)/base*100)%>%
  ggplot(aes(x=h.percent.change, y=loess.per.change, color=variable))+
  geom_point(size=1)+
  facet_wrap(~ID, scales='free')

