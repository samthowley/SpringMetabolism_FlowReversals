source("03_Scripts/ANALYSIS/disturbance isolation functions.R")

(file.names <- list.files(path="02_Clean_data/Chem", pattern=".csv", full.names=TRUE))
file.names<-file.names[c(1,4,2)]

data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})


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


flood.response%>%filter(variable %in% c('GPP', 'DO'))%>%
ggplot(aes(x=h.percent.change, y=reponse.percent.change, color=variable, shape=ID))+
  geom_point()+
  geom_smooth(method='lm', aes(group = ID), se=F)+
  theme_bw()


