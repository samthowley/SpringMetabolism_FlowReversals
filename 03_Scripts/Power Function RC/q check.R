
depth <- read_csv("02_Clean_data/Chem/depth.csv")

startDate <- "2021-04-03"
endDate <- "2024-08-06"
parameterCd <- c('00010','00300','00065', '00060')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)

IDwidth<-16.8

IUedit<-IU%>%
  rename('discharge'='X_00060_00000', 'depth'='X_00065_00000')%>%
  mutate(Date=as.Date(dateTime))%>%
  summarise(depth=mean(depth, na.rm=T), discharge=mean(discharge, na.rm=T), .by=Date)%>%
  mutate(
    depth=depth-14, 
    velocity=discharge/(depth*IDwidth)
    )


ggplot(IUedit, aes(x=log10(depth), y=log10(discharge)))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()

#GB Vent#####
GB_Flow <- read_excel("01_Raw_data/County Data/GB_Flow.xlsx", skip = 25)%>%
  mutate(Date=as.Date(Date))
range(GB_Flow$Date)

GB.width<-7.4

GB.depth <- read_csv("02_Clean_data/Chem/depth.csv")%>%
  filter(ID=='GB')%>%
  mutate(Date=as.Date(Date))%>%
  summarise(depth=mean(depth, na.rm=T), .by=Date)%>%
  left_join(GB.flow, by='Date')%>%
  mutate(velocity=Discharge/(GB.width*depth))%>%
  filter(Discharge>1)
  

ggplot(GB.depth, aes(x=log10(depth), y=log10(Discharge)))+
  geom_point()+
  theme_bw()


#LF#######
LF_Vent_Flow <- read_excel("01_Raw_data/County Data/LF.Vent_Flow.xlsx", skip=25)%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, Discharge)

LF_Vent_Stage <- read_excel("01_Raw_data/County Data/LF.Vent_Stage.xlsx",  skip = 25)%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, `Level NGVD29`)

widthLF<- 6.4
  
LF.depth<-depth%>%
  filter(ID=='LF')%>%
  mutate(Date=as.Date(Date))%>%
  summarise(depth=mean(depth, na.rm=T), .by=Date)%>%
  left_join(LF_Vent_Flow, by='Date')%>%
  #filter(!is.na(Discharge, Discharge>1))%>%
  mutate(velocity=Discharge/(depth*widthLF))


left_join(LF_Vent_Flow, LF_Vent_Stage)%>%
  ggplot(aes(x=`Level NGVD29`, y=Discharge))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()


left_join(LF_Vent_Flow, LF.depth)%>%
  ggplot(aes(x=depth, y=velocity))+
  geom_point()+
  theme_bw()
#OS#############
OS_Flow <- read_excel("01_Raw_data/County Data/OS_Flow.xlsx", skip=25)%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, Discharge)

OS_Stage <- read_excel("01_Raw_data/County Data/OS_Stage.xlsx",  skip = 25)%>%
  mutate(Date=as.Date(Date))%>%
  select(Date, `Level NGVD29`)


OS.depth<-depth%>%
  filter(ID=='OS')%>%
  mutate(Date=as.Date(Date))%>%
  summarise(depth=mean(depth, na.rm=T), .by=Date)%>%
  left_join(OS_Flow, by='Date')%>%
  filter(Discharge>1)%>%
  mutate(velocity=Discharge/(depth*widthLF))

OS.depth%>%
  ggplot(aes(x=log10(depth), y=log10(Discharge)))+
  geom_point()+
  theme_bw()

