startDate <- "2024-06-17"
endDate <- "2024-07-25"
parameterCd <- c('00010','00300','00065')
ventID<-'02322700'

IU<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU<-IU %>% rename('Date'='dateTime', 'temp.water'='X_00010_00000', 'DO.obs'='X_00300_00000')%>%
  mutate(depth=X_00065_00000-13.72)%>%
  mutate(min=minute(Date)) %>% filter(min==0) %>%
  mutate(DO.sat= Cs(temp.water), solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         light=calc_light(solar.time,  29.8, -82.6) )

IU<-IU %>% select(DO.obs,depth,temp.water,DO.sat,solar.time,light)

bayes_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_specs <- specs(bayes_name, K600_daily_meanlog_meanlog=0.1, K600_daily_meanlog_sdlog=0.001, GPP_daily_lower=0,
                     burnin_steps=1000, saved_steps=1000)
mm<- metab(bayes_specs, IU)
prediction2 <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,GPP_Rhat,ER_Rhat,K600_daily_Rhat)

write_csv(prediction2, "04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")


IU<- read_csv("04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")
IU<-IU %>%mutate(ID=='IU')%>%rename("GPPavg"='GPP_daily_mean','ER'='ER_daily_mean','K600_1d'='K600_daily_mean', 'Date'='date')

write_csv(IU, "04_Outputs/Stream metabolizer results/not parsed/IU/IU_07252024.csv")

file.names <- list.files(path="04_Outputs/Stream metabolizer results/not parsed/IU", pattern=".csv", full.names=TRUE)
IU_all<-data.frame()
for(fil in file.names){
  site <- read_csv(fil)
  IU_all<-rbind(IU_all,site)}
IU_all<-IU_all %>% select(Date, ER, GPPavg, K600_1d,GPP_Rhat,ER_Rhat,K600_daily_Rhat,ID)


write_csv(IU_all, "04_Outputs/Stream metabolizer results/IU.csv")
write_csv(IU_all, "04_Outputs/Stream metabolizer results/not parsed/IU.csv")
