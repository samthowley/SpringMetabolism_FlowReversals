####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(zoo)
library(cowplot)
# siteBO<-LFFR
recovery_calc <- function(siteBO) {
  x<-c("Date","DO","depth","ER","GPPavg")

  siteBO <- siteBO %>%
    group_by(count = cumsum(c(TRUE, diff(Date) >= 1))) %>%
    ungroup()

  ind <- which.max(siteBO$depth)
  siteBO_prior <- siteBO[seq_len(ind - 1), ]
  siteBO_prior<-filter(siteBO_prior, count<10)
  siteBO_disturb <- siteBO[ind:nrow(siteBO), ]

  GPP_prior<-mean(siteBO_prior$GPPavg, na.rm=T)
  ER_prior<-mean(siteBO_prior$ER, na.rm=T)
  h_prior<-mean(siteBO_prior$depth, na.rm=T)
  DO_prior<-mean(siteBO_prior$DO, na.rm=T)

  siteBO_disturb$GPPavg_ratio<-siteBO_disturb$GPPavg/GPP_prior
  siteBO_disturb$ER_ratio<-siteBO_disturb$ER/ER_prior
  siteBO_disturb$DO_ratio<-siteBO_disturb$DO/DO_prior
  siteBO_disturb$depth_ratio<-siteBO_disturb$depth/h_prior

  u<-
    siteBO_disturb %>%
    mutate( ERmean= rollapply(ER_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  u<-u[,c(16,1)]
  siteBO_disturb<-left_join(siteBO_disturb,u,by=c('Date'))

  v<-
    siteBO_disturb %>%
    mutate( GPPmean= rollapply(GPPavg_ratio,4,mean, fill=NA, partial=TRUE, align='left'))
  v<-v[,c(17,1)]
  siteBO_disturb<-left_join(siteBO_disturb,v,by=c('Date'))

  w<-
    siteBO_disturb %>%
    mutate( DOmean= rollapply(DO,4,mean, fill=NA, partial=TRUE, align='left'))
  w<-w[,c(18,1)]
  siteBO_disturb<-left_join(siteBO_disturb,w,by=c('Date'))

  y<-
    siteBO_disturb %>%
    mutate( hmean= rollapply(depth,4,mean, fill=NA, partial=TRUE, align='left'))
  y<-y[,c(19,1)]
  siteBO_disturb<-left_join(siteBO_disturb,y,by=c('Date'))

  #siteBO_disturb$hdiff<-max(siteBO$depth_diff, na.rm = T)
  return(siteBO_disturb)}
extract_recovery <- function(siterecov) {
  modER<-lm(ERmean ~ count, data = siterecov)
  cf <- coef(modER)
  (InterceptER<- cf[1])
  (SlopeER <- cf[2])
  (ER_recov<-(1-InterceptER)/SlopeER)

  modGPP<-lm(GPPmean ~ count, data = siterecov)
  cf <- coef(modGPP)
  (InterceptGPP<- cf[1])
  (SlopeGPP <- cf[2])
  (GPP_recov<-(1-InterceptGPP)/SlopeGPP)

  modDO<-lm(DOmean ~ count, data = siterecov)
  cf <- coef(modDO)
  (InterceptDO<- cf[1])
  (SlopeDO <- cf[2])
  (DO_recov<-(1-InterceptDO)/SlopeDO)

  modH<-lm(hmean ~ count, data = siterecov)
  cf <- coef(modH)
  (InterceptH<- cf[1])
  (SlopeH <- cf[2])
  (H_recov<-(1-InterceptH)/SlopeH)

  (h_diff<-max(siterecov$depth_diff, na.rm=T))

  IDFR_ls<-list(GPP_recov,ER_recov,DO_recov,H_recov,h_diff)
  df<- data.frame(IDFR_ls[[1]],IDFR_ls[[2]],IDFR_ls[[3]],
                  IDFR_ls[[4]],IDFR_ls[[5]])
  colnames(df)[1]<-'GPP_recov'
  colnames(df)[2]<-'ER_recov'
  colnames(df)[3]<-'DO_recov'
  colnames(df)[4]<-'H_recov'
  colnames(df)[5]<-'h_diff'


  return(df)}
##### data####

master <- read_csv("04_Outputs/ForRecovery.csv")

master<-master %>%group_by(ID) %>% mutate(depth_min=min(depth, na.rm=T))
master$depth_diff<-master$depth-master$depth_min
master$days<-as.Date(master$Date)
master <- master[!duplicated(master[c('days', 'ID')]),]

IDs<-split(master,master$ID)
AM<-IDs[[1]]
GB<-IDs[[2]]
ID<-IDs[[3]]
LF<-IDs[[4]]
OS<-IDs[[5]]
IU<-IDs[[6]]

####AM####
AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-02-01" & Date<"2023-03-11"~ 2))
AMFR<-filter(AMFR, RI==2)
# ggplot(AMFR, aes(Date))+
#   geom_line(aes(y=DO), size=1)

AMFR_02<-recovery_calc(AMFR)

AMFR_02<-filter(AMFR_02, count<32)
# ggplot(AMFR_02, aes(count))+
#   geom_line(aes(y=ERmean), size=1)+
#   geom_line(aes(y=GPPmean), size=1)+
#   geom_line(aes(y=DOmean), size=1)

AMFR_02<-extract_recovery(AMFR_02)



AMFR<- AM %>% mutate(RI = case_when(
  days> "2023-06-01" & days<"2023-07-25"~ 2))
AMFR<-filter(AMFR, RI==2)
ggplot(AMFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

AMFR_06<-recovery_calc(AMFR)

AMFR_06<-filter(AMFR_06, count>35)
# ggplot(AMFR_06, aes(count))+
#   geom_line(aes(y=ERmean, color='ER'), size=1)+
#   geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#   geom_line(aes(y=DOmean, color='DO'), size=1)
AMFR_06<-extract_recovery(AMFR_06)

AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-08-22" & Date<"2023-10-01"~ 2))
AMFR<-filter(AMFR, RI==2)
# ggplot(AMFR, aes(Date))+
#   geom_line(aes(y=ER), size=1)

AMFR_08<-recovery_calc(AMFR)

# ggplot(AMFR_08, aes(count))+
#     geom_line(aes(y=ERmean, color='ER'), size=1)+
#     geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#     geom_line(aes(y=DOmean, color='DO'), size=1)
AMFR_08<-extract_recovery(AMFR_08)


AMFR<- AM %>% mutate(RI = case_when(
  Date> "2023-12-12" & Date<"2024-01-11"~ 2))
AMFR<-filter(AMFR, RI==2)
# ggplot(AMFR, aes(Date))+
#   geom_line(aes(y=DO), size=1)

AMFR_0124<-recovery_calc(AMFR)
# ggplot(AMFR_0124, aes(count))+
#       geom_line(aes(y=ERmean, color='ER'), size=1)+
#       geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#       geom_line(aes(y=DOmean, color='DO'), size=1)
AMFR_0124<-extract_recovery(AMFR_0124)

AM_tbl<-rbind(AMFR_02,AMFR_06,AMFR_08)
AM_tbl$ID<-'AM'
AM_tbl$num<-5
AM_tbl$IF <- c("rev","bo",'bo')

####OS####
OSFR<- OS %>% mutate(RI = case_when(
  Date> "2022-08-10" & Date<"2022-10-18"~ 2))
OSFR<-filter(OSFR, RI==2)
ggplot(OSFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

OSFR_08<-recovery_calc(OSFR)
# ggplot(OSFR_08, aes(count))+
#   geom_line(aes(y=ERmean, color='ER'), size=1)+
#   geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#   geom_line(aes(y=DOmean, color='DO'), size=1)
OSFR_08<-extract_recovery(OSFR_08)



OSFR<- OS %>% mutate(RI = case_when(
  Date> "2023-12-18" & Date<"2024-02-05"~ 2))
OSFR<-filter(OSFR, RI==2)
# ggplot(OSFR, aes(Date))+
#   geom_line(aes(y=DO), size=1)

OSFR_12<-recovery_calc(OSFR)

OSFR_12<-filter(OSFR_12, count<35)
# ggplot(OSFR_12, aes(count))+
#   geom_line(aes(y=ERmean, color='ER'), size=1)+
#   geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#   geom_line(aes(y=DOmean, color='DO'), size=1)
OSFR_12<-extract_recovery(OSFR_12)

OS_tbl<-rbind(OSFR_08,OSFR_12)
OS_tbl$ID<-'OS'
OS_tbl$num<-4
OS_tbl$IF <- c('bo','rev')

####LF####
LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-02-05" & Date<"2023-04-01"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

LFFR_02<-recovery_calc(LFFR)

LFFR_02<-filter(LFFR_02, count>40)
ggplot(LFFR_02, aes(count))+
  # geom_line(aes(y=ERmean, color='ER'), size=1)+
  # geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=hmean, color='DO'), size=1)

LFFR_02<-extract_recovery(LFFR_02)


LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-06-15" & Date<"2023-08-10"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

LFFR_06<-recovery_calc(LFFR)

# ggplot(LFFR_06, aes(count))+
#   geom_line(aes(y=ERmean, color='ER'), size=1)+
#   geom_line(aes(y=GPPmean, color='GPP'), size=1)
  #geom_line(aes(y=hmean, color='h'), size=1)
LFFR_06<-extract_recovery(LFFR_06)

LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-08-21" & Date<"2023-10-12"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

LFFR_08<-recovery_calc(LFFR)

LFFR_08<-filter(LFFR_08, count>50)
# ggplot(LFFR_08, aes(count))+
#   # geom_line(aes(y=ERmean, color='ER'), size=1)+
#   # geom_line(aes(y=GPPmean, color='GPP'), size=1)+
#   geom_line(aes(y=hmean, color='h'), size=1)
LFFR_08<-extract_recovery(LFFR_08)


LFFR<- LF %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-02-05"~ 2))
LFFR<-filter(LFFR, RI==2)
ggplot(LFFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

LFFR_0124<-recovery_calc(LFFR)
LFFR_0124<-filter(LFFR_0124, count>30)
ggplot(LFFR_0124, aes(count))+
  geom_line(aes(y=ERmean), size=1)+
  geom_line(aes(y=GPPmean), size=1)

LFFR_0124<-extract_recovery(LFFR_0124)


LF_tbl<-rbind(LFFR_02,LFFR_06,LFFR_08)
LF_tbl$ID<-'LF'
LF_tbl$num<-2
LF_tbl$IF <- c("h","h",'h')


####GB####
GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-07-10" & Date<"2023-8-21"~ 2))
GBFR<-filter(GBFR, RI==2)
ggplot(GBFR, aes(Date))+
  geom_line(aes(y=DO), size=1)

GBFR_08<-recovery_calc(GBFR)

ggplot(GBFR_08, aes(count))+
  geom_line(aes(y=ERmean), size=1)+
  geom_line(aes(y=GPPmean), size=1)+
  geom_line(aes(y=DOmean), size=1)

GBFR_08<-extract_recovery(GBFR_08)


GBFR<- GB %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-02-11"~ 2))
GBFR<-filter(GBFR, RI==2)
ggplot(GBFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

GBFR_12<-recovery_calc(GBFR)

ggplot(GBFR_12, aes(count))+
  geom_line(aes(y=ERmean), size=1)+
  geom_line(aes(y=GPPmean), size=1)+
  geom_line(aes(y=DOmean), size=1)
GBFR_12<-extract_recovery(GBFR_12)

GBFR<- GB %>% mutate(RI = case_when(
  Date> "2022-08-01" & Date<"2022-10-20"~ 2))
GBFR<-filter(GBFR, RI==2)
ggplot(GBFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

GBFR_0822<-recovery_calc(GBFR)

ggplot(GBFR_0822, aes(count))+
  geom_line(aes(y=ERmean), size=1)+
  geom_line(aes(y=GPPmean), size=1)+
  geom_line(aes(y=DOmean), size=1)
GBFR_0822<-extract_recovery(GBFR_0822)


GB_tbl<-rbind(GBFR_0822,GBFR_08,GBFR_12)
GB_tbl$ID<-'GB'
GB_tbl$num<-3
GB_tbl$IF <- c("h","h",'rev')


####ID####
IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-02-10" & Date<"2023-03-15"~ 2))
IDFR<-filter(IDFR, RI==2)
ggplot(IDFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

IDFR_02<-recovery_calc(IDFR)

IDFR_02<-filter(IDFR_02, count>23)
ggplot(IDFR_02, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)

IDFR_02<-extract_recovery(IDFR_02)


IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-08-01" & Date<"2023-10-31"~ 2))
IDFR<-filter(IDFR, RI==2)
ggplot(IDFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

IDFR_0822<-recovery_calc(IDFR)

ggplot(IDFR_0822, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)
IDFR_0822<-extract_recovery(IDFR_0822)



IDFR<- ID %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-02-05"~ 2))
IDFR<-filter(IDFR, RI==2)
ggplot(IDFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

IDFR_0124<-recovery_calc(IDFR)

ggplot(IDFR_0124, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)
IDFR_0124<-extract_recovery(IDFR_0124)



ID_tbl<-rbind(IDFR_0822,IDFR_02,IDFR_0124)
ID_tbl$ID<-'ID'
ID_tbl$num<-1
ID_tbl$IF <- c("h","h","h")


####IU####
IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-01-01" & Date<"2023-05-01"~ 2))
IUFR<-filter(IUFR, RI==2)
ggplot(IUFR, aes(Date))+
  geom_line(aes(y=depth), size=1)

IUFR_02<-recovery_calc(IUFR)

ggplot(IUFR_02, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)

IUFR_02<-extract_recovery(IUFR_02)


IUFR<- IU %>% mutate(RI = case_when(
  Date> "2022-08-20" & Date<"2022-10-20"~ 2))
IUFR<-filter(IUFR, RI==2)
ggplot(IUFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

IUFR_0822<-recovery_calc(IUFR)
IUFR_0822<-filter(IUFR_0822, count> 40)
ggplot(IUFR_0822, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)
IUFR_0822<-extract_recovery(IUFR_0822)



IUFR<- IU %>% mutate(RI = case_when(
  Date> "2023-12-01" & Date<"2024-01-31"~ 2))
IUFR<-filter(IUFR, RI==2)
ggplot(IUFR, aes(Date))+
  geom_line(aes(y=ER), size=1)

IUFR_0124<-recovery_calc(IUFR)

ggplot(IUFR_0124, aes(count))+
  geom_line(aes(y=ERmean, color='ER'), size=1)+
  geom_line(aes(y=GPPmean, color='GPP'), size=1)+
  geom_line(aes(y=DOmean, color='DO'), size=1)
IUFR_0124<-extract_recovery(IUFR_0124)



IU_tbl<-rbind(IUFR_0822,IUFR_02,IUFR_0124)
IU_tbl$ID<-'IU'
IU_tbl$num<-1
IU_tbl$IF <- c("h","h","h")

###compile####
recov<-rbind(IU_tbl, ID_tbl, LF_tbl, GB_tbl, AM_tbl, OS_tbl)
recov$GPP_recov[recov$GPP_recov<0] <- NA
recov$ER_recov[recov$ER_recov<0] <- NA
recov$H_recov[recov$H_recov<0] <- NA


recov$GPP_ratio<-recov$H_recov/recov$GPP_recov
recov$ER_ratio<-recov$H_recov/recov$ER_recov
recov$GPP_ratio[recov$GPP_ratio>10] <- NA
recov$ER_ratio[recov$ER_ratio>10] <- NA

write_csv(recov, "04_Outputs/recovery_analysis.csv")

recov<-read_csv("04_Outputs/recovery_analysis.csv")
recov$a<-'a'
cols<-c(
  "h"="deepskyblue3",
  "bo"="burlywood4",
  "rev"="black")
recov$IF<- factor(recov$IF, levels=c("h","bo","rev"))

h<-expression(paste( h[i]-h[min]~(Δh)))
hdiff<-('h'~Delta)
y<-expression(paste( (Stage[Recovery])/(Metabolic[Recovery])))


theme_sam<-theme()+    theme(axis.text.x = element_text(size = 27, angle=0),
                             axis.text.y = element_text(size = 27, angle=0),
                             axis.title.x = element_text(size = 27, angle=0),
                             legend.position = "none",
                             legend.text= element_text(size = 27),
                             panel.background = element_rect(fill = 'white'),
                             panel.grid.major = element_line(color = 'white'),
                             panel.grid.minor = element_line(color = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



(f<-ggplot(recov, aes(h_diff, shape=ID, color=IF))+
    geom_point(aes(y=GPP_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("GPP Recovery")+
    xlab(hdiff)+
    ylab(y)+scale_y_continuous(trans='log10')+theme_sam+
    theme(
          axis.title.y =element_text(size = 25, color='darkgreen'),
          plot.title = element_text(size = 27, color='darkgreen')))

(g<-ggplot(recov, aes(h_diff, shape=ID, color=IF))+
    geom_point(aes(y=ER_ratio), size=6)+
    geom_hline(yintercept = 1, linetype='dashed')+
    scale_colour_manual(name="", values = cols,
                        labels=c("High Stage Event", "Brownout","Flow Reversal"))+
    ggtitle("ER Recovery")+
    xlab(hdiff)+
    ylab(y)+scale_y_continuous(trans='log10')+theme_sam+
    theme(
      axis.title.y =element_text(size = 25, color='darkred'),
      plot.title = element_text(size = 27, color='darkred')))

both<-plot_grid(f,g, ncol=2)
ggsave(filename="recovery.jpeg",
       plot = both,
       width =12,
       height = 6,
       units = "in")


