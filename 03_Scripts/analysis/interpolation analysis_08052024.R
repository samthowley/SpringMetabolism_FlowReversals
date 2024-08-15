
rm(list=ls())
##packages######
library(tidyverse)
library(readxl)
library(nls2)
library(lme4)
library(segmented)

#functions####
interp_brk.pnts <- function(site, brks) { 
  
  GPPfit<-lm(GPP~ depth, data=site)
  seg_GPPfit <- segmented(GPPfit, seg.Z = ~depth, npsi = brks)
  GPPbp<-seg_GPPfit$psi[, 2] #extact break points
  
  segmented_model <- segmented(GPPfit, seg.Z = ~depth, psi = c(GPPbp[1][[1]], GPPbp[2][[1]]))
  (GPPslopes<-slope(segmented_model)) #extract slope for segments
  (GPPintercepts<-intercept(segmented_model)) #extract intercepts for segments
  
  site<- site %>% mutate(brk.pnt=case_when(depth<=GPPbp[1][[1]]~'1',depth>=GPPbp[2][[1]]~'3'))
  site$brk.pnt[is.na(site$brk.pnt)]<-'2'
  
  site<- site %>% mutate(fitted_GPP=case_when(brk.pnt=='1'~depth*GPPslopes$depth[1]+GPPintercepts$depth[1],
                                              brk.pnt=='2'~depth*GPPslopes$depth[2]+GPPintercepts$depth[2],
                                              brk.pnt=='3'~depth*GPPslopes$depth[3]+GPPintercepts$depth[3]))
  
  
  
  ERfit<-lm(ER~ depth, data=site)
  seg_ERfit <- segmented(ERfit, seg.Z = ~depth, npsi = brks)
  ERbp<-seg_ERfit$psi[, 2] #extact break points
  
  
  segmented_model <- segmented(ERfit, seg.Z = ~depth, psi = c(ERbp[1][[1]], ERbp[2][[1]]))
  (ERslopes<-slope(segmented_model)) #extract slope for segments
  (ERintercepts<-intercept(segmented_model)) #extract intercepts for segments
  
  site<- site %>% mutate(brk.pnt=case_when(depth<=ERbp[1][[1]]~'1',depth>=ERbp[2][[1]]~'3'))
  site$brk.pnt[is.na(site$brk.pnt)]<-'2'
  
  site<- site %>% mutate(fitted_ER=case_when(brk.pnt=='1'~depth*ERslopes$depth[1]+ERintercepts$depth[1],
                                             brk.pnt=='2'~depth*ERslopes$depth[2]+ERintercepts$depth[2],
                                             brk.pnt=='3'~depth*ERslopes$depth[3]+ERintercepts$depth[3]))
  

  return(site)}

#constants######
theme_set(theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_text(size = 24, color = "black"),
                             axis.title.x =element_text(size = 24),
                             plot.title = element_text(size = 24),
                             legend.position = "none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black")))
#get data####
metabolism<-read_csv('02_Clean_data/master_metabolism4.csv')
master<-metabolism %>%select(ER, GPP, depth, Date, ID) %>%rename('day'='Date')  


LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")

u<-rbind(LF_rC,AM_rC,GB_rC,OS_rC,ID_rC)

rC <- lmList(depth ~ u | ID, data=u)
(cf <- coef(rC))

master <- master %>%
  mutate(u= case_when(
    ID== 'AM'~ (10^cf[1,1]) *depth^(cf[1,2]),
    ID== 'GB'~ (10^cf[2,1]) *depth^(cf[2,2]),
    ID== 'ID'~ (10^cf[3,1]) *depth^(cf[3,2]),
    ID== 'LF'~ (10^cf[4,1]) *depth^(cf[4,2]),
    ID== 'OS'~ (10^cf[5,1]) *depth^(cf[5,2])))

sites<-split(master,master$ID)
AM<-sites[[1]]
GB<-sites[[2]]
ID<-sites[[3]]
IU<-sites[[4]]
LF<-sites[[5]]
OS<-sites[[6]]

###############
#GB nls#########

model_GPP <- nls2(
  GPP ~ log(u)+(depth)^(-c),
  data = GB,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10),
    d = seq(0, 3, length.out = 10)),
  algorithm = "brute-force")

#coef(model_1var)
GB$predicted_GPP <- predict(model_GPP, newdata = GB)
ggplot(data=GB, aes(x=depth))+geom_point(aes(y=GPP),size=1)+
  geom_point(aes(y=predicted_GPP),size=1, color='green')


#terrible
model_ER <- nls2(
  ER ~ (log(u)*(depth)^(-a))-b,
  data = GB,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10),
    d = seq(0, 3, length.out = 10)),
  algorithm = "brute-force")

#coef(model_1var)
GB$predicted_ER <- predict(model_ER, newdata = GB)
ggplot(data=GB, aes(x=depth))+geom_point(aes(y=ER),size=1)+
  geom_point(aes(y=predicted_ER-13),size=1, color='green')


#OS nls#########

model_GPP <- nls2(
  GPP ~ log(u)+(depth)^(-c),
  data = OS,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10),
    d = seq(0, 3, length.out = 10)),
  algorithm = "brute-force")

#coef(model_1var)
OS$predicted_GPP <- predict(model_GPP, newdata = OS)
ggplot(data=OS, aes(x=depth))+geom_point(aes(y=GPP),size=1)+
  geom_point(aes(y=predicted_GPP),size=1, color='green')



model_ER <- nls2(
  ER ~ (log(u)*(depth)^(-a))-b,
  data = OS,
  start = expand.grid(
    a = seq(0, 15, length.out = 10),
    b = seq(0, 1, length.out = 10),
    c = seq(0, 5, length.out = 10),
    d = seq(0, 3, length.out = 10)),
  algorithm = "brute-force")

#coef(model_1var)
OS$predicted_ER <- predict(model_ER, newdata = OS)
ggplot(data=OS, aes(x=depth))+geom_point(aes(y=ER),size=1)+
  geom_point(aes(y=predicted_ER-13),size=1, color='green')

#bp########

ID<-interp_brk.pnts(ID, 2) #good

IU<-interp_brk.pnts(IU, 2) #ER looks good 

GB<-interp_brk.pnts(GB, 3) #LOOKS TERRIBLE

AM<-interp_brk.pnts(AM, 2) #ER looks good

OS<-interp_brk.pnts(OS, 2) #GPP looks good

LF<-interp_brk.pnts(LF, 2) #decent


ggplot(data=OS, aes(x=depth)) +
    geom_point(aes(y=GPP),size=1, color='green')+geom_point(aes(y=ER),size=1, color='red')+
  geom_line(aes(y=fitted_GPP))+geom_line(aes(y=fitted_ER))
  
#GB bp##########

GPPfit<-lm(GPP~ depth, data=GB)
seg_GPPfit <- segmented(GPPfit, seg.Z = ~depth, npsi = 3)
GPPbp<-seg_GPPfit$psi[, 2] #extact break points

segmented_model <- segmented(GPPfit, seg.Z = ~depth, psi = c(GPPbp[1][[1]], GPPbp[2][[1]], GPPbp[3][[1]]))
(GPPslopes<-slope(segmented_model)) #extract slope for segments
(GPPintercepts<-intercept(segmented_model)) #extract intercepts for segments

GB<- GB %>% mutate(brk.pnt=case_when(depth<=GPPbp[1][[1]]~'1',
                                     depth>=GPPbp[1][[1]] & depth<=GPPbp[2][[1]] ~'2',
                                     depth>=GPPbp[2][[1]] & depth<=GPPbp[3][[1]]~'3',
                                     depth>=GPPbp[4][[1]]~4))

GB<- GB %>% mutate(fitted_GPP=case_when(brk.pnt=='1'~depth*GPPslopes$depth[1]+GPPintercepts$depth[1],
                                        brk.pnt=='2'~depth*GPPslopes$depth[2]+GPPintercepts$depth[2],
                                        brk.pnt=='3'~depth*GPPslopes$depth[3]+GPPintercepts$depth[3]))



ERfit<-lm(ER~ depth, data=GB)
seg_ERfit <- segmented(ERfit, seg.Z = ~depth, npsi = brks)
ERbp<-seg_ERfit$psi[, 2] #extact break points


segmented_model <- segmented(ERfit, seg.Z = ~depth, psi = c(ERbp[1][[1]], ERbp[2][[1]]))
(ERslopes<-slope(segmented_model)) #extract slope for segments
(ERintercepts<-intercept(segmented_model)) #extract intercepts for segments

GB<- GB %>% mutate(brk.pnt=case_when(depth<=ERbp[1][[1]]~'1',depth>=ERbp[2][[1]]~'3'))
GB$brk.pnt[is.na(GB$brk.pnt)]<-'2'

GB<- GB %>% mutate(fitted_ER=case_when(brk.pnt=='1'~depth*ERslopes$depth[1]+ERintercepts$depth[1],
                                       brk.pnt=='2'~depth*ERslopes$depth[2]+ERintercepts$depth[2],
                                       brk.pnt=='3'~depth*ERslopes$depth[3]+ERintercepts$depth[3]))



