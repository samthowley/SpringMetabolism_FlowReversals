
rm(list=ls())
##packages######
library(tidyverse)
library(readxl)
library(lme4)
library(dataRetrieval)

#functions####
interp_2brk.pnts <- function(site, brks) { 
  
  GPPfit<-lm(GPP~ depth, data=site)
  seg_GPPfit <- segmented(GPPfit, seg.Z = ~depth, npsi = brks)
  GPPbp<-seg_GPPfit$psi[, 2] #extact break points
  
  segmented_model <- segmented(GPPfit, seg.Z = ~depth, psi = c(GPPbp[1][[1]], GPPbp[2][[1]]))
  (GPPslopes<-slope(segmented_model)) #extract slope for segments
  (GPPintercepts<-intercept(segmented_model)) #extract intercepts for segments
  
  site<- site %>% mutate(brk.pnt.gpp=case_when(depth<=GPPbp[1][[1]]~'1',depth>=GPPbp[2][[1]]~'3'))
  site$brk.pnt.gpp[is.na(site$brk.pnt.gpp)]<-'2'
  
  site<- site %>% mutate(fitted_GPP=case_when(brk.pnt.gpp=='1'~depth*GPPslopes$depth[1]+GPPintercepts$depth[1],
                                              brk.pnt.gpp=='2'~depth*GPPslopes$depth[2]+GPPintercepts$depth[2],
                                              brk.pnt.gpp=='3'~depth*GPPslopes$depth[3]+GPPintercepts$depth[3]))
  
  
  
  ERfit<-lm(ER~ depth, data=site)
  seg_ERfit <- segmented(ERfit, seg.Z = ~depth, npsi = brks)
  ERbp<-seg_ERfit$psi[, 2] #extact break points
  
  
  segmented_model <- segmented(ERfit, seg.Z = ~depth, psi = c(ERbp[1][[1]], ERbp[2][[1]]))
  (ERslopes<-slope(segmented_model)) #extract slope for segments
  (ERintercepts<-intercept(segmented_model)) #extract intercepts for segments
  
  site<- site %>% mutate(brk.pnt.er=case_when(depth<=ERbp[1][[1]]~'1',depth>=ERbp[2][[1]]~'3'))
  site$brk.pnt.er[is.na(site$brk.pnt.er)]<-'2'
  
  site<- site %>% mutate(fitted_ER=case_when(brk.pnt.er=='1'~depth*ERslopes$depth[1]+ERintercepts$depth[1],
                                             brk.pnt.er=='2'~depth*ERslopes$depth[2]+ERintercepts$depth[2],
                                             brk.pnt.er=='3'~depth*ERslopes$depth[3]+ERintercepts$depth[3]))
  

  return(site)}
interp_1brk.pnts <- function(site) { 
  
  GPPfit<-lm(GPP~ depth, data=site)
  seg_GPPfit <- segmented(GPPfit, seg.Z = ~depth, npsi = 1)
  GPPbp<-seg_GPPfit$psi[, 2] #extact break points
  
  segmented_model <- segmented(GPPfit, seg.Z = ~depth, psi = c(GPPbp[1][[1]]))
  (GPPslopes<-slope(segmented_model)) #extract slope for segments
  (GPPintercepts<-intercept(segmented_model)) #extract intGPPcepts for segments
  
  site<- site %>% mutate(brk.pnt.gpp=case_when(depth<=GPPbp[1][[1]]~'1',depth>GPPbp[1][[1]]~'2'))
  
  site<- site %>% mutate(fitted_GPP=case_when(brk.pnt.gpp=='1'~depth*GPPslopes$depth[1]+GPPintercepts$depth[1],
                                              brk.pnt.gpp=='2'~depth*GPPslopes$depth[2]+GPPintercepts$depth[2]))
  
  ERfit<-lm(ER~ depth, data=site)
  seg_ERfit <- segmented(ERfit, seg.Z = ~depth, npsi = 1)
  ERbp<-seg_ERfit$psi[, 2] #extact break points
  
  segmented_model <- segmented(ERfit, seg.Z = ~depth, psi = c(ERbp[1][[1]]))
  (ERslopes<-slope(segmented_model)) #extract slope for segments
  (ERintercepts<-intercept(segmented_model)) #extract intercepts for segments
  
  site<- site %>% mutate(brk.pnt.er=case_when(depth<=ERbp[1][[1]]~'1',depth>ERbp[1][[1]]~'2'))
  
  site<- site %>% mutate(fitted_ER=case_when(brk.pnt.er=='1'~depth*ERslopes$depth[1]+ERintercepts$depth[1],
                                             brk.pnt.er=='2'~depth*ERslopes$depth[2]+ERintercepts$depth[2]))
  
  return(site)}

nls_function <- function(site) { 
  exponential_modelGPP <- nlsLM(GPP ~ a * exp(b * depth)*log(u), data = site, start = list(a = 1, b = 0.1))
  site$exponential_GPP <- predict(exponential_modelGPP, newdata = site)
  
  exponential_modelER <- nlsLM(ER ~ a * exp(b * depth)*log(u), data = site, start = list(a = 1, b = 0.1))
  site$exponential_ER <- predict(exponential_modelER, newdata = site)
  
  # ggplot(data=AM, aes(x=depth)) +
  #   geom_point(aes(y=GPP),size=1, color='green')+
  #   geom_point(aes(y=ER),size=1, color='red')+
  #   geom_line(aes(y=exponential_GPP))+geom_line(aes(y=exponential_ER))
  
  cosine_modelGPP <- nlsLM(GPP ~ a * cos(b * depth + c)*log(u)+ d, data = site, start = list(a = 1, b = 1, c = 0, d = 0))
  site$cosine_GPP <- predict(cosine_modelGPP, newdata = site)
  
  cosine_modelER <- nlsLM(ER ~ a * cos(b * depth + c)*log(u)+ d, data = site,  start = list(a = 1, b = 1, c = 0, d = 0))
  site$cosine_ER <- predict(cosine_modelER, newdata = site)
  
  # ggplot(data=AM, aes(x=depth)) +
  #   geom_point(aes(y=GPP),size=1, color='green')+
  #   geom_point(aes(y=ER),size=1, color='red')+
  #   geom_line(aes(y=cosine_GPP))+geom_line(aes(y=cosine_ER))  
  
  return(site)}

#constants######
theme_set(theme(axis.text.x = element_text(size = 24, angle=0),
                             axis.text.y = element_text(size = 24, angle=0),
                             axis.title.y =element_blank(),
                             axis.title.x =element_text(size = 24),
                             plot.title = element_text(size = 24),
                             legend.position = "bottom",
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


startDate <- "2022-04-17"
endDate <- "2024-07-25"
parameterCd <- c('00060')
ventID<-'02322700'

IU_q<- readNWISuv(ventID,parameterCd, startDate, endDate)
IU_q<-IU_q %>% rename('q_f3.s'='X_00060_00000')%>%mutate(day=as.Date(dateTime)) %>% select(day,q_f3.s)
IU<-left_join(IU, IU_q, by='day')
IU<-IU %>% mutate(u=q_f3.s/(depth*65))%>%select(ER, GPP, depth, day, ID,u)

#nls#########
library("minpack.lm")

IU<-nls_function(IU) 

ID<-nls_function(ID) 

GB<-nls_function(GB) 

AM<-nls_function(AM) 

OS<-nls_function(OS) 

LF<-nls_function(LF) 

#bp########
library(segmented)

ID<-interp_2brk.pnts(ID, 2) #good

IU<-interp_2brk.pnts(IU, 2) #ER looks good 

GB<-interp_2brk.pnts(GB, 2) #LOOKS TERRIBLE

AM<-interp_2brk.pnts(AM, 2) #ER looks good

OS<-interp_2brk.pnts(OS, 2) #GPP looks good

LF<-interp_2brk.pnts(LF, 2) #decent


OS<-interp_1brk.pnts(OS) #GPP looks good

#GB bp####
GPPfit<-lm(GPP~ depth, data=GB)
seg_GPPfit <- segmented(GPPfit, seg.Z = ~depth, npsi = 3)
GPPbp<-seg_GPPfit$psi[, 2] #extact break points

segmented_model <- segmented(GPPfit, seg.Z = ~depth, psi = c(GPPbp[1][[1]], GPPbp[2][[1]], GPPbp[3][[1]]))
(GPPslopes<-slope(segmented_model)) #extract slope for segments
(GPPintercepts<-intercept(segmented_model)) #extract intercepts for segments

GB<- GB %>% mutate(brk.pnt.gpp=case_when(depth<=GPPbp[1][[1]]~'1',
                                         depth>=GPPbp[1][[1]] & depth<=GPPbp[2][[1]] ~'2',
                                         depth>=GPPbp[2][[1]] & depth<=GPPbp[3][[1]]~'3',
                                         depth>=GPPbp[3][[1]]~'4'))

GB<- GB %>% mutate(fitted_GPP=case_when(brk.pnt.gpp=='1'~depth*GPPslopes$depth[1]+GPPintercepts$depth[1],
                                        brk.pnt.gpp=='2'~depth*GPPslopes$depth[2]+GPPintercepts$depth[2],
                                        brk.pnt.gpp=='3'~depth*GPPslopes$depth[3]+GPPintercepts$depth[3],
                                        brk.pnt.gpp=='4'~depth*GPPslopes$depth[4]+GPPintercepts$depth[4]))

ERfit<-lm(ER~ depth, data=GB)
seg_ERfit <- segmented(ERfit, seg.Z = ~depth, npsi = 1)
ERbp<-seg_ERfit$psi[, 2] #extact break points

segmented_model <- segmented(ERfit, seg.Z = ~depth, psi = c(ERbp[1][[1]]))
(ERslopes<-slope(segmented_model)) #extract slope for segments
(ERintercepts<-intercept(segmented_model)) #extract intercepts for segments

GB<- GB %>% mutate(brk.pnt.er=case_when(depth<=ERbp[1][[1]]~'1',depth>ERbp[1][[1]]~'2'))

GB<- GB %>% mutate(fitted_ER=case_when(brk.pnt.er=='1'~depth*ERslopes$depth[1]+ERintercepts$depth[1],
                                       brk.pnt.er=='2'~depth*ERslopes$depth[2]+ERintercepts$depth[2]))


#compile####

compiled_interp<-rbind(IU, ID, GB, AM, LF, OS)
names(OS)

ggplot(OS, aes(x=depth)) + 
  geom_point(aes(y=GPP), color='darkgreen') + geom_point(aes(y=ER), color='darkred') + 
  
  #geom_line(aes(y=exponential_GPP), color='blue', size=1) + geom_line(aes(y=exponential_ER), color='blue', size=1) +
  
  geom_line(aes(y=cosine_GPP), color='blue', size=1) + geom_line(aes(y=cosine_ER), color='blue', size=1) + 
  
  geom_line(aes(y=fitted_GPP), size=1) + geom_line(aes(y=fitted_ER), size=1) + 
  facet_wrap(~ ID, ncol=2)

compiled_interp<-compiled_interp%>% 
  mutate(cosine_diff.er=ER/cosine_ER,
         cosine_diff.gpp=GPP/cosine_GPP,
         brk_diff.er=ER/fitted_ER, 
         brk_diff.gpp=GPP/fitted_GPP)

compiled_interp$brk_diff.gpp[compiled_interp$brk_diff.gpp< -100]<-NA
compiled_interp$cosine_diff.gpp[compiled_interp$cosine_diff.gpp< -10]<-NA
compiled_interp$cosine_diff.gpp[compiled_interp$cosine_diff.gpp> 10]<-NA

ggplot(compiled_interp, aes(x=depth)) + ggtitle('ER/cosine ER')+
  geom_point(aes(y=cosine_diff.er))+facet_wrap(~ ID, ncol=2)


