####packages and functions######
library(tidyverse)
library(readxl)
library(measurements)
library(cowplot)
library(ggpmisc)

LF_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "LF")
AM_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "AM")
GB_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "GB")
OS_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "OS")
ID_rC<- read_excel("04_Outputs/rC_k600_edited.xlsx",sheet = "ID")

u_theme<- theme()+  theme(axis.text.x = element_text(size = 12, angle=0),
                          axis.text.y = element_text(size = 16, angle=0),
                          axis.title.y =element_text(size = 16),
                          axis.title.x =element_text(size = 16),
                          plot.title = element_text(size = 19),
                          legend.position = "none",
                          panel.background = element_rect(fill = 'white'),
                          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))



ggplot(AM_rC, aes(x=depth, y=u)) + 
  geom_point()+ u_theme
  # stat_correlation(mapping = use_label(c('R')), size=5,
  #                                         label.y = 0.9,label.x = 0.63)+
  # stat_poly_line(formula = y ~ x, color='black') +
  # stat_poly_eq(mapping = use_label(c("eq")),
  #              label.y = 1,
  #              label.x = 0.6,
  #              size=5)


