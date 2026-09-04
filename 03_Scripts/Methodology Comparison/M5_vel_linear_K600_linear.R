rm(list = ls())
source("03_Scripts/Methodology Comparison/_engine_two_station.R")

## ---- config -----------------------------------------------------------------
LABEL     <- "M5_velLinear_K600linear"
VEL_FORM  <- "linear"        # "power" | "linear"
K600_SRC  <- "RC"           # "RC" | "freewater"
K600_FORM <- "linear"        # "power" | "linear"  (ignored when freewater)
K600_PRED <- "depth"        # "depth" | "velocity"  -- predictor for the K600 RC
EXCL      <- "base"         # "base" | "strict"     -- point-exclusion severity

recipe <- uniform_recipe(vel_form  = VEL_FORM,
                         k600_src  = K600_SRC,
                         k600_form = K600_FORM,
                         k600_pred = K600_PRED,
                         excl      = EXCL)

## ---- run --------------------------------------------------------------------
res   <- run_two_station(recipe, LABEL)
daily <- res$daily
score <- score_methodology(daily)

## ---- plots ------------------------------------------------------------------
daily %>%
  mutate(ID = factor(ID, levels = sites)) %>%
  filter(ID %in% c('AM', 'LF', 'GB'))%>%
  ggplot(aes(x = date)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = GPP_RANGE[1], ymax = GPP_RANGE[2], fill = "#1b9e77", alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = ER_RANGE[1],  ymax = ER_RANGE[2],  fill = "#d95f02", alpha = 0.08) +
  geom_point(aes(y = GPP, color = "GPP"), size = 0.8) +
  geom_point(aes(y = ER,  color = "ER"),  size = 0.8) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(GPP = "#1b9e77", ER = "#d95f02")) +
  facet_wrap(~ID, scales = "free", ncol = 1) +
  labs(title = LABEL, subtitle = "shaded = plausible range",
       x = NULL, y = "g O2 m-2 d-1") +
  theme_bw(base_size = 10)

library(cowplot)

plot_grid(

daily %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")))%>%
  filter(ID %in% c('AM', 'LF', 'GB'))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = ER, color=reach_test_mode), size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol=2) +
  ggtitle("Velocity Linear, K600 Linear: ER")+
  theme_bw(base_size = 10)+
  theme(legend.position = "bottom") 

,

daily %>%
  mutate(ID = factor(ID, levels = c("AM", "GB", "LF", "ID", "OS")))%>%
  filter(ID %in% c('AM', 'LF', 'GB'))%>%
  ggplot(aes(x = depth)) +
  geom_point(aes(y = GPP, color=reach_test_mode), size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ID, scales = "free", ncol=2) +
  ggtitle("Velocity Linear, K600 Linear: GPP")+
  theme_bw(base_size = 10)+
  theme(legend.position = "bottom"),
ncol=2
)
