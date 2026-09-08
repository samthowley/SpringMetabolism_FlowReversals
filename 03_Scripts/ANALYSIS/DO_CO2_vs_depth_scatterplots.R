library(tidyverse)

site_colors <- c(AM = "#E41A1C", GB = "#377EB8", ID = "#4DAF4A",
                 LF = "#984EA3", OS = "#FF7F00", IU = "#A65628")

theme_spring <- function() {
  theme_bw(base_size = 11) +
    theme(
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold"),
      panel.grid.minor  = element_blank(),
      legend.position   = "none"
    )
}

##---- daily means ----
chem_hourly <- read_csv("02_Clean_data/master_chem1.csv", show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

daily <- chem_hourly %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(DO    = mean(DO,    na.rm = TRUE),
            CO2   = mean(CO2,   na.rm = TRUE),
            depth = mean(depth, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(ID = factor(ID, levels = c("IU", "ID", "GB", "LF", "AM", "OS")))

##---- DO vs depth ----
do_depth_plot <- ggplot(daily, aes(x = depth, y = DO, color = ID)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Depth (m)", y = expression(Daily~mean~DO~(mg~L^-1))) +
  theme_spring()+
  ggtitle ('DO Daily Mean~Depth')
do_depth_plot

##---- CO2 vs depth ----
co2_depth_plot <- ggplot(daily, aes(x = depth, y = CO2, color = ID)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Depth (m)", y = expression(Daily~mean~CO[2]~(ppm))) +
  theme_spring()+
  ggtitle ('CO2 Daily Mean~Depth')
co2_depth_plot

library(cowplot)

plot_grid(do_depth_plot, co2_depth_plot, ncol = 2)
