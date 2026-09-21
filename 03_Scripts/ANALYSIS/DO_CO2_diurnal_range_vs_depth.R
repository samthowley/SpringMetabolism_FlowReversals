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

##---- daily diurnal range (max - min) + daily mean depth ----
chem_hourly <- read_csv("02_Clean_data/master_chem1.csv", show_col_types = FALSE) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"))

diurnal <- chem_hourly %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(ID, Date) %>%
  summarise(DO.diurnal  = if (all(is.na(DO)))  NA_real_ else max(DO,  na.rm = TRUE) - min(DO,  na.rm = TRUE),
            CO2.diurnal = if (all(is.na(CO2))) NA_real_ else max(CO2, na.rm = TRUE) - min(CO2, na.rm = TRUE),
            depth       = mean(depth, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(ID = factor(ID, levels = c("IU", "ID", "GB", "LF", "AM", "OS")))

##---- DO diurnal range vs depth ----
do_diurnal_plot <- ggplot(diurnal, aes(x = depth, y = DO.diurnal, color = ID)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Depth (m)", y = expression(Diurnal~DO~range~(mg~L^-1))) +
  theme_spring()
do_diurnal_plot

##---- CO2 diurnal range vs depth ----
co2_diurnal_plot <- ggplot(diurnal, aes(x = depth, y = CO2.diurnal, color = ID)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = site_colors) +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Depth (m)", y = expression(Diurnal~CO[2]~range~(ppm))) +
  theme_spring()
co2_diurnal_plot


plot_grid(do_diurnal_plot, co2_diurnal_plot, ncol = 2)
