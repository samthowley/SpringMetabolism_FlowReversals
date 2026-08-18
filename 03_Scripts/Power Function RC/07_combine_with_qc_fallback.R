# Combines two-station and one-station results using a QC-based fallback
# rule instead of the depth-threshold rule in the original "combine one and
# two station results.R": use two-station on any day it produced a QC-passing
# estimate (already the definition of two.station.results_M*.csv -- reach
# test passed, day/night resolved, GPP<=34/ER>=-34); use one-station on every
# other day, regardless of depth. See chat: the AM/LF mid-depth failures are
# not bad data -- change.DO.flux traces a normal diel curve, but K.flux
# (modeled reaeration) isn't large enough there to push the pre-dawn residual
# negative, so two-station can't resolve those days. One-station doesn't
# share that failure mode (its day/night split uses solar time, not flux
# sign), so it's a legitimate fallback, not a way of hiding bad data.

library(tidyverse)

outdir <- "04_Outputs/Power Function RC"
sites <- c("AM", "GB", "ID", "LF")
methods <- c("M6_breakpoint_stat", "M7_breakpoint_domain")

depth_daily <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  filter(ID %in% sites) %>%
  group_by(ID, date) %>%
  summarise(depth = mean(depth, na.rm = TRUE), .groups = "drop")

one_station <- read_csv("04_Outputs/one.station.metabolism.csv", show_col_types = FALSE) %>%
  filter(ID %in% sites) %>%
  distinct(ID, date, .keep_all = TRUE) %>%
  select(ID, date, GPP1 = GPP, ER1 = ER)

for (m in methods) {
  two <- read_csv(file.path(outdir, paste0("two.station.results_", m, ".csv")), show_col_types = FALSE) %>%
    distinct(ID, date, .keep_all = TRUE) %>%
    select(ID, date, GPP2 = GPP, ER2 = ER)

  combined <- depth_daily %>%
    left_join(two, by = c("ID", "date")) %>%
    left_join(one_station, by = c("ID", "date")) %>%
    mutate(
      source = case_when(
        !is.na(GPP2) & !is.na(ER2) ~ "two-station",
        !is.na(GPP1) & !is.na(ER1) ~ "one-station",
        TRUE ~ NA_character_
      ),
      GPP = coalesce(GPP2, GPP1),
      ER = coalesce(ER2, ER1)
    ) %>%
    filter(!is.na(source)) %>%
    select(ID, date, depth, source, GPP, ER)

  out_path <- file.path(outdir, paste0("combined_metabolism_", m, ".csv"))
  write_csv(combined, out_path)

  cat("\n===", m, "===\n")
  cat("n days:", nrow(combined), " (two-station:", sum(combined$source == "two-station"),
      ", one-station:", sum(combined$source == "one-station"), ")\n")
  print(combined %>% count(ID, source) %>% pivot_wider(names_from = source, values_from = n, values_fill = 0))
}

cat("\nDone -> combined_metabolism_M6_breakpoint_stat.csv, combined_metabolism_M7_breakpoint_domain.csv\n")

# ---- plot: continuous ER/GPP vs depth, colored by source, both methods -----
plot_data <- map_dfr(methods, function(m) {
  read_csv(file.path(outdir, paste0("combined_metabolism_", m, ".csv")), show_col_types = FALSE) %>%
    mutate(method = m)
}) %>%
  mutate(method_label = recode(method,
                                M6_breakpoint_stat = "M6 breakpoint (statistical)",
                                M7_breakpoint_domain = "M7 breakpoint (max sampled depth)")) %>%
  pivot_longer(c(GPP, ER), names_to = "flux", values_to = "value")

for (s in sites) {
  p <- plot_data %>% filter(ID == s) %>%
    ggplot(aes(x = depth, y = value, color = source)) +
    geom_point(size = 1.1, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "grey70") +
    facet_grid(flux ~ method_label, scales = "free_y") +
    scale_color_manual(values = c("two-station" = "#1b7837", "one-station" = "#762a83")) +
    labs(title = paste0(s, ": combined metabolism with QC-based fallback (gap-free by construction)"),
         subtitle = "Every day now has an estimate: two-station where it passed its own QC, one-station otherwise",
         x = "depth (m)", y = NULL, color = NULL) +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(outdir, paste0("figures/10_qc_fallback_", s, ".png")), p, width = 8, height = 6.5, dpi = 150)
}

cat("- per-site figures: figures/10_qc_fallback_<ID>.png\n")
