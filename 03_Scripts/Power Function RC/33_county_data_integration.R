rm(list=ls())
library(tidyverse)
library(readxl)

# County data integration (01_Raw_data/County Data/):
#  1. VentDO comparison: county WQ files (DO_mg/L) vs our own VentDO.csv, for
#     GB and LF. NOTE: no AM WQ file exists in this folder -- only
#     AM.Vent_Flow.xlsx (Discharge only, no DO column) -- so AM can't be
#     compared here despite being requested; flagging rather than fabricating.
#  2. GB discharge from county gauge (GB_Flow.xlsx = same file as
#     01_Raw_data/02322355_Flow.xlsx, confirmed byte-identical), CFS -> m3/s.
#  3. Depth and discharge comparison ggplots across sites.
#
# County exports store Date as an Excel serial number (e.g. 33100.47) --
# converted with the standard Excel epoch (1899-12-30).

outdir <- "04_Outputs/Power Function RC"
excel_date <- function(x) as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC")

## ==================== 1. VentDO comparison (GB, LF) =========================
# DO_mg/L has a slash in the name -- read raw and rename by position instead
read_wq_do <- function(f, id) {
  raw <- read_excel(file.path("01_Raw_data/County Data", f), skip = 13, col_names = FALSE)
  hdr <- as.character(unlist(raw[1, ]))
  d <- raw[-1, ]
  names(d) <- make.unique(hdr)  # many repeated "Code" columns, one per parameter
  d %>%
    transmute(ID = id,
              Date = excel_date(as.numeric(Date)),
              Water_Temp_C = as.numeric(Water_Temp_C),
              Depth_m = as.numeric(`Depth_Of_Collection-m`),
              DO_mg_L = as.numeric(`DO_mg/L`)) %>%
    filter(!is.na(DO_mg_L))
}

gb_wq <- read_wq_do("GB.Vent_WQ.xlsx", "GB")
lf_wq <- read_wq_do("LF.Vent_WQ.xlsx", "LF")
county_do <- bind_rows(gb_wq, lf_wq)

cat("=== County DO_mg/L (GB, LF) summary ===\n")
print(county_do %>% group_by(ID) %>% summarise(n=n(), min=round(min(DO_mg_L),2), median=round(median(DO_mg_L),2), max=round(max(DO_mg_L),2),
                                                  date_min=min(Date), date_max=max(Date)))

our_ventdo <- read_csv("02_Clean_data/Chem/VentDO.csv", show_col_types = FALSE) %>%
  filter(ID %in% c("GB","LF"), VentDO >= 0)

cat("\n=== Our VentDO.csv (GB, LF) summary ===\n")
print(our_ventdo %>% group_by(ID) %>% summarise(n=n(), min=round(min(VentDO),2), median=round(median(VentDO),2), max=round(max(VentDO),2)))

cat("\n*** NOTE: no AM WQ/DO file found in County Data -- only AM.Vent_Flow.xlsx exists (Discharge only). AM not compared. ***\n")

p_do <- ggplot() +
  geom_point(data = our_ventdo %>% mutate(source="ours"), aes(x=Date, y=VentDO, color=source), alpha=0.6) +
  geom_point(data = county_do %>% mutate(source="county") %>% rename(VentDO=DO_mg_L), aes(x=Date, y=VentDO, color=source), alpha=0.6) +
  scale_color_manual(values=c(ours="black", county="#d95f02")) +
  facet_wrap(~ID, scales="free") +
  labs(title="VentDO: ours vs. county WQ data", y="DO (mg/L)") +
  theme_bw(base_size=12)
ggsave(file.path(outdir, "figures", "33_ventdo_ours_vs_county.png"), p_do, width=10, height=5, dpi=150)

write_csv(county_do, file.path(outdir, "33_county_ventdo.csv"))

## ==================== 2. GB discharge from county gauge =====================
cfs_to_m3s <- 0.0283168
cfs_to_m3day <- cfs_to_m3s * 86400

gb_flow_raw <- read_excel("01_Raw_data/County Data/GB_Flow.xlsx", skip = 25, col_names = FALSE)
hdr <- as.character(unlist(gb_flow_raw[1, ]))
gb_flow <- gb_flow_raw[-1, ]
names(gb_flow) <- hdr
gb_flow <- gb_flow %>%
  transmute(ID = "GB",
            Date = excel_date(as.numeric(Date)),
            discharge_cfs = as.numeric(Discharge),
            discharge_m3s = discharge_cfs * cfs_to_m3s,
            discharge_m3day = discharge_cfs * cfs_to_m3day) %>%
  filter(!is.na(discharge_cfs), discharge_cfs > 0)

cat("\n=== GB county discharge, converted (parameter code 00060 = CFS, standard USGS convention) ===\n")
print(gb_flow %>% summarise(n=n(), cfs_min=round(min(discharge_cfs),1), cfs_median=round(median(discharge_cfs),1), cfs_max=round(max(discharge_cfs),1),
                              m3s_min=round(min(discharge_m3s),3), m3s_median=round(median(discharge_m3s),3), m3s_max=round(max(discharge_m3s),3),
                              m3day_min=round(min(discharge_m3day)), m3day_median=round(median(discharge_m3day)), m3day_max=round(max(discharge_m3day))))

write_csv(gb_flow, file.path(outdir, "33_GB_county_discharge_m3s.csv"))

## ==================== 3. Depth & discharge comparison across sites ==========
depth_all <- read_csv("02_Clean_data/Chem/depth.csv", show_col_types = FALSE) %>%
  filter(ID %in% c("AM","GB","ID","LF"))

p_depth <- ggplot(depth_all, aes(x=Date, y=depth)) +
  geom_line() +
  facet_wrap(~ID, scales="free_y") +
  labs(title="Depth by site") +
  theme_bw(base_size=12)
ggsave(file.path(outdir, "figures", "33_depth_by_site.png"), p_depth, width=11, height=7, dpi=150)

# discharge: use the depth-discharge RC (38_discharge_RC.csv) for AM/ID/LF, county-derived for GB
# (was 31_discharge_direct_RC.csv, removed -- superseded by script 38's combined ours+county 2-segment fit)
direct_discharge <- read_csv(file.path(outdir, "38_discharge_RC.csv"), show_col_types = FALSE) %>%
  select(Date, ID, discharge)
gb_for_plot <- gb_flow %>% select(Date, ID, discharge = discharge_m3day)
discharge_all <- bind_rows(direct_discharge %>% filter(ID != "GB"), gb_for_plot)

p_discharge <- ggplot(discharge_all, aes(x=Date, y=discharge)) +
  geom_point(size=0.8, alpha=0.5) +
  facet_wrap(~ID, scales="free_y") +
  labs(title="Discharge by site (m3/day) -- GB from county gauge, others from direct depth fit") +
  theme_bw(base_size=12)
ggsave(file.path(outdir, "figures", "33_discharge_by_site.png"), p_discharge, width=11, height=7, dpi=150)

cat("\nWrote figures/33_ventdo_ours_vs_county.png, figures/33_depth_by_site.png, figures/33_discharge_by_site.png\n")
cat("Wrote 33_county_ventdo.csv, 33_GB_county_discharge_m3s.csv\n")
