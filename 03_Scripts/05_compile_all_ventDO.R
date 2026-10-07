
library(readxl)
library(tidyverse)

raw_root <- "01_Raw_data/Hobo/Roving DO"
outdir   <- "04_Outputs/Power Function RC"
sites    <- c("AM", "GB", "LF", "OS")

site_aliases <- list(
  AM = c("allenmill", "allen mill"),
  GB = c("gilchristblue", "gilchrist blue", "gilblue", "gil blue"),
  LF = c("littlefanning", "little fanning", "lilfan"),
  OS = c("otter")
)

norm <- function(x) x %>% tolower() %>% str_replace_all("[ _]", "")

# ---- pick the sheet that belongs to this site, not just sheet 1 -----------
pick_sheet <- function(sheet_names, site_id) {
  if (length(sheet_names) == 1) return(list(sheet = sheet_names[1], note = NA_character_))
  n <- norm(sheet_names)
  target_alias <- norm(site_aliases[[site_id]])
  other_aliases <- norm(unlist(site_aliases[setdiff(sites, site_id)]))

  is_target <- Reduce(`|`, lapply(target_alias, function(a) str_detect(n, fixed(a))), accumulate = FALSE)
  is_other  <- Reduce(`|`, lapply(other_aliases, function(a) str_detect(n, fixed(a))), accumulate = FALSE)

  # exact single-site match, no other-site alias in the same name
  clean_target <- which(is_target & !is_other)
  if (length(clean_target) >= 1) return(list(sheet = sheet_names[clean_target[1]], note = NA_character_))

  # any target match at all (even if combined-name)
  any_target <- which(is_target)
  if (length(any_target) >= 1) return(list(sheet = sheet_names[any_target[1]], note = NA_character_))

  # nothing names this site: avoid a sheet that explicitly names ANOTHER site
  safe <- which(!is_other)
  if (length(safe) >= 1) return(list(sheet = sheet_names[safe[1]], note = "no sheet named for this site; picked first non-conflicting sheet"))

  list(sheet = sheet_names[1], note = "ambiguous: all sheets named for other sites; used sheet 1")
}

# ---- flexible date parsing (formats vary file to file) --------------------
parse_flex_date <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  x <- as.character(x)
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "mdy HMS p", "mdy HM p", "mdy HMS", "mdy HM"),
    tz = "UTC", quiet = TRUE
  ))
}

# ---- read one file, standardizing to Date/DO/Temp -------------------------
read_one <- function(f, site_id) {
  ext <- tolower(tools::file_ext(f))
  sheet_note <- NA_character_

  if (ext %in% c("xlsx", "xls")) {
    sh <- excel_sheets(f)
    choice <- pick_sheet(sh, site_id)
    sheet_note <- choice$note
    df <- suppressMessages(read_excel(f, sheet = choice$sheet))
  } else {
    first_line <- readLines(f, n = 1, warn = FALSE)
    skip_n <- if (str_detect(first_line, regex("plot title", ignore_case = TRUE))) 1 else 0
    df <- suppressMessages(read_csv(f, skip = skip_n, show_col_types = FALSE, col_types = cols(.default = "c")))
  }

  nms <- tolower(names(df))
  date_col <- which(str_detect(nms, "date"))[1]
  do_col   <- which(str_detect(nms, "do conc") | str_detect(nms, "^do$") | str_detect(nms, "^\\.\\.\\.?do"))[1]
  if (is.na(do_col)) do_col <- which(str_detect(nms, "\\bdo\\b"))[1]
  temp_col <- which(str_detect(nms, "temp"))[1]

  if (is.na(date_col) || is.na(do_col) || is.na(temp_col)) {
    # fallback: positional. Skip a leading index/"#" column if present.
    start <- if (nms[1] %in% c("#", "...1") || str_detect(nms[1], "^\\.\\.\\.")) 2 else 1
    date_col <- start; do_col <- start + 1; temp_col <- start + 2
  }

  tibble(
    Date = parse_flex_date(df[[date_col]]),
    DO   = suppressWarnings(as.numeric(df[[do_col]])),
    Temp = suppressWarnings(as.numeric(df[[temp_col]]))
  ) %>%
    filter(!is.na(Date), !is.na(DO)) %>%
    mutate(sheet_note = sheet_note)
}

# ---- extract every file for every site -------------------------------------
extract_all <- map_dfr(sites, function(site_id) {
  files <- list.files(file.path(raw_root, site_id), full.names = TRUE)
  map_dfr(files, function(f) {
    df <- tryCatch(read_one(f, site_id), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return(tibble(ID = site_id, file = basename(f), status = "READ ERROR / NO DATA"))
    }
    n_total <- nrow(df)
    valid <- df %>% filter(DO <= 7)  # discard sensor-out-of-water readings
    if (nrow(valid) == 0) {
      return(tibble(ID = site_id, file = basename(f), status = "ALL READINGS >7, EXCLUDED",
                     visit_date = as.character(as.Date(min(df$Date))), n_total = n_total))
    }
    tibble(ID = site_id, file = basename(f), status = "OK",
           visit_date = as.character(as.Date(min(valid$Date))),
           n_total = n_total, n_used = nrow(valid), n_discarded = n_total - nrow(valid),
           mean_DO = round(mean(valid$DO), 3), sd_DO = round(sd(valid$DO), 3),
           mean_Temp = round(mean(valid$Temp, na.rm = TRUE), 2),
           sheet_note = first(valid$sheet_note))
  })
})

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
write_csv(extract_all, file.path(outdir, "ventdo_all_extraction_log.csv"))

cat("=== Extraction status counts ===\n")
print(extract_all %>% count(ID, status))

# ---- manual corrections (Samantha, verified after reviewing the outlier screen) ----
# Filenames matched against the ORIGINAL folder they were extracted from.
relabel <- tribble(
  ~ID,   ~file,                                        ~correct_ID,
  "GB",  "Roving_GB_DO_01032022.csv",                   "AM",
  "GB",  "ROVING_GB_DO_08152022.xlsx",                  "AM",
  "LF",  "roving_LittleFanning_DO_05172023 (4).csv",    "GB",
  "AM",  "rovingDO_AM_05292023.csv",                    "GB",
  "LF",  "ROVINGDO_LF_08242022 (2).xlsx",                "OS",
  "OS",  "roving_OS_05182023 (3).csv",                  "OS",  # kept as OS despite the flag -- Samantha trusts these are genuinely high OS readings
  "OS",  "roving_OS_05182023 (1).xlsx",                 "OS",
  "LF",  "rovingDO_LF_05302023.csv",                    "OS",
  "LF",  "Roving_LF_03082023.csv",                      "LF"  # kept as LF despite the flag -- mild z, likely natural variability
)
remove_files <- tribble(
  ~ID,  ~file,
  "GB", "rovingDO_GB_AM.csv",
  "AM", "rovingDO_AM.csv"
)

extract_all <- extract_all %>%
  left_join(relabel, by = c("ID", "file")) %>%
  mutate(ID = coalesce(correct_ID, ID)) %>%
  select(-correct_ID) %>%
  anti_join(remove_files, by = c("ID", "file"))  # remove_files' IDs are untouched by the relabel above, so this still matches correctly

# ---- outlier / mislabeling screen ------------------------------------------
# Robust per-site baseline (median + MAD) from OK visits, then flag any visit
# that's far from its OWN site's baseline but sits inside ANOTHER site's baseline
# -- the same signature that caught the AM/GB mixup in the GB-only pass.
ok <- extract_all %>% filter(status == "OK")

site_baseline <- ok %>%
  group_by(ID) %>%
  summarise(med = median(mean_DO), mad = mad(mean_DO), lo = quantile(mean_DO, 0.1), hi = quantile(mean_DO, 0.9), .groups = "drop")


flag_visit <- function(id, val) {
  own <- site_baseline %>% filter(ID == id)
  own_z <- abs(val - own$med) / max(own$mad, 0.05)
  matches_other <- site_baseline %>% filter(ID != id) %>%
    mutate(inside = val >= lo - 0.3 & val <= hi + 0.3) %>%
    filter(inside) %>% pull(ID)
  tibble(own_z = own_z, matches_other_site = paste(matches_other, collapse = ","))
}

outlier_screen <- ok %>%
  rowwise() %>%
  mutate(flag_visit(ID, mean_DO)) %>%
  ungroup() %>%
  mutate(manually_verified = file %in% relabel$file,
         flagged = own_z > 3 & matches_other_site != "" & !manually_verified) %>%
  arrange(desc(flagged), desc(own_z))


# ---- build master VentDO series --------------------------------------------
new_ventdo <- outlier_screen %>%
  filter(!flagged) %>%
  transmute(ID, Date = ymd_hms(paste(visit_date, "00:00:00")), VentDO = mean_DO, VentTemp = mean_Temp)

id_ventdo <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>%
  filter(ID == "ID") %>%
  select(ID, Date, VentDO, VentTemp)

# ---- county WQ data (GB, LF) -----------------------------------------------
# 01_Raw_data/County Data/{GB,LF}.Vent_WQ.xlsx -- SRWMD/USGS grab-sample WQ
# visits, includes DO_mg/L. No equivalent WQ file exists for AM in this
# folder (only AM.Vent_Flow.xlsx, discharge-only), so AM isn't extended here.
# Adds many more visits per site (GB: +213, LF: +21) spanning back to the
# 1990s, well beyond our own ~2022-2023 roving-visit window -- the extra
# historical rows don't affect the two-station pipeline (which only pulls
# VentDO within the project's own date range via fill()), but give a much
# richer baseline for the rating-curve work.
excel_date <- function(x) as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC")

read_county_wq <- function(f, site_id) {
  raw <- read_excel(file.path("01_Raw_data/County Data", f), skip = 13, col_names = FALSE)
  hdr <- as.character(unlist(raw[1, ]))
  d <- raw[-1, ]
  names(d) <- make.unique(hdr)  # many repeated "Code" columns, one per parameter
  d %>%
    transmute(ID = site_id,
              Date = excel_date(as.numeric(Date)),
              VentDO = as.numeric(`DO_mg/L`),
              VentTemp = as.numeric(Water_Temp_C)) %>%
    filter(!is.na(VentDO))
}

county_ventdo <- bind_rows(
  read_county_wq("GB.Vent_WQ.xlsx", "GB"),
  read_county_wq("LF.Vent_WQ.xlsx", "LF")
)

# known-bad point, confirmed against county data: our own 2023-03-15 GB
# reading (VentDO=0.222) is far outside both our own site's normal range
# (3.6-4.8) and the county's contemporaneous range for the same date window
# -- VentTemp on that row (71.1) is normal, so this looks like an isolated
# DO-probe misread, not a mislabeled/whole-row failure like the cases above.
county_confirmed_bad <- tribble(
  ~ID,  ~Date,
  "GB", ymd_hms("2023-03-15 00:00:00")
)

master_ventdo <- bind_rows(new_ventdo, id_ventdo, county_ventdo) %>%
  anti_join(county_confirmed_bad, by = c("ID", "Date")) %>%
  arrange(ID, Date) %>% distinct()

write_csv(master_ventdo, "02_Clean_data/Chem/VentDO.csv")  # replaces the old file at Samantha's request

