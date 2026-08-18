# =============================================================================
# Compile ALL sites' roving VentDO field files into one master VentDO series.
#
# Source (already copied from the network Roving_edited share, see
# 01_Raw_data/Hobo/Roving DO/<ID>/): AM, GB, LF, OS.
# ID keeps its existing continuous USGS-gauge VentDO (pulled in 03_Scripts/VentDO.R,
# already in 04_Outputs/VentDO.csv) -- no roving grab files exist for ID, it doesn't
# need this treatment (already the best-instrumented site).
#
# Data-quality rules (Samantha, established during the GB investigation):
#   - Any reading >7 mg/L within a file = sensor out of the water. Discard those
#     rows, keep the rest of the visit.
#   - Visit averages, not per-timestamp matching.
#   - Dates come from the DATA, never the filename -- confirmed necessary here:
#     many files' embedded timestamps don't match their filenames at all
#     (e.g. everything named "roving_OS_05182023 (1)" through "(7)" is actually
#     7 different, unrelated visit dates).
#   - Some excels contain BOTH sites' data as separate sheets (a joint AM+GB or
#     LF+Otter field visit) and got copied into both site folders -- e.g.
#     "Roving_GB_DO_01182022.xlsx" (in the GB folder) has sheets
#     "AllenMill | GilchristBlue"; naively taking sheet 1 would silently pull
#     AM's numbers into GB. Sheet selection below matches the sheet name to the
#     target site instead of defaulting to sheet 1.
# =============================================================================

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

cat("\n=== After manual relabel/remove ===\n")
print(extract_all %>% filter(file %in% c(relabel$file, remove_files$file)) %>%
        select(ID, file, status, visit_date, mean_DO))

# ---- outlier / mislabeling screen ------------------------------------------
# Robust per-site baseline (median + MAD) from OK visits, then flag any visit
# that's far from its OWN site's baseline but sits inside ANOTHER site's baseline
# -- the same signature that caught the AM/GB mixup in the GB-only pass.
ok <- extract_all %>% filter(status == "OK")

site_baseline <- ok %>%
  group_by(ID) %>%
  summarise(med = median(mean_DO), mad = mad(mean_DO), lo = quantile(mean_DO, 0.1), hi = quantile(mean_DO, 0.9), .groups = "drop")

cat("\n=== Per-site VentDO baseline (from this extraction) ===\n")
print(site_baseline)

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

write_csv(outlier_screen, file.path(outdir, "ventdo_all_outlier_screen.csv"))

cat("\n=== FLAGGED visits (>3 MAD from own site's median AND within another site's range) ===\n")
print(outlier_screen %>% filter(flagged) %>%
        select(ID, file, visit_date, mean_DO, sd_DO, own_z, matches_other_site, sheet_note), n = 50)

cat("\n=== sheet-selection notes (ambiguous sheet picks, worth a manual glance) ===\n")
print(extract_all %>% filter(!is.na(sheet_note)) %>% select(ID, file, sheet_note))

# ---- build master VentDO series --------------------------------------------
new_ventdo <- outlier_screen %>%
  filter(!flagged) %>%
  transmute(ID, Date = ymd_hms(paste(visit_date, "00:00:00")), VentDO = mean_DO, VentTemp = mean_Temp)

id_ventdo <- read_csv("04_Outputs/VentDO.csv", show_col_types = FALSE) %>%
  filter(ID == "ID") %>%
  select(ID, Date, VentDO, VentTemp)

master_ventdo <- bind_rows(new_ventdo, id_ventdo) %>% arrange(ID, Date) %>% distinct()

write_csv(master_ventdo, file.path(outdir, "VentDO_all.csv"))
write_csv(master_ventdo, "02_Clean_data/Chem/VentDO_all.csv")
write_csv(master_ventdo, "02_Clean_data/Chem/VentDO.csv")  # replaces the old file at Samantha's request

cat("\n=== Master VentDO series written ===\n")
cat("02_Clean_data/Chem/VentDO_all.csv and 02_Clean_data/Chem/VentDO.csv (replaced), n =", nrow(master_ventdo), "\n")
print(master_ventdo %>% filter(ID != "ID") %>% count(ID))
cat("(ID kept as-is from the continuous USGS series: n =", nrow(id_ventdo), ")\n")

cat("\nDone. Outputs: ventdo_all_extraction_log.csv, ventdo_all_outlier_screen.csv, VentDO_all.csv\n")
