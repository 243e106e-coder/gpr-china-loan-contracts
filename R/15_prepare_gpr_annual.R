suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
})

outdir <- "outputs/stage3_gpr_baseline"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

trad_file <- "data/raw/gpr/data_gpr_export.xls"
ai_global_file <- "data/raw/gpr/ai_gpr_data_monthly.csv"
ai_country_file <- "data/raw/gpr/ai_gpr_country_monthly.csv"

if (!file.exists(trad_file)) {
  hit <- list.files("data/raw", pattern="data_gpr_export\\.xls$", recursive=TRUE, full.names=TRUE)
  if (length(hit)) trad_file <- hit[1]
}
if (!file.exists(ai_global_file)) {
  hit <- list.files("data/raw", pattern="ai_gpr_data_monthly\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(hit)) ai_global_file <- hit[1]
}
if (!file.exists(ai_country_file)) {
  hit <- list.files("data/raw", pattern="ai_gpr_country_monthly\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(hit)) ai_country_file <- hit[1]
}

if (!file.exists(trad_file)) stop("Traditional GPR file missing.")
if (!file.exists(ai_global_file)) stop("AI global GPR file missing.")
if (!file.exists(ai_country_file)) stop("AI country GPR file missing.")

trad <- readxl::read_excel(trad_file, sheet=1)
names(trad) <- janitor::make_clean_names(names(trad))
if (!("month" %in% names(trad))) stop("Traditional GPR file has no month field.")

trad <- trad %>%
  mutate(
    month = as.Date(month),
    year = lubridate::year(month)
  )

wanted_trad <- intersect(c("gpr","gprt","gpra","gprh","gprht","gprha"), names(trad))

trad_annual <- trad %>%
  group_by(year) %>%
  summarise(
    across(all_of(wanted_trad), ~ mean(as.numeric(.x), na.rm=TRUE)),
    .groups="drop"
  )

write_csv(trad_annual, file.path(outdir, "01_global_gpr_annual.csv"))

ai_global <- read_csv(ai_global_file, show_col_types=FALSE, progress=FALSE)
names(ai_global) <- janitor::make_clean_names(names(ai_global))

date_candidates <- c("month","date","period")
date_col <- date_candidates[date_candidates %in% names(ai_global)][1]
if (is.na(date_col)) stop("Cannot identify date column in AI global GPR.")

ai_global <- ai_global %>%
  mutate(
    gpr_date = suppressWarnings(as.Date(.data[[date_col]])),
    year = lubridate::year(gpr_date)
  )

num_cols <- names(ai_global)[vapply(ai_global, is.numeric, logical(1))]
num_cols <- setdiff(num_cols, "year")

ai_global_annual <- ai_global %>%
  group_by(year) %>%
  summarise(across(all_of(num_cols), ~ mean(.x, na.rm=TRUE)), .groups="drop")

write_csv(ai_global_annual, file.path(outdir, "02_ai_global_gpr_annual.csv"))

ai_country <- read_csv(ai_country_file, show_col_types=FALSE, progress=FALSE)
names(ai_country) <- janitor::make_clean_names(names(ai_country))

date_candidates <- c("month","date","period")
date_col <- date_candidates[date_candidates %in% names(ai_country)][1]
if (is.na(date_col)) stop("Cannot identify date column in AI country GPR.")

country_candidates <- c("country","country_name","location","economy")
country_col <- country_candidates[country_candidates %in% names(ai_country)][1]
if (is.na(country_col)) stop("Cannot identify country column in AI country GPR.")

ai_country <- ai_country %>%
  mutate(
    gpr_date = suppressWarnings(as.Date(.data[[date_col]])),
    year = lubridate::year(gpr_date)
  )

num_cols <- names(ai_country)[vapply(ai_country, is.numeric, logical(1))]
num_cols <- setdiff(num_cols, "year")

ai_country_annual <- ai_country %>%
  group_by(.data[[country_col]], year) %>%
  summarise(across(all_of(num_cols), ~ mean(.x, na.rm=TRUE)), .groups="drop")

names(ai_country_annual)[1] <- "country"

write_csv(ai_country_annual, file.path(outdir, "03_ai_country_gpr_annual.csv"))

write_csv(
  tibble(
    source=c("traditional_global","ai_global","ai_country"),
    file=c(trad_file,ai_global_file,ai_country_file)
  ),
  file.path(outdir,"00_gpr_source_manifest.csv")
)
