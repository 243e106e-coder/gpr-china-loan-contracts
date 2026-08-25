# R/15_prepare_gpr_annual.R
# FIX v2: use official Stata GPR file to avoid readxl coercing GPR/GPRT/GPRA
# into logical TRUE values.

suppressPackageStartupMessages({
  library(readr)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
})

outdir <- "outputs/stage3_gpr_baseline"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
rawdir <- "data/raw/gpr"
dir.create(rawdir, recursive=TRUE, showWarnings=FALSE)

dta_url <- "https://www.matteoiacoviello.com/gpr_replication_files/data_paper/data_gpr_export.dta"
dta_file <- file.path(rawdir, "data_gpr_export.dta")

if(!file.exists(dta_file) || file.info(dta_file)$size <= 0) {
  message("Downloading official GPR Stata file...")
  utils::download.file(
    dta_url,
    destfile=dta_file,
    mode="wb",
    method="libcurl",
    quiet=FALSE
  )
}

if(!file.exists(dta_file)) stop("Official GPR DTA download failed.")

g <- haven::read_dta(dta_file)
names(g) <- janitor::make_clean_names(names(g))

if(!("month" %in% names(g))) stop("DTA has no month field.")
if(!("gpr" %in% names(g))) stop("DTA has no GPR field.")

# Stata monthly date can arrive as labelled numeric, Date, or datetime.
# Avoid dependency on zoo by using Stata monthly-date arithmetic directly
# when month is numeric.
if(is.numeric(g$month) && !inherits(g$month,"Date")) {
  mnum <- as.numeric(g$month)
  # Stata monthly date: 0 = Jan 1960.
  year_vec <- 1960L + floor(mnum / 12)
  month_vec <- (mnum %% 12) + 1L
  gpr_date <- as.Date(sprintf("%04d-%02d-01", year_vec, month_vec))
} else {
  gpr_date <- as.Date(g$month)
}

g <- g %>%
  mutate(
    gpr_date = gpr_date,
    year = lubridate::year(gpr_date)
  )

wanted <- intersect(
  c("gpr","gprt","gpra","gprh","gprht","gprha"),
  names(g)
)

# Explicit numeric conversion and variance audit.
for(v in wanted) g[[v]] <- as.numeric(g[[v]])

audit <- tibble(
  variable=wanted,
  class=vapply(g[wanted], function(x) paste(class(x),collapse="/"), character(1)),
  n_nonmissing=vapply(g[wanted], function(x) sum(!is.na(x)), integer(1)),
  n_unique=vapply(g[wanted], function(x) dplyr::n_distinct(x,na.rm=TRUE), integer(1)),
  sd=vapply(g[wanted], function(x) sd(x,na.rm=TRUE), numeric(1))
)

write_csv(audit,file.path(outdir,"00e_gpr_numeric_variation_audit.csv"))

if(n_distinct(g$gpr,na.rm=TRUE) <= 1 || sd(g$gpr,na.rm=TRUE)==0) {
  stop(
    "Official GPR series has no numeric variation after DTA import. ",
    "Do not proceed with regressions.",
    call.=FALSE
  )
}

annual <- g %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    across(
      all_of(wanted),
      ~ if(all(is.na(.x))) NA_real_ else mean(.x,na.rm=TRUE)
    ),
    .groups="drop"
  )

# Second guardrail: contract-period GPR must vary.
contract_period <- annual %>% filter(year>=1990, year<=2025)

if(nrow(contract_period)==0 ||
   n_distinct(contract_period$gpr,na.rm=TRUE)<=1 ||
   sd(contract_period$gpr,na.rm=TRUE)==0) {
  stop("Annual GPR is constant in 1990–2025; import is invalid.", call.=FALSE)
}

write_csv(annual,file.path(outdir,"01_global_gpr_annual.csv"))

write_csv(
  tibble(
    source="Caldara-Iacoviello official replication DTA",
    url=dta_url,
    local_file=dta_file,
    gpr_1990_2025_min=min(contract_period$gpr,na.rm=TRUE),
    gpr_1990_2025_max=max(contract_period$gpr,na.rm=TRUE),
    gpr_1990_2025_sd=sd(contract_period$gpr,na.rm=TRUE),
    gpr_1990_2025_unique=n_distinct(contract_period$gpr,na.rm=TRUE)
  ),
  file.path(outdir,"00_gpr_source_manifest.csv")
)

message(
  "Correct GPR imported from DTA. 1990–2025 unique annual GPR values: ",
  n_distinct(contract_period$gpr,na.rm=TRUE),
  "; SD=",round(sd(contract_period$gpr,na.rm=TRUE),4)
)
