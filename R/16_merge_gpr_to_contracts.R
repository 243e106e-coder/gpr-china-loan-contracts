suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3_gpr_baseline"

master_file <- "outputs/stage2b_harmonization/09_stage2b_master_dataset.csv"
if (!file.exists(master_file)) stop("Stage 2B master dataset missing.")

d <- read_csv(master_file, show_col_types=FALSE, progress=FALSE)
if (!("year" %in% names(d))) stop("Contract year field is missing.")

gpr <- read_csv(file.path(outdir,"01_global_gpr_annual.csv"), show_col_types=FALSE)

merged <- d %>%
  mutate(year = as.integer(year)) %>%
  left_join(gpr, by="year")

write_csv(merged, file.path(outdir,"04_contracts_with_global_gpr.csv"))

coverage <- merged %>%
  summarise(
    contracts=n(),
    with_gpr=sum(!is.na(gpr)),
    pct_with_gpr=100*with_gpr/contracts,
    min_year=min(year,na.rm=TRUE),
    max_year=max(year,na.rm=TRUE)
  )

write_csv(coverage, file.path(outdir,"05_gpr_merge_coverage.csv"))

country_candidates <- c("borrower_country","country")
ccol <- country_candidates[country_candidates %in% names(merged)][1]

if (!is.na(ccol)) {
  country_audit <- merged %>%
    count(country_raw=.data[[ccol]], sort=TRUE)
  write_csv(country_audit, file.path(outdir,"06_borrower_country_audit.csv"))
}
