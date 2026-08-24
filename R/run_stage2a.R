required <- c(
  "readr","readxl","haven","dplyr","purrr",
  "stringr","tidyr","janitor"
)
missing <- required[
  !vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))
]
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")

# GitHub runner starts fresh. Ensure prior-stage files and HCL raw data exist.
hcl_found <- length(list.files(
  "data/raw/hcl2",
  pattern="How_China_Lends_Dataset_Version_2_0\\.xlsx$",
  recursive=TRUE, full.names=TRUE
)) > 0

if (!hcl_found) {
  message("HCL workbook absent; running Stage 1 download/audit first.")
  source("R/run_stage1.R")
}

manifest_exists <- file.exists(
  "outputs/stage1_5_pricing_recovery/09_pricing_recovery_manifest.csv"
)

if (!manifest_exists) {
  message("Stage 1.5 manifest absent; running Stage 1.5 first.")
  source("R/05_pricing_recovery_audit.R")
}

source("R/06_download_clg_global.R")
source("R/07_audit_and_match_clg.R")
source("R/08_build_baseline_sample.R")
source("R/09_stage2a_summary.R")
