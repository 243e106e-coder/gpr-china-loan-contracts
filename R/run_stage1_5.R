required <- c(
  "readr", "readxl", "dplyr", "tidyr",
  "stringr", "purrr", "janitor"
)

missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# If Stage 1 data are absent, run Stage 1 first.
hcl_found <- length(list.files(
  "data/raw/hcl2",
  pattern = "How_China_Lends_Dataset_Version_2_0\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)) > 0

if (!hcl_found) {
  message("HCL workbook absent. Running Stage 1 first...")
  source("R/run_stage1.R")
}

source("R/05_pricing_recovery_audit.R")
