required <- c(
  "readr", "readxl", "haven", "dplyr", "purrr",
  "stringr", "tidyr", "janitor"
)

missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

source("R/01_download_data.R")
source("R/02_audit_hcl.R")
source("R/03_audit_gpr.R")
source("R/04_stage1_summary.R")
