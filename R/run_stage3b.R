# R/run_stage3b.R
# Paper 1 — Stage 3B runner

required <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "janitor",
  "countrycode",
  "fixest",
  "broom"
)

missing <- required[
  !vapply(
    required,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing)) {
  install.packages(
    missing,
    repos = "https://cloud.r-project.org"
  )
}

# Stage 3B depends on Stage 3 estimation sample.
if (
  !file.exists(
    "outputs/stage3_gpr_baseline/07_stage3_estimation_sample.csv"
  )
) {
  message("Stage 3 outputs absent; running Stage 3 first.")
  source("R/run_stage3.R")
}

source("R/20_parse_ai_country_gpr_wide.R")
source("R/21_map_borrower_country_to_ai_gpr.R")
source("R/22_run_global_gpr_sanity.R")
source("R/23_run_country_gpr_twfe.R")
source("R/24_legal_separation_diagnostics.R")
source("R/25_stage3b_summary.R")

message("Stage 3B completed successfully.")
