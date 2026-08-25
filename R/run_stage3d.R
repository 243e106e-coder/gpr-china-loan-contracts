required <- c(
  "readr",
  "dplyr",
  "stringr",
  "tidyr",
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

if (length(missing) > 0) {
  install.packages(
    missing,
    repos = "https://cloud.r-project.org"
  )
}

if (!file.exists(
  "outputs/stage3c_robustness/02_stage3c_analysis_data.csv"
)) {
  message("Stage 3C outputs absent; running Stage 3C first.")
  source("R/run_stage3c.R")
}

source("R/34_audit_legal_architecture_fields.R")
source("R/35_build_manual_legal_review_templates.R")
source("R/36_code_safe_legal_indicators.R")
source("R/37_merge_manual_legal_classifications.R")
source("R/38_run_legal_architecture_twfe.R")
source("R/39_manual_classification_readiness.R")
source("R/40_stage3d_summary.R")

message("Stage 3D legal architecture completed successfully.")
