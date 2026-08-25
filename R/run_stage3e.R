required <- c(
  "readr","dplyr","stringr","countrycode","fixest","broom"
)

missing <- required[
  !vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
]

if(length(missing)) {
  install.packages(missing,repos="https://cloud.r-project.org")
}

if(!file.exists("outputs/stage3d_legal_architecture/07_legal_architecture_analysis_dataset.csv")) {
  message("Stage 3D output absent; running Stage 3D first.")
  source("R/run_stage3d.R")
}

source("R/41_classify_governing_law.R")
source("R/42_classify_arbitration.R")
source("R/43_run_governing_law_choice_models.R")
source("R/44_run_arbitration_choice_models.R")
source("R/45_run_cross_default_confirmatory.R")
source("R/46_stage3e_summary.R")

message("Stage 3E completed successfully.")
