required <- c(
  "readr","dplyr","fixest","broom"
)

missing <- required[
  !vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
]

if(length(missing)) {
  install.packages(missing,repos="https://cloud.r-project.org")
}

if(!file.exists("outputs/stage3e_legal_choice/05_contracts_legal_choice_classified.csv")) {
  message("Stage 3E output absent; running Stage 3E first.")
  source("R/run_stage3e.R")
}

source("R/47_prepare_stage3f_targets.R")
source("R/48_run_confirmatory_legal_models.R")
source("R/49_leave_one_country_out_legal.R")
source("R/50_leave_one_year_out_legal.R")
source("R/51_crisis_exclusion_legal.R")
source("R/52_wild_cluster_legal.R")
source("R/53_stage3f_summary.R")

message("Stage 3F completed successfully.")
