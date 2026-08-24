required <- c(
  "readr","readxl","dplyr","tidyr","stringr","lubridate",
  "janitor","fixest","broom"
)
missing <- required[
  !vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
]
if(length(missing)) install.packages(missing,repos="https://cloud.r-project.org")

if(!file.exists("outputs/stage2b_harmonization/09_stage2b_master_dataset.csv")) {
  message("Stage 2B master absent; running Stage 2B first.")
  source("R/run_stage2b.R")
}

source("R/15_prepare_gpr_annual.R")
source("R/16_merge_gpr_to_contracts.R")
source("R/17_build_baseline_estimation_sample.R")
source("R/18_run_baseline_models.R")
source("R/19_stage3_diagnostics.R")
