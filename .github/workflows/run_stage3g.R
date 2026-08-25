required <- c("readr","dplyr","fixest","broom")

missing <- required[
  !vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
]

if(length(missing)) {
  install.packages(missing,repos="https://cloud.r-project.org")
}

if(!file.exists("outputs/stage3c_robustness/02_stage3c_analysis_data.csv")) {
  message("Stage 3C missing; running Stage 3C.")
  source("R/run_stage3c.R")
}

if(!file.exists("outputs/stage3f_legal_validation/01_stage3f_analysis_data.csv")) {
  message("Stage 3F missing; running Stage 3F.")
  source("R/run_stage3f.R")
}

source("R/54_prepare_stage3g_targets.R")
source("R/55_run_final_models.R")
source("R/56_run_wild_cluster_final.R")
source("R/57_final_loo_and_crisis.R")
source("R/58_build_final_results_table.R")
source("R/59_stage3g_summary.R")

message("Stage 3G completed successfully.")
