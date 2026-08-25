required <- c("readr","dplyr","fixest","broom")
missing <- required[!vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
if(length(missing)) install.packages(missing,repos="https://cloud.r-project.org")

if(!file.exists("outputs/stage3b_identification/06_contracts_with_country_gpr.csv")) {
  message("Stage 3B outputs absent; running Stage 3B first.")
  source("R/run_stage3b.R")
}

source("R/26_prepare_stage3c_data.R")
source("R/27_run_standardized_twfe_controls.R")
source("R/28_run_legal_clause_lpm.R")
source("R/29_leave_one_country_out.R")
source("R/30_leave_one_year_out.R")
source("R/31_crisis_exclusion_tests.R")
source("R/32_optional_small_cluster_inference.R")
source("R/33_stage3c_summary.R")

message("Stage 3C completed successfully.")
