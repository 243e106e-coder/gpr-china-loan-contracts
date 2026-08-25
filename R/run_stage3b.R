# R/run_stage3b.R
required <- c(
  "readr","haven","dplyr","tidyr","stringr","lubridate",
  "janitor","countrycode","fixest","broom"
)
missing <- required[
  !vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
]
if(length(missing)) install.packages(missing,repos="https://cloud.r-project.org")

# Rebuild the Stage 3 GPR exposure files every time.
# This is intentional because older Stage 3 artifacts contained gpr == 1.
source("R/15_prepare_gpr_annual.R")

# Stage 2B master must already exist; if not, rebuild it.
if(!file.exists("outputs/stage2b_harmonization/09_stage2b_master_dataset.csv")) {
  message("Stage 2B master absent; running Stage 2B first.")
  source("R/run_stage2b.R")
}

# Re-merge corrected annual global GPR and reconstruct flags.
source("R/16_merge_gpr_to_contracts.R")
source("R/17_build_baseline_estimation_sample.R")

# Stage 3B identification.
source("R/20_parse_ai_country_gpr_wide.R")
source("R/21_map_borrower_country_to_ai_gpr.R")
source("R/22_run_global_gpr_sanity.R")
source("R/23_run_country_gpr_twfe.R")
source("R/24_legal_separation_diagnostics.R")
source("R/25_stage3b_summary.R")

message("Stage 3B completed successfully with corrected GPR import.")
