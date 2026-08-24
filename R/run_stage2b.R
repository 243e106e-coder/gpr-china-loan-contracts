required <- c("readr","dplyr","stringr","tidyr")
missing <- required[!vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
if(length(missing)) install.packages(missing,repos="https://cloud.r-project.org")

need <- c(
 "outputs/stage2a_pricing_recovery/07_hcl_clg_exact_record_matches.csv",
 "outputs/stage2a_pricing_recovery/11_stage2a_analysis_sample.csv"
)
if(any(!file.exists(need))) {
  message("Stage 2A outputs absent; running Stage 2A first.")
  source("R/run_stage2a.R")
}

source("R/10_audit_pricing_values.R")
source("R/11_harmonize_pricing.R")
source("R/12_code_legal_terms.R")
source("R/13_build_stage2b_master.R")
source("R/14_stage2b_summary.R")
