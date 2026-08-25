required <- c(
  "readr",
  "dplyr",
  "fixest",
  "broom"
)

missing <- required[
  !vapply(
    required,
    requireNamespace,
    quietly=TRUE,
    FUN.VALUE=logical(1)
  )
]

if(length(missing)) {
  install.packages(
    missing,
    repos="https://cloud.r-project.org"
  )
}

needed <- c(
  "outputs/stage3g_final_inference/01_financial_final_data.csv",
  "outputs/stage3g_final_inference/02_legal_final_data.csv",
  "outputs/stage3g_final_inference/03_final_hypotheses.csv",
  "outputs/stage3g_final_inference/04_final_clustered_results.csv",
  "outputs/stage3g_final_inference/10_final_leave_one_country_summary.csv",
  "outputs/stage3g_final_inference/11_final_leave_one_year_summary.csv"
)

if(any(!file.exists(needed))) {
  message("Stage 3G base outputs absent; running full Stage 3G first.")
  source("R/run_stage3g.R")
}

source("R/56_run_wild_cluster_final.R")
source("R/58_build_final_results_table.R")
source("R/59_stage3g_summary.R")

message("Stage 3G wild-cluster patch completed successfully.")
