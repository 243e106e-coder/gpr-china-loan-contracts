required <- c("readr","dplyr","fixest","broom")
missing <- required[!vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if(length(missing)) install.packages(missing, repos="https://cloud.r-project.org")

needed <- c(
  "outputs/stage3g_final_inference/01_financial_final_data.csv",
  "outputs/stage3g_final_inference/02_legal_final_data.csv",
  "outputs/stage3g_final_inference/03_final_hypotheses.csv",
  "outputs/stage3g_final_inference/04_final_clustered_results.csv",
  "outputs/stage3g_final_inference/10_final_leave_one_country_summary.csv",
  "outputs/stage3g_final_inference/11_final_leave_one_year_summary.csv"
)

if(any(!file.exists(needed))) {
  message("Base Stage 3G outputs are missing.")
  candidates <- c("R/run_stage3g.R","R/run_stage3g_final.R","R/run_stage3g_full.R")
  found <- candidates[file.exists(candidates)]
  if(length(found) == 0) {
    stop(paste0(
      "No Stage 3G master runner found.\nExpected one of:\n",
      paste0(" - ", candidates, collapse="\n"),
      "\nPlease tell me the actual Stage 3G master runner filename."
    ), call.=FALSE)
  }
  message("Running Stage 3G master runner: ", found[1])
  source(found[1])
}

source("R/56_run_wild_cluster_final_v2.R")
source("R/58_build_final_results_table_v2.R")
source("R/59_stage3g_summary_v2.R")
message("Stage 3G Wild Cluster Singleton Fix V2 completed.")
