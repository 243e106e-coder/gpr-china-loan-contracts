suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3c_robustness"

if(!requireNamespace("fwildclusterboot",quietly=TRUE)) {
  write_csv(
    tibble(
      status="SKIPPED",
      reason="fwildclusterboot is not installed; country-clustered fixest inference remains available."
    ),
    file.path(outdir,"13_wild_cluster_bootstrap_status.csv")
  )
} else {
  write_csv(
    tibble(
      status="AVAILABLE",
      reason="fwildclusterboot is installed; run targeted boottest manually after reviewing cluster count."
    ),
    file.path(outdir,"13_wild_cluster_bootstrap_status.csv")
  )
}
message("32_optional_small_cluster_inference.R completed.")
