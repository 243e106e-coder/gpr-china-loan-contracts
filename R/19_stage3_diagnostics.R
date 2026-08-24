suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3_gpr_baseline"

coef <- read_csv(file.path(outdir,"10_gpr_baseline_coefficients.csv"),
                 show_col_types=FALSE)
counts <- read_csv(file.path(outdir,"08_baseline_estimation_counts.csv"),
                   show_col_types=FALSE)
coverage <- read_csv(file.path(outdir,"05_gpr_merge_coverage.csv"),
                     show_col_types=FALSE)

lines <- c(
  "# Paper 1 — Stage 3 GPR Baseline Summary",
  "",
  paste0("- GPR merge coverage: ",coverage$with_gpr," / ",coverage$contracts,
         " (",round(coverage$pct_with_gpr,1),"%)"),
  "",
  "## Baseline estimation counts",
  paste0("- ",counts$outcome,": ",counts$n),
  "",
  "## GPR coefficients",
  if(nrow(coef)>0) paste0(
    "- ",coef$model,": beta=",round(coef$estimate,4),
    ", p=",round(coef$p.value,4),
    ", N=",coef$nobs
  ) else "- No GPR coefficients estimated.",
  "",
  "## Interpretation rules",
  "- These are baseline associations, not yet the final causal specification.",
  "- Annual GPR is used because the current HCL pipeline supplies contract year, not a verified signing month.",
  "- Do not interpret significance alone as causal evidence.",
  "- Threat/Act decomposition and country-specific GPR belong in the next robustness/identification stage."
)

writeLines(lines,file.path(outdir,"STAGE3_SUMMARY.md"))
message(paste(lines,collapse="\n"))
