suppressPackageStartupMessages({library(readr);library(dplyr)})
outdir <- "outputs/stage2b_harmonization"
cov <- read_csv(file.path(outdir,"05_harmonized_pricing_coverage.csv"),show_col_types=FALSE)
dec <- read_csv(file.path(outdir,"10_stage2b_decision_table.csv"),show_col_types=FALSE)

lines <- c(
  "# Paper 1 — Stage 2B Harmonization Summary",
  "",
  "## Pricing coverage after harmonization",
  paste0("- ",cov$measure,": ",cov$nonmissing," / ",cov$n,
         " (",round(cov$pct,1),"%)"),
  "",
  "## Coding status",
  paste0("- ",dec$item,": ",dec$status," — ",dec$reason),
  "",
  "## Hard rules",
  "- default_interest_rate and HCL penalty_interest_rate are not ordinary loan pricing.",
  "- governing-law and arbitration categories are exported for manual legal review.",
  "- No Neither/PricingOnly/ContractOnly/Both outcome is created at Stage 2B; that requires an explicit identification/response definition."
)
writeLines(lines,file.path(outdir,"STAGE2B_SUMMARY.md"))
message(paste(lines,collapse="\n"))
