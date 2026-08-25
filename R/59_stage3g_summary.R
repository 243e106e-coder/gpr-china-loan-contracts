suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3g_final_inference"

final <- read_csv(
  file.path(outdir,"12_final_results_table.csv"),
  show_col_types=FALSE
)

lines <- c(
  "# Paper 1 — Stage 3G Final Inference",
  "",
  "## Final targeted results",
  paste0(
    "- ",final$hypothesis_id,
    " ",final$interpretation,
    ": beta=",round(final$estimate,4),
    ", clustered p=",round(final$p.value,4),
    ", Holm=",round(final$p_holm_all,4),
    ", BH=",round(final$p_bh_all,4),
    ", wild=",
    ifelse(is.na(final$p_value_wild_cluster),"NA",round(final$p_value_wild_cluster,4)),
    ", LOO-country sign=",round(final$loo_country_sign_share,3),
    ", LOO-year sign=",round(final$loo_year_sign_share,3),
    ", strength=",final$final_strength
  ),
  "",
  "## Interpretation",
  "- F1/F2 are the financial core: pricing and maturity.",
  "- L2/L3/L4 are the legal validation block retained from Stage 3F.",
  "- No new outcome is searched in Stage 3G.",
  "- Results are still observational and should not be described as causal without an additional identification design."
)

writeLines(
  lines,
  file.path(outdir,"STAGE3G_SUMMARY.md")
)

message(paste(lines,collapse="\n"))
