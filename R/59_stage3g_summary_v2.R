suppressPackageStartupMessages({
  library(readr)
})

outdir <- "outputs/stage3g_final_inference"

audit <- read_csv(
  file.path(outdir,"06a_wildcluster_sample_audit.csv"),
  show_col_types=FALSE
)

final <- read_csv(
  file.path(outdir,"12_final_results_table.csv"),
  show_col_types=FALSE
)

lines <- c(
  "# Paper 1 — Stage 3G Wild Cluster Singleton Fix v2",
  "",
  "## Sample audit",
  paste0(
    "- ",audit$hypothesis_id,
    ": raw=",audit$n_raw_complete,
    ", pruned=",audit$n_after_singleton_pruning,
    ", FEOLS=",audit$n_feols,
    ", expected=",audit$stage3g_expected_n,
    ", N match=",audit$n_match,
    ", clusters=",audit$country_clusters,
    ", years=",audit$year_fe,
    ", min country cell=",audit$min_country_cell,
    ", min year cell=",audit$min_year_cell
  ),
  "",
  "## Final inference",
  paste0(
    "- ",final$hypothesis_id,
    " ",final$interpretation,
    ": beta=",round(final$estimate,4),
    ", clustered p=",round(final$p.value,4),
    ", wild status=",final$wild_status,
    ", wild p=",
    ifelse(
      is.na(final$p_value_wild_cluster),
      "NA",
      round(final$p_value_wild_cluster,4)
    ),
    ", strength=",final$final_strength
  ),
  "",
  "## Rule",
  "- No new hypothesis is added.",
  "- Wild cluster uses 9,999 Rademacher draws.",
  "- Borrower-country and year singletons are removed explicitly before FE estimation.",
  "- If wild status is not OK, inspect the hypothesis-specific print/summary files."
)

writeLines(
  lines,
  file.path(outdir,"STAGE3G_SUMMARY.md")
)

message(paste(lines,collapse="\n"))
