suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3f_legal_validation"

main <- read_csv(
  file.path(outdir,"03_confirmatory_legal_results.csv"),
  show_col_types=FALSE
)

loo_c <- read_csv(
  file.path(outdir,"06_legal_leave_one_country_out_summary.csv"),
  show_col_types=FALSE
)

loo_y <- read_csv(
  file.path(outdir,"08_legal_leave_one_year_out_summary.csv"),
  show_col_types=FALSE
)

wild <- read_csv(
  file.path(outdir,"10_legal_wild_cluster_results.csv"),
  show_col_types=FALSE
)

lines <- c(
  "# Paper 1 — Stage 3F Legal Validation",
  "",
  "## Confirmatory legal hypotheses",
  paste0(
    "- ",main$hypothesis_id,
    " ",main$interpretation,
    ": beta=",round(main$estimate,4),
    ", p=",round(main$p.value,4),
    ", Holm=",round(main$p_holm,4),
    ", BH=",round(main$p_bh,4),
    ", N=",main$nobs,
    ", clusters=",main$n_country_clusters,
    ", sign_ok=",main$sign_matches_hypothesis
  ),
  "",
  "## Leave-one-country-out",
  paste0(
    "- ",loo_c$hypothesis_id,
    ": min=",round(loo_c$min_beta,4),
    ", max=",round(loo_c$max_beta,4),
    ", expected-sign share=",round(loo_c$share_expected_sign,3)
  ),
  "",
  "## Leave-one-year-out",
  paste0(
    "- ",loo_y$hypothesis_id,
    ": min=",round(loo_y$min_beta,4),
    ", max=",round(loo_y$max_beta,4),
    ", expected-sign share=",round(loo_y$share_expected_sign,3)
  ),
  "",
  "## Wild-cluster status",
  paste(capture.output(print(wild)),collapse=" "),
  "",
  "## Decision rule",
  "- The four hypotheses are treated as pre-specified confirmatory tests for this stage.",
  "- Holm/Bonferroni/BH corrections are reported because Stage 3E screened multiple legal outcomes.",
  "- A legal result is substantially stronger if the expected sign survives all leave-one-country/year-out runs.",
  "- These tests strengthen credibility but still do not by themselves establish causality."
)

writeLines(
  lines,
  file.path(outdir,"STAGE3F_SUMMARY.md")
)

message(paste(lines,collapse="\n"))
