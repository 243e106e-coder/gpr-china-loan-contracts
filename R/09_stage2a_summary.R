suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

out_dir <- "outputs/stage2a_pricing_recovery"

download <- read_csv(file.path(out_dir, "00_clg_download_status.csv"), show_col_types=FALSE)
matches <- read_csv(file.path(out_dir, "06_record_id_match_summary.csv"), show_col_types=FALSE)
samples <- read_csv(file.path(out_dir, "12_baseline_sample_counts.csv"), show_col_types=FALSE)

coverage_file <- file.path(out_dir, "09_pricing_recovery_coverage.csv")
coverage <- if (file.exists(coverage_file)) {
  read_csv(coverage_file, show_col_types=FALSE)
} else tibble()

best_match <- if (nrow(matches)>0) max(matches$matched_contracts, na.rm=TRUE) else 0

lines <- c(
  "# Paper 1 — Stage 2A Pricing Recovery Summary",
  "",
  paste0("- Official CLG-Global download success: ", download$download_ok[1]),
  paste0("- Best exact AidData record-ID match count: ", best_match, " / 371"),
  "",
  "## Recovered pricing coverage",
  if (nrow(coverage)>0 && all(c("role","column","n_nonmissing","pct_of_371") %in% names(coverage))) {
    paste0(
      "- ", coverage$role, " [", coverage$column, "]: ",
      coverage$n_nonmissing, " contracts (",
      round(coverage$pct_of_371,1), "%)"
    )
  } else {
    "- No pricing fields recovered automatically."
  },
  "",
  "## Baseline samples",
  paste0("- ", samples$sample, ": ", samples$n),
  "",
  "## Decision rule",
  "A. If ordinary interest-rate and maturity coverage is substantial, proceed with Pricing-or-Contracting.",
  "B. If pricing coverage is sparse but legal-clause variation is strong, pivot the main paper toward Legal Risk Allocation.",
  "C. Never substitute penalty_interest_rate for the ordinary contractual lending rate.",
  "D. Exact AidData record-ID matches are preferred; fuzzy matching is intentionally excluded at this stage."
)

writeLines(lines, file.path(out_dir, "STAGE2A_SUMMARY.md"))
message(paste(lines, collapse="\n"))
