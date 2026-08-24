suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

audit_dir <- "outputs/stage1_audit"

tables <- read_csv(file.path(audit_dir, "hcl_table_inventory.csv"), show_col_types = FALSE)
cands  <- read_csv(file.path(audit_dir, "hcl_column_candidates.csv"), show_col_types = FALSE)
miss   <- read_csv(file.path(audit_dir, "hcl_missingness.csv"), show_col_types = FALSE)

key_roles <- c(
  "borrower_country", "signing_date", "year", "lender",
  "interest_rate", "spread_margin", "maturity", "grace_period",
  "amount", "collateral", "escrow", "guarantee",
  "cross_default", "acceleration", "termination",
  "sovereign_immunity", "governing_law", "sector", "ppg"
)

key <- cands %>%
  filter(role %in% key_roles) %>%
  left_join(
    miss %>% select(table, column_clean, n, pct_missing),
    by = c("table", "column_clean")
  ) %>%
  arrange(role, pct_missing)

write_csv(key, file.path(audit_dir, "KEY_stage1_candidates_with_missingness.csv"))

summary_lines <- c(
  "# Paper 1 Stage 1 Audit Summary",
  "",
  paste0("- Tables/sheets successfully read: ", sum(tables$read_ok %in% TRUE, na.rm = TRUE)),
  paste0("- Candidate fields identified: ", nrow(cands)),
  "",
  "## Next decision",
  "Use KEY_stage1_candidates_with_missingness.csv to select the actual HCL columns for:",
  "borrower country, signing date, lender, interest/spread, maturity, collateral, guarantee, escrow and other legal protections.",
  "",
  "Do not run final regressions until these mappings are verified."
)

writeLines(summary_lines, file.path(audit_dir, "STAGE1_SUMMARY.md"))
message(paste(summary_lines, collapse = "\n"))
