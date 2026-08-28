suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(janitor)
})

# ============================================================
# Paper 1 — Stage 1.5 substantive field audit
# Robust HCL workbook discovery for GitHub Actions / local runs.
# ============================================================

outdir <- "outputs/stage1_5_substantive_audit"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

find_hcl_workbook <- function() {
  preferred <- c(
    "data/How_China_Lends_Dataset_Version_2_0.xlsx",
    "How_China_Lends_Dataset_Version_2_0.xlsx",
    "inputs/How_China_Lends_Dataset_Version_2_0.xlsx"
  )
  preferred <- preferred[file.exists(preferred)]
  if (length(preferred)) return(preferred[1])

  search_roots <- c(
    "data/raw/hcl2/unzipped",
    "data/raw/hcl2",
    "data",
    "inputs",
    "."
  )

  candidates <- character()
  for (root in search_roots) {
    if (!dir.exists(root)) next
    hits <- list.files(
      root,
      pattern = "\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    candidates <- unique(c(candidates, hits))
  }

  if (!length(candidates)) return(NA_character_)

  # Prefer files whose names clearly refer to How China Lends / HCL.
  scored <- tibble(path = candidates) %>%
    mutate(
      fname = basename(path),
      score =
        10L * str_detect(fname, regex("How.*China.*Lends", ignore_case = TRUE)) +
         5L * str_detect(fname, regex("HCL", ignore_case = TRUE)) +
         3L * str_detect(fname, regex("Dataset.*Version.*2", ignore_case = TRUE)) +
         1L * str_detect(path, regex("data/raw/hcl2/unzipped", ignore_case = TRUE))
    ) %>%
    arrange(desc(score), path)

  # Verify the workbook actually contains ContractData.
  for (p in scored$path) {
    sh <- tryCatch(excel_sheets(p), error = function(e) character())
    if ("ContractData" %in% sh) return(p)
  }

  NA_character_
}

hcl_path <- find_hcl_workbook()
if (is.na(hcl_path)) {
  stop(
    paste0(
      "Could not locate an HCL 2.0 workbook containing sheet 'ContractData'. ",
      "Expected R/01_download_data.R to have downloaded and unzipped HCL under ",
      "data/raw/hcl2/unzipped/."
    ),
    call. = FALSE
  )
}

message("Using HCL workbook: ", hcl_path)

raw <- read_excel(hcl_path, sheet = "ContractData")
orig_names <- names(raw)
dat <- raw %>% clean_names()

name_map <- tibble(
  column_original = orig_names,
  column_clean = names(dat)
)
write_csv(name_map, file.path(outdir, "00_column_name_map.csv"))

normalize_chr <- function(x) {
  y <- as.character(x)
  y <- str_squish(y)
  y[y %in% c("", "NA", "N/A", "na", "n/a", ".", "..", "...")] <- NA_character_
  y
}

distribution_table <- function(df, col) {
  x <- normalize_chr(df[[col]])
  tibble(value = x) %>%
    mutate(value_display = if_else(is.na(value), "<MISSING>", value)) %>%
    count(value_display, sort = TRUE, name = "n") %>%
    mutate(column = col, pct = n / sum(n)) %>%
    select(column, value = value_display, n, pct)
}

coverage_row <- function(df, col) {
  x <- normalize_chr(df[[col]])
  n <- length(x)
  n_nonmissing <- sum(!is.na(x))
  n_unique_nonmissing <- n_distinct(x[!is.na(x)])

  neg_regex <- regex(
    "^(0|no|none|false|not applicable|n/?a|absent|no provision|not specified)$",
    ignore_case = TRUE
  )
  explicit_negative <- !is.na(x) & str_detect(x, neg_regex)

  tibble(
    column = col,
    n = n,
    n_nonmissing = n_nonmissing,
    pct_nonmissing = n_nonmissing / n,
    n_unique_nonmissing = n_unique_nonmissing,
    n_explicit_negative_like = sum(explicit_negative),
    pct_explicit_negative_like = mean(explicit_negative),
    n_other_nonmissing = sum(!is.na(x) & !explicit_negative)
  )
}

legal_fields_requested <- c(
  "collateral",
  "guarantor",
  "guarantor_name",
  "escrow_account",
  "revenue_account",
  "cross_default",
  "cross_default_des",
  "governing_law",
  "governing_law_party",
  "governing_law_des",
  "arbitration",
  "arbitration_des",
  "confidentiality_general",
  "confidentiality_borrower",
  "confidentiality_lender",
  "confidentiality_des_borrower",
  "confidentiality_des_lender"
)

legal_fields <- intersect(legal_fields_requested, names(dat))

coverage <- map_dfr(legal_fields, ~ coverage_row(dat, .x))
write_csv(coverage, file.path(outdir, "01_substantive_field_coverage.csv"))

dist_all <- map_dfr(legal_fields, ~ distribution_table(dat, .x))
write_csv(dist_all, file.path(outdir, "02_substantive_field_value_distributions.csv"))

for (cc in legal_fields) {
  write_csv(
    distribution_table(dat, cc),
    file.path(outdir, paste0("dist_", cc, ".csv"))
  )
}

all_cols <- names(dat)

candidate_patterns <- list(
  signing_date = c(
    "sign", "signature", "signed", "agreement_date", "contract_date",
    "approval_date", "commitment_date", "date"
  ),
  year = c("^year$", "signing_year", "contract_year", "approval_year"),
  maturity = c("maturity", "matur", "tenor", "term", "duration"),
  grace = c("grace", "grace_period"),
  pricing = c(
    "interest", "spread", "margin", "pricing", "coupon", "rate",
    "libor", "sofr", "euribor"
  )
)

candidate_hits <- imap_dfr(candidate_patterns, function(patterns, role) {
  rgx <- paste(patterns, collapse = "|")
  hits <- all_cols[str_detect(all_cols, regex(rgx, ignore_case = TRUE))]
  if (!length(hits)) {
    return(tibble(role = role, column_clean = NA_character_))
  }
  tibble(role = role, column_clean = hits)
}) %>%
  left_join(name_map, by = "column_clean") %>%
  select(role, column_original, column_clean)

write_csv(
  candidate_hits,
  file.path(outdir, "03_date_maturity_grace_pricing_candidates.csv")
)

candidate_profile <- candidate_hits %>%
  filter(!is.na(column_clean)) %>%
  mutate(profile = map(column_clean, function(cc) {
    x <- normalize_chr(dat[[cc]])
    vals <- unique(x[!is.na(x)])
    tibble(
      class = class(dat[[cc]])[1],
      n = length(x),
      n_nonmissing = sum(!is.na(x)),
      pct_nonmissing = mean(!is.na(x)),
      n_unique_nonmissing = n_distinct(x[!is.na(x)]),
      examples = paste(head(vals, 8), collapse = " | ")
    )
  })) %>%
  unnest(profile)

write_csv(candidate_profile, file.path(outdir, "04_candidate_profiles.csv"))

pricing_candidates <- candidate_hits %>%
  filter(role == "pricing", !is.na(column_clean))

pricing_safety <- pricing_candidates %>%
  mutate(
    ordinary_pricing_status = case_when(
      column_clean %in% c("penalty_interest_rate", "penalty_interest_rate_des") ~
        "DO_NOT_USE_AS_ORDINARY_PRICING",
      TRUE ~ "REQUIRES_MANUAL_VERIFICATION"
    ),
    note = case_when(
      column_clean == "penalty_interest_rate" ~
        "Penalty/default interest is not the ordinary loan interest rate.",
      column_clean == "penalty_interest_rate_des" ~
        "Description of penalty/default interest; not ordinary pricing.",
      TRUE ~
        "Name match only. Verify substantive meaning before use."
    )
  )

write_csv(pricing_safety, file.path(outdir, "05_pricing_safety_audit.csv"))

if ("penalty_interest_rate" %in% names(dat)) {
  message("SAFETY: penalty_interest_rate exists but is explicitly excluded from ordinary pricing.")
}

id_fields <- intersect(
  c(
    "contract_id",
    "aid_data_record_id",
    "aid_data_parent_id",
    "borrower_country",
    "borrower_name",
    "borrower_type",
    "creditor_country",
    "creditor_name",
    "creditor_type",
    "ppg_debt",
    "year"
  ),
  names(dat)
)

id_coverage <- map_dfr(id_fields, ~ coverage_row(dat, .x))
write_csv(id_coverage, file.path(outdir, "06_key_identifier_coverage.csv"))

if ("governing_law" %in% names(dat)) {
  write_csv(
    distribution_table(dat, "governing_law"),
    file.path(outdir, "07_governing_law_distribution.csv")
  )
}
if ("governing_law_party" %in% names(dat)) {
  write_csv(
    distribution_table(dat, "governing_law_party"),
    file.path(outdir, "08_governing_law_party_distribution.csv")
  )
}
if ("arbitration" %in% names(dat)) {
  write_csv(
    distribution_table(dat, "arbitration"),
    file.path(outdir, "09_arbitration_distribution.csv")
  )
}
if ("arbitration_des" %in% names(dat)) {
  write_csv(
    distribution_table(dat, "arbitration_des"),
    file.path(outdir, "10_arbitration_description_distribution.csv")
  )
}

mapping <- tibble(
  research_concept = c(
    "Borrower country",
    "Creditor name",
    "Creditor type",
    "Collateral",
    "Guarantee indicator/source",
    "Guarantee name/details",
    "Escrow account",
    "Revenue account",
    "Cross-default",
    "Cross-default details",
    "Governing law",
    "Governing-law party",
    "Arbitration",
    "Arbitration details",
    "Ordinary loan pricing",
    "Signing date/year",
    "Maturity",
    "Grace period"
  ),
  preferred_hcl_column = c(
    ifelse("borrower_country" %in% names(dat), "borrower_country", NA),
    ifelse("creditor_name" %in% names(dat), "creditor_name", NA),
    ifelse("creditor_type" %in% names(dat), "creditor_type", NA),
    ifelse("collateral" %in% names(dat), "collateral", NA),
    ifelse("guarantor" %in% names(dat), "guarantor", NA),
    ifelse("guarantor_name" %in% names(dat), "guarantor_name", NA),
    ifelse("escrow_account" %in% names(dat), "escrow_account", NA),
    ifelse("revenue_account" %in% names(dat), "revenue_account", NA),
    ifelse("cross_default" %in% names(dat), "cross_default", NA),
    ifelse("cross_default_des" %in% names(dat), "cross_default_des", NA),
    ifelse("governing_law" %in% names(dat), "governing_law", NA),
    ifelse("governing_law_party" %in% names(dat), "governing_law_party", NA),
    ifelse("arbitration" %in% names(dat), "arbitration", NA),
    ifelse("arbitration_des" %in% names(dat), "arbitration_des", NA),
    NA_character_,
    ifelse("year" %in% names(dat), "year (verify definition)", NA),
    NA_character_,
    NA_character_
  ),
  status = c(
    rep("VERIFY_VALUE_CODING", 14),
    "USE_CLG_HARMONIZED_PRICING_NOT_PENALTY_RATE",
    "VERIFY_YEAR_DEFINITION_AND_SEARCH_SOURCE",
    "NOT_FOUND_IN_HCL_CONTRACTDATA_NAME_SCAN",
    "NOT_FOUND_IN_HCL_CONTRACTDATA_NAME_SCAN"
  ),
  note = c(
    rep("Inspect distributions before final recoding.", 14),
    "Penalty interest must not be used as the normal lending rate/spread.",
    "Verify what event the HCL year field represents.",
    "If not present in HCL ContractData, retain the already harmonized external/source variable and document provenance.",
    "If not present in HCL ContractData, retain the already harmonized external/source variable and document provenance."
  )
)

write_csv(mapping, file.path(outdir, "11_recommended_field_mapping.csv"))

top_value_text <- function(col, k = 5) {
  if (!(col %in% names(dat))) return("not present")
  tab <- distribution_table(dat, col) %>% slice_head(n = k)
  paste0(tab$value, "=", tab$n, collapse = "; ")
}

summary_lines <- c(
  "# Paper 1 — Stage 1.5 Substantive Field Audit",
  "",
  paste0("- HCL workbook: ", hcl_path),
  paste0("- ContractData rows: ", nrow(dat)),
  paste0("- ContractData columns: ", ncol(dat)),
  paste0("- Legal fields audited: ", length(legal_fields)),
  "",
  "## Critical safeguards",
  "- `penalty_interest_rate` is explicitly excluded from ordinary loan pricing.",
  "- A non-missing column is not treated as substantive coverage; value distributions are audited.",
  "- No regression is estimated in this stage.",
  "",
  "## Candidate search",
  paste0(
    "- Signing/date candidates: ",
    paste(
      na.omit(candidate_hits$column_clean[candidate_hits$role == "signing_date"]),
      collapse = ", "
    )
  ),
  paste0(
    "- Maturity candidates: ",
    paste(
      na.omit(candidate_hits$column_clean[candidate_hits$role == "maturity"]),
      collapse = ", "
    )
  ),
  paste0(
    "- Grace candidates: ",
    paste(
      na.omit(candidate_hits$column_clean[candidate_hits$role == "grace"]),
      collapse = ", "
    )
  ),
  paste0(
    "- Pricing-name candidates: ",
    paste(
      na.omit(candidate_hits$column_clean[candidate_hits$role == "pricing"]),
      collapse = ", "
    )
  ),
  "",
  "## Selected legal field snapshots",
  paste0("- collateral: ", top_value_text("collateral")),
  paste0("- guarantor: ", top_value_text("guarantor")),
  paste0("- escrow_account: ", top_value_text("escrow_account")),
  paste0("- cross_default: ", top_value_text("cross_default")),
  paste0("- governing_law_party: ", top_value_text("governing_law_party")),
  paste0("- arbitration: ", top_value_text("arbitration")),
  "",
  "## Next step",
  "Review 03_date_maturity_grace_pricing_candidates.csv,",
  "04_candidate_profiles.csv, 05_pricing_safety_audit.csv,",
  "and 11_recommended_field_mapping.csv before locking the master dataset."
)

writeLines(summary_lines, file.path(outdir, "STAGE1_5_SUMMARY.md"))

message("Stage 1.5 substantive field audit completed.")
message("Outputs written to: ", outdir)
