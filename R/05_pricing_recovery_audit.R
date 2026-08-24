suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(janitor)
})

options(stringsAsFactors = FALSE, timeout = 900)

outdir <- "outputs/stage1_5_pricing_recovery"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Locate the HCL 2.0 workbook produced by Stage 1.
hcl_candidates <- list.files(
  "data/raw/hcl2",
  pattern = "How_China_Lends_Dataset_Version_2_0\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(hcl_candidates) == 0) {
  stop(
    paste0(
      "HCL 2.0 workbook not found under data/raw/hcl2.\n",
      "Run Stage 1 first (R/run_stage1.R), then rerun Stage 1.5."
    )
  )
}

hcl_file <- hcl_candidates[1]
message("Using HCL workbook: ", hcl_file)

# -------------------------------------------------------------------
# A. Read the actual ContractData table
# -------------------------------------------------------------------
contract_raw <- readxl::read_excel(hcl_file, sheet = "ContractData")
original_names <- names(contract_raw)
contract <- contract_raw
names(contract) <- janitor::make_clean_names(names(contract))

write_csv(
  tibble(
    column_original = original_names,
    column_clean = names(contract),
    class = map_chr(contract, ~ paste(class(.x), collapse = "/")),
    n = nrow(contract),
    n_missing = map_int(
      contract,
      ~ sum(is.na(.x) | (is.character(.x) & trimws(.x) == ""))
    ),
    pct_missing = 100 * n_missing / n
  ),
  file.path(outdir, "01_contractdata_column_audit.csv")
)

# -------------------------------------------------------------------
# B. Read ReadMe WITHOUT headers and search for official definitions
# -------------------------------------------------------------------
readme <- readxl::read_excel(
  hcl_file,
  sheet = "ReadMe",
  col_names = FALSE
)

# ReadMe columns have mixed Excel types (character/numeric/logical).
# Convert every source column to character BEFORE pivot_longer() so vctrs
# does not try to combine incompatible column types.
readme_long <- readme %>%
  mutate(across(everything(), ~ as.character(.x))) %>%
  mutate(readme_row = row_number()) %>%
  pivot_longer(
    cols = -readme_row,
    names_to = "readme_col",
    values_to = "text",
    values_transform = list(text = as.character)
  ) %>%
  filter(!is.na(text), str_trim(text) != "")

pricing_terms <- c(
  "interest", "interest rate", "spread", "margin",
  "maturity", "tenor", "grace", "repayment",
  "amortization", "principal", "fee", "commitment fee",
  "arrangement fee", "management fee", "penalty"
)

legal_terms <- c(
  "collateral", "escrow", "guarantee", "guarantor",
  "cross-default", "cross default", "acceleration",
  "termination", "sovereign immunity",
  "governing law", "jurisdiction", "arbitration",
  "Paris Club", "confidentiality"
)

search_readme <- function(terms, category) {
  pat <- paste(str_replace_all(terms, "([\\.\\+\\*\\?\\[\\]\\^\\$\\(\\)\\{\\}=!<>\\|:\\-])", "\\\\\\1"),
               collapse = "|")
  readme_long %>%
    filter(str_detect(text, regex(pat, ignore_case = TRUE))) %>%
    mutate(category = category) %>%
    select(category, readme_row, readme_col, text)
}

readme_hits <- bind_rows(
  search_readme(pricing_terms, "pricing"),
  search_readme(legal_terms, "legal")
) %>%
  distinct() %>%
  arrange(category, readme_row, readme_col)

write_csv(
  readme_long,
  file.path(outdir, "02_readme_raw_long.csv")
)

write_csv(
  readme_hits,
  file.path(outdir, "03_readme_pricing_legal_hits.csv")
)

# -------------------------------------------------------------------
# C. Search the REAL ContractData column names for pricing fields
# -------------------------------------------------------------------
pricing_patterns <- tibble(
  role = c(
    "interest_rate", "spread_margin", "maturity", "tenor",
    "grace_period", "repayment", "amortization", "fees",
    "penalty_interest"
  ),
  pattern = c(
    "(^|_)interest(_|$)|interest_rate",
    "spread|margin|basis_point|bps",
    "maturity",
    "tenor|term_year|loan_term",
    "grace",
    "repayment",
    "amort",
    "fee",
    "penalty_interest"
  )
)

pricing_column_hits <- map_dfr(seq_len(nrow(pricing_patterns)), function(i) {
  hit <- names(contract)[
    str_detect(
      names(contract),
      regex(pricing_patterns$pattern[i], ignore_case = TRUE)
    )
  ]

  if (length(hit) == 0) {
    tibble(
      role = pricing_patterns$role[i],
      column = NA_character_,
      n_nonmissing = 0L,
      pct_nonmissing = 0
    )
  } else {
    map_dfr(hit, function(v) {
      x <- contract[[v]]
      nonmissing <- !(is.na(x) | (is.character(x) & trimws(x) == ""))
      tibble(
        role = pricing_patterns$role[i],
        column = v,
        n_nonmissing = sum(nonmissing),
        pct_nonmissing = 100 * mean(nonmissing)
      )
    })
  }
})

write_csv(
  pricing_column_hits,
  file.path(outdir, "04_pricing_column_hits.csv")
)

# Explicit warning if ordinary pricing variables are absent.
ordinary_pricing_present <- pricing_column_hits %>%
  filter(role %in% c(
    "interest_rate", "spread_margin", "maturity",
    "tenor", "grace_period", "repayment", "amortization", "fees"
  )) %>%
  filter(!is.na(column)) %>%
  nrow() > 0

# -------------------------------------------------------------------
# D. Audit variation of legal variables.
#     Do NOT recode yet; preserve actual values.
# -------------------------------------------------------------------
legal_vars <- c(
  "loan_contract",
  "hcl_sample",
  "contract_incomplete",
  "ppg_debt",
  "syndicated_commercial",
  "restructuring",
  "cofinancing_multilaterals",
  "cofinancing_and_syndicated",
  "guarantor",
  "collateral",
  "escrow_account",
  "reserve_account",
  "revenue_account",
  "confidentiality_general",
  "confidentiality_borrower",
  "confidentiality_lender",
  "paris_clause",
  "cross_default",
  "penalty_interest_rate",
  "governing_law",
  "jurisdiction",
  "arbitration"
)

legal_vars <- intersect(legal_vars, names(contract))

value_frequency <- map_dfr(legal_vars, function(v) {
  x <- contract[[v]]
  tibble(value = as.character(x)) %>%
    mutate(
      value = if_else(
        is.na(value) | str_trim(value) == "",
        "<MISSING>",
        value
      )
    ) %>%
    count(value, name = "n") %>%
    mutate(
      variable = v,
      pct = 100 * n / sum(n)
    ) %>%
    select(variable, value, n, pct)
})

write_csv(
  value_frequency,
  file.path(outdir, "05_legal_variable_value_frequencies.csv")
)

variation_summary <- value_frequency %>%
  filter(value != "<MISSING>") %>%
  group_by(variable) %>%
  summarise(
    n_distinct_nonmissing = n_distinct(value),
    modal_value = value[which.max(n)],
    modal_n = max(n),
    modal_pct = max(pct),
    .groups = "drop"
  ) %>%
  mutate(
    has_variation = n_distinct_nonmissing > 1,
    useful_binary_candidate =
      n_distinct_nonmissing == 2 & modal_pct < 99
  )

write_csv(
  variation_summary,
  file.path(outdir, "06_legal_variable_variation_summary.csv")
)

# -------------------------------------------------------------------
# E. Borrower-country / year / lender coverage
# -------------------------------------------------------------------
coverage_vars <- intersect(
  c("borrower_country", "year", "creditor_name", "creditor_type",
    "borrower_type", "contract_category", "ppg_debt"),
  names(contract)
)

for (v in coverage_vars) {
  tbl <- contract %>%
    count(.data[[v]], name = "n_contracts", sort = TRUE)
  write_csv(
    tbl,
    file.path(outdir, paste0("coverage_", v, ".csv"))
  )
}

if (all(c("borrower_country", "year") %in% names(contract))) {
  country_year <- contract %>%
    count(borrower_country, year, name = "n_contracts") %>%
    arrange(borrower_country, year)

  write_csv(
    country_year,
    file.path(outdir, "07_country_year_contract_counts.csv")
  )

  country_fe_feasibility <- contract %>%
    group_by(borrower_country) %>%
    summarise(
      n_contracts = n(),
      n_years = n_distinct(year),
      min_year = suppressWarnings(min(year, na.rm = TRUE)),
      max_year = suppressWarnings(max(year, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      usable_for_within_country =
        n_contracts >= 2 & n_years >= 2
    ) %>%
    arrange(desc(n_contracts))

  write_csv(
    country_fe_feasibility,
    file.path(outdir, "08_country_FE_feasibility.csv")
  )
}

# -------------------------------------------------------------------
# F. Recovery manifest keyed by AidData record IDs
# -------------------------------------------------------------------
manifest_vars <- intersect(
  c(
    "contract_id", "aid_data_record_id", "aid_data_parent_id",
    "year", "borrower_country", "borrower_name",
    "creditor_name", "creditor_type",
    "commitment_orig", "currency", "commitment_usd",
    "loan_contract", "contract_category", "ppg_debt",
    "project_title", "source"
  ),
  names(contract)
)

recovery_manifest <- contract %>%
  select(all_of(manifest_vars))

write_csv(
  recovery_manifest,
  file.path(outdir, "09_pricing_recovery_manifest.csv")
)

# -------------------------------------------------------------------
# G. Audit source field — useful for locating the original contract/project
# -------------------------------------------------------------------
if ("source" %in% names(contract)) {
  source_audit <- contract %>%
    transmute(
      contract_id = if ("contract_id" %in% names(contract)) contract_id else NA,
      aid_data_record_id =
        if ("aid_data_record_id" %in% names(contract)) aid_data_record_id else NA,
      source = as.character(source),
      has_url = str_detect(source, "https?://"),
      extracted_url = str_extract(source, "https?://[^\\s,;\\)\\]]+")
    )

  write_csv(
    source_audit,
    file.path(outdir, "10_source_url_audit.csv")
  )
}

# -------------------------------------------------------------------
# H. Produce an automatic Stage 1.5 decision summary
# -------------------------------------------------------------------
important_legal <- intersect(
  c("collateral", "guarantor", "escrow_account", "cross_default",
    "governing_law", "arbitration"),
  variation_summary$variable
)

important_variation <- variation_summary %>%
  filter(variable %in% important_legal)

n_contracts <- nrow(contract)
n_record_ids <- if ("aid_data_record_id" %in% names(contract)) {
  sum(!is.na(contract$aid_data_record_id) &
        trimws(as.character(contract$aid_data_record_id)) != "")
} else NA_integer_

lines <- c(
  "# Paper 1 — Stage 1.5 Pricing Recovery Audit",
  "",
  paste0("- HCL contracts read: ", n_contracts),
  paste0("- Contracts with AidData record ID: ", n_record_ids),
  paste0("- Ordinary pricing fields found directly in ContractData: ",
         ifelse(ordinary_pricing_present, "YES", "NO")),
  "",
  "## Legal-variable variation",
  if (nrow(important_variation) > 0) {
    paste0(
      "- ", important_variation$variable,
      ": distinct=", important_variation$n_distinct_nonmissing,
      ", modal share=", round(important_variation$modal_pct, 1), "%"
    )
  } else {
    "- No target legal variables available."
  },
  "",
  "## Decision rule",
  "1. If ordinary interest/maturity/grace fields are absent, do NOT use penalty_interest_rate as the normal loan rate.",
  "2. Use 09_pricing_recovery_manifest.csv to recover price and maturity data from linked AidData/project/contract sources.",
  "3. Only create binary legal indicators after inspecting 05_legal_variable_value_frequencies.csv.",
  "4. Country fixed effects are credible only for countries flagged usable_for_within_country in 08_country_FE_feasibility.csv."
)

writeLines(
  lines,
  file.path(outdir, "STAGE1_5_SUMMARY.md")
)

message(paste(lines, collapse = "\n"))
