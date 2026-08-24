# R/08_build_baseline_sample.R
# Paper 1 — Stage 2A
# Build baseline estimation samples after exact HCL–CLG matching.
# Revised to avoid the tibble() name-shadowing bug caused by using `sample`
# both as a dataframe object and as an output column name.

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
})

out_dir <- "outputs/stage2a_pricing_recovery"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# Locate HCL workbook
# -------------------------------------------------------------------

hcl_candidates <- list.files(
  "data/raw/hcl2",
  pattern = "How_China_Lends_Dataset_Version_2_0\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(hcl_candidates) == 0) {
  stop(
    "HCL workbook missing under data/raw/hcl2.",
    call. = FALSE
  )
}

hcl_file <- hcl_candidates[[1]]

message("Using HCL workbook: ", hcl_file)

hcl <- readxl::read_excel(
  hcl_file,
  sheet = "ContractData"
)

names(hcl) <- janitor::make_clean_names(names(hcl))

# -------------------------------------------------------------------
# Defensive checks for fields used in sample definitions
# -------------------------------------------------------------------

required_sample_vars <- c(
  "contract_id",
  "contract_category",
  "ppg_debt",
  "creditor_type"
)

missing_vars <- setdiff(
  required_sample_vars,
  names(hcl)
)

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Required HCL variables are missing: ",
      paste(missing_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

# -------------------------------------------------------------------
# Build estimation-sample flags
# -------------------------------------------------------------------
# IMPORTANT:
# Use `analysis_sample` as dataframe name.
# Do NOT name the dataframe `sample`, because later tibble(sample = ...)
# would shadow that object inside tibble evaluation.

analysis_sample <- hcl %>%
  mutate(
    sample_all_371 = 1L,

    sample_loan_agreement = as.integer(
      !is.na(contract_category) &
        contract_category == "Loan Agreement"
    ),

    sample_ppg = as.integer(
      !is.na(ppg_debt) &
        ppg_debt == 1
    ),

    sample_loan_ppg = as.integer(
      !is.na(contract_category) &
        contract_category == "Loan Agreement" &
        !is.na(ppg_debt) &
        ppg_debt == 1
    ),

    sample_policy_bank = as.integer(
      !is.na(creditor_type) &
        creditor_type == "Policy Bank"
    ),

    sample_loan_ppg_policybank = as.integer(
      !is.na(contract_category) &
        contract_category == "Loan Agreement" &
        !is.na(ppg_debt) &
        ppg_debt == 1 &
        !is.na(creditor_type) &
        creditor_type == "Policy Bank"
    ),

    sample_exclude_restructuring = as.integer(
      !str_detect(
        tolower(
          ifelse(
            is.na(contract_category),
            "",
            contract_category
          )
        ),
        "reschedul|restructur|swap|deed of security|mortgage"
      )
    )
  )

# -------------------------------------------------------------------
# Merge recovered pricing fields if Stage 2A exact-match recovery exists
# -------------------------------------------------------------------

pricing_file <- file.path(
  out_dir,
  "10_recovered_pricing_dataset.csv"
)

if (file.exists(pricing_file)) {

  message("Merging recovered CLG pricing fields from: ", pricing_file)

  pricing <- readr::read_csv(
    pricing_file,
    show_col_types = FALSE,
    progress = FALSE
  )

  if (!("contract_id" %in% names(pricing))) {
    warning(
      "Recovered pricing dataset has no contract_id; pricing merge skipped."
    )
  } else {

    # Keep only fields not already present in HCL,
    # plus contract_id as the exact merge key.
    pricing_extra <- setdiff(
      names(pricing),
      names(hcl)
    )

    pkeep <- unique(
      c(
        "contract_id",
        pricing_extra
      )
    )

    pkeep <- intersect(
      pkeep,
      names(pricing)
    )

    # Avoid accidental row multiplication if pricing data contain duplicate contract IDs.
    pricing_dup <- pricing %>%
      filter(!is.na(contract_id)) %>%
      count(
        contract_id,
        name = "n_rows"
      ) %>%
      filter(n_rows > 1) %>%
      arrange(desc(n_rows))

    write_csv(
      pricing_dup,
      file.path(
        out_dir,
        "10b_pricing_duplicate_contract_ids.csv"
      )
    )

    pricing_unique <- pricing %>%
      distinct(
        contract_id,
        .keep_all = TRUE
      ) %>%
      select(
        all_of(pkeep)
      )

    n_before <- nrow(analysis_sample)

    analysis_sample <- analysis_sample %>%
      left_join(
        pricing_unique,
        by = "contract_id"
      )

    n_after <- nrow(analysis_sample)

    if (n_after != n_before) {
      stop(
        paste0(
          "Unexpected row multiplication after pricing merge: ",
          n_before,
          " -> ",
          n_after
        ),
        call. = FALSE
      )
    }
  }

} else {

  warning(
    paste0(
      "Recovered pricing file not found: ",
      pricing_file,
      ". Baseline sample flags will still be produced."
    )
  )
}

# -------------------------------------------------------------------
# Save full analysis sample
# -------------------------------------------------------------------

analysis_sample_file <- file.path(
  out_dir,
  "11_stage2a_analysis_sample.csv"
)

write_csv(
  analysis_sample,
  analysis_sample_file
)

message(
  "Analysis sample written: ",
  analysis_sample_file,
  " | rows = ",
  nrow(analysis_sample)
)

# -------------------------------------------------------------------
# Baseline sample counts
# -------------------------------------------------------------------
# The output column can safely be named `sample` now because the dataframe
# itself is called `analysis_sample`.

sample_counts <- tibble(
  sample = c(
    "All HCL contracts",
    "Loan Agreement",
    "PPG debt",
    "Loan Agreement + PPG",
    "Policy Bank",
    "Loan Agreement + PPG + Policy Bank",
    "Exclude restructuring/security-only contracts"
  ),

  n = c(
    nrow(analysis_sample),

    sum(
      analysis_sample$sample_loan_agreement,
      na.rm = TRUE
    ),

    sum(
      analysis_sample$sample_ppg,
      na.rm = TRUE
    ),

    sum(
      analysis_sample$sample_loan_ppg,
      na.rm = TRUE
    ),

    sum(
      analysis_sample$sample_policy_bank,
      na.rm = TRUE
    ),

    sum(
      analysis_sample$sample_loan_ppg_policybank,
      na.rm = TRUE
    ),

    sum(
      analysis_sample$sample_exclude_restructuring,
      na.rm = TRUE
    )
  )
)

sample_counts <- sample_counts %>%
  mutate(
    pct_of_all = 100 * n / nrow(analysis_sample)
  )

write_csv(
  sample_counts,
  file.path(
    out_dir,
    "12_baseline_sample_counts.csv"
  )
)

print(sample_counts)

# -------------------------------------------------------------------
# Additional overlap diagnostics
# -------------------------------------------------------------------

sample_overlap <- analysis_sample %>%
  summarise(
    n_all = n(),

    n_loan_agreement =
      sum(sample_loan_agreement, na.rm = TRUE),

    n_ppg =
      sum(sample_ppg, na.rm = TRUE),

    n_loan_ppg =
      sum(sample_loan_ppg, na.rm = TRUE),

    n_policy_bank =
      sum(sample_policy_bank, na.rm = TRUE),

    n_loan_ppg_policybank =
      sum(
        sample_loan_ppg_policybank,
        na.rm = TRUE
      ),

    n_exclude_restructuring =
      sum(
        sample_exclude_restructuring,
        na.rm = TRUE
      )
  )

write_csv(
  sample_overlap,
  file.path(
    out_dir,
    "13_baseline_sample_overlap_summary.csv"
  )
)

cat("\n====================================\n")
cat("Stage 2A — Baseline Sample Builder\n")
cat("====================================\n")
cat("Total HCL contracts: ", nrow(analysis_sample), "\n", sep = "")
cat(
  "Loan Agreements: ",
  sum(analysis_sample$sample_loan_agreement, na.rm = TRUE),
  "\n",
  sep = ""
)
cat(
  "PPG debt: ",
  sum(analysis_sample$sample_ppg, na.rm = TRUE),
  "\n",
  sep = ""
)
cat(
  "Loan Agreement + PPG: ",
  sum(analysis_sample$sample_loan_ppg, na.rm = TRUE),
  "\n",
  sep = ""
)
cat(
  "Loan Agreement + PPG + Policy Bank: ",
  sum(
    analysis_sample$sample_loan_ppg_policybank,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)
cat("====================================\n\n")

message("R/08_build_baseline_sample.R completed successfully.")
