suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
})

out_dir <- "outputs/stage2a_pricing_recovery"

# Locate HCL workbook
hcl_candidates <- list.files(
  "data/raw/hcl2",
  pattern = "How_China_Lends_Dataset_Version_2_0\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(hcl_candidates)==0) stop("HCL workbook missing.")

hcl <- readxl::read_excel(hcl_candidates[1], sheet="ContractData")
names(hcl) <- janitor::make_clean_names(names(hcl))

# Main estimation sample hierarchy. Preserve all rows and flag samples.
sample <- hcl %>%
  mutate(
    sample_all_371 = 1L,
    sample_loan_agreement = as.integer(contract_category == "Loan Agreement"),
    sample_ppg = as.integer(ppg_debt == 1),
    sample_loan_ppg = as.integer(
      contract_category == "Loan Agreement" & ppg_debt == 1
    ),
    sample_policy_bank = as.integer(creditor_type == "Policy Bank"),
    sample_loan_ppg_policybank = as.integer(
      contract_category == "Loan Agreement" &
        ppg_debt == 1 &
        creditor_type == "Policy Bank"
    ),
    sample_exclude_restructuring = as.integer(
      !str_detect(
        tolower(ifelse(is.na(contract_category), "", contract_category)),
        "reschedul|restructur|swap|deed of security|mortgage"
      )
    )
  )

# Merge recovered pricing if available
pricing_file <- file.path(out_dir, "10_recovered_pricing_dataset.csv")
if (file.exists(pricing_file)) {
  pricing <- read_csv(pricing_file, show_col_types=FALSE)

  # Avoid duplicating HCL columns; keep only recovered fields not already in HCL plus contract_id.
  pkeep <- c(
    "contract_id",
    setdiff(names(pricing), names(hcl))
  )
  pkeep <- unique(intersect(pkeep, names(pricing)))

  sample <- sample %>%
    left_join(pricing %>% select(all_of(pkeep)), by="contract_id")
}

write_csv(sample, file.path(out_dir, "11_stage2a_analysis_sample.csv"))

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
    nrow(sample),
    sum(sample$sample_loan_agreement, na.rm=TRUE),
    sum(sample$sample_ppg, na.rm=TRUE),
    sum(sample$sample_loan_ppg, na.rm=TRUE),
    sum(sample$sample_policy_bank, na.rm=TRUE),
    sum(sample$sample_loan_ppg_policybank, na.rm=TRUE),
    sum(sample$sample_exclude_restructuring, na.rm=TRUE)
  )
)

write_csv(sample_counts, file.path(out_dir, "12_baseline_sample_counts.csv"))
print(sample_counts)
