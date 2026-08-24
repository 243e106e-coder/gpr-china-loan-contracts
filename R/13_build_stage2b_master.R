suppressPackageStartupMessages({
  library(readr); library(dplyr)
})
outdir <- "outputs/stage2b_harmonization"
base <- read_csv("outputs/stage2a_pricing_recovery/11_stage2a_analysis_sample.csv",
                 show_col_types=FALSE, progress=FALSE)
p <- read_csv(file.path(outdir,"04_harmonized_pricing.csv"),
              show_col_types=FALSE, progress=FALSE)
l <- read_csv(file.path(outdir,"06_legal_coding_conservative.csv"),
              show_col_types=FALSE, progress=FALSE)

pkeep <- c("contract_id",setdiff(names(p),names(base)))
lkeep <- c("contract_id",setdiff(names(l),names(base)))

master <- base %>%
  left_join(p %>% select(all_of(unique(pkeep))), by="contract_id") %>%
  left_join(l %>% select(all_of(unique(lkeep))), by="contract_id")

if(nrow(master)!=nrow(base)) stop("Stage 2B merge changed row count.")

write_csv(master,file.path(outdir,"09_stage2b_master_dataset.csv"))

# IMPORTANT: do not construct Neither/PricingOnly/ContractOnly/Both yet.
# Those are RESPONSE categories and require an explicit empirical definition
# after GPR exposure is merged and the treatment/response threshold is chosen.
decision <- tibble(
  item=c(
    "Pricing harmonized",
    "Binary legal terms coded",
    "Governing law categories",
    "Arbitration categories",
    "Joint risk-allocation outcome"
  ),
  status=c(
    "READY",
    "READY",
    "MANUAL REVIEW REQUIRED",
    "MANUAL REVIEW REQUIRED",
    "NOT YET DEFINED"
  ),
  reason=c(
    "Uses interest_at_t0, with fixed-rate fallback only.",
    "Uses HCL's existing 0/1 coding.",
    "Do not automatically label PRC/GBR/USA/etc. as stronger or weaker protection.",
    "Institution names and forums require substantive legal classification.",
    "Neither/PricingOnly/ContractOnly/Both cannot be inferred merely from whether a field is observed."
  )
)
write_csv(decision,file.path(outdir,"10_stage2b_decision_table.csv"))
