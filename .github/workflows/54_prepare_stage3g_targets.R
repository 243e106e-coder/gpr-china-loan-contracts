suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3g_final_inference"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

fin_file <- "outputs/stage3c_robustness/02_stage3c_analysis_data.csv"
legal_file <- "outputs/stage3f_legal_validation/01_stage3f_analysis_data.csv"

if(!file.exists(fin_file)) stop("Stage 3C financial data missing.")
if(!file.exists(legal_file)) stop("Stage 3F legal data missing.")

fin <- read_csv(fin_file, show_col_types=FALSE, progress=FALSE)
legal <- read_csv(legal_file, show_col_types=FALSE, progress=FALSE)

# Harmonize country ID
fin_country <- c("borrower_country_stage3c","borrower_country","country")[
  c("borrower_country_stage3c","borrower_country","country") %in% names(fin)
][1]

legal_country <- c("borrower_country_stage3f","borrower_country_stage3c","borrower_country","country")[
  c("borrower_country_stage3f","borrower_country_stage3c","borrower_country","country") %in% names(legal)
][1]

if(is.na(fin_country) || is.na(legal_country)) stop("Borrower-country identifier missing.")

fin <- fin %>% mutate(borrower_country_final=as.character(.data[[fin_country]]))
legal <- legal %>% mutate(borrower_country_final=as.character(.data[[legal_country]]))

write_csv(fin,file.path(outdir,"01_financial_final_data.csv"))
write_csv(legal,file.path(outdir,"02_legal_final_data.csv"))

targets <- tibble(
  block = c("financial","financial","legal","legal","legal"),
  hypothesis_id = c("F1","F2","L2","L3","L4"),
  outcome = c(
    "pricing_rate_t0",
    "maturity_years",
    "law_borrower",
    "arb_mainland_china",
    "arb_international_third"
  ),
  gpr_measure = c(
    "gpr_ai_all_z",
    "gpr_ai_all_z",
    "gpr_ai_initiator_z",
    "gpr_ai_respondent_z",
    "gpr_ai_respondent_z"
  ),
  expected_sign = c("+","-","-","-","+"),
  interpretation = c(
    "All GPR -> pricing",
    "All GPR -> maturity",
    "Initiator GPR -> borrower-home governing law",
    "Respondent GPR -> mainland-China arbitration",
    "Respondent GPR -> international/third-party arbitration"
  )
)

write_csv(targets,file.path(outdir,"03_final_hypotheses.csv"))

message("54_prepare_stage3g_targets.R completed.")
