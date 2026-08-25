suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3f_legal_validation"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

infile <- "outputs/stage3e_legal_choice/05_contracts_legal_choice_classified.csv"
if(!file.exists(infile)) stop("Stage 3E classified legal-choice dataset missing.")

d <- read_csv(infile, show_col_types=FALSE, progress=FALSE)

required <- c(
  "main_sample","year",
  "law_prc","law_borrower",
  "arb_mainland_china","arb_international_third",
  "gpr_ai_initiator_z","gpr_ai_respondent_z"
)

miss <- setdiff(required,names(d))
if(length(miss)) stop("Missing Stage 3F variables: ",paste(miss,collapse=", "))

country_var <- c("borrower_country_stage3c","borrower_country","country")[
  c("borrower_country_stage3c","borrower_country","country") %in% names(d)
][1]

if(is.na(country_var)) stop("Borrower-country variable missing.")

d <- d %>%
  mutate(
    borrower_country_stage3f = as.character(.data[[country_var]])
  )

write_csv(
  d,
  file.path(outdir,"01_stage3f_analysis_data.csv")
)

targets <- tibble(
  hypothesis_id = c("H1","H2","H3","H4"),
  outcome = c(
    "law_prc",
    "law_borrower",
    "arb_mainland_china",
    "arb_international_third"
  ),
  gpr_measure = c(
    "gpr_ai_initiator_z",
    "gpr_ai_initiator_z",
    "gpr_ai_respondent_z",
    "gpr_ai_respondent_z"
  ),
  expected_sign = c("+","-","-","+"),
  interpretation = c(
    "Initiator GPR -> PRC governing law",
    "Initiator GPR -> borrower-home law",
    "Respondent GPR -> mainland-China arbitration",
    "Respondent GPR -> international/third-party arbitration"
  )
)

write_csv(
  targets,
  file.path(outdir,"02_confirmatory_hypotheses.csv")
)

message("47_prepare_stage3f_targets.R completed.")
