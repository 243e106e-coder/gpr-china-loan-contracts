suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

outdir <- "outputs/stage3d_legal_architecture"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

infile <- "outputs/stage3c_robustness/02_stage3c_analysis_data.csv"

if (!file.exists(infile)) {
  infile <- "outputs/stage3b_identification/06_contracts_with_country_gpr.csv"
}

if (!file.exists(infile)) {
  stop("No Stage 3B/3C analysis dataset found.", call. = FALSE)
}

d <- read_csv(infile, show_col_types = FALSE, progress = FALSE)

# ------------------------------------------------------------
# Audit likely legal/enforcement fields already present in HCL
# ------------------------------------------------------------

patterns <- list(
  governing_law = "governing.*law|applicable.*law|choice.*law",
  arbitration = "arbitr|dispute.*resolution",
  sovereign_immunity = "sovereign.*immun|waiver.*immun|immunity",
  enforcement = "enforc|remed|execution|judgment",
  confidentiality = "confidential|secrecy",
  stabilization = "stabil|change.*law|change.*legislation",
  cross_default = "cross.*default",
  collateral = "collateral|security",
  escrow = "escrow",
  guarantee = "guarant",
  acceleration = "accelerat",
  termination = "terminat",
  pari_passu = "pari.*passu",
  negative_pledge = "negative.*pledge"
)

candidate_rows <- list()

for (role in names(patterns)) {
  hit <- names(d)[
    str_detect(
      names(d),
      regex(patterns[[role]], ignore_case = TRUE)
    )
  ]

  if (length(hit) > 0) {
    for (v in hit) {
      candidate_rows[[length(candidate_rows)+1]] <- tibble(
        role = role,
        variable = v,
        class = paste(class(d[[v]]), collapse = "/"),
        n_nonmissing = sum(
          !is.na(d[[v]]) &
            trimws(as.character(d[[v]])) != ""
        ),
        n_distinct = n_distinct(d[[v]], na.rm = TRUE)
      )
    }
  }
}

candidate_df <- bind_rows(candidate_rows)

write_csv(
  candidate_df,
  file.path(outdir, "01_legal_field_candidates.csv")
)

# Full frequency table for every detected legal field
freq <- bind_rows(lapply(unique(candidate_df$variable), function(v) {
  d %>%
    count(
      value = as.character(.data[[v]]),
      sort = TRUE
    ) %>%
    mutate(
      variable = v,
      .before = 1
    )
}))

write_csv(
  freq,
  file.path(outdir, "02_legal_field_value_frequencies.csv")
)

# Snapshot of all detected legal fields by contract
keep <- unique(c(
  intersect(
    c(
      "contract_id",
      "aid_data_record_id",
      "aid_data_record_id_hcl",
      "borrower_country",
      "country",
      "year",
      "creditor_type",
      "main_sample"
    ),
    names(d)
  ),
  unique(candidate_df$variable)
))

write_csv(
  d %>% select(all_of(keep)),
  file.path(outdir, "03_legal_architecture_contract_snapshot.csv")
)

message("34_audit_legal_architecture_fields.R completed.")
