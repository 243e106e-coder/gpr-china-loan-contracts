suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

outdir <- "outputs/stage3d_legal_architecture"
d <- read_csv(
  "outputs/stage3d_legal_architecture/03_legal_architecture_contract_snapshot.csv",
  show_col_types = FALSE,
  progress = FALSE
)

# ------------------------------------------------------------
# Governing law manual review
# ------------------------------------------------------------

law_var <- c(
  "governing_law_raw",
  "governing_law"
)[c("governing_law_raw","governing_law") %in% names(d)][1]

if (!is.na(law_var)) {

  law_review <- d %>%
    transmute(
      governing_law_raw = as.character(.data[[law_var]])
    ) %>%
    filter(
      !is.na(governing_law_raw),
      str_trim(governing_law_raw) != ""
    ) %>%
    count(
      governing_law_raw,
      sort = TRUE,
      name = "n_contracts"
    ) %>%
    mutate(
      manual_law_family = NA_character_,
      manual_jurisdiction = NA_character_,
      manual_home_host_third = NA_character_,
      manual_common_civil_other = NA_character_,
      manual_enforcement_orientation = NA_character_,
      reviewer_notes = NA_character_
    )

  write_csv(
    law_review,
    file.path(outdir, "04_governing_law_manual_review.csv")
  )
}

# ------------------------------------------------------------
# Arbitration manual review
# ------------------------------------------------------------

arb_var <- c(
  "arbitration_raw",
  "arbitration"
)[c("arbitration_raw","arbitration") %in% names(d)][1]

if (!is.na(arb_var)) {

  arb_review <- d %>%
    transmute(
      arbitration_raw = as.character(.data[[arb_var]])
    ) %>%
    filter(
      !is.na(arbitration_raw),
      str_trim(arbitration_raw) != ""
    ) %>%
    count(
      arbitration_raw,
      sort = TRUE,
      name = "n_contracts"
    ) %>%
    mutate(
      manual_forum_type = NA_character_,
      manual_forum_name = NA_character_,
      manual_seat_country = NA_character_,
      manual_domestic_international = NA_character_,
      manual_third_country = NA_character_,
      reviewer_notes = NA_character_
    )

  write_csv(
    arb_review,
    file.path(outdir, "05_arbitration_manual_review.csv")
  )
}

message("35_build_manual_legal_review_templates.R completed.")
