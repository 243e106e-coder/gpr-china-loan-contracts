suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3d_legal_architecture"

d <- read_csv(
  file.path(outdir, "06_legal_safe_coded_dataset.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

# ------------------------------------------------------------
# Optional merge of completed manual review files.
# If user has not filled them yet, workflow continues safely.
# ------------------------------------------------------------

law_review_file <- file.path(
  outdir,
  "04_governing_law_manual_review.csv"
)

arb_review_file <- file.path(
  outdir,
  "05_arbitration_manual_review.csv"
)

law_var <- c("governing_law_raw","governing_law")[
  c("governing_law_raw","governing_law") %in% names(d)
][1]

arb_var <- c("arbitration_raw","arbitration")[
  c("arbitration_raw","arbitration") %in% names(d)
][1]

if (!is.na(law_var) && file.exists(law_review_file)) {

  lr <- read_csv(
    law_review_file,
    show_col_types = FALSE
  )

  if (
    "governing_law_raw" %in% names(lr) &&
    any(!is.na(lr$manual_law_family))
  ) {

    d <- d %>%
      left_join(
        lr,
        by = setNames(
          "governing_law_raw",
          law_var
        )
      )
  }
}

if (!is.na(arb_var) && file.exists(arb_review_file)) {

  ar <- read_csv(
    arb_review_file,
    show_col_types = FALSE
  )

  if (
    "arbitration_raw" %in% names(ar) &&
    any(!is.na(ar$manual_forum_type))
  ) {

    d <- d %>%
      left_join(
        ar,
        by = setNames(
          "arbitration_raw",
          arb_var
        )
      )
  }
}

write_csv(
  d,
  file.path(outdir, "07_legal_architecture_analysis_dataset.csv")
)

message("37_merge_manual_legal_classifications.R completed.")
