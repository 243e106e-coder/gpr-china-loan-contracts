suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3d_legal_architecture"

law_file <- file.path(
  outdir,
  "04_governing_law_manual_review.csv"
)

arb_file <- file.path(
  outdir,
  "05_arbitration_manual_review.csv"
)

rows <- list()

if (file.exists(law_file)) {

  x <- read_csv(
    law_file,
    show_col_types = FALSE
  )

  rows[[length(rows)+1]] <- tibble(
    component = "governing_law",
    categories = nrow(x),
    manually_classified = if (
      "manual_law_family" %in% names(x)
    ) {
      sum(!is.na(x$manual_law_family))
    } else 0
  )
}

if (file.exists(arb_file)) {

  x <- read_csv(
    arb_file,
    show_col_types = FALSE
  )

  rows[[length(rows)+1]] <- tibble(
    component = "arbitration",
    categories = nrow(x),
    manually_classified = if (
      "manual_forum_type" %in% names(x)
    ) {
      sum(!is.na(x$manual_forum_type))
    } else 0
  )
}

status <- bind_rows(rows) %>%
  mutate(
    pct_classified =
      100 * manually_classified / categories,
    ready_for_substantive_legal_model =
      manually_classified == categories &
      categories > 0
  )

write_csv(
  status,
  file.path(
    outdir,
    "10_manual_classification_readiness.csv"
  )
)

message("39_manual_classification_readiness.R completed.")
