suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

outdir <- "outputs/stage3b_identification"

d <- read_csv(
  file.path(
    outdir,
    "06_contracts_with_country_gpr.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

if (
  !all(
    c(
      "legal_any",
      "creditor_type"
    ) %in% names(d)
  )
) {

  warning(
    "legal_any or creditor_type absent; separation audit skipped."
  )

  write_csv(
    tibble(
      note = "legal_any or creditor_type absent."
    ),
    file.path(
      outdir,
      "13_legal_logit_separation_risk.csv"
    )
  )

} else {

  dd <- d

  if ("main_sample" %in% names(dd)) {
    dd <- dd %>%
      filter(main_sample == 1)
  }

  tab <- dd %>%
    filter(
      !is.na(creditor_type),
      !is.na(legal_any)
    ) %>%
    count(
      creditor_type,
      legal_any,
      name = "n"
    ) %>%
    complete(
      creditor_type,
      legal_any = c(0, 1),
      fill = list(n = 0)
    ) %>%
    arrange(
      creditor_type,
      legal_any
    )

  write_csv(
    tab,
    file.path(
      outdir,
      "12_legal_any_by_creditor_type.csv"
    )
  )

  sep <- tab %>%
    group_by(creditor_type) %>%
    summarise(
      n0 = sum(
        n[legal_any == 0]
      ),
      n1 = sum(
        n[legal_any == 1]
      ),
      separation_risk =
        (n0 == 0 | n1 == 0),
      .groups = "drop"
    )

  write_csv(
    sep,
    file.path(
      outdir,
      "13_legal_logit_separation_risk.csv"
    )
  )
}

message("24_legal_separation_diagnostics.R completed.")
