suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3d_legal_architecture"

d <- read_csv(
  file.path(
    outdir,
    "07_legal_architecture_analysis_dataset.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

country_var <- c(
  "borrower_country_stage3c",
  "borrower_country",
  "country"
)[
  c(
    "borrower_country_stage3c",
    "borrower_country",
    "country"
  ) %in% names(d)
][1]

if (is.na(country_var)) {
  stop("Borrower country field missing.", call. = FALSE)
}

gpr_vars <- intersect(
  c(
    "gpr_ai_all_z",
    "gpr_ai_initiator_z",
    "gpr_ai_respondent_z",
    "gpr_ai_spillover_z"
  ),
  names(d)
)

if (length(gpr_vars) == 0) {
  stop("Standardized country GPR variables missing.", call. = FALSE)
}

# Binary outcomes that are safe to estimate with LPM.
legal_outcomes <- intersect(
  c(
    "collateral_bin",
    "escrow_bin",
    "guarantor_bin",
    "cross_default_bin",
    "governing_law_present",
    "arbitration_present"
  ),
  names(d)
)

controls <- intersect(
  c(
    "log_loan_amount",
    "maturity_years",
    "grace_period_years",
    "creditor_type"
  ),
  names(d)
)

res <- list()

for (y in legal_outcomes) {

  for (g in gpr_vars) {

    dd <- d %>%
      filter(
        main_sample == 1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        !is.na(year)
      )

    if (nrow(dd) < 40) next
    if (n_distinct(dd[[y]], na.rm = TRUE) < 2) next

    rhs <- paste(
      c(g, controls),
      collapse = " + "
    )

    f <- as.formula(
      paste0(
        y,
        " ~ ",
        rhs,
        " | ",
        country_var,
        " + year"
      )
    )

    m <- tryCatch(
      feols(
        f,
        data = dd,
        vcov = as.formula(
          paste0("~", country_var)
        )
      ),
      error = function(e) NULL
    )

    if (is.null(m)) next

    nm <- paste(y, g, sep = "__")

    res[[nm]] <- broom::tidy(
      m,
      conf.int = TRUE
    ) %>%
      mutate(
        outcome = y,
        gpr_measure = g,
        nobs = nobs(m),
        n_country_clusters =
          n_distinct(dd[[country_var]]),
        model = "legal_architecture_lpm_twfe_cluster_country",
        .before = 1
      )
  }
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(
    outdir,
    "08_legal_architecture_twfe_results.csv"
  )
)

if (nrow(ans) > 0) {

  write_csv(
    ans %>%
      filter(term %in% gpr_vars),
    file.path(
      outdir,
      "09_legal_architecture_gpr_coefficients.csv"
    )
  )

} else {

  write_csv(
    tibble(
      note = "No legal architecture model could be estimated."
    ),
    file.path(
      outdir,
      "09_legal_architecture_gpr_coefficients.csv"
    )
  )
}

message("38_run_legal_architecture_twfe.R completed.")
