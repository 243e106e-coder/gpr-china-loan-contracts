suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
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

country_candidates <- c(
  "borrower_country",
  "country"
)

country_var <- country_candidates[
  country_candidates %in% names(d)
][1]

if (is.na(country_var)) {
  stop(
    "Borrower country unavailable for TWFE.",
    call. = FALSE
  )
}

gpr_vars <- intersect(
  c(
    "gpr_ai_all",
    "gpr_ai_initiator",
    "gpr_ai_respondent",
    "gpr_ai_spillover"
  ),
  names(d)
)

if (length(gpr_vars) == 0) {
  stop(
    "No country-specific GPR measures were merged.",
    call. = FALSE
  )
}

outcomes <- list(
  pricing_rate_t0 = "est_pricing",
  maturity_years = "est_maturity",
  grace_period_years = "est_grace",
  legal_protection_count = "est_legal_count"
)

res <- list()

for (g in gpr_vars) {

  for (y in names(outcomes)) {

    flag <- outcomes[[y]]

    dd <- d %>%
      filter(
        .data[[flag]] == 1,
        !is.na(.data[[g]]),
        !is.na(.data[[y]]),
        !is.na(.data[[country_var]]),
        !is.na(year)
      )

    if (nrow(dd) < 30) next

    f <- as.formula(
      paste0(
        y,
        " ~ ",
        g,
        " | ",
        country_var,
        " + year"
      )
    )

    m <- feols(
      f,
      data = dd,
      vcov = "hetero"
    )

    nm <- paste(
      y,
      g,
      sep = "__"
    )

    res[[nm]] <- broom::tidy(
      m,
      conf.int = TRUE
    ) %>%
      mutate(
        outcome = y,
        gpr_measure = g,
        nobs = nobs(m),
        model = "country_gpr_twfe",
        .before = 1
      )
  }
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(
    outdir,
    "10_country_gpr_twfe_results.csv"
  )
)

if (nrow(ans) > 0) {

  coef <- ans %>%
    filter(
      term %in% gpr_vars
    )

  write_csv(
    coef,
    file.path(
      outdir,
      "11_country_gpr_twfe_coefficients.csv"
    )
  )

} else {

  write_csv(
    tibble(
      note = "No country-GPR TWFE model had enough usable observations."
    ),
    file.path(
      outdir,
      "11_country_gpr_twfe_coefficients.csv"
    )
  )
}

message("23_run_country_gpr_twfe.R completed.")
