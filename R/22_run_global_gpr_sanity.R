suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3b_identification"

d <- read_csv(
  "outputs/stage3_gpr_baseline/07_stage3_estimation_sample.csv",
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

outcomes <- list(
  pricing_rate_t0 = "est_pricing",
  maturity_years = "est_maturity",
  grace_period_years = "est_grace",
  legal_protection_count = "est_legal_count"
)

res <- list()

for (y in names(outcomes)) {

  flag <- outcomes[[y]]

  dd <- d %>%
    filter(
      .data[[flag]] == 1,
      !is.na(gpr),
      !is.na(.data[[y]])
    )

  if (nrow(dd) < 20) next

  if (!is.na(country_var)) {
    f <- as.formula(
      paste0(
        y,
        " ~ gpr | ",
        country_var
      )
    )
  } else {
    f <- as.formula(
      paste0(
        y,
        " ~ gpr"
      )
    )
  }

  m <- feols(
    f,
    data = dd,
    vcov = "hetero"
  )

  res[[y]] <- broom::tidy(
    m,
    conf.int = TRUE
  ) %>%
    mutate(
      outcome = y,
      nobs = nobs(m),
      model = "global_gpr_sanity",
      .before = 1
    )
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(
    outdir,
    "09_global_gpr_sanity_results.csv"
  )
)

message("22_run_global_gpr_sanity.R completed.")
