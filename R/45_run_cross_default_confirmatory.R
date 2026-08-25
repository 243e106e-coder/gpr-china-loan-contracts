suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3e_legal_choice"
d <- read_csv(file.path(outdir,"05_contracts_legal_choice_classified.csv"),
              show_col_types=FALSE,progress=FALSE)

if(!("cross_default_bin" %in% names(d))) {
  write_csv(tibble(note="cross_default_bin absent."),
            file.path(outdir,"11_cross_default_confirmatory.csv"))
} else {
  country_var <- c("borrower_country_stage3c","borrower_country","country")[
    c("borrower_country_stage3c","borrower_country","country") %in% names(d)
  ][1]

  controls <- intersect(
    c("log_loan_amount","maturity_years","grace_period_years","creditor_type"),
    names(d)
  )

  gpr_vars <- intersect(
    c("gpr_ai_all_z","gpr_ai_initiator_z","gpr_ai_respondent_z","gpr_ai_spillover_z"),
    names(d)
  )

  res <- list()
  for(g in gpr_vars) {
    dd <- d %>% filter(
      main_sample==1,
      !is.na(cross_default_bin),
      !is.na(.data[[g]]),
      !is.na(.data[[country_var]]),
      !is.na(year)
    )

    rhs <- paste(c(g,controls),collapse=" + ")
    f <- as.formula(paste0("cross_default_bin ~ ",rhs," | ",country_var," + year"))
    m <- feols(f,data=dd,vcov=as.formula(paste0("~",country_var)))

    res[[g]] <- broom::tidy(m,conf.int=TRUE) %>%
      filter(term==g) %>%
      mutate(
        gpr_measure=g,
        nobs=nobs(m),
        n_country_clusters=n_distinct(dd[[country_var]]),
        .before=1
      )
  }

  write_csv(bind_rows(res),file.path(outdir,"11_cross_default_confirmatory.csv"))
}

message("45_run_cross_default_confirmatory.R completed.")
