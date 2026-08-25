suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3e_legal_choice"
d <- read_csv(file.path(outdir,"05_contracts_legal_choice_classified.csv"),
              show_col_types=FALSE,progress=FALSE)

country_var <- c("borrower_country_stage3c","borrower_country","country")[
  c("borrower_country_stage3c","borrower_country","country") %in% names(d)
][1]
if(is.na(country_var)) stop("Borrower country missing.")

gpr_vars <- intersect(
  c("gpr_ai_all_z","gpr_ai_initiator_z","gpr_ai_respondent_z","gpr_ai_spillover_z"),
  names(d)
)

outcomes <- intersect(
  c("law_prc","law_borrower","law_third_party","law_english","law_us_newyork"),
  names(d)
)

controls <- intersect(
  c("log_loan_amount","maturity_years","grace_period_years","creditor_type"),
  names(d)
)

res <- list()

for(y in outcomes) {
  for(g in gpr_vars) {
    dd <- d %>%
      filter(
        main_sample==1,
        governing_law_choice_usable==1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        !is.na(year)
      )

    if(nrow(dd)<40 || n_distinct(dd[[y]],na.rm=TRUE)<2) next

    rhs <- paste(c(g,controls),collapse=" + ")
    f <- as.formula(paste0(y," ~ ",rhs," | ",country_var," + year"))

    m <- tryCatch(
      feols(f,data=dd,vcov=as.formula(paste0("~",country_var))),
      error=function(e) NULL
    )
    if(is.null(m)) next

    nm <- paste(y,g,sep="__")
    res[[nm]] <- broom::tidy(m,conf.int=TRUE) %>%
      mutate(
        model="governing_law_choice_lpm_twfe",
        outcome=y,
        gpr_measure=g,
        nobs=nobs(m),
        n_country_clusters=n_distinct(dd[[country_var]]),
        .before=1
      )
  }
}

ans <- bind_rows(res)
write_csv(ans,file.path(outdir,"07_governing_law_choice_results.csv"))

if(nrow(ans)>0) {
  write_csv(
    ans %>% filter(term %in% gpr_vars),
    file.path(outdir,"08_governing_law_gpr_coefficients.csv")
  )
}

message("43_run_governing_law_choice_models.R completed.")
