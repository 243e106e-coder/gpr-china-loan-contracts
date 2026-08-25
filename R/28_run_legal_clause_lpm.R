suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3c_robustness"
d <- read_csv(file.path(outdir,"02_stage3c_analysis_data.csv"),show_col_types=FALSE,progress=FALSE)
country_var <- "borrower_country_stage3c"

legal_vars <- intersect(
  c("collateral_bin","escrow_bin","guarantor_bin","cross_default_bin"),
  names(d)
)

if(length(legal_vars)==0) {
  write_csv(tibble(note="No legal binary clause variables found."),
            file.path(outdir,"06_legal_clause_lpm_results.csv"))
} else {
  gpr_vars <- c("gpr_ai_all_z","gpr_ai_initiator_z","gpr_ai_respondent_z","gpr_ai_spillover_z")
  controls <- intersect(c("log_loan_amount","maturity_years","grace_period_years","creditor_type"),names(d))
  res <- list()

  for(y in legal_vars) {
    for(g in gpr_vars) {
      dd <- d %>% filter(main_sample==1,!is.na(.data[[y]]),!is.na(.data[[g]]),!is.na(.data[[country_var]]),!is.na(year))
      if(nrow(dd)<40 || n_distinct(dd[[y]],na.rm=TRUE)<2) next

      rhs <- paste(c(g,controls),collapse=" + ")
      f <- as.formula(paste0(y," ~ ",rhs," | ",country_var," + year"))
      m <- feols(f,data=dd,vcov=as.formula(paste0("~",country_var)))

      nm <- paste(y,g,sep="__")
      res[[nm]] <- broom::tidy(m,conf.int=TRUE) %>%
        mutate(
          model="legal_clause_lpm_twfe_cluster_country",
          outcome=y,
          gpr_measure=g,
          nobs=nobs(m),
          n_country_clusters=n_distinct(dd[[country_var]]),
          .before=1
        )
    }
  }

  ans <- bind_rows(res)
  write_csv(ans,file.path(outdir,"06_legal_clause_lpm_results.csv"))
  if(nrow(ans)>0) {
    write_csv(ans %>% filter(term %in% gpr_vars),
              file.path(outdir,"07_legal_clause_gpr_coefficients.csv"))
  }
}
message("28_run_legal_clause_lpm.R completed.")
