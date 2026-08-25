suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3c_robustness"
d <- read_csv(file.path(outdir,"02_stage3c_analysis_data.csv"),show_col_types=FALSE,progress=FALSE)

country_var <- "borrower_country_stage3c"
gpr_vars <- c("gpr_ai_all_z","gpr_ai_initiator_z","gpr_ai_respondent_z","gpr_ai_spillover_z")

outcomes <- list(
  pricing_rate_t0="est3c_pricing",
  maturity_years="est3c_maturity",
  grace_period_years="est3c_grace"
)

res <- list()
mods <- list()

for(g in gpr_vars) {
  for(y in names(outcomes)) {
    flag <- outcomes[[y]]
    controls <- c("log_loan_amount","creditor_type")
    if(y=="pricing_rate_t0") controls <- c(controls,"maturity_years","grace_period_years")
    if(y=="maturity_years") controls <- c(controls,"grace_period_years")
    if(y=="grace_period_years") controls <- c(controls,"maturity_years")
    controls <- controls[controls %in% names(d)]

    rhs <- paste(c(g,controls),collapse=" + ")
    f <- as.formula(paste0(y," ~ ",rhs," | ",country_var," + year"))

    dd <- d %>% filter(.data[[flag]]==1,!is.na(.data[[g]]),!is.na(.data[[country_var]]),!is.na(year))
    if(nrow(dd)<40) next

    m <- feols(f,data=dd,vcov=as.formula(paste0("~",country_var)))
    nm <- paste(y,g,sep="__")
    mods[[nm]] <- m
    res[[nm]] <- broom::tidy(m,conf.int=TRUE) %>%
      mutate(
        model="standardized_twfe_controls_cluster_country",
        outcome=y,
        gpr_measure=g,
        nobs=nobs(m),
        n_country_clusters=n_distinct(dd[[country_var]]),
        .before=1
      )
  }
}

ans <- bind_rows(res)
write_csv(ans,file.path(outdir,"03_standardized_twfe_full_results.csv"))
write_csv(ans %>% filter(term %in% gpr_vars),file.path(outdir,"04_standardized_gpr_coefficients.csv"))

sink(file.path(outdir,"05_standardized_model_summaries.txt"))
for(nm in names(mods)) {
  cat("\n====================\n",nm,"\n====================\n")
  print(summary(mods[[nm]]))
}
sink()
message("27_run_standardized_twfe_controls.R completed.")
