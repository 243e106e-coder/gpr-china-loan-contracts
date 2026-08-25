suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3c_robustness"
d <- read_csv(file.path(outdir,"02_stage3c_analysis_data.csv"),show_col_types=FALSE,progress=FALSE)

country_var <- "borrower_country_stage3c"
g <- "gpr_ai_all_z"

exclusions <- list(
  exclude_2001=2001,
  exclude_gfc_2008_2009=c(2008,2009),
  exclude_2014_2015=c(2014,2015),
  exclude_covid_2020_2021=c(2020,2021),
  exclude_ukraine_2022_2023=c(2022,2023)
)

outcomes <- list(
  pricing_rate_t0="est3c_pricing",
  maturity_years="est3c_maturity",
  grace_period_years="est3c_grace"
)

res <- list()

for(ex_name in names(exclusions)) {
  yrs <- exclusions[[ex_name]]
  for(y in names(outcomes)) {
    flag <- outcomes[[y]]
    dd <- d %>% filter(.data[[flag]]==1,!is.na(.data[[g]]),!(year %in% yrs))
    if(nrow(dd)<40) next

    f <- as.formula(paste0(y," ~ ",g," | ",country_var," + year"))
    m <- tryCatch(feols(f,data=dd,vcov=as.formula(paste0("~",country_var))),error=function(e) NULL)
    if(is.null(m)) next

    res[[paste(ex_name,y,sep="__")]] <- broom::tidy(m,conf.int=TRUE) %>%
      filter(term==g) %>%
      mutate(
        exclusion=ex_name,
        excluded_years=paste(yrs,collapse=";"),
        outcome=y,
        nobs=nobs(m),
        .before=1
      )
  }
}

write_csv(bind_rows(res),file.path(outdir,"12_crisis_exclusion_results.csv"))
message("31_crisis_exclusion_tests.R completed.")
