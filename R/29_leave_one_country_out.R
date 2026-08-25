suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3c_robustness"
d <- read_csv(file.path(outdir,"02_stage3c_analysis_data.csv"),show_col_types=FALSE,progress=FALSE)

country_var <- "borrower_country_stage3c"
g <- "gpr_ai_all_z"
outcomes <- list(
  pricing_rate_t0="est3c_pricing",
  maturity_years="est3c_maturity",
  grace_period_years="est3c_grace"
)

countries <- sort(unique(d[[country_var]][!is.na(d[[country_var]])]))
res <- list()

for(y in names(outcomes)) {
  flag <- outcomes[[y]]
  for(drop_country in countries) {
    dd <- d %>% filter(.data[[flag]]==1,!is.na(.data[[g]]),.data[[country_var]]!=drop_country)
    if(nrow(dd)<40 || n_distinct(dd[[country_var]])<5) next

    f <- as.formula(paste0(y," ~ ",g," | ",country_var," + year"))
    m <- tryCatch(feols(f,data=dd,vcov=as.formula(paste0("~",country_var))),error=function(e) NULL)
    if(is.null(m) || !(g %in% names(coef(m)))) next

    res[[paste(y,drop_country,sep="__")]] <- tibble(
      outcome=y,
      dropped_country=drop_country,
      estimate=as.numeric(coef(m)[g]),
      std_error=sqrt(vcov(m)[g,g]),
      nobs=nobs(m),
      n_country_clusters=n_distinct(dd[[country_var]])
    )
  }
}

ans <- bind_rows(res)
write_csv(ans,file.path(outdir,"08_leave_one_country_out.csv"))

summ <- ans %>%
  group_by(outcome) %>%
  summarise(
    n_runs=n(),
    min_beta=min(estimate,na.rm=TRUE),
    max_beta=max(estimate,na.rm=TRUE),
    median_beta=median(estimate,na.rm=TRUE),
    share_positive=mean(estimate>0,na.rm=TRUE),
    share_negative=mean(estimate<0,na.rm=TRUE),
    .groups="drop"
  )

write_csv(summ,file.path(outdir,"09_leave_one_country_out_summary.csv"))
message("29_leave_one_country_out.R completed.")
