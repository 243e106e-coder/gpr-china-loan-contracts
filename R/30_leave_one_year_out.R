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

years <- sort(unique(d$year[!is.na(d$year)]))
res <- list()

for(y in names(outcomes)) {
  flag <- outcomes[[y]]
  for(drop_year in years) {
    dd <- d %>% filter(.data[[flag]]==1,!is.na(.data[[g]]),year!=drop_year)
    if(nrow(dd)<40) next

    f <- as.formula(paste0(y," ~ ",g," | ",country_var," + year"))
    m <- tryCatch(feols(f,data=dd,vcov=as.formula(paste0("~",country_var))),error=function(e) NULL)
    if(is.null(m) || !(g %in% names(coef(m)))) next

    res[[paste(y,drop_year,sep="__")]] <- tibble(
      outcome=y,
      dropped_year=drop_year,
      estimate=as.numeric(coef(m)[g]),
      std_error=sqrt(vcov(m)[g,g]),
      nobs=nobs(m)
    )
  }
}

ans <- bind_rows(res)
write_csv(ans,file.path(outdir,"10_leave_one_year_out.csv"))

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

write_csv(summ,file.path(outdir,"11_leave_one_year_out_summary.csv"))
message("30_leave_one_year_out.R completed.")
