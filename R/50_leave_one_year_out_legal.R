suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3f_legal_validation"

d <- read_csv(
  file.path(outdir,"01_stage3f_analysis_data.csv"),
  show_col_types=FALSE,
  progress=FALSE
)

targets <- read_csv(
  file.path(outdir,"02_confirmatory_hypotheses.csv"),
  show_col_types=FALSE
)

country_var <- "borrower_country_stage3f"

controls <- intersect(
  c(
    "log_loan_amount",
    "maturity_years",
    "grace_period_years",
    "creditor_type"
  ),
  names(d)
)

years <- sort(unique(d$year[!is.na(d$year)]))
res <- list()

for(i in seq_len(nrow(targets))) {

  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  hid <- targets$hypothesis_id[i]

  for(drop_year in years) {

    dd <- d %>%
      filter(
        main_sample==1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        year!=drop_year
      )

    if(nrow(dd)<40) next
    if(n_distinct(dd[[y]],na.rm=TRUE)<2) next

    rhs <- paste(c(g,controls),collapse=" + ")
    f <- as.formula(
      paste0(
        y," ~ ",rhs,
        " | ",country_var," + year"
      )
    )

    m <- tryCatch(
      feols(
        f,
        data=dd,
        vcov=as.formula(paste0("~",country_var))
      ),
      error=function(e) NULL
    )

    if(is.null(m) || !(g %in% names(coef(m)))) next

    res[[paste(hid,drop_year,sep="__")]] <- tibble(
      hypothesis_id=hid,
      outcome=y,
      gpr_measure=g,
      dropped_year=drop_year,
      estimate=as.numeric(coef(m)[g]),
      std_error=sqrt(vcov(m)[g,g]),
      nobs=nobs(m)
    )
  }
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(outdir,"07_legal_leave_one_year_out.csv")
)

summ <- ans %>%
  left_join(
    targets %>% select(hypothesis_id,expected_sign),
    by="hypothesis_id"
  ) %>%
  group_by(hypothesis_id,outcome,gpr_measure,expected_sign) %>%
  summarise(
    n_runs=n(),
    min_beta=min(estimate,na.rm=TRUE),
    max_beta=max(estimate,na.rm=TRUE),
    median_beta=median(estimate,na.rm=TRUE),
    share_positive=mean(estimate>0,na.rm=TRUE),
    share_negative=mean(estimate<0,na.rm=TRUE),
    share_expected_sign=case_when(
      first(expected_sign)=="+" ~ mean(estimate>0,na.rm=TRUE),
      first(expected_sign)=="-" ~ mean(estimate<0,na.rm=TRUE),
      TRUE ~ NA_real_
    ),
    .groups="drop"
  )

write_csv(
  summ,
  file.path(outdir,"08_legal_leave_one_year_out_summary.csv")
)

message("50_leave_one_year_out_legal.R completed.")
