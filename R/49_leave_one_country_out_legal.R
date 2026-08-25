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

countries <- sort(unique(d[[country_var]][!is.na(d[[country_var]])]))
res <- list()

for(i in seq_len(nrow(targets))) {

  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  hid <- targets$hypothesis_id[i]

  for(drop_country in countries) {

    dd <- d %>%
      filter(
        main_sample==1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        .data[[country_var]]!=drop_country,
        !is.na(year)
      )

    if(nrow(dd)<40 || n_distinct(dd[[country_var]])<5) next
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

    res[[paste(hid,drop_country,sep="__")]] <- tibble(
      hypothesis_id=hid,
      outcome=y,
      gpr_measure=g,
      dropped_country=drop_country,
      estimate=as.numeric(coef(m)[g]),
      std_error=sqrt(vcov(m)[g,g]),
      nobs=nobs(m),
      n_country_clusters=n_distinct(dd[[country_var]])
    )
  }
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(outdir,"05_legal_leave_one_country_out.csv")
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
  file.path(outdir,"06_legal_leave_one_country_out_summary.csv")
)

message("49_leave_one_country_out_legal.R completed.")
