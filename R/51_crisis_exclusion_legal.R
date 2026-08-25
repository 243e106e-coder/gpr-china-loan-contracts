suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
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

exclusions <- list(
  exclude_2001=2001,
  exclude_gfc_2008_2009=c(2008,2009),
  exclude_2014_2015=c(2014,2015),
  exclude_covid_2020_2021=c(2020,2021),
  exclude_ukraine_2022_2023=c(2022,2023)
)

res <- list()

for(i in seq_len(nrow(targets))) {

  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  hid <- targets$hypothesis_id[i]

  for(ex_name in names(exclusions)) {

    yrs <- exclusions[[ex_name]]

    dd <- d %>%
      filter(
        main_sample==1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        !(year %in% yrs)
      )

    if(nrow(dd)<40 || n_distinct(dd[[y]],na.rm=TRUE)<2) next

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

    if(is.null(m)) next

    res[[paste(hid,ex_name,sep="__")]] <- broom::tidy(
      m,
      conf.int=TRUE
    ) %>%
      filter(term==g) %>%
      mutate(
        hypothesis_id=hid,
        outcome=y,
        gpr_measure=g,
        exclusion=ex_name,
        excluded_years=paste(yrs,collapse=";"),
        nobs=nobs(m),
        .before=1
      )
  }
}

write_csv(
  bind_rows(res),
  file.path(outdir,"09_legal_crisis_exclusion_results.csv")
)

message("51_crisis_exclusion_legal.R completed.")
