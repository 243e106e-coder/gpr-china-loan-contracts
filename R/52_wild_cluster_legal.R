suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3f_legal_validation"

if(!requireNamespace("fwildclusterboot",quietly=TRUE)) {

  write_csv(
    tibble(
      status="SKIPPED",
      reason="fwildclusterboot not installed. Clustered fixest results remain available."
    ),
    file.path(outdir,"10_legal_wild_cluster_results.csv")
  )

} else {

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

  res <- list()

  for(i in seq_len(nrow(targets))) {

    y <- targets$outcome[i]
    g <- targets$gpr_measure[i]
    hid <- targets$hypothesis_id[i]

    dd <- d %>%
      filter(
        main_sample==1,
        !is.na(.data[[y]]),
        !is.na(.data[[g]]),
        !is.na(.data[[country_var]]),
        !is.na(year)
      )

    rhs <- paste(c(g,controls),collapse=" + ")
    f <- as.formula(
      paste0(
        y," ~ ",rhs,
        " | ",country_var," + year"
      )
    )

    m <- feols(f,data=dd)

    bt <- tryCatch(
      fwildclusterboot::boottest(
        m,
        param=g,
        clustid=country_var,
        B=9999,
        type="rademacher"
      ),
      error=function(e) NULL
    )

    if(is.null(bt)) next

    res[[hid]] <- tibble(
      hypothesis_id=hid,
      outcome=y,
      gpr_measure=g,
      p_value_wild_cluster=suppressWarnings(as.numeric(bt$p_val))
    )
  }

  if(length(res)>0) {
    write_csv(
      bind_rows(res),
      file.path(outdir,"10_legal_wild_cluster_results.csv")
    )
  } else {
    write_csv(
      tibble(
        status="FAILED",
        reason="fwildclusterboot available but no boottest result returned."
      ),
      file.path(outdir,"10_legal_wild_cluster_results.csv")
    )
  }
}

message("52_wild_cluster_legal.R completed.")
