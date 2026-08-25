suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3g_final_inference"

fin <- read_csv(file.path(outdir,"01_financial_final_data.csv"),show_col_types=FALSE,progress=FALSE)
legal <- read_csv(file.path(outdir,"02_legal_final_data.csv"),show_col_types=FALSE,progress=FALSE)
targets <- read_csv(file.path(outdir,"03_final_hypotheses.csv"),show_col_types=FALSE)

if(!requireNamespace("fwildclusterboot",quietly=TRUE)) {

  write_csv(
    tibble(
      hypothesis_id=targets$hypothesis_id,
      status="SKIPPED",
      p_value_wild_cluster=NA_real_,
      reason="fwildclusterboot not installed"
    ),
    file.path(outdir,"06_final_wild_cluster_results.csv")
  )

} else {

  country_var <- "borrower_country_final"
  res <- list()

  for(i in seq_len(nrow(targets))) {

    block <- targets$block[i]
    hid <- targets$hypothesis_id[i]
    y <- targets$outcome[i]
    g <- targets$gpr_measure[i]

    d <- if(block=="financial") fin else legal

    controls <- intersect(c("log_loan_amount","creditor_type"),names(d))

    if(block=="financial" && y=="pricing_rate_t0") {
      controls <- c(controls,intersect(c("maturity_years","grace_period_years"),names(d)))
    }

    if(block=="financial" && y=="maturity_years") {
      controls <- c(controls,intersect(c("grace_period_years"),names(d)))
    }

    if(block=="legal") {
      controls <- c(controls,intersect(c("maturity_years","grace_period_years"),names(d)))
    }

    controls <- unique(controls)

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

    if(is.null(bt)) {
      res[[hid]] <- tibble(
        hypothesis_id=hid,
        status="FAILED",
        p_value_wild_cluster=NA_real_,
        reason="boottest returned no result"
      )
    } else {
      res[[hid]] <- tibble(
        hypothesis_id=hid,
        status="OK",
        p_value_wild_cluster=suppressWarnings(as.numeric(bt$p_val)),
        reason=NA_character_
      )
    }
  }

  write_csv(
    bind_rows(res),
    file.path(outdir,"06_final_wild_cluster_results.csv")
  )
}

message("56_run_wild_cluster_final.R completed.")
