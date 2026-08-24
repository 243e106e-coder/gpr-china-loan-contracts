suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3_gpr_baseline"
d <- read_csv(file.path(outdir,"04_contracts_with_global_gpr.csv"),
              show_col_types=FALSE, progress=FALSE)

required <- c(
  "contract_id","year","gpr",
  "pricing_rate_t0","maturity_years","grace_period_years",
  "legal_any","legal_protection_count",
  "sample_loan_ppg"
)

miss <- setdiff(required,names(d))
if(length(miss)) stop("Missing Stage 3 variables: ",paste(miss,collapse=", "))

est <- d %>%
  mutate(
    main_sample = as.integer(sample_loan_ppg==1),

    est_pricing = as.integer(
      main_sample==1 &
      !is.na(gpr) &
      !is.na(pricing_rate_t0)
    ),

    est_maturity = as.integer(
      main_sample==1 &
      !is.na(gpr) &
      !is.na(maturity_years)
    ),

    est_grace = as.integer(
      main_sample==1 &
      !is.na(gpr) &
      !is.na(grace_period_years)
    ),

    est_legal_any = as.integer(
      main_sample==1 &
      !is.na(gpr) &
      !is.na(legal_any)
    ),

    est_legal_count = as.integer(
      main_sample==1 &
      !is.na(gpr) &
      !is.na(legal_protection_count)
    )
  )

write_csv(est,file.path(outdir,"07_stage3_estimation_sample.csv"))

counts <- tibble(
  outcome=c("pricing_rate_t0","maturity_years","grace_period_years",
            "legal_any","legal_protection_count"),
  n=c(
    sum(est$est_pricing,na.rm=TRUE),
    sum(est$est_maturity,na.rm=TRUE),
    sum(est$est_grace,na.rm=TRUE),
    sum(est$est_legal_any,na.rm=TRUE),
    sum(est$est_legal_count,na.rm=TRUE)
  )
)

write_csv(counts,file.path(outdir,"08_baseline_estimation_counts.csv"))
