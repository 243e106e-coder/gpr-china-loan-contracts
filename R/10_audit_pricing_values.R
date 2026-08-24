suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
})

infile <- "outputs/stage2a_pricing_recovery/07_hcl_clg_exact_record_matches.csv"
outdir <- "outputs/stage2b_harmonization"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
if (!file.exists(infile)) stop("Missing Stage 2A exact-match file: ", infile)

d <- read_csv(infile, show_col_types=FALSE, progress=FALSE)

pricing_vars <- intersect(c(
  "interest_rate_type","fixed_interest_rate","reference_rate",
  "margin_on_reference_rate","interest_at_t0","default_interest_rate",
  "maturity","grace_period","loan_tenor",
  "first_loan_repayment_date","last_loan_repayment_date"
), names(d))

audit <- bind_rows(lapply(pricing_vars, function(v) {
  x <- d[[v]]
  tibble(
    variable=v,
    n=nrow(d),
    nonmissing=sum(!is.na(x) & trimws(as.character(x))!=""),
    pct_nonmissing=100*nonmissing/n,
    n_distinct=n_distinct(x, na.rm=TRUE)
  )
}))
write_csv(audit, file.path(outdir,"01_pricing_variable_audit.csv"))

freq <- bind_rows(lapply(pricing_vars, function(v) {
  d %>% count(value=as.character(.data[[v]]), sort=TRUE) %>%
    mutate(variable=v, .before=1)
}))
write_csv(freq, file.path(outdir,"02_pricing_value_frequencies.csv"))

# Explicitly audit variable-rate structure.
if (all(c("interest_rate_type","reference_rate","margin_on_reference_rate","interest_at_t0") %in% names(d))) {
  floating <- d %>%
    filter(interest_rate_type=="Variable Interest Rate") %>%
    transmute(
      contract_id, aid_data_record_id_hcl, borrower_country, year,
      interest_rate_type, reference_rate, margin_on_reference_rate, interest_at_t0
    )
  write_csv(floating, file.path(outdir,"03_floating_rate_audit.csv"))
}
