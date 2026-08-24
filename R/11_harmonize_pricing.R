suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})

infile <- "outputs/stage2a_pricing_recovery/07_hcl_clg_exact_record_matches.csv"
outdir <- "outputs/stage2b_harmonization"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
d <- read_csv(infile, show_col_types=FALSE, progress=FALSE)

needed <- c("contract_id","interest_rate_type","fixed_interest_rate",
            "reference_rate","margin_on_reference_rate","interest_at_t0",
            "maturity","grace_period")
miss <- setdiff(needed,names(d))
if(length(miss)) stop("Missing pricing columns: ",paste(miss,collapse=", "))

pricing <- d %>%
  mutate(
    pricing_type = case_when(
      interest_rate_type=="Fixed Interest Rate" ~ "fixed",
      interest_rate_type=="Variable Interest Rate" ~ "floating",
      TRUE ~ "unknown"
    ),

    # Primary comparable contractual rate:
    # AidData's interest_at_t0 is retained as the harmonized all-in rate at origination.
    # For fixed loans, fixed_interest_rate is used when interest_at_t0 is absent.
    pricing_rate_t0 = case_when(
      !is.na(interest_at_t0) ~ as.numeric(interest_at_t0),
      pricing_type=="fixed" & !is.na(fixed_interest_rate) ~ as.numeric(fixed_interest_rate),
      TRUE ~ NA_real_
    ),

    pricing_rate_source = case_when(
      !is.na(interest_at_t0) ~ "AidData interest_at_t0",
      pricing_type=="fixed" & !is.na(fixed_interest_rate) ~ "fixed_interest_rate fallback",
      TRUE ~ NA_character_
    ),

    floating_margin = if_else(
      pricing_type=="floating",
      as.numeric(margin_on_reference_rate),
      NA_real_
    ),

    fixed_rate = if_else(
      pricing_type=="fixed",
      as.numeric(fixed_interest_rate),
      NA_real_
    ),

    maturity_years = as.numeric(maturity),
    grace_period_years = as.numeric(grace_period),

    pricing_usable = as.integer(!is.na(pricing_rate_t0)),
    maturity_usable = as.integer(!is.na(maturity_years)),
    grace_usable = as.integer(!is.na(grace_period_years))
  )

# Never use default_interest_rate / penalty_interest_rate as ordinary pricing.
keep <- intersect(c(
  "contract_id","aid_data_record_id_hcl","borrower_country","year",
  "contract_category","ppg_debt","creditor_type",
  "interest_rate_type","pricing_type","reference_rate",
  "fixed_interest_rate","margin_on_reference_rate","interest_at_t0",
  "pricing_rate_t0","pricing_rate_source","fixed_rate","floating_margin",
  "maturity_years","grace_period_years",
  "pricing_usable","maturity_usable","grace_usable"
), names(pricing))

write_csv(pricing %>% select(all_of(keep)),
          file.path(outdir,"04_harmonized_pricing.csv"))

coverage <- pricing %>%
  summarise(
    n=n(),
    pricing_rate_t0=sum(!is.na(pricing_rate_t0)),
    fixed_rate=sum(!is.na(fixed_rate)),
    floating_margin=sum(!is.na(floating_margin)),
    maturity=sum(!is.na(maturity_years)),
    grace=sum(!is.na(grace_period_years))
  ) %>%
  pivot_longer(-n,names_to="measure",values_to="nonmissing") %>%
  mutate(pct=100*nonmissing/n)
write_csv(coverage,file.path(outdir,"05_harmonized_pricing_coverage.csv"))
