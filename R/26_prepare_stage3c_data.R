suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3c_robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

infile <- "outputs/stage3b_identification/06_contracts_with_country_gpr.csv"
if (!file.exists(infile)) stop("Stage 3B contract-country-GPR file missing.", call. = FALSE)

d <- read_csv(infile, show_col_types = FALSE, progress = FALSE)

required <- c(
  "contract_id","year","main_sample",
  "pricing_rate_t0","maturity_years","grace_period_years",
  "gpr_ai_all","gpr_ai_initiator","gpr_ai_respondent","gpr_ai_spillover"
)
miss <- setdiff(required, names(d))
if (length(miss)) stop("Missing Stage 3C variables: ", paste(miss, collapse=", "))

country_var <- c("borrower_country","country")[c("borrower_country","country") %in% names(d)][1]
if (is.na(country_var)) stop("Borrower-country field missing.")

d <- d %>%
  mutate(borrower_country_stage3c = as.character(.data[[country_var]]))

amount_var <- c("commitment_usd","adjusted_amount","amount_usd")[
  c("commitment_usd","adjusted_amount","amount_usd") %in% names(d)
][1]

if (!is.na(amount_var)) {
  d <- d %>%
    mutate(
      log_loan_amount = ifelse(
        !is.na(.data[[amount_var]]) & as.numeric(.data[[amount_var]]) > 0,
        log(as.numeric(.data[[amount_var]])),
        NA_real_
      )
    )
} else {
  d$log_loan_amount <- NA_real_
}

gpr_vars <- c("gpr_ai_all","gpr_ai_initiator","gpr_ai_respondent","gpr_ai_spillover")
main <- d %>% filter(main_sample == 1)

stats <- bind_rows(lapply(gpr_vars, function(v) {
  x <- main[[v]]
  tibble(
    variable=v,
    mean=mean(x,na.rm=TRUE),
    sd=sd(x,na.rm=TRUE),
    n_nonmissing=sum(!is.na(x)),
    n_unique=n_distinct(x,na.rm=TRUE)
  )
}))
write_csv(stats,file.path(outdir,"01_gpr_standardization_stats.csv"))

for(v in gpr_vars) {
  mu <- stats$mean[stats$variable==v]
  sig <- stats$sd[stats$variable==v]
  if(!is.finite(sig) || sig<=0) stop("No usable variation in ",v)
  d[[paste0(v,"_z")]] <- (d[[v]]-mu)/sig
}

d <- d %>%
  mutate(
    est3c_pricing = as.integer(main_sample==1 & !is.na(pricing_rate_t0)),
    est3c_maturity = as.integer(main_sample==1 & !is.na(maturity_years)),
    est3c_grace = as.integer(main_sample==1 & !is.na(grace_period_years))
  )

write_csv(d,file.path(outdir,"02_stage3c_analysis_data.csv"))
message("26_prepare_stage3c_data.R completed.")
