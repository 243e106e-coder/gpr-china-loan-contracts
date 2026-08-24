suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3_gpr_baseline"
d <- read_csv(file.path(outdir,"07_stage3_estimation_sample.csv"),
              show_col_types=FALSE, progress=FALSE)

possible_controls <- c("commitment_usd","maturity_years","grace_period_years")
controls <- possible_controls[possible_controls %in% names(d)]

country_candidates <- c("borrower_country","country")
country_var <- country_candidates[country_candidates %in% names(d)][1]

if ("creditor_type" %in% names(d)) controls <- c(controls,"creditor_type")

rhs <- paste(c("gpr",controls), collapse=" + ")

make_formula <- function(y, fe=FALSE) {
  if (fe && !is.na(country_var)) {
    as.formula(paste0(y," ~ ",rhs," | ",country_var))
  } else {
    as.formula(paste0(y," ~ ",rhs))
  }
}

results <- list()
models <- list()

run_model <- function(data, y, family=c("ols","logit"), fe=FALSE, label) {
  family <- match.arg(family)
  f <- make_formula(y,fe)

  m <- if (family=="ols") {
    feols(f, data=data, vcov="hetero")
  } else {
    feglm(f, data=data, family="binomial", vcov="hetero")
  }

  models[[label]] <<- m

  broom::tidy(m, conf.int=TRUE) %>%
    mutate(model=label, outcome=y, nobs=nobs(m), .before=1)
}

dp <- d %>% filter(est_pricing==1)
results[["pricing_pooled"]] <- run_model(
  dp,"pricing_rate_t0","ols",FALSE,"pricing_pooled"
)
if(!is.na(country_var)) {
  results[["pricing_countryFE"]] <- run_model(
    dp,"pricing_rate_t0","ols",TRUE,"pricing_countryFE"
  )
}

dm <- d %>% filter(est_maturity==1)
results[["maturity_pooled"]] <- run_model(
  dm,"maturity_years","ols",FALSE,"maturity_pooled"
)

dg <- d %>% filter(est_grace==1)
results[["grace_pooled"]] <- run_model(
  dg,"grace_period_years","ols",FALSE,"grace_pooled"
)

dl <- d %>% filter(est_legal_any==1)
results[["legal_any_logit"]] <- run_model(
  dl,"legal_any","logit",FALSE,"legal_any_logit"
)

dc <- d %>% filter(est_legal_count==1)
results[["legal_count_ols"]] <- run_model(
  dc,"legal_protection_count","ols",FALSE,"legal_count_ols"
)

res <- bind_rows(results)
write_csv(res,file.path(outdir,"09_baseline_model_results.csv"))

gpr_res <- res %>%
  filter(term=="gpr") %>%
  select(model,outcome,nobs,estimate,std.error,conf.low,conf.high,p.value)

write_csv(gpr_res,file.path(outdir,"10_gpr_baseline_coefficients.csv"))

sink(file.path(outdir,"11_baseline_model_summaries.txt"))
for(nm in names(models)) {
  cat("\n========================\n",nm,"\n========================\n")
  print(summary(models[[nm]]))
}
sink()
