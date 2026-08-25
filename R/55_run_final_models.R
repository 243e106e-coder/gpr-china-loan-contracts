suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(broom)
})

outdir <- "outputs/stage3g_final_inference"

fin <- read_csv(file.path(outdir,"01_financial_final_data.csv"),show_col_types=FALSE,progress=FALSE)
legal <- read_csv(file.path(outdir,"02_legal_final_data.csv"),show_col_types=FALSE,progress=FALSE)
targets <- read_csv(file.path(outdir,"03_final_hypotheses.csv"),show_col_types=FALSE)

country_var <- "borrower_country_final"

results <- list()
models <- list()

for(i in seq_len(nrow(targets))) {

  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]

  d <- if(block=="financial") fin else legal

  controls <- intersect(
    c("log_loan_amount","creditor_type"),
    names(d)
  )

  if(block=="financial" && y=="pricing_rate_t0") {
    controls <- c(
      controls,
      intersect(c("maturity_years","grace_period_years"),names(d))
    )
  }

  if(block=="financial" && y=="maturity_years") {
    controls <- c(
      controls,
      intersect(c("grace_period_years"),names(d))
    )
  }

  if(block=="legal") {
    controls <- c(
      controls,
      intersect(c("maturity_years","grace_period_years"),names(d))
    )
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

  if(nrow(dd)<40) next
  if(n_distinct(dd[[y]],na.rm=TRUE)<2 && block=="legal") next

  rhs <- paste(c(g,controls),collapse=" + ")
  f <- as.formula(
    paste0(
      y," ~ ",rhs,
      " | ",country_var," + year"
    )
  )

  m <- feols(
    f,
    data=dd,
    vcov=as.formula(paste0("~",country_var))
  )

  models[[hid]] <- m

  results[[hid]] <- broom::tidy(
    m,
    conf.int=TRUE
  ) %>%
    filter(term==g) %>%
    mutate(
      block=block,
      hypothesis_id=hid,
      outcome=y,
      gpr_measure=g,
      expected_sign=targets$expected_sign[i],
      interpretation=targets$interpretation[i],
      nobs=nobs(m),
      n_country_clusters=n_distinct(dd[[country_var]]),
      sign_ok=case_when(
        targets$expected_sign[i]=="+" ~ estimate>0,
        targets$expected_sign[i]=="-" ~ estimate<0,
        TRUE ~ NA
      ),
      .before=1
    )
}

ans <- bind_rows(results)

if(nrow(ans)>0) {
  ans <- ans %>%
    mutate(
      p_holm_all=p.adjust(p.value,method="holm"),
      p_bh_all=p.adjust(p.value,method="BH"),
      p_bonferroni_all=p.adjust(p.value,method="bonferroni")
    )
}

write_csv(ans,file.path(outdir,"04_final_clustered_results.csv"))

sink(file.path(outdir,"05_final_model_summaries.txt"))
for(hid in names(models)) {
  cat("\n====================================\n")
  cat(hid,"\n")
  cat("====================================\n")
  print(summary(models[[hid]]))
}
sink()

message("55_run_final_models.R completed.")
