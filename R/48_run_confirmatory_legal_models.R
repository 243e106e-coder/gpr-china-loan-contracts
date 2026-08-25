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

res <- list()
mods <- list()

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

  if(nrow(dd)<40) next
  if(n_distinct(dd[[y]],na.rm=TRUE)<2) next

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

  mods[[hid]] <- m

  res[[hid]] <- broom::tidy(
    m,
    conf.int=TRUE
  ) %>%
    filter(term==g) %>%
    mutate(
      hypothesis_id=hid,
      outcome=y,
      gpr_measure=g,
      expected_sign=targets$expected_sign[i],
      interpretation=targets$interpretation[i],
      nobs=nobs(m),
      n_country_clusters=n_distinct(dd[[country_var]]),
      .before=1
    )
}

ans <- bind_rows(res)

# Multiple-testing correction across the four pre-specified hypotheses.
if(nrow(ans)>0) {
  ans <- ans %>%
    mutate(
      p_bonferroni = p.adjust(p.value, method="bonferroni"),
      p_holm = p.adjust(p.value, method="holm"),
      p_bh = p.adjust(p.value, method="BH"),
      sign_matches_hypothesis = case_when(
        expected_sign=="+" ~ estimate>0,
        expected_sign=="-" ~ estimate<0,
        TRUE ~ NA
      )
    )
}

write_csv(
  ans,
  file.path(outdir,"03_confirmatory_legal_results.csv")
)

sink(file.path(outdir,"04_confirmatory_legal_model_summaries.txt"))
for(hid in names(mods)) {
  cat("\n==============================\n")
  cat(hid,"\n")
  cat("==============================\n")
  print(summary(mods[[hid]]))
}
sink()

message("48_run_confirmatory_legal_models.R completed.")
