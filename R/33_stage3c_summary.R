suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3c_robustness"

coef <- read_csv(file.path(outdir,"04_standardized_gpr_coefficients.csv"),show_col_types=FALSE)
loo_c <- read_csv(file.path(outdir,"09_leave_one_country_out_summary.csv"),show_col_types=FALSE)
loo_y <- read_csv(file.path(outdir,"11_leave_one_year_out_summary.csv"),show_col_types=FALSE)

main_all <- coef %>% filter(gpr_measure=="gpr_ai_all_z")

lines <- c(
  "# Paper 1 — Stage 3C Robustness Summary",
  "",
  "## Main standardized GPR-all results",
  if(nrow(main_all)>0) {
    paste0(
      "- ",main_all$outcome,
      ": beta per 1 SD GPR=",round(main_all$estimate,4),
      ", p=",round(main_all$p.value,4),
      ", N=",main_all$nobs,
      ", country clusters=",main_all$n_country_clusters
    )
  } else "- No standardized GPR-all coefficients available.",
  "",
  "## Leave-one-country-out sign stability",
  paste0(
    "- ",loo_c$outcome,
    ": min=",round(loo_c$min_beta,4),
    ", max=",round(loo_c$max_beta,4),
    ", positive share=",round(loo_c$share_positive,3),
    ", negative share=",round(loo_c$share_negative,3)
  ),
  "",
  "## Leave-one-year-out sign stability",
  paste0(
    "- ",loo_y$outcome,
    ": min=",round(loo_y$min_beta,4),
    ", max=",round(loo_y$max_beta,4),
    ", positive share=",round(loo_y$share_positive,3),
    ", negative share=",round(loo_y$share_negative,3)
  ),
  "",
  "## Interpretation rules",
  "- Standardized coefficients are effects associated with a one-SD increase in borrower-country GPR.",
  "- Country-clustered SE are preferred to heteroskedasticity-only SE.",
  "- Stable signs under leave-one-country/year-out strengthen credibility but do not prove causality.",
  "- Legal clauses are estimated separately with LPM + borrower-country FE + year FE.",
  "- Do not construct Neither/PricingOnly/ContractOnly/Both yet."
)

writeLines(lines,file.path(outdir,"STAGE3C_SUMMARY.md"))
message(paste(lines,collapse="\n"))
