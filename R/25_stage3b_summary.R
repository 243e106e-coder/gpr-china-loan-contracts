suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3b_identification"

cov <- read_csv(
  file.path(
    outdir,
    "07_country_gpr_merge_coverage.csv"
  ),
  show_col_types = FALSE
)

san <- read_csv(
  file.path(
    outdir,
    "09_global_gpr_sanity_results.csv"
  ),
  show_col_types = FALSE
)

twfe_file <- file.path(
  outdir,
  "11_country_gpr_twfe_coefficients.csv"
)

twfe <- if (file.exists(twfe_file)) {
  read_csv(
    twfe_file,
    show_col_types = FALSE
  )
} else {
  tibble()
}

san_gpr <- san %>%
  filter(term == "gpr")

lines <- c(
  "# Paper 1 — Stage 3B Identification Diagnostics",
  "",
  paste0(
    "- Contracts: ",
    cov$contracts
  ),
  paste0(
    "- Contracts with borrower-country GPR(all): ",
    cov$with_country_gpr_all,
    " (",
    round(
      cov$pct_country_gpr,
      1
    ),
    "%)"
  ),
  "",
  "## Global-GPR sanity check",
  "- Uses borrower-country FE but deliberately no year FE.",
  "- This is descriptive, not the preferred identification.",
  if (nrow(san_gpr) > 0) {
    paste0(
      "- ",
      san_gpr$outcome,
      ": beta(gpr)=",
      round(
        san_gpr$estimate,
        5
      ),
      ", p=",
      round(
        san_gpr$p.value,
        4
      ),
      ", N=",
      san_gpr$nobs
    )
  } else {
    "- No global-GPR sanity coefficient available."
  },
  "",
  "## Preferred Stage 3B design",
  "- Borrower-country GPR varies across country and year, permitting borrower-country FE + year FE.",
  "- all / initiator / respondent / spillover are estimated separately.",
  if (
    nrow(twfe) > 0 &&
    all(
      c(
        "estimate",
        "p.value",
        "nobs"
      ) %in% names(twfe)
    )
  ) {
    paste0(
      "- ",
      twfe$outcome,
      " × ",
      twfe$gpr_measure,
      ": beta=",
      round(
        twfe$estimate,
        5
      ),
      ", p=",
      round(
        twfe$p.value,
        4
      ),
      ", N=",
      twfe$nobs
    )
  } else {
    "- No country-GPR TWFE coefficient available."
  },
  "",
  "## Guardrails",
  "- Do not interpret the global-GPR sanity regression as causal.",
  "- Do not create Neither/PricingOnly/ContractOnly/Both yet.",
  "- legal_any logit remains diagnostic until separation is resolved.",
  "- Exposure × Global GPR is intentionally left for Stage 3C because a defensible exposure measure has not yet been selected."
)

writeLines(
  lines,
  file.path(
    outdir,
    "STAGE3B_SUMMARY.md"
  )
)

message(
  paste(
    lines,
    collapse = "\n"
  )
)
