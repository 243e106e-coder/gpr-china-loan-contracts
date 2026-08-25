suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3d_legal_architecture"

coef_file <- file.path(
  outdir,
  "09_legal_architecture_gpr_coefficients.csv"
)

coef <- if (file.exists(coef_file)) {
  read_csv(
    coef_file,
    show_col_types = FALSE
  )
} else {
  tibble()
}

ready <- read_csv(
  file.path(
    outdir,
    "10_manual_classification_readiness.csv"
  ),
  show_col_types = FALSE
)

lines <- c(
  "# Paper 1 — Stage 3D Legal Architecture Summary",
  "",
  "## Safe legal outcomes",
  "- Collateral / escrow / guarantor / cross-default use existing 0/1 coding.",
  "- Governing-law and arbitration presence can be tested, but presence does NOT mean stronger legal protection.",
  "",
  "## Current GPR coefficients",
  if (
    nrow(coef) > 0 &&
    all(c("estimate","p.value") %in% names(coef))
  ) {
    paste0(
      "- ",
      coef$outcome,
      " × ",
      coef$gpr_measure,
      ": beta=",
      round(coef$estimate,4),
      ", p=",
      round(coef$p.value,4),
      ", N=",
      coef$nobs
    )
  } else {
    "- No coefficient available."
  },
  "",
  "## Manual legal classification readiness",
  paste0(
    "- ",
    ready$component,
    ": ",
    ready$manually_classified,
    "/",
    ready$categories,
    " categories classified (",
    round(ready$pct_classified,1),
    "%)"
  ),
  "",
  "## Rule",
  "- Do not interpret governing law or arbitration substantively until the manual legal classification is completed.",
  "- Non-significance of legal clauses is retained as an economically meaningful contrast to pricing/maturity results, not hidden.",
  "- This stage tests legal architecture; it does not force a legal contribution if the data do not support one."
)

writeLines(
  lines,
  file.path(
    outdir,
    "STAGE3D_SUMMARY.md"
  )
)

message(
  paste(
    lines,
    collapse = "\n"
  )
)
