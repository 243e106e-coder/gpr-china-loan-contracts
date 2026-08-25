suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

outdir <- "outputs/stage3d_legal_architecture"

infile <- "outputs/stage3c_robustness/02_stage3c_analysis_data.csv"
if (!file.exists(infile)) {
  infile <- "outputs/stage3b_identification/06_contracts_with_country_gpr.csv"
}

d <- read_csv(infile, show_col_types = FALSE, progress = FALSE)

to01 <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  ifelse(z %in% c(0,1), z, NA_real_)
}

# Safe indicators: only use variables already coded as 0/1.
indicator_map <- list(
  collateral_bin = c("collateral_bin","collateral"),
  escrow_bin = c("escrow_bin","escrow_account"),
  guarantor_bin = c("guarantor_bin","guarantor"),
  cross_default_bin = c("cross_default_bin","cross_default")
)

for (newvar in names(indicator_map)) {
  if (!(newvar %in% names(d))) {
    src <- indicator_map[[newvar]][
      indicator_map[[newvar]] %in% names(d)
    ][1]

    if (!is.na(src)) {
      d[[newvar]] <- to01(d[[src]])
    }
  }
}

# Presence-only indicators for complex legal clauses.
# Presence is NOT interpreted as stronger protection.
law_var <- c("governing_law_raw","governing_law")[
  c("governing_law_raw","governing_law") %in% names(d)
][1]

arb_var <- c("arbitration_raw","arbitration")[
  c("arbitration_raw","arbitration") %in% names(d)
][1]

if (!is.na(law_var)) {
  d$governing_law_present <- as.integer(
    !is.na(d[[law_var]]) &
      str_trim(as.character(d[[law_var]])) != ""
  )
}

if (!is.na(arb_var)) {
  d$arbitration_present <- as.integer(
    !is.na(d[[arb_var]]) &
      str_trim(as.character(d[[arb_var]])) != ""
  )
}

write_csv(
  d,
  file.path(outdir, "06_legal_safe_coded_dataset.csv")
)

message("36_code_safe_legal_indicators.R completed.")
