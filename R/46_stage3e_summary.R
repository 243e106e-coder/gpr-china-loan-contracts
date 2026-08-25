suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3e_legal_choice"

law_counts <- read_csv(file.path(outdir,"03_governing_law_choice_counts.csv"),show_col_types=FALSE)
arb_counts <- read_csv(file.path(outdir,"06_arbitration_choice_counts.csv"),show_col_types=FALSE)

law_file <- file.path(outdir,"08_governing_law_gpr_coefficients.csv")
arb_file <- file.path(outdir,"10_arbitration_gpr_coefficients.csv")
cross_file <- file.path(outdir,"11_cross_default_confirmatory.csv")

law <- if(file.exists(law_file)) read_csv(law_file,show_col_types=FALSE) else tibble()
arb <- if(file.exists(arb_file)) read_csv(arb_file,show_col_types=FALSE) else tibble()
cross <- if(file.exists(cross_file)) read_csv(cross_file,show_col_types=FALSE) else tibble()

lines <- c(
  "# Paper 1 — Stage 3E Substantive Legal Choice",
  "",
  "## Governing-law usable counts",
  paste0(
    "- usable=",law_counts$usable_relation,
    "; PRC=",law_counts$prc_law,
    "; borrower=",law_counts$borrower_law,
    "; third-party=",law_counts$third_party_law,
    "; English=",law_counts$english_law,
    "; US/New York=",law_counts$us_newyork_law
  ),
  "",
  "## Arbitration usable counts",
  paste0(
    "- usable=",arb_counts$usable_arbitration,
    "; mainland China=",arb_counts$mainland_china,
    "; Hong Kong=",arb_counts$hongkong,
    "; international/third-party=",arb_counts$international_third,
    "; borrower/local=",arb_counts$borrower_local,
    "; private/ad hoc=",arb_counts$private_adhoc
  ),
  "",
  "## Governing-law GPR results",
  if(nrow(law)>0) paste0(
    "- ",law$outcome," × ",law$gpr_measure,
    ": beta=",round(law$estimate,4),
    ", p=",round(law$p.value,4),
    ", N=",law$nobs
  ) else "- No governing-law coefficient.",
  "",
  "## Arbitration GPR results",
  if(nrow(arb)>0) paste0(
    "- ",arb$outcome," × ",arb$gpr_measure,
    ": beta=",round(arb$estimate,4),
    ", p=",round(arb$p.value,4),
    ", N=",arb$nobs
  ) else "- No arbitration coefficient.",
  "",
  "## Decision rule",
  "- Treat PRC/borrower/third-party governing-law choice as substantive legal outcomes.",
  "- Arbitration classification is based on forum/institution family, not assumed seat when seat is not observed.",
  "- Unresolved categories are excluded rather than guessed.",
  "- If substantive legal-choice outcomes remain null after these tests, downgrade law to a mechanism contrast rather than forcing a legal contribution."
)

writeLines(lines,file.path(outdir,"STAGE3E_SUMMARY.md"))
message(paste(lines,collapse="\n"))
