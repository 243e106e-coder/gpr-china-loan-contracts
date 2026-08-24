suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
})

infile <- "outputs/stage2a_pricing_recovery/11_stage2a_analysis_sample.csv"
outdir <- "outputs/stage2b_harmonization"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
d <- read_csv(infile, show_col_types=FALSE, progress=FALSE)

req <- c("contract_id","collateral","escrow_account","guarantor",
         "cross_default","governing_law","arbitration")
miss <- setdiff(req,names(d))
if(length(miss)) stop("Missing legal columns: ",paste(miss,collapse=", "))

# Safe binary conversion only for variables already coded 0/1 in HCL.
to01 <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  ifelse(y %in% c(0,1), y, NA_real_)
}

legal <- d %>%
  transmute(
    contract_id,
    collateral_bin=to01(collateral),
    escrow_bin=to01(escrow_account),
    guarantor_bin=to01(guarantor),
    cross_default_bin=to01(cross_default),
    governing_law_raw=as.character(governing_law),
    arbitration_raw=as.character(arbitration)
  ) %>%
  mutate(
    governing_law_present=as.integer(!is.na(governing_law_raw) & str_trim(governing_law_raw)!=""),
    arbitration_present=as.integer(!is.na(arbitration_raw) & str_trim(arbitration_raw)!=""),

    # Conservative count index: only explicit protections/presence.
    # Governing-law and arbitration categories themselves remain RAW for manual review.
    legal_protection_count =
      rowSums(across(c(collateral_bin,escrow_bin,guarantor_bin,
                       cross_default_bin,governing_law_present,
                       arbitration_present)), na.rm=TRUE),

    legal_nonmissing_count =
      rowSums(!is.na(across(c(collateral_bin,escrow_bin,guarantor_bin,
                              cross_default_bin,governing_law_present,
                              arbitration_present)))),

    legal_any = as.integer(legal_protection_count > 0)
  )

write_csv(legal,file.path(outdir,"06_legal_coding_conservative.csv"))

# Manual-review dictionaries. Do NOT auto-classify Chinese/third-party/neutral law or forum.
law_review <- legal %>%
  count(governing_law_raw, sort=TRUE, name="n") %>%
  mutate(
    manual_category=NA_character_,
    notes=NA_character_
  )
arb_review <- legal %>%
  count(arbitration_raw, sort=TRUE, name="n") %>%
  mutate(
    manual_category=NA_character_,
    notes=NA_character_
  )
write_csv(law_review,file.path(outdir,"07_governing_law_manual_review.csv"))
write_csv(arb_review,file.path(outdir,"08_arbitration_manual_review.csv"))
