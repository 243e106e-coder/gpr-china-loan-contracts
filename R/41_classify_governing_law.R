suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(countrycode)
})

outdir <- "outputs/stage3e_legal_choice"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

review_file <- "outputs/stage3d_legal_architecture/04_governing_law_manual_review.csv"
analysis_file <- "outputs/stage3d_legal_architecture/07_legal_architecture_analysis_dataset.csv"

if (!file.exists(review_file)) stop("Missing Stage 3D governing-law review file.")
if (!file.exists(analysis_file)) stop("Missing Stage 3D legal analysis dataset.")

review <- read_csv(review_file, show_col_types=FALSE, progress=FALSE)
d <- read_csv(analysis_file, show_col_types=FALSE, progress=FALSE)

# ------------------------------------------------------------
# Conservative mapping of the ACTUAL 18 categories observed.
# We map jurisdiction when the raw text clearly identifies one.
# Ambiguous/Mixed/Unknown entries remain unresolved.
# ------------------------------------------------------------

law_map <- review %>%
  mutate(
    raw_norm = str_squish(as.character(governing_law_raw)),

    law_iso3 = case_when(
      raw_norm == "PRC" ~ "CHN",
      raw_norm == "GBR" ~ "GBR",
      raw_norm == "USA" ~ "USA",
      raw_norm == "ECU" ~ "ECU",
      raw_norm == "BIH" ~ "BIH",
      raw_norm == "UGA" ~ "UGA",
      raw_norm == "AUS" ~ "AUS",
      str_detect(raw_norm, "^GBR, except") ~ "GBR",
      raw_norm == "GHA" ~ "GHA",
      raw_norm == "NYC" ~ "USA",
      raw_norm == "New York Law" ~ "USA",
      raw_norm == "RUS" ~ "RUS",
      raw_norm == "BRA" ~ "BRA",
      raw_norm == "COL" ~ "COL",
      raw_norm == "VE" ~ "VEN",
      TRUE ~ NA_character_
    ),

    law_jurisdiction_group = case_when(
      law_iso3 == "CHN" ~ "PRC_law",
      law_iso3 == "GBR" ~ "English_law",
      law_iso3 == "USA" ~ "US_or_NewYork_law",
      !is.na(law_iso3) ~ "other_national_law",
      raw_norm == "Mixed" ~ "mixed",
      raw_norm == "UNK" ~ "unknown",
      TRUE ~ "unresolved"
    ),

    law_common_civil_other = case_when(
      law_iso3 %in% c("GBR","USA","UGA","GHA","AUS") ~ "common_law",
      law_iso3 %in% c("CHN","ECU","BIH","RUS","BRA","COL","VEN") ~ "civil_or_socialist_law",
      TRUE ~ "unresolved"
    ),

    auto_classification_confidence = case_when(
      !is.na(law_iso3) ~ "high",
      raw_norm %in% c("Mixed","UNK") ~ "explicitly_ambiguous",
      TRUE ~ "manual_review_required"
    )
  ) %>%
  select(
    governing_law_raw,
    n_contracts,
    law_iso3,
    law_jurisdiction_group,
    law_common_civil_other,
    auto_classification_confidence
  )

write_csv(
  law_map,
  file.path(outdir, "01_governing_law_classification.csv")
)

# ------------------------------------------------------------
# Merge to contract level and classify lender/borrower/third-party.
# Chinese state creditor => PRC is lender-home jurisdiction.
# Borrower jurisdiction is inferred from borrower country.
# ------------------------------------------------------------

law_var <- c("governing_law_raw","governing_law")[
  c("governing_law_raw","governing_law") %in% names(d)
][1]

if (is.na(law_var)) stop("No governing-law variable in Stage 3D dataset.")

country_var <- c("borrower_country_stage3c","borrower_country","country")[
  c("borrower_country_stage3c","borrower_country","country") %in% names(d)
][1]

if (is.na(country_var)) stop("No borrower-country variable.")

d2 <- d %>%
  mutate(
    governing_law_key = as.character(.data[[law_var]]),
    borrower_country_legal = as.character(.data[[country_var]]),
    borrower_iso3_legal = countrycode(
      borrower_country_legal,
      origin="country.name",
      destination="iso3c",
      warn=FALSE
    )
  ) %>%
  left_join(
    law_map,
    by=c("governing_law_key"="governing_law_raw")
  ) %>%
  mutate(
    governing_law_relation = case_when(
      is.na(law_iso3) ~ "unresolved",
      law_iso3 == "CHN" ~ "lender_home",
      !is.na(borrower_iso3_legal) & law_iso3 == borrower_iso3_legal ~ "borrower_home",
      TRUE ~ "third_party"
    ),

    law_prc = as.integer(governing_law_relation == "lender_home"),
    law_borrower = as.integer(governing_law_relation == "borrower_home"),
    law_third_party = as.integer(governing_law_relation == "third_party"),
    law_english = as.integer(law_jurisdiction_group == "English_law"),
    law_us_newyork = as.integer(law_jurisdiction_group == "US_or_NewYork_law"),

    governing_law_choice_usable = as.integer(
      governing_law_relation %in% c("lender_home","borrower_home","third_party")
    )
  )

write_csv(
  d2,
  file.path(outdir, "02_contracts_governing_law_classified.csv")
)

coverage <- d2 %>%
  summarise(
    contracts=n(),
    with_governing_law=sum(!is.na(governing_law_key) & str_trim(governing_law_key)!=""),
    usable_relation=sum(governing_law_choice_usable==1,na.rm=TRUE),
    prc_law=sum(law_prc==1,na.rm=TRUE),
    borrower_law=sum(law_borrower==1,na.rm=TRUE),
    third_party_law=sum(law_third_party==1,na.rm=TRUE),
    english_law=sum(law_english==1,na.rm=TRUE),
    us_newyork_law=sum(law_us_newyork==1,na.rm=TRUE)
  )

write_csv(
  coverage,
  file.path(outdir, "03_governing_law_choice_counts.csv")
)

message("41_classify_governing_law.R completed.")
