suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

outdir <- "outputs/stage3e_legal_choice"

review_file <- "outputs/stage3d_legal_architecture/05_arbitration_manual_review.csv"
contract_file <- file.path(outdir, "02_contracts_governing_law_classified.csv")

if (!file.exists(review_file)) stop("Missing arbitration review file.")
if (!file.exists(contract_file)) stop("Run governing-law classification first.")

review <- read_csv(review_file, show_col_types=FALSE, progress=FALSE)
d <- read_csv(contract_file, show_col_types=FALSE, progress=FALSE)

# ------------------------------------------------------------
# Conservative classification of actual observed arbitration strings.
# We classify institution/forum family, not an assumed arbitral seat.
# ICC/UNCITRAL rules do NOT reveal seat; we do not invent one.
# ------------------------------------------------------------

arb_map <- review %>%
  mutate(
    raw_norm = str_squish(as.character(arbitration_raw)),
    raw_low = str_to_lower(raw_norm),

    arbitration_family = case_when(
      str_detect(raw_low, "cietac|china international economic and trade") ~ "china_CIETAC",
      str_detect(raw_low, "^bac$") ~ "china_BAC",
      str_detect(raw_low, "hkiac|hong kong international arbitration") ~ "hongkong_HKIAC",
      str_detect(raw_low, "lcia|london court") ~ "international_LCIA",
      str_detect(raw_low, "siac|singapore international arbitration") ~ "international_SIAC",
      str_detect(raw_low, "international chamber of commerce|^icc$|court of arbitration of the international chamber") ~ "international_ICC",
      str_detect(raw_low, "uncitral|unicitral|united nations commission on international trade law") ~ "international_UNCITRAL",
      str_detect(raw_low, "icsid|international center for the settlement of investment") ~ "international_ICSID",
      str_detect(raw_low, "cemarc|cámara argentina") ~ "borrower_or_local_institution",
      str_detect(raw_low, "procuraduría general del estado") ~ "borrower_or_local_institution",
      str_detect(raw_low, "arbitrazh court|russian federation") ~ "borrower_or_local_court",
      str_detect(raw_low, "courts of england") ~ "court_not_arbitration",
      raw_low == "private" ~ "private_or_ad_hoc",
      raw_low %in% c("c","tac") ~ "unresolved",
      str_detect(raw_low, "neutral, third-party international commercial tribunal") ~ "neutral_third_party",
      str_detect(raw_low, "english law|law of the russian federation") ~ "not_clear_arbitration_forum",
      TRUE ~ "unresolved"
    ),

    arbitration_channel = case_when(
      arbitration_family %in% c("china_CIETAC","china_BAC") ~ "mainland_china_forum",
      arbitration_family == "hongkong_HKIAC" ~ "hongkong_forum",
      arbitration_family %in% c(
        "international_LCIA","international_SIAC","international_ICC",
        "international_UNCITRAL","international_ICSID","neutral_third_party"
      ) ~ "international_or_third_party",
      arbitration_family %in% c(
        "borrower_or_local_institution","borrower_or_local_court"
      ) ~ "borrower_or_local",
      arbitration_family == "private_or_ad_hoc" ~ "private_or_ad_hoc",
      TRUE ~ "unresolved"
    ),

    arbitration_auto_confidence = case_when(
      arbitration_channel != "unresolved" ~ "high_or_moderate",
      TRUE ~ "manual_review_required"
    )
  ) %>%
  select(
    arbitration_raw,
    n_contracts,
    arbitration_family,
    arbitration_channel,
    arbitration_auto_confidence
  )

write_csv(
  arb_map,
  file.path(outdir, "04_arbitration_classification.csv")
)

arb_var <- c("arbitration_raw","arbitration")[
  c("arbitration_raw","arbitration") %in% names(d)
][1]

if (is.na(arb_var)) stop("No arbitration variable in contract dataset.")

d2 <- d %>%
  mutate(
    arbitration_key = as.character(.data[[arb_var]])
  ) %>%
  left_join(
    arb_map,
    by=c("arbitration_key"="arbitration_raw")
  ) %>%
  mutate(
    arb_mainland_china = as.integer(arbitration_channel=="mainland_china_forum"),
    arb_hongkong = as.integer(arbitration_channel=="hongkong_forum"),
    arb_international_third = as.integer(arbitration_channel=="international_or_third_party"),
    arb_borrower_local = as.integer(arbitration_channel=="borrower_or_local"),
    arb_private_adhoc = as.integer(arbitration_channel=="private_or_ad_hoc"),
    arbitration_choice_usable = as.integer(
      arbitration_channel %in% c(
        "mainland_china_forum",
        "hongkong_forum",
        "international_or_third_party",
        "borrower_or_local",
        "private_or_ad_hoc"
      )
    )
  )

write_csv(
  d2,
  file.path(outdir, "05_contracts_legal_choice_classified.csv")
)

coverage <- d2 %>%
  summarise(
    contracts=n(),
    with_arbitration=sum(!is.na(arbitration_key) & str_trim(arbitration_key)!=""),
    usable_arbitration=sum(arbitration_choice_usable==1,na.rm=TRUE),
    mainland_china=sum(arb_mainland_china==1,na.rm=TRUE),
    hongkong=sum(arb_hongkong==1,na.rm=TRUE),
    international_third=sum(arb_international_third==1,na.rm=TRUE),
    borrower_local=sum(arb_borrower_local==1,na.rm=TRUE),
    private_adhoc=sum(arb_private_adhoc==1,na.rm=TRUE)
  )

write_csv(
  coverage,
  file.path(outdir, "06_arbitration_choice_counts.csv")
)

message("42_classify_arbitration.R completed.")
