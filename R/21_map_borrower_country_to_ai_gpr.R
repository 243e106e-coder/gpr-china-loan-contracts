suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(countrycode)
})

outdir <- "outputs/stage3b_identification"

contracts <- read_csv(
  "outputs/stage3_gpr_baseline/07_stage3_estimation_sample.csv",
  show_col_types = FALSE,
  progress = FALSE
)

gpr <- read_csv(
  file.path(
    outdir,
    "02_ai_country_gpr_annual_long.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

country_candidates <- c(
  "borrower_country",
  "country"
)

country_var <- country_candidates[
  country_candidates %in% names(contracts)
][1]

if (is.na(country_var)) {
  stop("Borrower-country field missing.", call. = FALSE)
}

cmap <- contracts %>%
  distinct(
    country_raw = .data[[country_var]]
  ) %>%
  mutate(
    iso3 = countrycode(
      country_raw,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  )

gmap <- gpr %>%
  distinct(country_key) %>%
  mutate(
    key_upper = str_to_upper(country_key),

    iso3 = case_when(
      key_upper == "USA" ~ "USA",
      key_upper == "US" ~ "USA",
      key_upper == "UK" ~ "GBR",
      key_upper == "UNITEDKINGDOM" ~ "GBR",
      key_upper == "RUSSIA" ~ "RUS",
      key_upper == "VIETNAM" ~ "VNM",
      key_upper == "ISRAEL" ~ "ISR",
      key_upper == "CHINA" ~ "CHN",
      key_upper == "INDIA" ~ "IND",
      key_upper == "JAPAN" ~ "JPN",
      key_upper == "KOREA" ~ "KOR",
      key_upper == "SOUTHKOREA" ~ "KOR",

      TRUE ~ countrycode(
        country_key,
        origin = "country.name",
        destination = "iso3c",
        warn = FALSE
      )
    )
  )

write_csv(
  cmap,
  file.path(
    outdir,
    "04_borrower_country_mapping.csv"
  )
)

write_csv(
  gmap,
  file.path(
    outdir,
    "05_ai_country_key_mapping.csv"
  )
)

gpr_iso <- gpr %>%
  left_join(
    gmap,
    by = "country_key"
  ) %>%
  filter(!is.na(iso3)) %>%
  select(
    -country_key,
    -key_upper
  )

merged <- contracts %>%
  left_join(
    cmap,
    by = setNames(
      "country_raw",
      country_var
    )
  ) %>%
  left_join(
    gpr_iso,
    by = c(
      "iso3",
      "year"
    )
  )

write_csv(
  merged,
  file.path(
    outdir,
    "06_contracts_with_country_gpr.csv"
  )
)

coverage <- merged %>%
  summarise(
    contracts = n(),
    mapped_iso3 = sum(!is.na(iso3)),
    with_country_gpr_all = sum(!is.na(gpr_ai_all)),
    pct_country_gpr =
      100 * with_country_gpr_all / contracts
  )

write_csv(
  coverage,
  file.path(
    outdir,
    "07_country_gpr_merge_coverage.csv"
  )
)

unmatched <- merged %>%
  filter(is.na(gpr_ai_all)) %>%
  count(
    country_raw,
    iso3,
    sort = TRUE
  )

write_csv(
  unmatched,
  file.path(
    outdir,
    "08_country_gpr_unmatched.csv"
  )
)

message("21_map_borrower_country_to_ai_gpr.R completed.")
