# R/21_map_borrower_country_to_ai_gpr.R
# Stage 3B — Map HCL borrower countries to AI country-GPR keys.
# FIX: explicitly create country_raw before joins so unmatched diagnostics
# can always reference it.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(countrycode)
})

outdir <- "outputs/stage3b_identification"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

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

# ------------------------------------------------------------------
# Identify borrower-country field
# ------------------------------------------------------------------

country_candidates <- c(
  "borrower_country",
  "country"
)

country_var <- country_candidates[
  country_candidates %in% names(contracts)
][1]

if (is.na(country_var)) {
  stop(
    "Borrower-country field missing.",
    call. = FALSE
  )
}

message(
  "Using borrower-country field: ",
  country_var
)

# IMPORTANT FIX:
# Create a stable country_raw field BEFORE joins.
contracts2 <- contracts %>%
  mutate(
    country_raw = as.character(
      .data[[country_var]]
    )
  )

# ------------------------------------------------------------------
# Borrower-country -> ISO3 mapping
# ------------------------------------------------------------------

cmap <- contracts2 %>%
  distinct(country_raw) %>%
  mutate(
    iso3 = countrycode(
      country_raw,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  )

# A few common alternative country labels, if present.
cmap <- cmap %>%
  mutate(
    iso3 = case_when(
      str_to_lower(country_raw) %in% c(
        "russia",
        "russian federation"
      ) ~ "RUS",

      str_to_lower(country_raw) %in% c(
        "viet nam",
        "vietnam"
      ) ~ "VNM",

      str_to_lower(country_raw) %in% c(
        "south korea",
        "korea, rep.",
        "republic of korea"
      ) ~ "KOR",

      str_to_lower(country_raw) %in% c(
        "laos",
        "lao pdr",
        "lao people's democratic republic"
      ) ~ "LAO",

      str_to_lower(country_raw) %in% c(
        "iran",
        "iran, islamic rep.",
        "islamic republic of iran"
      ) ~ "IRN",

      TRUE ~ iso3
    )
  )

# ------------------------------------------------------------------
# AI-country key -> ISO3 mapping
# ------------------------------------------------------------------

gmap <- gpr %>%
  distinct(country_key) %>%
  mutate(
    key_upper = str_to_upper(country_key),

    iso3 = case_when(
      key_upper %in% c("USA", "US") ~ "USA",
      key_upper %in% c("UK", "UNITEDKINGDOM") ~ "GBR",
      key_upper == "RUSSIA" ~ "RUS",
      key_upper %in% c("VIETNAM", "VIETNAM") ~ "VNM",
      key_upper == "ISRAEL" ~ "ISR",
      key_upper == "CHINA" ~ "CHN",
      key_upper == "INDIA" ~ "IND",
      key_upper == "JAPAN" ~ "JPN",
      key_upper %in% c("KOREA", "SOUTHKOREA") ~ "KOR",

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

# ------------------------------------------------------------------
# Prepare country-GPR panel indexed by ISO3 + year
# ------------------------------------------------------------------

gpr_iso <- gpr %>%
  left_join(
    gmap,
    by = "country_key"
  ) %>%
  filter(
    !is.na(iso3)
  ) %>%
  select(
    iso3,
    year,
    starts_with("gpr_ai_")
  )

# Check whether multiple AI keys map to same ISO3-year.
gpr_dup <- gpr_iso %>%
  count(
    iso3,
    year,
    name = "n_rows"
  ) %>%
  filter(
    n_rows > 1
  ) %>%
  arrange(
    desc(n_rows),
    iso3,
    year
  )

write_csv(
  gpr_dup,
  file.path(
    outdir,
    "05b_ai_country_iso3_year_duplicates.csv"
  )
)

# If duplicate ISO3-year rows exist, aggregate rather than multiplying contracts.
gpr_iso_unique <- gpr_iso %>%
  group_by(
    iso3,
    year
  ) %>%
  summarise(
    across(
      starts_with("gpr_ai_"),
      ~ if (all(is.na(.x))) {
        NA_real_
      } else {
        mean(.x, na.rm = TRUE)
      }
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# Merge onto contracts
# ------------------------------------------------------------------

merged <- contracts2 %>%
  left_join(
    cmap,
    by = "country_raw"
  ) %>%
  left_join(
    gpr_iso_unique,
    by = c(
      "iso3",
      "year"
    )
  )

# Guard against accidental row multiplication.
if (nrow(merged) != nrow(contracts2)) {
  stop(
    paste0(
      "Country-GPR merge changed contract row count: ",
      nrow(contracts2),
      " -> ",
      nrow(merged)
    ),
    call. = FALSE
  )
}

write_csv(
  merged,
  file.path(
    outdir,
    "06_contracts_with_country_gpr.csv"
  )
)

# ------------------------------------------------------------------
# Coverage diagnostics
# ------------------------------------------------------------------

has_all <- if ("gpr_ai_all" %in% names(merged)) {
  !is.na(merged$gpr_ai_all)
} else {
  rep(FALSE, nrow(merged))
}

coverage <- tibble(
  contracts = nrow(merged),

  mapped_iso3 = sum(
    !is.na(merged$iso3)
  ),

  pct_mapped_iso3 =
    100 * sum(!is.na(merged$iso3)) /
    nrow(merged),

  with_country_gpr_all = sum(
    has_all
  ),

  pct_country_gpr =
    100 * sum(has_all) /
    nrow(merged)
)

write_csv(
  coverage,
  file.path(
    outdir,
    "07_country_gpr_merge_coverage.csv"
  )
)

# ------------------------------------------------------------------
# Unmatched-country audit
# FIXED: country_raw now always exists.
# ------------------------------------------------------------------

unmatched <- merged %>%
  filter(
    !has_all
  ) %>%
  count(
    country_raw,
    iso3,
    sort = TRUE,
    name = "n_contracts"
  )

write_csv(
  unmatched,
  file.path(
    outdir,
    "08_country_gpr_unmatched.csv"
  )
)

# Also report mapped countries successfully covered.
matched_country <- merged %>%
  filter(
    has_all
  ) %>%
  count(
    country_raw,
    iso3,
    sort = TRUE,
    name = "n_contracts"
  )

write_csv(
  matched_country,
  file.path(
    outdir,
    "08b_country_gpr_matched.csv"
  )
)

cat("\n========================================\n")
cat("Stage 3B — Borrower Country GPR Mapping\n")
cat("========================================\n")
cat("Contracts: ", nrow(merged), "\n", sep = "")
cat(
  "ISO3 mapped: ",
  coverage$mapped_iso3,
  " (",
  round(coverage$pct_mapped_iso3, 1),
  "%)\n",
  sep = ""
)
cat(
  "Country GPR(all) matched: ",
  coverage$with_country_gpr_all,
  " (",
  round(coverage$pct_country_gpr, 1),
  "%)\n",
  sep = ""
)
cat("========================================\n\n")

message("21_map_borrower_country_to_ai_gpr.R completed successfully.")
