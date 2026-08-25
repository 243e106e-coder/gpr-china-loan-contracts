suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
})

outdir <- "outputs/stage3b_identification"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

f <- "data/raw/gpr/ai_gpr_country_monthly.csv"

if (!file.exists(f)) {
  hit <- list.files(
    "data/raw",
    pattern = "ai_gpr_country_monthly\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(hit)) f <- hit[[1]]
}

if (!file.exists(f)) {
  stop("AI country GPR file missing.", call. = FALSE)
}

x <- read_csv(
  f,
  show_col_types = FALSE,
  progress = FALSE
)

names(x) <- janitor::make_clean_names(names(x))

date_candidates <- c("date", "month", "period")
date_col <- date_candidates[
  date_candidates %in% names(x)
][1]

if (is.na(date_col)) {
  stop("No date/month column detected in AI country GPR.", call. = FALSE)
}

parse_date <- function(z) {
  if (inherits(z, "Date")) return(z)

  if (is.numeric(z)) {
    return(as.Date(z, origin = "1899-12-30"))
  }

  z <- as.character(z)

  out <- suppressWarnings(as.Date(z))

  if (all(is.na(out))) {
    out <- as.Date(
      suppressWarnings(
        lubridate::ym(z, quiet = TRUE)
      )
    )
  }

  if (all(is.na(out))) {
    out <- as.Date(
      suppressWarnings(
        lubridate::ymd(z, quiet = TRUE)
      )
    )
  }

  out
}

measure_cols <- names(x)[
  str_detect(
    names(x),
    "_(all|initiator|respondent|spillover)$"
  )
]

if (length(measure_cols) == 0) {
  stop(
    "No AI-country wide GPR columns ending in _all/_initiator/_respondent/_spillover.",
    call. = FALSE
  )
}

long <- x %>%
  mutate(
    gpr_date = parse_date(.data[[date_col]]),
    year = lubridate::year(gpr_date)
  ) %>%
  select(
    gpr_date,
    year,
    all_of(measure_cols)
  ) %>%
  pivot_longer(
    cols = all_of(measure_cols),
    names_to = "country_measure",
    values_to = "gpr_ai"
  ) %>%
  extract(
    country_measure,
    into = c("country_key", "gpr_role"),
    regex = "^(.*)_(all|initiator|respondent|spillover)$",
    remove = TRUE
  ) %>%
  mutate(
    gpr_ai = suppressWarnings(
      as.numeric(gpr_ai)
    )
  )

write_csv(
  long,
  file.path(
    outdir,
    "01_ai_country_gpr_monthly_long.csv"
  )
)

annual <- long %>%
  filter(!is.na(year)) %>%
  group_by(
    country_key,
    gpr_role,
    year
  ) %>%
  summarise(
    gpr_ai = if (all(is.na(gpr_ai))) {
      NA_real_
    } else {
      mean(gpr_ai, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = gpr_role,
    values_from = gpr_ai,
    names_prefix = "gpr_ai_"
  )

write_csv(
  annual,
  file.path(
    outdir,
    "02_ai_country_gpr_annual_long.csv"
  )
)

write_csv(
  annual %>%
    count(country_key, sort = TRUE),
  file.path(
    outdir,
    "03_ai_country_keys.csv"
  )
)

message("20_parse_ai_country_gpr_wide.R completed.")
