# R/15_prepare_gpr_annual.R
# Paper 1 — Stage 3
# Prepare annual GPR measures.
#
# Design:
# 1. Traditional global GPR is REQUIRED because the Stage 3 baseline uses it.
# 2. AI global GPR and AI country GPR are OPTIONAL robustness/audit sources.
# 3. Failure to parse AI-country schema must NOT terminate the baseline workflow.
# 4. We export AI-country schema so its actual structure can be inspected before
#    using it in country-specific robustness exercises.

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
})

outdir <- "outputs/stage3_gpr_baseline"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

trad_file <- "data/raw/gpr/data_gpr_export.xls"
ai_global_file <- "data/raw/gpr/ai_gpr_data_monthly.csv"
ai_country_file <- "data/raw/gpr/ai_gpr_country_monthly.csv"

# ------------------------------------------------------------------
# Locate downloaded files if Stage 1 placed them in a different folder
# ------------------------------------------------------------------

locate_file <- function(default_path, pattern) {
  if (file.exists(default_path)) return(default_path)

  hit <- list.files(
    "data/raw",
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(hit) > 0) return(hit[[1]])
  default_path
}

trad_file <- locate_file(
  trad_file,
  "data_gpr_export\\.xls$"
)

ai_global_file <- locate_file(
  ai_global_file,
  "ai_gpr_data_monthly\\.csv$"
)

ai_country_file <- locate_file(
  ai_country_file,
  "ai_gpr_country_monthly\\.csv$"
)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

safe_date <- function(x) {

  # Already Date/POSIX
  if (inherits(x, "Date")) return(x)

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }

  # Excel serial date
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x <- as.character(x)

  # Try common formats.
  out <- suppressWarnings(as.Date(x))

  if (all(is.na(out))) {
    out <- suppressWarnings(
      lubridate::ym(x, quiet = TRUE)
    )
    out <- as.Date(out)
  }

  if (all(is.na(out))) {
    out <- suppressWarnings(
      lubridate::ymd(x, quiet = TRUE)
    )
    out <- as.Date(out)
  }

  out
}

find_date_col <- function(nms) {

  exact <- c(
    "month",
    "date",
    "period",
    "year_month",
    "yearmonth",
    "month_date",
    "time"
  )

  hit <- exact[exact %in% nms]

  if (length(hit) > 0) return(hit[[1]])

  regex_hit <- nms[
    stringr::str_detect(
      nms,
      regex("month|date|period|time", ignore_case = TRUE)
    )
  ]

  if (length(regex_hit) > 0) return(regex_hit[[1]])

  NA_character_
}

find_country_col <- function(nms) {

  exact <- c(
    "country",
    "country_name",
    "countryname",
    "location",
    "economy",
    "entity",
    "country_code",
    "countrycode",
    "iso",
    "iso2",
    "iso3",
    "iso_code",
    "iso3c",
    "name"
  )

  hit <- exact[exact %in% nms]

  if (length(hit) > 0) return(hit[[1]])

  regex_hit <- nms[
    stringr::str_detect(
      nms,
      regex(
        "country|economy|location|entity|iso|nation",
        ignore_case = TRUE
      )
    )
  ]

  if (length(regex_hit) > 0) return(regex_hit[[1]])

  NA_character_
}

write_schema <- function(df, source_name, filename) {

  schema <- tibble(
    source = source_name,
    column = names(df),
    class = vapply(
      df,
      function(x) paste(class(x), collapse = "/"),
      FUN.VALUE = character(1)
    ),
    n_nonmissing = vapply(
      df,
      function(x) {
        sum(
          !is.na(x) &
            trimws(as.character(x)) != ""
        )
      },
      FUN.VALUE = integer(1)
    ),
    sample_values = vapply(
      df,
      function(x) {
        vals <- unique(
          as.character(
            x[
              !is.na(x) &
                trimws(as.character(x)) != ""
            ]
          )
        )

        if (length(vals) == 0) return("")

        paste(
          head(vals, 5),
          collapse = " | "
        )
      },
      FUN.VALUE = character(1)
    )
  )

  write_csv(
    schema,
    file.path(outdir, filename)
  )
}

# ==================================================================
# 1. Traditional global GPR — REQUIRED
# ==================================================================

if (!file.exists(trad_file)) {
  stop(
    "Traditional GPR file missing. Stage 3 baseline cannot proceed.",
    call. = FALSE
  )
}

message("Reading traditional GPR: ", trad_file)

trad <- readxl::read_excel(
  trad_file,
  sheet = 1
)

names(trad) <- janitor::make_clean_names(
  names(trad)
)

write_schema(
  trad,
  "traditional_global",
  "00a_traditional_gpr_schema.csv"
)

if (!("month" %in% names(trad))) {
  stop(
    "Traditional GPR file has no month field.",
    call. = FALSE
  )
}

trad <- trad %>%
  mutate(
    month = safe_date(month),
    year = lubridate::year(month)
  )

wanted_trad <- intersect(
  c(
    "gpr",
    "gprt",
    "gpra",
    "gprh",
    "gprht",
    "gprha"
  ),
  names(trad)
)

if (!("gpr" %in% wanted_trad)) {
  stop(
    "Traditional GPR file does not contain the baseline GPR column.",
    call. = FALSE
  )
}

trad_annual <- trad %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    across(
      all_of(wanted_trad),
      ~ {
        z <- suppressWarnings(as.numeric(.x))
        if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
      }
    ),
    .groups = "drop"
  )

write_csv(
  trad_annual,
  file.path(
    outdir,
    "01_global_gpr_annual.csv"
  )
)

message(
  "Traditional annual GPR prepared: ",
  nrow(trad_annual),
  " years."
)

# ==================================================================
# 2. AI global GPR — OPTIONAL
# ==================================================================

ai_global_ok <- FALSE
ai_global_note <- "File not found or not parsed."

if (file.exists(ai_global_file)) {

  message("Auditing AI global GPR: ", ai_global_file)

  tryCatch({

    ai_global <- read_csv(
      ai_global_file,
      show_col_types = FALSE,
      progress = FALSE
    )

    names(ai_global) <- janitor::make_clean_names(
      names(ai_global)
    )

    write_schema(
      ai_global,
      "ai_global",
      "00b_ai_global_gpr_schema.csv"
    )

    date_col <- find_date_col(
      names(ai_global)
    )

    if (is.na(date_col)) {
      warning(
        "AI global GPR date column not identified; skipping annual AI-global aggregation."
      )
      ai_global_note <<-
        "Date column not identified; schema exported."

    } else {

      ai_global <- ai_global %>%
        mutate(
          gpr_date = safe_date(
            .data[[date_col]]
          ),
          year = lubridate::year(
            gpr_date
          )
        )

      num_cols <- names(ai_global)[
        vapply(
          ai_global,
          is.numeric,
          FUN.VALUE = logical(1)
        )
      ]

      num_cols <- setdiff(
        num_cols,
        "year"
      )

      if (length(num_cols) == 0) {

        warning(
          "AI global GPR has no numeric measure columns; skipping."
        )

        ai_global_note <<-
          "No numeric measure columns detected."

      } else {

        ai_global_annual <- ai_global %>%
          filter(!is.na(year)) %>%
          group_by(year) %>%
          summarise(
            across(
              all_of(num_cols),
              ~ {
                if (all(is.na(.x))) {
                  NA_real_
                } else {
                  mean(.x, na.rm = TRUE)
                }
              }
            ),
            .groups = "drop"
          )

        write_csv(
          ai_global_annual,
          file.path(
            outdir,
            "02_ai_global_gpr_annual.csv"
          )
        )

        ai_global_ok <<- TRUE
        ai_global_note <<-
          paste0(
            "Parsed successfully using date column: ",
            date_col
          )
      }
    }

  }, error = function(e) {

    warning(
      paste0(
        "AI global GPR parsing failed but baseline will continue: ",
        conditionMessage(e)
      )
    )

    ai_global_note <<-
      paste0(
        "Parsing failed: ",
        conditionMessage(e)
      )
  })

} else {

  warning(
    "AI global GPR file not found; baseline will continue with traditional global GPR."
  )

  ai_global_note <-
    "File not found; optional source skipped."
}

# ==================================================================
# 3. AI country GPR — OPTIONAL
# ==================================================================
# IMPORTANT:
# This source is NOT needed for the Stage 3 baseline, which uses
# traditional global GPR. If its schema is unfamiliar, export the
# schema and continue. We will harmonize country GPR in a later
# robustness/identification stage.

ai_country_ok <- FALSE
ai_country_note <- "File not found or not parsed."
ai_country_format <- NA_character_
ai_country_country_col <- NA_character_
ai_country_date_col <- NA_character_

if (file.exists(ai_country_file)) {

  message(
    "Auditing AI country GPR: ",
    ai_country_file
  )

  tryCatch({

    ai_country <- read_csv(
      ai_country_file,
      show_col_types = FALSE,
      progress = FALSE
    )

    names(ai_country) <- janitor::make_clean_names(
      names(ai_country)
    )

    write_schema(
      ai_country,
      "ai_country",
      "00c_ai_country_gpr_schema.csv"
    )

    date_col <- find_date_col(
      names(ai_country)
    )

    country_col <- find_country_col(
      names(ai_country)
    )

    ai_country_date_col <<- date_col
    ai_country_country_col <<- country_col

    # -----------------------------
    # Case A: long format detected
    # -----------------------------
    if (
      !is.na(date_col) &&
      !is.na(country_col)
    ) {

      ai_country_format <<- "long"

      ai_country2 <- ai_country %>%
        mutate(
          gpr_date = safe_date(
            .data[[date_col]]
          ),
          year = lubridate::year(
            gpr_date
          )
        )

      num_cols <- names(ai_country2)[
        vapply(
          ai_country2,
          is.numeric,
          FUN.VALUE = logical(1)
        )
      ]

      num_cols <- setdiff(
        num_cols,
        "year"
      )

      if (length(num_cols) == 0) {

        warning(
          "AI country GPR long-format fields detected, but no numeric measure columns were found. Baseline will continue."
        )

        ai_country_note <<-
          "Long format detected but no numeric GPR measure found."

      } else {

        ai_country_annual <- ai_country2 %>%
          filter(
            !is.na(year),
            !is.na(.data[[country_col]])
          ) %>%
          group_by(
            country = as.character(
              .data[[country_col]]
            ),
            year
          ) %>%
          summarise(
            across(
              all_of(num_cols),
              ~ {
                if (all(is.na(.x))) {
                  NA_real_
                } else {
                  mean(.x, na.rm = TRUE)
                }
              }
            ),
            .groups = "drop"
          )

        write_csv(
          ai_country_annual,
          file.path(
            outdir,
            "03_ai_country_gpr_annual.csv"
          )
        )

        ai_country_ok <<- TRUE
        ai_country_note <<-
          paste0(
            "Long format parsed; country=",
            country_col,
            ", date=",
            date_col
          )
      }

    # -----------------------------
    # Case B: likely wide format
    # -----------------------------
    } else if (
      !is.na(date_col) &&
      is.na(country_col)
    ) {

      ai_country_format <<-
        "possible_wide_format"

      non_date_cols <- setdiff(
        names(ai_country),
        date_col
      )

      wide_audit <- tibble(
        possible_country_measure_column =
          non_date_cols,
        class = vapply(
          ai_country[
            non_date_cols
          ],
          function(x) {
            paste(
              class(x),
              collapse = "/"
            )
          },
          FUN.VALUE = character(1)
        )
      )

      write_csv(
        wide_audit,
        file.path(
          outdir,
          "03a_ai_country_possible_wide_columns.csv"
        )
      )

      warning(
        paste0(
          "AI country GPR has a date column ('",
          date_col,
          "') but no explicit country column. ",
          "It may be wide-format. Schema exported; ",
          "country-specific aggregation skipped for Stage 3 baseline."
        )
      )

      ai_country_note <<-
        paste0(
          "Possible wide format; date=",
          date_col,
          "; no explicit country column. Baseline unaffected."
        )

    # -----------------------------
    # Case C: unknown schema
    # -----------------------------
    } else {

      ai_country_format <<-
        "unrecognized"

      warning(
        paste0(
          "Could not safely identify AI country GPR long-format schema. ",
          "Schema has been exported to 00c_ai_country_gpr_schema.csv. ",
          "Stage 3 baseline will continue using traditional global GPR."
        )
      )

      ai_country_note <<-
        "Schema unrecognized; optional country-GPR parsing skipped."
    }

  }, error = function(e) {

    warning(
      paste0(
        "AI country GPR parsing failed but Stage 3 baseline will continue: ",
        conditionMessage(e)
      )
    )

    ai_country_note <<-
      paste0(
        "Parsing failed: ",
        conditionMessage(e)
      )
  })

} else {

  warning(
    "AI country GPR file not found; optional country-specific source skipped."
  )

  ai_country_note <-
    "File not found; optional source skipped."
}

# ==================================================================
# 4. Source-status manifest
# ==================================================================

source_manifest <- tibble(
  source = c(
    "traditional_global",
    "ai_global",
    "ai_country"
  ),

  file = c(
    trad_file,
    ai_global_file,
    ai_country_file
  ),

  required_for_stage3_baseline = c(
    TRUE,
    FALSE,
    FALSE
  ),

  parsed_successfully = c(
    TRUE,
    ai_global_ok,
    ai_country_ok
  ),

  note = c(
    paste0(
      "Required baseline source; annual global GPR generated with ",
      nrow(trad_annual),
      " year observations."
    ),
    ai_global_note,
    ai_country_note
  )
)

write_csv(
  source_manifest,
  file.path(
    outdir,
    "00_gpr_source_manifest.csv"
  )
)

ai_country_detection <- tibble(
  format_detected = ai_country_format,
  date_column_detected = ai_country_date_col,
  country_column_detected = ai_country_country_col,
  parsed_successfully = ai_country_ok,
  note = ai_country_note
)

write_csv(
  ai_country_detection,
  file.path(
    outdir,
    "00d_ai_country_detection_status.csv"
  )
)

cat("\n========================================\n")
cat("Stage 3 — GPR Preparation\n")
cat("========================================\n")
cat("Traditional global GPR: REQUIRED / READY\n")
cat(
  "AI global GPR: ",
  ifelse(ai_global_ok, "READY", "OPTIONAL / SKIPPED"),
  "\n",
  sep = ""
)
cat(
  "AI country GPR: ",
  ifelse(ai_country_ok, "READY", "OPTIONAL / SKIPPED"),
  "\n",
  sep = ""
)
cat("Stage 3 baseline can proceed with traditional global GPR.\n")
cat("========================================\n\n")

message("R/15_prepare_gpr_annual.R completed successfully.")
