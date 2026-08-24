# R/07_audit_and_match_clg.R
# Stage 2A: Audit and match HCL contracts to CLG records
# Revised version: robust matched-contract counting without depending on joined column names.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# -----------------------------
# Helpers
# -----------------------------

normalize_id <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

detect_col <- function(df, candidates, label) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(
      sprintf(
        "Could not find %s column. Tried: %s",
        label,
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  hit[[1]]
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

# -----------------------------
# Paths
# -----------------------------
# Override these with command-line arguments if desired:
# Rscript R/07_audit_and_match_clg.R manifest.csv clg.csv output_dir

args <- commandArgs(trailingOnly = TRUE)

manifest_path <- if (length(args) >= 1) args[[1]] else "data/processed/hcl_manifest.csv"
clg_path      <- if (length(args) >= 2) args[[2]] else "data/raw/clg.csv"
output_dir    <- if (length(args) >= 3) args[[3]] else "outputs/stage2a_clg_match"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load data
# -----------------------------

manifest <- safe_read_csv(manifest_path)
clg <- safe_read_csv(clg_path)

# -----------------------------
# Detect IDs
# -----------------------------

hcl_id_col <- detect_col(
  manifest,
  c(
    "hcl_record_id",
    "record_id",
    "recordid",
    "RecordID",
    "recordId",
    "id"
  ),
  "HCL record ID"
)

clg_id_col <- detect_col(
  clg,
  c(
    "clg_match_id",
    "hcl_record_id",
    "record_id",
    "recordid",
    "RecordID",
    "recordId",
    "id"
  ),
  "CLG match ID"
)

manifest <- manifest %>%
  mutate(
    hcl_record_id = normalize_id(.data[[hcl_id_col]])
  )

clg <- clg %>%
  mutate(
    clg_match_id = normalize_id(.data[[clg_id_col]])
  )

# -----------------------------
# Basic audit
# -----------------------------

n_manifest <- nrow(manifest)
n_manifest_nonmissing_id <- sum(!is.na(manifest$hcl_record_id))
n_manifest_unique_id <- n_distinct(manifest$hcl_record_id, na.rm = TRUE)

n_clg <- nrow(clg)
n_clg_nonmissing_id <- sum(!is.na(clg$clg_match_id))
n_clg_unique_id <- n_distinct(clg$clg_match_id, na.rm = TRUE)

# -----------------------------
# Match
# -----------------------------

# Keep full CLG rows for inspection, but use existence-based counting below.
m <- manifest %>%
  left_join(
    clg,
    by = c("hcl_record_id" = "clg_match_id"),
    suffix = c("_hcl", "_clg")
  )

# IMPORTANT FIX:
# Count matched HCL contracts directly from the original IDs.
# This avoids failures caused by left_join() renaming/removing the expected
# id column, and avoids inflating matched_n when one CLG ID appears multiple times.
matched_flag <- !is.na(manifest$hcl_record_id) &
  manifest$hcl_record_id %in% clg$clg_match_id

matched_n <- sum(matched_flag, na.rm = TRUE)
unmatched_n <- n_manifest - matched_n
match_rate <- if (n_manifest > 0) matched_n / n_manifest else NA_real_

manifest_audit <- manifest %>%
  mutate(
    matched_to_clg = matched_flag
  )

unmatched <- manifest_audit %>%
  filter(!matched_to_clg)

matched_manifest <- manifest_audit %>%
  filter(matched_to_clg)

# Duplicate IDs can create multiple joined rows. Audit them explicitly.
manifest_duplicate_ids <- manifest %>%
  filter(!is.na(hcl_record_id)) %>%
  count(hcl_record_id, name = "n_hcl_rows") %>%
  filter(n_hcl_rows > 1) %>%
  arrange(desc(n_hcl_rows), hcl_record_id)

clg_duplicate_ids <- clg %>%
  filter(!is.na(clg_match_id)) %>%
  count(clg_match_id, name = "n_clg_rows") %>%
  filter(n_clg_rows > 1) %>%
  arrange(desc(n_clg_rows), clg_match_id)

summary_tbl <- tibble(
  metric = c(
    "hcl_rows",
    "hcl_nonmissing_ids",
    "hcl_unique_ids",
    "clg_rows",
    "clg_nonmissing_ids",
    "clg_unique_ids",
    "matched_contracts",
    "unmatched_contracts",
    "match_rate"
  ),
  value = c(
    n_manifest,
    n_manifest_nonmissing_id,
    n_manifest_unique_id,
    n_clg,
    n_clg_nonmissing_id,
    n_clg_unique_id,
    matched_n,
    unmatched_n,
    match_rate
  )
)

# -----------------------------
# Write outputs
# -----------------------------

write_csv(
  summary_tbl,
  file.path(output_dir, "00_clg_match_summary.csv")
)

write_csv(
  manifest_audit,
  file.path(output_dir, "01_hcl_manifest_with_clg_match_flag.csv")
)

write_csv(
  matched_manifest,
  file.path(output_dir, "02_hcl_matched_contracts.csv")
)

write_csv(
  unmatched,
  file.path(output_dir, "03_hcl_unmatched_contracts.csv")
)

write_csv(
  manifest_duplicate_ids,
  file.path(output_dir, "04_hcl_duplicate_ids.csv")
)

write_csv(
  clg_duplicate_ids,
  file.path(output_dir, "05_clg_duplicate_ids.csv")
)

write_csv(
  m,
  file.path(output_dir, "06_hcl_clg_joined_rows.csv")
)

# -----------------------------
# Console report
# -----------------------------

cat("\n==============================\n")
cat("Stage 2A: HCL–CLG match audit\n")
cat("==============================\n")
cat(sprintf("HCL rows:             %d\n", n_manifest))
cat(sprintf("HCL non-missing IDs:  %d\n", n_manifest_nonmissing_id))
cat(sprintf("HCL unique IDs:       %d\n", n_manifest_unique_id))
cat(sprintf("CLG rows:             %d\n", n_clg))
cat(sprintf("CLG non-missing IDs:  %d\n", n_clg_nonmissing_id))
cat(sprintf("CLG unique IDs:       %d\n", n_clg_unique_id))
cat(sprintf("Matched contracts:    %d\n", matched_n))
cat(sprintf("Unmatched contracts:  %d\n", unmatched_n))

if (!is.na(match_rate)) {
  cat(sprintf("Match rate:           %.2f%%\n", 100 * match_rate))
}

cat(sprintf("Output directory:     %s\n", output_dir))
cat("==============================\n\n")

# Fail only for structural problems, not merely because some contracts are unmatched.
if (n_manifest == 0) {
  stop("HCL manifest contains zero rows.", call. = FALSE)
}

if (n_manifest_nonmissing_id == 0) {
  stop("HCL manifest contains no usable record IDs.", call. = FALSE)
}

if (n_clg_nonmissing_id == 0) {
  stop("CLG data contains no usable match IDs.", call. = FALSE)
}

cat("Stage 2A audit completed successfully.\n")
