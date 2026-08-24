suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(haven)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(janitor)
})

options(stringsAsFactors = FALSE)

raw_dir <- "data/raw/clg_global/unzipped"
out_dir <- "outputs/stage2a_pricing_recovery"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- helpers ----------
safe_read <- function(path, sheet = NA_character_) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(path, sheet = sheet)
    } else if (ext == "dta") {
      haven::read_dta(path)
    } else if (ext == "rds") {
      readRDS(path)
    } else {
      NULL
    }
  }, error = function(e) {
    structure(list(error = conditionMessage(e)), class = "read_error")
  })
}

norm_id <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  # Keep digits and decimal-free integer representation when Excel reads IDs numeric
  x <- ifelse(
    !is.na(x) & str_detect(x, "^[0-9]+\\.0+$"),
    str_replace(x, "\\.0+$", ""),
    x
  )
  x
}

# ---------- locate CLG tabular data ----------
all_files <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
tab_files <- all_files[
  tolower(tools::file_ext(all_files)) %in% c("csv", "xlsx", "xls", "dta", "rds")
]

if (length(tab_files) == 0) stop("No tabular CLG files found.")

specs <- list()
for (p in tab_files) {
  ext <- tolower(tools::file_ext(p))
  if (ext %in% c("xlsx", "xls")) {
    sheets <- tryCatch(readxl::excel_sheets(p), error = function(e) character())
    for (s in sheets) specs[[length(specs)+1]] <- list(path=p, sheet=s)
  } else {
    specs[[length(specs)+1]] <- list(path=p, sheet=NA_character_)
  }
}

# Candidate patterns are intentionally broad for AUDIT only.
patterns <- list(
  record_id = "(^|_)(record|project).*id($|_)|aiddata.*record.*id|^id$",
  parent_id = "parent.*id",
  interest_rate = "interest.*rate|lending.*rate|rate.*interest",
  spread = "spread|margin|basis.*point|bps",
  maturity = "maturity",
  grace_period = "grace",
  tenor = "tenor|loan.*term|term.*year",
  repayment = "repayment|amort",
  commitment_amount = "commitment.*amount|amount.*usd|commitment.*usd",
  currency = "currency",
  country = "recipient.*country|borrower.*country|country.*name|country",
  year = "commitment.*year|year",
  lender = "financier|lender|creditor",
  flow_type = "flow.*type|financial.*type|loan"
)

table_inventory <- list()
column_dict <- list()
candidate_dict <- list()
loaded_tables <- list()

for (sp in specs) {
  x <- safe_read(sp$path, sp$sheet)
  tbl_name <- ifelse(
    is.na(sp$sheet),
    basename(sp$path),
    paste0(basename(sp$path), "::", sp$sheet)
  )

  if (inherits(x, "read_error") || !is.data.frame(x)) {
    table_inventory[[length(table_inventory)+1]] <- tibble(
      table = tbl_name, path = sp$path, sheet = sp$sheet,
      nrow = NA_integer_, ncol = NA_integer_,
      read_ok = FALSE,
      error = if (inherits(x, "read_error")) x$error else "not data.frame"
    )
    next
  }

  original <- names(x)
  clean <- janitor::make_clean_names(original)
  names(x) <- clean

  loaded_tables[[tbl_name]] <- x

  table_inventory[[length(table_inventory)+1]] <- tibble(
    table = tbl_name, path = sp$path, sheet = sp$sheet,
    nrow = nrow(x), ncol = ncol(x),
    read_ok = TRUE, error = NA_character_
  )

  column_dict[[length(column_dict)+1]] <- tibble(
    table = tbl_name,
    column_original = original,
    column_clean = clean,
    class = map_chr(x, ~ paste(class(.x), collapse="/")),
    n_nonmissing = map_int(x, ~ sum(!is.na(.x)))
  )

  for (role in names(patterns)) {
    hit <- clean[str_detect(clean, regex(patterns[[role]], ignore_case=TRUE))]
    if (length(hit)) {
      candidate_dict[[length(candidate_dict)+1]] <- tibble(
        table = tbl_name,
        role = role,
        column_clean = hit
      )
    }
  }
}

table_inventory_df <- bind_rows(table_inventory) %>% arrange(desc(read_ok), desc(nrow))
column_dict_df <- bind_rows(column_dict)
candidate_df <- bind_rows(candidate_dict) %>% distinct() %>% arrange(role, table)

write_csv(table_inventory_df, file.path(out_dir, "02_clg_table_inventory.csv"))
write_csv(column_dict_df, file.path(out_dir, "03_clg_column_dictionary.csv"))
write_csv(candidate_df, file.path(out_dir, "04_clg_pricing_id_candidates.csv"))

# ---------- load HCL pricing recovery manifest ----------
manifest_file <- "outputs/stage1_5_pricing_recovery/09_pricing_recovery_manifest.csv"

if (!file.exists(manifest_file)) {
  stop("Stage 1.5 manifest not found: ", manifest_file)
}

manifest <- readr::read_csv(manifest_file, show_col_types = FALSE) %>%
  mutate(
    hcl_record_id = norm_id(aid_data_record_id),
    hcl_parent_id = norm_id(aid_data_parent_id)
  )

# ---------- score tables for likely project-level dataset ----------
score_table <- candidate_df %>%
  mutate(
    weight = case_when(
      role == "record_id" ~ 10,
      role == "interest_rate" ~ 5,
      role == "maturity" ~ 5,
      role == "grace_period" ~ 5,
      role == "spread" ~ 3,
      role == "repayment" ~ 2,
      TRUE ~ 0
    )
  ) %>%
  group_by(table) %>%
  summarise(score = sum(weight), roles = paste(sort(unique(role)), collapse=";"), .groups="drop") %>%
  left_join(table_inventory_df %>% select(table, nrow, ncol), by="table") %>%
  arrange(desc(score), desc(nrow))

write_csv(score_table, file.path(out_dir, "05_clg_table_scores.csv"))

if (nrow(score_table) == 0) stop("Could not identify any CLG candidate table.")

# Try tables in descending score until we find one with an ID and matches.
match_results <- list()
table_match_summary <- list()

for (tbl_name in score_table$table) {
  x <- loaded_tables[[tbl_name]]
  if (is.null(x)) next

  tbl_cands <- candidate_df %>% filter(table == tbl_name)
  id_cols <- tbl_cands %>% filter(role == "record_id") %>% pull(column_clean)

  if (length(id_cols) == 0) next

  # Try each ID candidate independently; do not silently pick by name alone.
  for (id_col in id_cols) {
    x2 <- x %>%
      mutate(clg_match_id = norm_id(.data[[id_col]]))

    m <- manifest %>%
      left_join(x2, by = c("hcl_record_id" = "clg_match_id"), suffix = c("_hcl", "_clg"))

    matched_n <- sum(!is.na(m[[id_col]]), na.rm=TRUE)
    # In some dplyr joins the original ID column may disappear if it was join column;
    # use nonmissing pricing/project fields as backup evidence.
    if (!(id_col %in% names(m))) {
      matched_n <- sum(manifest$hcl_record_id %in% x2$clg_match_id, na.rm=TRUE)
    }

    table_match_summary[[length(table_match_summary)+1]] <- tibble(
      table = tbl_name,
      id_column = id_col,
      matched_contracts = matched_n,
      total_hcl = nrow(manifest),
      pct_matched = 100 * matched_n / nrow(manifest)
    )
  }
}

match_summary <- bind_rows(table_match_summary) %>%
  arrange(desc(matched_contracts))

write_csv(match_summary, file.path(out_dir, "06_record_id_match_summary.csv"))

if (nrow(match_summary) == 0 || max(match_summary$matched_contracts, na.rm=TRUE) == 0) {
  warning("No exact record-ID matches found in CLG. Parent-ID recovery will be audited next.")
}

# Pick the highest exact-match combination.
best <- match_summary %>% slice_max(matched_contracts, n=1, with_ties=FALSE)

if (nrow(best) == 1 && best$matched_contracts > 0) {
  best_table <- best$table
  best_id <- best$id_column
  clg <- loaded_tables[[best_table]] %>%
    mutate(clg_match_id = norm_id(.data[[best_id]]))

  merged <- manifest %>%
    left_join(clg, by = c("hcl_record_id" = "clg_match_id"), suffix = c("_hcl", "_clg"))

  write_csv(merged, file.path(out_dir, "07_hcl_clg_exact_record_matches.csv"))

  # Identify actual pricing columns in the chosen table.
  chosen_cands <- candidate_df %>% filter(table == best_table)

  pricing_roles <- c(
    "interest_rate", "spread", "maturity", "grace_period",
    "tenor", "repayment", "commitment_amount", "currency"
  )

  actual_pricing <- chosen_cands %>%
    filter(role %in% pricing_roles) %>%
    distinct(role, column_clean)

  write_csv(actual_pricing, file.path(out_dir, "08_actual_pricing_columns.csv"))

  pricing_coverage <- map_dfr(seq_len(nrow(actual_pricing)), function(i) {
    role <- actual_pricing$role[i]
    col <- actual_pricing$column_clean[i]
    # Joined columns may have _clg suffix when HCL has same name.
    candidates <- c(col, paste0(col, "_clg"))
    found <- candidates[candidates %in% names(merged)][1]

    if (is.na(found) || length(found) == 0) {
      return(tibble(role=role, column=col, n_nonmissing=0L, pct_of_371=0))
    }

    val <- merged[[found]]
    n_non <- sum(!is.na(val) & trimws(as.character(val)) != "")
    tibble(
      role=role,
      column=found,
      n_nonmissing=n_non,
      pct_of_371=100*n_non/nrow(manifest)
    )
  })

  write_csv(pricing_coverage, file.path(out_dir, "09_pricing_recovery_coverage.csv"))

  # Compact recovered dataset with HCL identifiers plus all discovered pricing fields.
  pricing_cols_in_merged <- pricing_coverage$column[
    pricing_coverage$column %in% names(merged)
  ]

  id_keep <- intersect(
    c(
      "contract_id", "aid_data_record_id", "aid_data_parent_id",
      "year", "borrower_country", "borrower_name",
      "creditor_name", "creditor_type", "commitment_usd",
      "loan_contract", "contract_category", "ppg_debt",
      "project_title", "source",
      "hcl_record_id", "hcl_parent_id"
    ),
    names(merged)
  )

  recovered <- merged %>%
    select(all_of(unique(c(id_keep, pricing_cols_in_merged))))

  write_csv(recovered, file.path(out_dir, "10_recovered_pricing_dataset.csv"))

} else {
  write_csv(tibble(note="No exact matches recovered."), file.path(out_dir, "08_actual_pricing_columns.csv"))
  write_csv(tibble(note="No exact matches recovered."), file.path(out_dir, "09_pricing_recovery_coverage.csv"))
  write_csv(manifest, file.path(out_dir, "10_recovered_pricing_dataset.csv"))
}
