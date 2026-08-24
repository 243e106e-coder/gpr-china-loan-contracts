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

source("R/00_config.R")

base <- "data/raw/hcl2/unzipped"

if (!dir.exists(base)) {
  stop("HCL 2.0 is not unzipped. Run R/01_download_data.R first.")
}

all_files <- list.files(base, recursive = TRUE, full.names = TRUE)
file_inventory <- tibble(
  path = all_files,
  file = basename(all_files),
  ext = tolower(tools::file_ext(all_files)),
  size_bytes = ifelse(file.info(all_files)$isdir, NA_real_, file.info(all_files)$size)
)

write_csv(file_inventory, "outputs/stage1_audit/hcl_file_inventory.csv")

tab_files <- file_inventory %>%
  filter(ext %in% c("csv", "xlsx", "xls", "dta", "rds"))

safe_read <- function(path, sheet = NA_character_, n_max = Inf) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE, n_max = n_max)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(path, sheet = sheet, n_max = n_max)
    } else if (ext == "dta") {
      x <- haven::read_dta(path)
      if (is.finite(n_max)) x <- head(x, n_max)
      x
    } else if (ext == "rds") {
      x <- readRDS(path)
      if (is.data.frame(x) && is.finite(n_max)) x <- head(x, n_max)
      x
    } else {
      NULL
    }
  }, error = function(e) {
    structure(list(error = conditionMessage(e)), class = "audit_read_error")
  })
}

table_specs <- list()

for (p in tab_files$path) {
  ext <- tolower(tools::file_ext(p))
  if (ext %in% c("xlsx", "xls")) {
    sheets <- tryCatch(readxl::excel_sheets(p), error = function(e) character())
    if (length(sheets) == 0) sheets <- NA_character_
    for (s in sheets) table_specs[[length(table_specs) + 1]] <- list(path = p, sheet = s)
  } else {
    table_specs[[length(table_specs) + 1]] <- list(path = p, sheet = NA_character_)
  }
}

candidate_patterns <- list(
  contract_id = "(contract|loan|record|project).*(id|number|no)|^id$",
  borrower_country = "borrow.*country|recipient.*country|debtor.*country|country",
  borrower = "borrower|debtor|recipient",
  lender = "lender|creditor|financier",
  signing_date = "sign.*date|signature.*date|contract.*date|agreement.*date|commit.*date|date.*sign",
  year = "(^|_)year($|_)|sign.*year|contract.*year|commit.*year",
  interest_rate = "interest.*rate|rate.*interest|all.in.*rate|coupon",
  spread_margin = "spread|margin|basis.*point|bps",
  maturity = "maturity|tenor|term.*year|loan.*term",
  grace_period = "grace",
  amount = "principal|loan.*amount|commit.*amount|face.*value|amount",
  currency = "currency",
  collateral = "collateral|secured|security",
  escrow = "escrow|special.*account|revenue.*account",
  guarantee = "guarantee|guarantor",
  cross_default = "cross.*default",
  acceleration = "accelerat",
  termination = "terminat|cancel",
  sovereign_immunity = "sovereign.*immun|immunity",
  governing_law = "governing.*law|applicable.*law",
  dispute_resolution = "arbitrat|dispute.*resolution|jurisdiction",
  confidentiality = "confidential|secrecy",
  paris_club = "paris.*club",
  sector = "sector|industry",
  ppg = "public.*guarante|ppg"
)

table_rows <- list()
col_rows <- list()
miss_rows <- list()
candidate_rows <- list()

for (sp in table_specs) {
  x <- safe_read(sp$path, sp$sheet)
  table_name <- ifelse(is.na(sp$sheet), basename(sp$path),
                       paste0(basename(sp$path), "::", sp$sheet))

  if (inherits(x, "audit_read_error")) {
    table_rows[[length(table_rows) + 1]] <- tibble(
      table = table_name, path = sp$path, sheet = sp$sheet,
      nrow = NA_integer_, ncol = NA_integer_, read_ok = FALSE,
      error = x$error
    )
    next
  }

  if (!is.data.frame(x)) {
    table_rows[[length(table_rows) + 1]] <- tibble(
      table = table_name, path = sp$path, sheet = sp$sheet,
      nrow = NA_integer_, ncol = NA_integer_, read_ok = FALSE,
      error = "Object is not a data.frame"
    )
    next
  }

  original_names <- names(x)
  clean_names <- janitor::make_clean_names(original_names)

  table_rows[[length(table_rows) + 1]] <- tibble(
    table = table_name, path = sp$path, sheet = sp$sheet,
    nrow = nrow(x), ncol = ncol(x), read_ok = TRUE, error = NA_character_
  )

  col_rows[[length(col_rows) + 1]] <- tibble(
    table = table_name,
    column_original = original_names,
    column_clean = clean_names,
    class = map_chr(x, ~ paste(class(.x), collapse = "/"))
  )

  miss_rows[[length(miss_rows) + 1]] <- tibble(
    table = table_name,
    column_original = original_names,
    column_clean = clean_names,
    n = nrow(x),
    n_missing = map_int(x, ~ sum(is.na(.x) | (is.character(.x) & trimws(.x) == ""))),
    pct_missing = if (nrow(x) == 0) NA_real_ else
      100 * map_int(x, ~ sum(is.na(.x) | (is.character(.x) & trimws(.x) == ""))) / nrow(x)
  )

  for (role in names(candidate_patterns)) {
    hits <- which(str_detect(clean_names, regex(candidate_patterns[[role]], ignore_case = TRUE)))
    if (length(hits)) {
      candidate_rows[[length(candidate_rows) + 1]] <- tibble(
        table = table_name,
        role = role,
        column_original = original_names[hits],
        column_clean = clean_names[hits]
      )
    }
  }
}

table_inventory <- bind_rows(table_rows) %>%
  arrange(desc(read_ok), desc(nrow), desc(ncol))

column_dictionary <- bind_rows(col_rows)
missingness <- bind_rows(miss_rows)
column_candidates <- bind_rows(candidate_rows) %>%
  distinct() %>%
  arrange(role, table, column_clean)

write_csv(table_inventory, "outputs/stage1_audit/hcl_table_inventory.csv")
write_csv(column_dictionary, "outputs/stage1_audit/hcl_column_dictionary.csv")
write_csv(missingness, "outputs/stage1_audit/hcl_missingness.csv")
write_csv(column_candidates, "outputs/stage1_audit/hcl_column_candidates.csv")

# A compact summary of which tables contain the most relevant candidate fields.
coverage <- column_candidates %>%
  count(table, role) %>%
  count(table, name = "n_candidate_roles") %>%
  left_join(table_inventory %>% select(table, nrow, ncol, read_ok), by = "table") %>%
  arrange(desc(n_candidate_roles), desc(nrow))

write_csv(coverage, "outputs/stage1_audit/hcl_table_candidate_coverage.csv")

message("HCL audit complete.")
message("Top candidate tables:")
print(head(coverage, 10))
