suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(purrr)
  library(janitor)
})

source("R/00_config.R")

out <- list()

# Traditional GPR Excel
trad <- "data/raw/gpr/data_gpr_export.xls"
if (file.exists(trad)) {
  sheets <- tryCatch(readxl::excel_sheets(trad), error = function(e) character())
  for (s in sheets) {
    x <- tryCatch(readxl::read_excel(trad, sheet = s), error = function(e) NULL)
    if (!is.null(x)) {
      out[[length(out) + 1]] <- tibble(
        source = "traditional_gpr",
        table = s,
        column_original = names(x),
        column_clean = janitor::make_clean_names(names(x)),
        class = purrr::map_chr(x, ~ paste(class(.x), collapse = "/")),
        nrow = nrow(x)
      )
    }
  }
}

# AI-GPR files
for (f in c(
  "data/raw/gpr/ai_gpr_data_monthly.csv",
  "data/raw/gpr/ai_gpr_country_monthly.csv"
)) {
  if (file.exists(f)) {
    x <- tryCatch(readr::read_csv(f, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(x)) {
      out[[length(out) + 1]] <- tibble(
        source = basename(f),
        table = basename(f),
        column_original = names(x),
        column_clean = janitor::make_clean_names(names(x)),
        class = purrr::map_chr(x, ~ paste(class(.x), collapse = "/")),
        nrow = nrow(x)
      )
    }
  }
}

gpr_dict <- bind_rows(out)
write_csv(gpr_dict, "outputs/stage1_audit/gpr_column_dictionary.csv")

if (nrow(gpr_dict) == 0) {
  warning("No GPR files could be read. See download_status.csv.")
} else {
  message("GPR audit complete.")
  print(gpr_dict %>% select(source, table, column_original) %>% head(40))
}
