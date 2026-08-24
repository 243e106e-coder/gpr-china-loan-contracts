suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(haven)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(janitor)
})

options(stringsAsFactors = FALSE, timeout = 1800)

raw_dir <- "data/raw/clg_global"
out_dir <- "outputs/stage2a_pricing_recovery"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Official AidData 2025 CLG-Global 1.0 download link exposed by the
# AidData dataset page:
CLG_URL <- "https://docs.aiddata.org/ad4/datasets/AidDatas_CLG_Global_Dataset_v1.0.zip"
zip_file <- file.path(raw_dir, "AidDatas_CLG_Global_Dataset_v1.0.zip")
unzip_dir <- file.path(raw_dir, "unzipped")

download_ok <- FALSE

if (file.exists(zip_file) && file.info(zip_file)$size > 0) {
  message("CLG ZIP already exists.")
  download_ok <- TRUE
} else {
  message("Downloading AidData CLG-Global 1.0...")
  download_ok <- tryCatch({
    utils::download.file(
      CLG_URL,
      destfile = zip_file,
      mode = "wb",
      method = "libcurl",
      quiet = FALSE
    )
    file.exists(zip_file) && file.info(zip_file)$size > 0
  }, error = function(e) {
    message("CLG DOWNLOAD ERROR: ", conditionMessage(e))
    FALSE
  })
}

write_csv(
  tibble(
    source = "AidData_CLG_Global_v1.0",
    url = CLG_URL,
    local_file = zip_file,
    download_ok = download_ok,
    size_bytes = if (file.exists(zip_file)) file.info(zip_file)$size else NA_real_
  ),
  file.path(out_dir, "00_clg_download_status.csv")
)

if (!download_ok) {
  stop("Could not download the official CLG-Global ZIP.")
}

dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

if (length(list.files(unzip_dir, recursive = TRUE, all.files = FALSE)) == 0) {
  utils::unzip(zip_file, exdir = unzip_dir)
}

all_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)

inventory <- tibble(
  path = all_files,
  file = basename(all_files),
  ext = tolower(tools::file_ext(all_files)),
  size_bytes = file.info(all_files)$size
)

write_csv(inventory, file.path(out_dir, "01_clg_file_inventory.csv"))
message("CLG files found: ", nrow(inventory))
