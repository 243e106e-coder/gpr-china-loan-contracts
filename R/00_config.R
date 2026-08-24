options(stringsAsFactors = FALSE, timeout = 900)

dirs <- c(
  "data/raw/hcl2",
  "data/raw/gpr",
  "outputs/stage1_audit"
)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

HCL_URL <- "https://docs.aiddata.org/ad4/datasets/How_China_Lends_Dataset_Version_2_0.zip"

# Official GPR site file paths.
# If Iacoviello changes a download filename in a future vintage, Stage 1 will
# report the failure without silently substituting another source.
GPR_TRADITIONAL_XLS_URL <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"
AI_GPR_MONTHLY_URL <- "https://www.matteoiacoviello.com/ai_gpr_files/ai_gpr_data_monthly.csv"
AI_GPR_COUNTRY_URL <- "https://www.matteoiacoviello.com/ai_gpr_files/ai_gpr_country_monthly.csv"

download_checked <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("Already exists: ", dest)
    return(invisible(TRUE))
  }

  message("Downloading: ", url)
  ok <- tryCatch({
    utils::download.file(url, destfile = dest, mode = "wb", method = "libcurl", quiet = FALSE)
    file.exists(dest) && file.info(dest)$size > 0
  }, error = function(e) {
    message("DOWNLOAD ERROR: ", conditionMessage(e))
    FALSE
  })

  if (!ok) {
    if (file.exists(dest)) unlink(dest)
    message("Could not download ", url)
  }

  invisible(ok)
}
