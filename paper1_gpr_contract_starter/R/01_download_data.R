source("R/00_config.R")

hcl_zip <- "data/raw/hcl2/How_China_Lends_Dataset_Version_2_0.zip"
hcl_ok <- download_checked(HCL_URL, hcl_zip)

if (hcl_ok) {
  unzip_dir <- "data/raw/hcl2/unzipped"
  if (!dir.exists(unzip_dir) || length(list.files(unzip_dir, recursive = TRUE)) == 0) {
    dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
    message("Unzipping HCL 2.0...")
    utils::unzip(hcl_zip, exdir = unzip_dir)
  }
}

gpr_files <- data.frame(
  name = c("traditional_gpr", "ai_gpr_monthly", "ai_gpr_country"),
  url = c(GPR_TRADITIONAL_XLS_URL, AI_GPR_MONTHLY_URL, AI_GPR_COUNTRY_URL),
  dest = c(
    "data/raw/gpr/data_gpr_export.xls",
    "data/raw/gpr/ai_gpr_data_monthly.csv",
    "data/raw/gpr/ai_gpr_country_monthly.csv"
  )
)

gpr_files$download_ok <- FALSE
for (i in seq_len(nrow(gpr_files))) {
  gpr_files$download_ok[i] <- download_checked(gpr_files$url[i], gpr_files$dest[i])
}

readr::write_csv(gpr_files, "outputs/stage1_audit/download_status.csv")
