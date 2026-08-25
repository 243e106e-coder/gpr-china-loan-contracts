suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3g_final_inference"

required_inputs <- c(
  file.path(outdir,"01_financial_final_data.csv"),
  file.path(outdir,"02_legal_final_data.csv"),
  file.path(outdir,"03_final_hypotheses.csv")
)

if(any(!file.exists(required_inputs))) {
  stop(
    "Stage 3G prepared inputs are missing. Run R/run_stage3g.R once first.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Install fwildclusterboot robustly.
# Official package documentation supports R-universe:
# https://s3alfisc.r-universe.dev
# ------------------------------------------------------------

install_fwild <- function() {

  if(requireNamespace("fwildclusterboot", quietly=TRUE)) {
    return(TRUE)
  }

  message("fwildclusterboot absent. Trying official R-universe...")

  ok <- tryCatch({
    install.packages(
      "fwildclusterboot",
      repos = c(
        s3alfisc = "https://s3alfisc.r-universe.dev",
        CRAN = "https://cloud.r-project.org"
      ),
      dependencies = TRUE
    )
    requireNamespace("fwildclusterboot", quietly=TRUE)
  }, error=function(e) {
    message("R-universe install error: ", conditionMessage(e))
    FALSE
  })

  if(ok) return(TRUE)

  message("R-universe failed. Trying CRAN fallback...")

  ok2 <- tryCatch({
    install.packages(
      "fwildclusterboot",
      repos="https://cloud.r-project.org",
      dependencies=TRUE
    )
    requireNamespace("fwildclusterboot", quietly=TRUE)
  }, error=function(e) {
    message("CRAN fallback error: ", conditionMessage(e))
    FALSE
  })

  ok2
}

fwild_ok <- install_fwild()

if(!fwild_ok) {

  si <- capture.output(sessionInfo())

  write_csv(
    tibble(
      hypothesis_id = c("F1","F2","L2","L3","L4"),
      status = "INSTALL_FAILED",
      p_value_wild_cluster = NA_real_,
      bootstrap_type = NA_character_,
      B = NA_integer_,
      seed = NA_integer_,
      reason = "fwildclusterboot could not be installed from official R-universe or CRAN."
    ),
    file.path(outdir,"06_final_wild_cluster_results.csv")
  )

  writeLines(
    si,
    file.path(outdir,"06b_wildcluster_session_info.txt")
  )

  stop(
    "fwildclusterboot installation failed. See 06b_wildcluster_session_info.txt",
    call. = FALSE
  )
}

message(
  "fwildclusterboot installed: version ",
  as.character(packageVersion("fwildclusterboot"))
)

fin <- read_csv(
  file.path(outdir,"01_financial_final_data.csv"),
  show_col_types=FALSE,
  progress=FALSE
)

legal <- read_csv(
  file.path(outdir,"02_legal_final_data.csv"),
  show_col_types=FALSE,
  progress=FALSE
)

targets <- read_csv(
  file.path(outdir,"03_final_hypotheses.csv"),
  show_col_types=FALSE
)

country_var <- "borrower_country_final"

# ------------------------------------------------------------
# Robust p-value extraction across package versions
# ------------------------------------------------------------

extract_boot_p <- function(bt) {

  candidates <- c(
    "p_val",
    "p.value",
    "pvalue",
    "p"
  )

  for(nm in candidates) {
    if(!is.null(bt[[nm]])) {
      val <- suppressWarnings(as.numeric(bt[[nm]][1]))
      if(length(val)==1 && is.finite(val)) return(val)
    }
  }

  # Try tidy method if available.
  td <- tryCatch(
    broom::tidy(bt),
    error=function(e) NULL
  )

  if(!is.null(td)) {
    pc <- intersect(
      c("p.value","p_value","pval","p.value.boot"),
      names(td)
    )
    if(length(pc)>0) {
      val <- suppressWarnings(as.numeric(td[[pc[1]]][1]))
      if(length(val)==1 && is.finite(val)) return(val)
    }
  }

  # Try summary object.
  sm <- tryCatch(summary(bt),error=function(e) NULL)

  if(!is.null(sm) && is.data.frame(sm)) {
    pc <- intersect(c("p.value","p_value","pval"),names(sm))
    if(length(pc)>0) {
      val <- suppressWarnings(as.numeric(sm[[pc[1]]][1]))
      if(length(val)==1 && is.finite(val)) return(val)
    }
  }

  NA_real_
}

# Reproducible seeds
set.seed(20260826)

if(requireNamespace("dqrng",quietly=TRUE)) {
  dqrng::dqset.seed(20260826)
}

B <- 9999L
res <- list()

for(i in seq_len(nrow(targets))) {

  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]

  d <- if(block=="financial") fin else legal

  controls <- intersect(
    c("log_loan_amount","creditor_type"),
    names(d)
  )

  if(block=="financial" && y=="pricing_rate_t0") {
    controls <- c(
      controls,
      intersect(
        c("maturity_years","grace_period_years"),
        names(d)
      )
    )
  }

  if(block=="financial" && y=="maturity_years") {
    controls <- c(
      controls,
      intersect(
        "grace_period_years",
        names(d)
      )
    )
  }

  if(block=="legal") {
    controls <- c(
      controls,
      intersect(
        c("maturity_years","grace_period_years"),
        names(d)
      )
    )
  }

  controls <- unique(controls)

  dd <- d %>%
    filter(
      main_sample==1,
      !is.na(.data[[y]]),
      !is.na(.data[[g]]),
      !is.na(.data[[country_var]]),
      !is.na(year)
    )

  if(nrow(dd)<40) {
    res[[hid]] <- tibble(
      hypothesis_id=hid,
      status="SKIPPED_SMALL_N",
      p_value_wild_cluster=NA_real_,
      bootstrap_type="WCR / Rademacher",
      B=B,
      seed=20260826L,
      nobs=nrow(dd),
      n_clusters=n_distinct(dd[[country_var]]),
      reason="Fewer than 40 usable observations."
    )
    next
  }

  rhs <- paste(c(g,controls),collapse=" + ")

  f <- as.formula(
    paste0(
      y,
      " ~ ",
      rhs,
      " | ",
      country_var,
      " + year"
    )
  )

  # fit without vcov: boottest handles cluster inference itself
  m <- feols(
    f,
    data=dd
  )

  bt <- tryCatch(
    fwildclusterboot::boottest(
      m,
      param=g,
      clustid=country_var,
      B=B,
      type="rademacher",
      impose_null=TRUE
    ),
    error=function(e) e
  )

  if(inherits(bt,"error")) {

    res[[hid]] <- tibble(
      hypothesis_id=hid,
      status="BOOTTEST_FAILED",
      p_value_wild_cluster=NA_real_,
      bootstrap_type="WCR / Rademacher",
      B=B,
      seed=20260826L,
      nobs=nobs(m),
      n_clusters=n_distinct(dd[[country_var]]),
      reason=conditionMessage(bt)
    )

  } else {

    pboot <- extract_boot_p(bt)

    res[[hid]] <- tibble(
      hypothesis_id=hid,
      status=ifelse(is.finite(pboot),"OK","P_EXTRACT_FAILED"),
      p_value_wild_cluster=pboot,
      bootstrap_type="WCR / Rademacher",
      B=B,
      seed=20260826L,
      nobs=nobs(m),
      n_clusters=n_distinct(dd[[country_var]]),
      reason=ifelse(
        is.finite(pboot),
        NA_character_,
        "boottest ran but p-value extraction failed."
      )
    )

    capture.output(
      print(bt),
      file=file.path(
        outdir,
        paste0("wildcluster_",hid,"_print.txt")
      )
    )

    capture.output(
      summary(bt),
      file=file.path(
        outdir,
        paste0("wildcluster_",hid,"_summary.txt")
      )
    )
  }
}

ans <- bind_rows(res)

write_csv(
  ans,
  file.path(outdir,"06_final_wild_cluster_results.csv")
)

if(any(ans$status!="OK")) {
  warning(
    "One or more wild-cluster tests did not return OK. ",
    "Inspect the per-hypothesis print/summary files and result reasons."
  )
}

message("56_run_wild_cluster_final.R completed.")
