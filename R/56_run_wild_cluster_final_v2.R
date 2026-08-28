suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3g_final_inference"
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)

req <- c(
  file.path(outdir,"01_financial_final_data.csv"),
  file.path(outdir,"02_legal_final_data.csv"),
  file.path(outdir,"03_final_hypotheses.csv"),
  file.path(outdir,"04_final_clustered_results.csv")
)
if(any(!file.exists(req))) stop("Stage 3G base outputs missing.", call.=FALSE)
if(!requireNamespace("fwildclusterboot",quietly=TRUE)) {
  stop("fwildclusterboot is not installed.", call.=FALSE)
}

fin <- read_csv(file.path(outdir,"01_financial_final_data.csv"), show_col_types=FALSE)
legal <- read_csv(file.path(outdir,"02_legal_final_data.csv"), show_col_types=FALSE)
targets <- read_csv(file.path(outdir,"03_final_hypotheses.csv"), show_col_types=FALSE)
main <- read_csv(file.path(outdir,"04_final_clustered_results.csv"), show_col_types=FALSE)

country_var <- "borrower_country_final"
year_var <- "year"

drop_singletons <- function(df, country_var, year_var) {
  x <- df
  iter <- 0L
  repeat {
    iter <- iter + 1L
    n0 <- nrow(x)

    bad_c <- x %>% count(.data[[country_var]], name="n") %>%
      filter(n <= 1) %>% pull(1)
    if(length(bad_c)) {
      x <- x %>% filter(!(.data[[country_var]] %in% bad_c))
    }

    bad_y <- x %>% count(.data[[year_var]], name="n") %>%
      filter(n <= 1) %>% pull(1)
    if(length(bad_y)) {
      x <- x %>% filter(!(.data[[year_var]] %in% bad_y))
    }

    if(nrow(x) == n0) break
    if(iter > 100L) stop("Singleton pruning failed to converge.")
  }
  attr(x, "iterations") <- iter
  x
}

build_sample_and_formulas <- function(d, block, y, g) {
  controls <- intersect(c("log_loan_amount","creditor_type"), names(d))

  if(block == "financial" && y == "pricing_rate_t0") {
    controls <- c(
      controls,
      intersect(c("maturity_years","grace_period_years"), names(d))
    )
  }
  if(block == "financial" && y == "maturity_years") {
    controls <- c(controls, intersect("grace_period_years", names(d)))
  }
  if(block == "legal") {
    controls <- c(
      controls,
      intersect(c("maturity_years","grace_period_years"), names(d))
    )
  }
  controls <- unique(controls)

  vars_needed <- unique(c(
    "main_sample", country_var, year_var, y, g, controls
  ))

  dd <- d %>%
    filter(main_sample == 1) %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.x)))

  rhs_core <- paste(c(g, controls), collapse=" + ")

  fe_formula <- as.formula(
    paste0(y, " ~ ", rhs_core, " | ", country_var, " + ", year_var)
  )

  lm_formula <- as.formula(
    paste0(
      y, " ~ ", rhs_core,
      " + factor(", country_var, ")",
      " + factor(", year_var, ")"
    )
  )

  list(
    data=dd,
    fe_formula=fe_formula,
    lm_formula=lm_formula,
    controls=controls
  )
}

extract_p <- function(bt) {
  # fwildclusterboot versions differ slightly in returned object structure.
  for(nm in c("p_val","p.value","pvalue","p")) {
    val <- tryCatch(bt[[nm]], error=function(e) NULL)
    if(!is.null(val)) {
      val <- suppressWarnings(as.numeric(val[1]))
      if(length(val) == 1 && is.finite(val)) return(val)
    }
  }

  sm <- tryCatch(summary(bt), error=function(e) NULL)
  if(!is.null(sm)) {
    for(nm in c("p_val","p.value","pvalue","p")) {
      val <- tryCatch(sm[[nm]], error=function(e) NULL)
      if(!is.null(val)) {
        val <- suppressWarnings(as.numeric(val[1]))
        if(length(val) == 1 && is.finite(val)) return(val)
      }
    }
  }

  txt <- c(
    tryCatch(capture.output(print(bt)), error=function(e) character()),
    tryCatch(capture.output(summary(bt)), error=function(e) character())
  )
  hit <- grep("p[- ]?value|p_val|Pr\\(", txt, ignore.case=TRUE, value=TRUE)
  if(length(hit)) {
    nums <- regmatches(hit, gregexpr("[0-9]*\\.?[0-9]+", hit))
    nums <- suppressWarnings(as.numeric(unlist(nums)))
    nums <- nums[is.finite(nums) & nums >= 0 & nums <= 1]
    if(length(nums)) return(tail(nums, 1))
  }

  NA_real_
}

set.seed(20260828)
B <- 9999L
coef_tol <- 1e-8

boot_res <- list()
audit_res <- list()

for(i in seq_len(nrow(targets))) {

  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  d <- if(block == "financial") fin else legal

  comp <- build_sample_and_formulas(d, block, y, g)
  raw <- comp$data
  pruned <- drop_singletons(raw, country_var, year_var)

  min_c <- min(table(pruned[[country_var]]))
  min_y <- min(table(pruned[[year_var]]))

  if(min_c < 2 || min_y < 2) {
    stop(paste0(hid, ": singleton remains after pruning."))
  }

  # Preserve the exact Stage 3G FE specification as an audit benchmark.
  m_fe <- feols(comp$fe_formula, data=pruned, fixef.rm="none")

  # Equivalent dummy-variable OLS representation for fwildclusterboot.
  m_lm <- lm(comp$lm_formula, data=pruned)

  beta_fe <- unname(coef(m_fe)[g])
  beta_lm <- unname(coef(m_lm)[g])
  beta_diff <- abs(beta_fe - beta_lm)

  if(!is.finite(beta_fe) || !is.finite(beta_lm)) {
    stop(paste0(hid, ": non-finite GPR coefficient in FEOLS/LM audit."))
  }

  if(beta_diff > coef_tol) {
    stop(
      paste0(
        hid,
        ": FEOLS and LM GPR coefficients do not match. FEOLS=",
        signif(beta_fe, 12),
        ", LM=", signif(beta_lm, 12),
        ", abs diff=", signif(beta_diff, 12)
      ),
      call.=FALSE
    )
  }

  # Critical fix: never pass country-name strings to boottest.
  # Integer cluster IDs avoid parsing failures for names containing spaces,
  # apostrophes, accents, commas, etc.
  cluster_id <- as.integer(factor(pruned[[country_var]]))

  expected_n <- main %>%
    filter(hypothesis_id == hid) %>%
    pull(nobs)
  if(length(expected_n) == 0) expected_n <- NA_integer_

  audit_res[[hid]] <- tibble(
    hypothesis_id=hid,
    n_raw_complete=nrow(raw),
    n_after_singleton_pruning=nrow(pruned),
    n_feols=nobs(m_fe),
    n_lm=nobs(m_lm),
    stage3g_expected_n=expected_n,
    n_match=ifelse(
      is.na(expected_n),
      NA,
      nobs(m_fe) == expected_n && nobs(m_lm) == expected_n
    ),
    country_clusters=n_distinct(pruned[[country_var]]),
    year_fe=n_distinct(pruned[[year_var]]),
    min_country_cell=min_c,
    min_year_cell=min_y,
    singleton_iterations=attr(pruned,"iterations"),
    beta_feols=beta_fe,
    beta_lm=beta_lm,
    beta_abs_diff=beta_diff,
    beta_match=beta_diff <= coef_tol
  )

  # Primary attempt: LM object + integer cluster vector.
  bt <- tryCatch(
    fwildclusterboot::boottest(
      m_lm,
      param=g,
      clustid=cluster_id,
      B=B,
      type="rademacher",
      impose_null=TRUE
    ),
    error=function(e) e
  )

  # Some package versions prefer a cluster variable name in the model frame.
  # If so, refit with a safe numeric cluster column and retry by name.
  if(inherits(bt, "error")) {
    msg1 <- conditionMessage(bt)

    pruned_boot <- pruned
    pruned_boot$cluster_id_wild <- cluster_id
    m_lm2 <- lm(comp$lm_formula, data=pruned_boot)

    bt2 <- tryCatch(
      fwildclusterboot::boottest(
        m_lm2,
        param=g,
        clustid="cluster_id_wild",
        B=B,
        type="rademacher",
        impose_null=TRUE
      ),
      error=function(e) e
    )

    if(inherits(bt2, "error")) {
      boot_res[[hid]] <- tibble(
        hypothesis_id=hid,
        status="BOOTTEST_FAILED",
        p_value_wild_cluster=NA_real_,
        nobs=nobs(m_lm),
        n_clusters=n_distinct(cluster_id),
        B=B,
        seed=20260828L,
        beta_feols=beta_fe,
        beta_lm=beta_lm,
        beta_match=beta_diff <= coef_tol,
        reason=paste0(
          "LM integer-vector clustid error: ", msg1,
          " | LM numeric-column clustid error: ",
          conditionMessage(bt2)
        )
      )
      next
    } else {
      bt <- bt2
    }
  }

  pboot <- extract_p(bt)

  capture.output(
    print(bt),
    file=file.path(outdir,paste0("wildcluster_",hid,"_print.txt"))
  )
  capture.output(
    summary(bt),
    file=file.path(outdir,paste0("wildcluster_",hid,"_summary.txt"))
  )

  boot_res[[hid]] <- tibble(
    hypothesis_id=hid,
    status=ifelse(is.finite(pboot),"OK","P_EXTRACT_FAILED"),
    p_value_wild_cluster=pboot,
    nobs=nobs(m_lm),
    n_clusters=n_distinct(cluster_id),
    B=B,
    seed=20260828L,
    beta_feols=beta_fe,
    beta_lm=beta_lm,
    beta_match=beta_diff <= coef_tol,
    reason=ifelse(
      is.finite(pboot),
      NA_character_,
      "Bootstrap ran, but p-value extraction failed. Inspect print/summary files."
    )
  )
}

audit_df <- bind_rows(audit_res)
boot_df <- bind_rows(boot_res)

write_csv(
  audit_df,
  file.path(outdir,"06a_wildcluster_sample_audit.csv")
)
write_csv(
  boot_df,
  file.path(outdir,"06_final_wild_cluster_results.csv")
)

if(any(audit_df$n_match %in% FALSE, na.rm=TRUE)) {
  stop("At least one hypothesis no longer matches the Stage 3G expected N.", call.=FALSE)
}
if(any(audit_df$beta_match %in% FALSE, na.rm=TRUE)) {
  stop("At least one LM coefficient does not match FEOLS.", call.=FALSE)
}

message("56_run_wild_cluster_final_v3_lm.R logic completed.")
message("Wild bootstrap status:")
print(boot_df %>% select(hypothesis_id,status,p_value_wild_cluster,nobs,n_clusters,beta_match))
