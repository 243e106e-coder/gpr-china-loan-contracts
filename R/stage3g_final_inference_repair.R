#!/usr/bin/env Rscript

# ======================================================================
# Paper 1 — Stage 3G Final Inference Repair
# ======================================================================
# Purpose
# -------
# Repair the inference/robustness pipeline WITHOUT changing hypotheses,
# outcomes, GPR measures, controls, fixed effects, or the main sample rule.
#
# Fixes:
#   1) Wild-cluster bootstrap:
#      - use borrower_country_final as a model-data cluster variable
#      - coerce country/year fixed effects to factor BEFORE feols()/boottest()
#      - never pass a vector of country names as clustid
#   2) Correct n_country_clusters from the actual estimation sample
#   3) Leave-one-country/year only over countries/years ACTUALLY used
#      by each baseline regression
#   4) Crisis-exclusion p-values are taken from the refitted model's
#      clustered coefficient table (same estimate / SE / p-value object)
#   5) Add Sri Lanka influence audit for F1/F2
#   6) Regenerate multiple-testing-adjusted final table
#
# Expected baseline samples (audit targets):
#   F1 = 186 observations, 26 country clusters, 20 years
#   F2 = 187 observations, 26 country clusters, 20 years
#   L2 = 187 observations, 26 country clusters, 20 years
#   L3 = 158 observations, 24 country clusters, 20 years
#   L4 = 158 observations, 24 country clusters, 20 years
#
# Usage
# -----
# Option A: put this script beside:
#   01_financial_final_data.csv
#   02_legal_final_data.csv
#
# Option B: provide environment variables:
#   FINANCIAL_DATA=/path/to/01_financial_final_data.csv
#   LEGAL_DATA=/path/to/02_legal_final_data.csv
#
# Run:
#   Rscript --vanilla stage3g_final_inference_repair.R
#
# Output directory:
#   stage3g_inference_repair/
# ======================================================================

options(stringsAsFactors = FALSE, warn = 1)

# ---------------------------- packages ---------------------------------

required_pkgs <- c("fixest", "fwildclusterboot", "dqrng")
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_pkgs)) {
  stop(
    "Missing required packages: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstall them first, e.g. install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))",
    call. = FALSE
  )
}

# ----------------------------- config ----------------------------------

OUT_DIR <- Sys.getenv("STAGE3G_REPAIR_OUT", "stage3g_inference_repair")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

B_WILD <- as.integer(Sys.getenv("WILD_B", "9999"))
SEED   <- as.integer(Sys.getenv("WILD_SEED", "20260828"))

# Crisis windows retained from the existing Stage 3G pipeline.
CRISIS_WINDOWS <- list(
  exclude_2001          = 2001L,
  exclude_gfc_2008_2009 = 2008:2009,
  exclude_2014_2015     = 2014:2015,
  exclude_covid_2020_2021 = 2020:2021,
  exclude_ukraine_2022_2023 = 2022:2023
)

EXPECTED_N <- c(F1 = 186L, F2 = 187L, L2 = 187L, L3 = 158L, L4 = 158L)
EXPECTED_G <- c(F1 = 26L,  F2 = 26L,  L2 = 26L,  L3 = 24L,  L4 = 24L)
EXPECTED_T <- c(F1 = 20L,  F2 = 20L,  L2 = 20L,  L3 = 20L,  L4 = 20L)

# ---------------------------- utilities --------------------------------

msg <- function(...) cat(sprintf(...), "\n")
stopf <- function(...) stop(sprintf(...), call. = FALSE)

find_input <- function(env_name, preferred_prefix) {
  env_path <- Sys.getenv(env_name, "")
  if (nzchar(env_path)) {
    if (!file.exists(env_path)) stopf("%s points to missing file: %s", env_name, env_path)
    return(normalizePath(env_path, mustWork = TRUE))
  }

  # Search recursively. Accept versioned names such as "(4)".
  files <- list.files(".", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  bn <- basename(files)
  hit <- files[startsWith(bn, preferred_prefix)]

  # Exclude our own output directory if rerunning.
  hit <- hit[!grepl(paste0("(^|/)", gsub("([.])", "\\\\\\1", OUT_DIR), "(/|$)"), hit)]

  if (!length(hit)) {
    stopf(
      "Could not locate input beginning with '%s'. Set %s explicitly.",
      preferred_prefix, env_name
    )
  }

  # Prefer shortest path, then lexical order.
  hit <- hit[order(nchar(hit), hit)]
  normalizePath(hit[1], mustWork = TRUE)
}

FINANCIAL_PATH <- find_input("FINANCIAL_DATA", "01_financial_final_data")
LEGAL_PATH     <- find_input("LEGAL_DATA",     "02_legal_final_data")

msg("Financial input: %s", FINANCIAL_PATH)
msg("Legal input:     %s", LEGAL_PATH)
msg("Output dir:      %s", normalizePath(OUT_DIR, mustWork = TRUE))

financial <- read.csv(FINANCIAL_PATH, check.names = FALSE)
legal     <- read.csv(LEGAL_PATH, check.names = FALSE)

needed_common <- c(
  "main_sample", "year", "borrower_country_final", "creditor_type",
  "log_loan_amount", "maturity_years", "grace_period_years"
)

assert_cols <- function(df, cols, label) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stopf("%s is missing columns: %s", label, paste(miss, collapse = ", "))
  }
}

assert_cols(financial,
            c(needed_common, "pricing_rate_t0", "gpr_ai_all_z"),
            "financial data")
assert_cols(legal,
            c(needed_common, "law_borrower", "arb_mainland_china",
              "arb_international_third", "gpr_ai_initiator_z",
              "gpr_ai_respondent_z"),
            "legal data")

# -------------------------- specifications ------------------------------

SPECS <- list(
  F1 = list(
    block = "financial",
    data = "financial",
    outcome = "pricing_rate_t0",
    gpr = "gpr_ai_all_z",
    expected_sign = "+",
    interpretation = "All GPR -> pricing",
    controls = c("log_loan_amount", "creditor_type",
                 "maturity_years", "grace_period_years")
  ),
  F2 = list(
    block = "financial",
    data = "financial",
    outcome = "maturity_years",
    gpr = "gpr_ai_all_z",
    expected_sign = "-",
    interpretation = "All GPR -> maturity",
    controls = c("log_loan_amount", "creditor_type",
                 "grace_period_years")
  ),
  L2 = list(
    block = "legal",
    data = "legal",
    outcome = "law_borrower",
    gpr = "gpr_ai_initiator_z",
    expected_sign = "-",
    interpretation = "Initiator GPR -> borrower-home governing law",
    controls = c("log_loan_amount", "creditor_type",
                 "maturity_years", "grace_period_years")
  ),
  L3 = list(
    block = "legal",
    data = "legal",
    outcome = "arb_mainland_china",
    gpr = "gpr_ai_respondent_z",
    expected_sign = "-",
    interpretation = "Respondent GPR -> mainland-China arbitration",
    controls = c("log_loan_amount", "creditor_type",
                 "maturity_years", "grace_period_years")
  ),
  L4 = list(
    block = "legal",
    data = "legal",
    outcome = "arb_international_third",
    gpr = "gpr_ai_respondent_z",
    expected_sign = "+",
    interpretation = "Respondent GPR -> international/third-party arbitration",
    controls = c("log_loan_amount", "creditor_type",
                 "maturity_years", "grace_period_years")
  )
)

get_source_data <- function(spec) {
  if (identical(spec$data, "financial")) financial else legal
}

build_formula <- function(spec) {
  rhs <- paste(c(spec$gpr, spec$controls), collapse = " + ")
  stats::as.formula(
    sprintf("%s ~ %s | borrower_country_final + year", spec$outcome, rhs),
    env = parent.frame()
  )
}

sign_ok <- function(beta, expected_sign) {
  if (!is.finite(beta)) return(FALSE)
  if (expected_sign == "+") beta > 0 else beta < 0
}

# Iteratively remove country/year singleton cells, matching the existing
# Stage 3G audit rule.
prune_singletons <- function(d) {
  repeat {
    n0 <- nrow(d)

    ct_country <- table(d$borrower_country_final)
    keep_country <- names(ct_country)[ct_country >= 2L]
    d <- d[d$borrower_country_final %in% keep_country, , drop = FALSE]

    ct_year <- table(d$year)
    keep_year <- names(ct_year)[ct_year >= 2L]
    d <- d[as.character(d$year) %in% keep_year, , drop = FALSE]

    if (nrow(d) == n0) break
  }
  d
}

prepare_sample <- function(spec, id) {
  d <- get_source_data(spec)

  vars <- unique(c(
    spec$outcome, spec$gpr, spec$controls,
    "main_sample", "borrower_country_final", "year"
  ))

  d <- d[d$main_sample == 1, vars, drop = FALSE]
  raw_main_n <- nrow(d)

  # Complete-case sample for the actual regression variables.
  cc_vars <- setdiff(vars, "main_sample")
  d <- d[stats::complete.cases(d[, cc_vars, drop = FALSE]), , drop = FALSE]
  complete_n <- nrow(d)

  d <- prune_singletons(d)
  pruned_n <- nrow(d)

  # Critical wild-cluster fix:
  # fwildclusterboot/fixest are safest when fixed effects are factors in
  # the original model data. DO THIS BEFORE estimation.
  d$borrower_country_final <- factor(d$borrower_country_final)
  d$year <- factor(d$year)
  d$creditor_type <- factor(d$creditor_type)

  # Drop unused levels after singleton pruning.
  d$borrower_country_final <- droplevels(d$borrower_country_final)
  d$year <- droplevels(d$year)
  d$creditor_type <- droplevels(d$creditor_type)

  audit <- data.frame(
    hypothesis_id = id,
    raw_main_sample_n = raw_main_n,
    complete_case_n = complete_n,
    final_pruned_n = pruned_n,
    n_country_clusters = nlevels(d$borrower_country_final),
    n_years = nlevels(d$year),
    expected_n = EXPECTED_N[[id]],
    expected_country_clusters = EXPECTED_G[[id]],
    expected_years = EXPECTED_T[[id]],
    n_match = pruned_n == EXPECTED_N[[id]],
    cluster_match = nlevels(d$borrower_country_final) == EXPECTED_G[[id]],
    year_match = nlevels(d$year) == EXPECTED_T[[id]],
    min_country_cell = min(table(d$borrower_country_final)),
    min_year_cell = min(table(d$year)),
    stringsAsFactors = FALSE
  )

  list(data = d, audit = audit)
}

# ------------------------- model estimation -----------------------------

fit_model <- function(spec, d) {
  fixest::feols(
    build_formula(spec),
    data = d,
    vcov = ~ borrower_country_final,
    warn = TRUE,
    notes = TRUE
  )
}

extract_term <- function(model, term) {
  ct <- fixest::coeftable(model)

  if (!term %in% rownames(ct)) {
    stopf("Term '%s' not found in coefficient table.", term)
  }

  r <- ct[term, , drop = FALSE]

  # fixest coefficient-table column labels are stable in meaning but can
  # differ slightly across versions, so locate by normalized labels.
  cn <- tolower(gsub("[^a-z]", "", colnames(r)))

  find_col <- function(keys) {
    hit <- which(vapply(keys, function(k) any(grepl(k, cn)), logical(1)))
    if (!length(hit)) return(NA_integer_)
    # Return first matching table column for the first matching pattern.
    for (k in keys) {
      j <- which(grepl(k, cn))
      if (length(j)) return(j[1])
    }
    NA_integer_
  }

  i_est <- find_col(c("^estimate$"))
  i_se  <- find_col(c("stderr", "std"))
  i_t   <- find_col(c("tvalue", "stat"))
  i_p   <- find_col(c("pr", "pvalue"))

  # Fallback to canonical fixest ordering:
  # Estimate, Std. Error, t value, Pr(>|t|)
  if (is.na(i_est)) i_est <- 1L
  if (is.na(i_se))  i_se  <- 2L
  if (is.na(i_t))   i_t   <- 3L
  if (is.na(i_p))   i_p   <- 4L

  ci <- tryCatch(
    stats::confint(model, parm = term, level = 0.95),
    error = function(e) NULL
  )

  ci_low <- ci_high <- NA_real_
  if (!is.null(ci)) {
    if (is.matrix(ci)) {
      ci_low <- as.numeric(ci[1, 1])
      ci_high <- as.numeric(ci[1, 2])
    } else if (length(ci) >= 2L) {
      ci_low <- as.numeric(ci[1])
      ci_high <- as.numeric(ci[2])
    }
  }

  list(
    estimate = as.numeric(r[1, i_est]),
    std_error = as.numeric(r[1, i_se]),
    statistic = as.numeric(r[1, i_t]),
    p_value = as.numeric(r[1, i_p]),
    conf_low = ci_low,
    conf_high = ci_high
  )
}

# ------------------------ wild cluster bootstrap ------------------------

extract_wild_p <- function(bt) {
  # Preferred public API.
  p <- tryCatch(
    as.numeric(fwildclusterboot::pval(bt))[1],
    error = function(e) NA_real_
  )
  if (is.finite(p)) return(p)

  # Robust fallbacks for version differences.
  sm <- tryCatch(summary(bt), error = function(e) NULL)
  if (!is.null(sm)) {
    if (is.data.frame(sm) && "p.value" %in% names(sm)) {
      p <- suppressWarnings(as.numeric(sm$p.value[1]))
      if (is.finite(p)) return(p)
    }
    if (is.list(sm) && !is.null(sm$p.value)) {
      p <- suppressWarnings(as.numeric(sm$p.value[1]))
      if (is.finite(p)) return(p)
    }
  }

  if (!is.null(bt$p_val)) {
    p <- suppressWarnings(as.numeric(bt$p_val[1]))
    if (is.finite(p)) return(p)
  }
  if (!is.null(bt$p.value)) {
    p <- suppressWarnings(as.numeric(bt$p.value[1]))
    if (is.finite(p)) return(p)
  }

  NA_real_
}

run_wild <- function(model, d, spec, id) {
  # Reproducibility for fwildclusterboot R engine / Rademacher weights.
  set.seed(SEED)
  dqrng::dqset.seed(SEED)

  # IMPORTANT:
  # clustid is the NAME of the cluster variable in the model's original
  # data. It is NOT d$borrower_country_final and NOT a vector of names.
  bt <- tryCatch(
    fwildclusterboot::boottest(
      model,
      param = spec$gpr,
      clustid = "borrower_country_final",
      B = B_WILD,
      type = "rademacher",
      seed = SEED
    ),
    error = function(e1) {
      # Some newer versions use 'weights_type' instead of/alongside 'type'.
      tryCatch(
        fwildclusterboot::boottest(
          model,
          param = spec$gpr,
          clustid = "borrower_country_final",
          B = B_WILD,
          seed = SEED
        ),
        error = function(e2) {
          structure(
            list(
              error = paste0(
                "Attempt 1: ", conditionMessage(e1),
                " | Attempt 2: ", conditionMessage(e2)
              )
            ),
            class = "stage3g_boot_error"
          )
        }
      )
    }
  )

  if (inherits(bt, "stage3g_boot_error")) {
    return(data.frame(
      hypothesis_id = id,
      status = "BOOTTEST_FAILED",
      p_value_wild_cluster = NA_real_,
      nobs = nobs(model),
      n_clusters = nlevels(d$borrower_country_final),
      B = B_WILD,
      seed = SEED,
      reason = bt$error,
      stringsAsFactors = FALSE
    ))
  }

  p <- extract_wild_p(bt)

  data.frame(
    hypothesis_id = id,
    status = if (is.finite(p)) "OK" else "P_EXTRACTION_FAILED",
    p_value_wild_cluster = p,
    nobs = nobs(model),
    n_clusters = nlevels(d$borrower_country_final),
    B = B_WILD,
    seed = SEED,
    reason = if (is.finite(p)) "" else
      "boottest ran, but p-value could not be extracted from returned object",
    stringsAsFactors = FALSE
  )
}

# -------------------------- baseline run --------------------------------

samples <- list()
models <- list()
audit_rows <- list()
base_rows <- list()
wild_rows <- list()

for (id in names(SPECS)) {
  spec <- SPECS[[id]]
  prep <- prepare_sample(spec, id)
  d <- prep$data

  samples[[id]] <- d
  audit_rows[[id]] <- prep$audit

  msg(
    "[%s] final N=%d | countries=%d | years=%d",
    id, nrow(d), nlevels(d$borrower_country_final), nlevels(d$year)
  )

  if (!prep$audit$n_match ||
      !prep$audit$cluster_match ||
      !prep$audit$year_match) {
    warning(
      sprintf(
        "[%s] sample audit does not match expected Stage 3G counts. Check input version.",
        id
      ),
      call. = FALSE
    )
  }

  m <- fit_model(spec, d)
  models[[id]] <- m
  z <- extract_term(m, spec$gpr)

  base_rows[[id]] <- data.frame(
    block = spec$block,
    hypothesis_id = id,
    outcome = spec$outcome,
    gpr_measure = spec$gpr,
    expected_sign = spec$expected_sign,
    interpretation = spec$interpretation,
    nobs = nobs(m),
    n_country_clusters = nlevels(d$borrower_country_final),
    n_years = nlevels(d$year),
    sign_ok = sign_ok(z$estimate, spec$expected_sign),
    term = spec$gpr,
    estimate = z$estimate,
    std.error = z$std_error,
    statistic = z$statistic,
    p.value = z$p_value,
    conf.low = z$conf_low,
    conf.high = z$conf_high,
    stringsAsFactors = FALSE
  )

  msg(
    "[%s] beta=%.6f | clustered SE=%.6f | clustered p=%.6g",
    id, z$estimate, z$std_error, z$p_value
  )

  wild_rows[[id]] <- run_wild(m, d, spec, id)
  msg(
    "[%s] wild status=%s | wild p=%s",
    id,
    wild_rows[[id]]$status,
    ifelse(is.na(wild_rows[[id]]$p_value_wild_cluster),
           "NA",
           sprintf("%.6g", wild_rows[[id]]$p_value_wild_cluster))
  )
}

audit_df <- do.call(rbind, audit_rows)
baseline_df <- do.call(rbind, base_rows)
wild_df <- do.call(rbind, wild_rows)

rownames(audit_df) <- NULL
rownames(baseline_df) <- NULL
rownames(wild_df) <- NULL

# Multiple-testing adjustment across the five PRE-SPECIFIED hypotheses.
baseline_df$p_holm_all <- p.adjust(baseline_df$p.value, method = "holm")
baseline_df$p_bh_all <- p.adjust(baseline_df$p.value, method = "BH")
baseline_df$p_bonferroni_all <- p.adjust(baseline_df$p.value, method = "bonferroni")

# ------------------------- leave one country ----------------------------

loo_country <- list()
k <- 0L

for (id in names(SPECS)) {
  spec <- SPECS[[id]]
  d0 <- samples[[id]]

  # Only countries ACTUALLY used by this hypothesis.
  countries <- levels(d0$borrower_country_final)

  for (cc in countries) {
    k <- k + 1L
    d <- d0[as.character(d0$borrower_country_final) != cc, , drop = FALSE]

    # Drop unused factor levels; do not introduce out-of-sample countries.
    d$borrower_country_final <- droplevels(d$borrower_country_final)
    d$year <- droplevels(d$year)
    d$creditor_type <- droplevels(d$creditor_type)

    fit <- tryCatch(fit_model(spec, d), error = function(e) e)

    if (inherits(fit, "error")) {
      loo_country[[k]] <- data.frame(
        hypothesis_id = id,
        dropped_country = cc,
        nobs = nrow(d),
        n_country_clusters = nlevels(d$borrower_country_final),
        estimate = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        sign_ok = NA,
        status = "FIT_FAILED",
        reason = conditionMessage(fit),
        stringsAsFactors = FALSE
      )
      next
    }

    z <- extract_term(fit, spec$gpr)
    loo_country[[k]] <- data.frame(
      hypothesis_id = id,
      dropped_country = cc,
      nobs = nobs(fit),
      n_country_clusters = nlevels(d$borrower_country_final),
      estimate = z$estimate,
      std_error = z$std_error,
      p_value = z$p_value,
      sign_ok = sign_ok(z$estimate, spec$expected_sign),
      status = "OK",
      reason = "",
      stringsAsFactors = FALSE
    )
  }
}

loo_country_df <- do.call(rbind, loo_country)
rownames(loo_country_df) <- NULL

loo_country_summary <- do.call(
  rbind,
  lapply(names(SPECS), function(id) {
    x <- loo_country_df[loo_country_df$hypothesis_id == id, , drop = FALSE]
    ok <- x$status == "OK"

    data.frame(
      hypothesis_id = id,
      expected_runs = nlevels(samples[[id]]$borrower_country_final),
      actual_runs = nrow(x),
      successful_runs = sum(ok),
      expected_sign_runs = sum(x$sign_ok[ok], na.rm = TRUE),
      expected_sign_share = if (sum(ok)) mean(x$sign_ok[ok], na.rm = TRUE) else NA_real_,
      sign_flip_countries = paste(x$dropped_country[ok & !x$sign_ok], collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
)
rownames(loo_country_summary) <- NULL

# --------------------------- leave one year -----------------------------

loo_year <- list()
k <- 0L

for (id in names(SPECS)) {
  spec <- SPECS[[id]]
  d0 <- samples[[id]]

  # Only years ACTUALLY used by this hypothesis.
  years <- levels(d0$year)

  for (yy in years) {
    k <- k + 1L
    d <- d0[as.character(d0$year) != yy, , drop = FALSE]

    d$borrower_country_final <- droplevels(d$borrower_country_final)
    d$year <- droplevels(d$year)
    d$creditor_type <- droplevels(d$creditor_type)

    fit <- tryCatch(fit_model(spec, d), error = function(e) e)

    if (inherits(fit, "error")) {
      loo_year[[k]] <- data.frame(
        hypothesis_id = id,
        dropped_year = yy,
        nobs = nrow(d),
        n_years = nlevels(d$year),
        estimate = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        sign_ok = NA,
        status = "FIT_FAILED",
        reason = conditionMessage(fit),
        stringsAsFactors = FALSE
      )
      next
    }

    z <- extract_term(fit, spec$gpr)
    loo_year[[k]] <- data.frame(
      hypothesis_id = id,
      dropped_year = yy,
      nobs = nobs(fit),
      n_years = nlevels(d$year),
      estimate = z$estimate,
      std_error = z$std_error,
      p_value = z$p_value,
      sign_ok = sign_ok(z$estimate, spec$expected_sign),
      status = "OK",
      reason = "",
      stringsAsFactors = FALSE
    )
  }
}

loo_year_df <- do.call(rbind, loo_year)
rownames(loo_year_df) <- NULL

loo_year_summary <- do.call(
  rbind,
  lapply(names(SPECS), function(id) {
    x <- loo_year_df[loo_year_df$hypothesis_id == id, , drop = FALSE]
    ok <- x$status == "OK"

    data.frame(
      hypothesis_id = id,
      expected_runs = nlevels(samples[[id]]$year),
      actual_runs = nrow(x),
      successful_runs = sum(ok),
      expected_sign_runs = sum(x$sign_ok[ok], na.rm = TRUE),
      expected_sign_share = if (sum(ok)) mean(x$sign_ok[ok], na.rm = TRUE) else NA_real_,
      sign_flip_years = paste(x$dropped_year[ok & !x$sign_ok], collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
)
rownames(loo_year_summary) <- NULL

# ------------------------- crisis exclusions ----------------------------

crisis_rows <- list()
k <- 0L

for (id in names(SPECS)) {
  spec <- SPECS[[id]]
  d0 <- samples[[id]]

  for (nm in names(CRISIS_WINDOWS)) {
    yrs <- as.character(CRISIS_WINDOWS[[nm]])

    k <- k + 1L
    d <- d0[!as.character(d0$year) %in% yrs, , drop = FALSE]

    d$borrower_country_final <- droplevels(d$borrower_country_final)
    d$year <- droplevels(d$year)
    d$creditor_type <- droplevels(d$creditor_type)

    fit <- tryCatch(fit_model(spec, d), error = function(e) e)

    if (inherits(fit, "error")) {
      crisis_rows[[k]] <- data.frame(
        hypothesis_id = id,
        exclusion = nm,
        excluded_years = paste(yrs, collapse = ","),
        nobs = nrow(d),
        n_country_clusters = nlevels(d$borrower_country_final),
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        sign_ok = NA,
        p_consistency_check = NA,
        status = "FIT_FAILED",
        reason = conditionMessage(fit),
        stringsAsFactors = FALSE
      )
      next
    }

    z <- extract_term(fit, spec$gpr)

    # Internal consistency diagnostic:
    # reported t statistic should equal estimate / SE up to numerical tolerance.
    t_rebuilt <- z$estimate / z$std_error
    p_consistent <- is.finite(z$statistic) &&
      isTRUE(all.equal(as.numeric(z$statistic), as.numeric(t_rebuilt),
                       tolerance = 1e-7))

    crisis_rows[[k]] <- data.frame(
      hypothesis_id = id,
      exclusion = nm,
      excluded_years = paste(yrs, collapse = ","),
      nobs = nobs(fit),
      n_country_clusters = nlevels(d$borrower_country_final),
      estimate = z$estimate,
      std_error = z$std_error,
      statistic = z$statistic,
      p_value = z$p_value,
      sign_ok = sign_ok(z$estimate, spec$expected_sign),
      p_consistency_check = p_consistent,
      status = "OK",
      reason = "",
      stringsAsFactors = FALSE
    )
  }
}

crisis_df <- do.call(rbind, crisis_rows)
rownames(crisis_df) <- NULL

# ---------------------- Sri Lanka influence audit -----------------------

sl_rows <- list()

for (id in c("F1", "F2")) {
  spec <- SPECS[[id]]
  d0 <- samples[[id]]
  countries <- levels(d0$borrower_country_final)

  base_z <- extract_term(models[[id]], spec$gpr)

  if (!"Sri Lanka" %in% countries) {
    sl_rows[[id]] <- data.frame(
      hypothesis_id = id,
      sri_lanka_in_baseline = FALSE,
      sri_lanka_n = 0L,
      baseline_n = nrow(d0),
      baseline_estimate = base_z$estimate,
      no_sri_lanka_n = NA_integer_,
      no_sri_lanka_estimate = NA_real_,
      sign_flip = NA,
      sri_lanka_share = 0,
      status = "NOT_IN_SAMPLE",
      stringsAsFactors = FALSE
    )
    next
  }

  n_sl <- sum(as.character(d0$borrower_country_final) == "Sri Lanka")
  d <- d0[as.character(d0$borrower_country_final) != "Sri Lanka", , drop = FALSE]
  d$borrower_country_final <- droplevels(d$borrower_country_final)
  d$year <- droplevels(d$year)
  d$creditor_type <- droplevels(d$creditor_type)

  m2 <- fit_model(spec, d)
  z2 <- extract_term(m2, spec$gpr)

  sl_rows[[id]] <- data.frame(
    hypothesis_id = id,
    sri_lanka_in_baseline = TRUE,
    sri_lanka_n = n_sl,
    baseline_n = nrow(d0),
    baseline_estimate = base_z$estimate,
    no_sri_lanka_n = nobs(m2),
    no_sri_lanka_estimate = z2$estimate,
    sign_flip = sign(base_z$estimate) != sign(z2$estimate),
    sri_lanka_share = n_sl / nrow(d0),
    status = "OK",
    stringsAsFactors = FALSE
  )
}

sri_lanka_df <- do.call(rbind, sl_rows)
rownames(sri_lanka_df) <- NULL

# ------------------------- final inference table ------------------------

final_df <- merge(
  baseline_df,
  wild_df[, c("hypothesis_id", "status", "p_value_wild_cluster",
              "n_clusters", "B", "seed", "reason")],
  by = "hypothesis_id",
  all.x = TRUE,
  sort = FALSE,
  suffixes = c("", "_wild")
)

names(final_df)[names(final_df) == "status"] <- "wild_status"
names(final_df)[names(final_df) == "reason"] <- "wild_reason"
names(final_df)[names(final_df) == "n_clusters"] <- "wild_n_clusters"

# Restore specification order.
final_df <- final_df[match(names(SPECS), final_df$hypothesis_id), , drop = FALSE]

# Conservative labeling: wild inference must succeed before "confirmed".
final_df$inference_strength <- ifelse(
  final_df$wild_status != "OK" | is.na(final_df$p_value_wild_cluster),
  "wild_unconfirmed",
  ifelse(
    final_df$p_value_wild_cluster < 0.05 & final_df$p_bh_all < 0.05,
    "strong",
    ifelse(
      final_df$p_value_wild_cluster < 0.10 & final_df$sign_ok,
      "moderate",
      "weak"
    )
  )
)

# ------------------------------ outputs ---------------------------------

write.csv(
  audit_df,
  file.path(OUT_DIR, "00_repaired_sample_audit.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  baseline_df,
  file.path(OUT_DIR, "01_repaired_clustered_results.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  wild_df,
  file.path(OUT_DIR, "02_repaired_wild_cluster_results.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  loo_country_df,
  file.path(OUT_DIR, "03_repaired_leave_one_country_out.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  loo_country_summary,
  file.path(OUT_DIR, "04_repaired_leave_one_country_summary.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  loo_year_df,
  file.path(OUT_DIR, "05_repaired_leave_one_year_out.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  loo_year_summary,
  file.path(OUT_DIR, "06_repaired_leave_one_year_summary.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  crisis_df,
  file.path(OUT_DIR, "07_repaired_crisis_exclusions.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  sri_lanka_df,
  file.path(OUT_DIR, "08_sri_lanka_influence_audit.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  final_df,
  file.path(OUT_DIR, "09_repaired_final_results_table.csv"),
  row.names = FALSE,
  na = ""
)

# Full model summaries for auditability.
sink(file.path(OUT_DIR, "10_repaired_model_summaries.txt"))
for (id in names(SPECS)) {
  cat("\n====================================\n")
  cat(id, "\n")
  cat("====================================\n")
  print(summary(models[[id]]))
}
sink()

# Machine-readable selected samples: country/year lists.
sample_membership <- do.call(
  rbind,
  lapply(names(SPECS), function(id) {
    d <- samples[[id]]
    data.frame(
      hypothesis_id = id,
      country = paste(levels(d$borrower_country_final), collapse = "; "),
      year = paste(levels(d$year), collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
)
write.csv(
  sample_membership,
  file.path(OUT_DIR, "11_actual_estimation_sample_membership.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------- README -----------------------------------

readme <- c(
  "# Paper 1 — Stage 3G Final Inference Repair",
  "",
  "This run does NOT add hypotheses or change the substantive model specification.",
  "",
  "Repairs implemented:",
  "1. Wild-cluster bootstrap uses clustid='borrower_country_final' from the model data.",
  "2. borrower_country_final and year are factors before feols()/boottest().",
  "3. Country cluster counts come from the actual estimation sample.",
  "4. LOO-country iterates only over countries in the actual baseline sample.",
  "5. LOO-year iterates only over years in the actual baseline sample.",
  "6. Crisis-exclusion estimate, SE, t and p all come from the same refitted clustered model.",
  "7. Sri Lanka influence is audited separately for F1/F2.",
  "",
  sprintf("Wild bootstrap draws: %d", B_WILD),
  sprintf("Seed: %d", SEED),
  "",
  "Expected baseline audit targets:",
  "F1: N=186, country clusters=26, years=20",
  "F2: N=187, country clusters=26, years=20",
  "L2: N=187, country clusters=26, years=20",
  "L3: N=158, country clusters=24, years=20",
  "L4: N=158, country clusters=24, years=20",
  "",
  "Interpret final inference only after checking:",
  "- 00_repaired_sample_audit.csv all match flags are TRUE",
  "- 02_repaired_wild_cluster_results.csv status == OK",
  "- LOO run counts equal actual cluster/year counts",
  "- 07_repaired_crisis_exclusions.csv p_consistency_check == TRUE"
)

writeLines(readme, file.path(OUT_DIR, "README_stage3g_inference_repair.md"))

# ---------------------------- console summary ---------------------------

cat("\n============================================================\n")
cat("STAGE 3G INFERENCE REPAIR COMPLETE\n")
cat("============================================================\n\n")

print(audit_df)
cat("\nRepaired baseline clustered results:\n")
print(
  baseline_df[, c(
    "hypothesis_id", "estimate", "std.error", "p.value",
    "nobs", "n_country_clusters", "p_holm_all", "p_bh_all"
  )],
  row.names = FALSE
)

cat("\nWild-cluster results:\n")
print(
  wild_df[, c(
    "hypothesis_id", "status", "p_value_wild_cluster",
    "nobs", "n_clusters", "B"
  )],
  row.names = FALSE
)

cat("\nLOO-country summary:\n")
print(loo_country_summary, row.names = FALSE)

cat("\nLOO-year summary:\n")
print(loo_year_summary, row.names = FALSE)

cat("\nSri Lanka influence audit:\n")
print(sri_lanka_df, row.names = FALSE)

cat("\nOutputs written to: ", normalizePath(OUT_DIR, mustWork = TRUE), "\n", sep = "")
