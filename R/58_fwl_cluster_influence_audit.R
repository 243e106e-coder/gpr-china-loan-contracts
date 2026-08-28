suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(ggplot2)
})

outdir <- "outputs/stage3g_final_inference"
audit_dir <- file.path(outdir, "cluster_influence_gpr_audit")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

req <- c(
  file.path(outdir, "01_financial_final_data.csv"),
  file.path(outdir, "02_legal_final_data.csv"),
  file.path(outdir, "03_final_hypotheses.csv"),
  file.path(outdir, "04_final_clustered_results.csv")
)
if (any(!file.exists(req))) {
  stop("Stage 3G base outputs missing.", call. = FALSE)
}

fin <- read_csv(file.path(outdir, "01_financial_final_data.csv"), show_col_types = FALSE)
legal <- read_csv(file.path(outdir, "02_legal_final_data.csv"), show_col_types = FALSE)
targets <- read_csv(file.path(outdir, "03_final_hypotheses.csv"), show_col_types = FALSE)
main <- read_csv(file.path(outdir, "04_final_clustered_results.csv"), show_col_types = FALSE)

country_var <- "borrower_country_final"
year_var <- "year"

drop_singletons <- function(df, country_var, year_var) {
  x <- df
  iter <- 0L
  repeat {
    iter <- iter + 1L
    n0 <- nrow(x)

    bad_c <- x %>%
      count(.data[[country_var]], name = "n") %>%
      filter(n <= 1) %>%
      pull(1)
    if (length(bad_c)) {
      x <- x %>% filter(!(.data[[country_var]] %in% bad_c))
    }

    bad_y <- x %>%
      count(.data[[year_var]], name = "n") %>%
      filter(n <= 1) %>%
      pull(1)
    if (length(bad_y)) {
      x <- x %>% filter(!(.data[[year_var]] %in% bad_y))
    }

    if (nrow(x) == n0) break
    if (iter > 100L) stop("Singleton pruning failed to converge.")
  }
  attr(x, "iterations") <- iter
  x
}

build_sample_and_formula <- function(d, block, y, g) {
  controls <- intersect(c("log_loan_amount", "creditor_type"), names(d))

  if (block == "financial" && y == "pricing_rate_t0") {
    controls <- c(
      controls,
      intersect(c("maturity_years", "grace_period_years"), names(d))
    )
  }
  if (block == "financial" && y == "maturity_years") {
    controls <- c(controls, intersect("grace_period_years", names(d)))
  }
  if (block == "legal") {
    controls <- c(
      controls,
      intersect(c("maturity_years", "grace_period_years"), names(d))
    )
  }
  controls <- unique(controls)

  vars_needed <- unique(c(
    "main_sample", country_var, year_var, y, g, controls
  ))

  dd <- d %>%
    filter(main_sample == 1) %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.x)))

  rhs <- paste(c(g, controls), collapse = " + ")
  f <- as.formula(
    paste0(y, " ~ ", rhs, " | ", country_var, " + ", year_var)
  )

  list(data = dd, formula = f, controls = controls)
}

safe_cluster_p <- function(model, g) {
  ct <- coeftable(model)
  if (!(g %in% rownames(ct))) return(NA_real_)
  pcol <- grep("Pr", colnames(ct), value = TRUE)
  if (!length(pcol)) return(NA_real_)
  as.numeric(ct[g, pcol[1]])
}

variance_decomp <- function(dd, g) {
  x <- dd[[g]]
  total_var <- var(x)

  country_means <- dd %>%
    group_by(.data[[country_var]]) %>%
    summarise(mu = mean(.data[[g]]), .groups = "drop")

  dd2 <- dd %>%
    left_join(country_means, by = setNames(country_var, country_var)) %>%
    mutate(x_within = .data[[g]] - mu)

  within_var <- var(dd2$x_within)
  ratio <- ifelse(is.finite(total_var) && total_var > 0, within_var / total_var, NA_real_)

  tibble(
    total_variance = total_var,
    within_country_variance = within_var,
    within_share_of_total = ratio
  )
}

country_variation_table <- function(dd, g, hid) {
  dd %>%
    group_by(.data[[country_var]]) %>%
    summarise(
      hypothesis_id = hid,
      n_obs = n(),
      n_years = n_distinct(.data[[year_var]]),
      gpr_mean = mean(.data[[g]]),
      gpr_sd = sd(.data[[g]]),
      gpr_min = min(.data[[g]]),
      gpr_max = max(.data[[g]]),
      gpr_range = max(.data[[g]]) - min(.data[[g]]),
      gpr_var = var(.data[[g]]),
      has_within_variation = n_distinct(.data[[g]]) > 1,
      .groups = "drop"
    ) %>%
    rename(country = all_of(country_var)) %>%
    arrange(desc(gpr_var), desc(n_obs))
}

year_variation_table <- function(dd, g, hid) {
  dd %>%
    group_by(.data[[year_var]]) %>%
    summarise(
      hypothesis_id = hid,
      n_obs = n(),
      n_countries = n_distinct(.data[[country_var]]),
      gpr_mean = mean(.data[[g]]),
      gpr_sd = sd(.data[[g]]),
      gpr_min = min(.data[[g]]),
      gpr_max = max(.data[[g]]),
      .groups = "drop"
    ) %>%
    rename(year = all_of(year_var)) %>%
    arrange(desc(gpr_sd))
}

leave_one_country_influence <- function(dd, f, g, hid, full_beta) {
  countries <- sort(unique(dd[[country_var]]))
  out <- vector("list", length(countries))

  for (j in seq_along(countries)) {
    cc <- countries[j]
    d2 <- dd %>% filter(.data[[country_var]] != cc)

    d2 <- drop_singletons(d2, country_var, year_var)

    if (nrow(d2) == 0 ||
        n_distinct(d2[[country_var]]) < 2 ||
        n_distinct(d2[[year_var]]) < 2) {
      out[[j]] <- tibble(
        hypothesis_id = hid,
        omitted_country = cc,
        nobs = nrow(d2),
        clusters = n_distinct(d2[[country_var]]),
        beta = NA_real_,
        clustered_p = NA_real_,
        beta_change = NA_real_,
        abs_beta_change = NA_real_,
        sign_same = NA
      )
      next
    }

    m <- tryCatch(
      feols(f, data = d2, cluster = country_var, fixef.rm = "none"),
      error = function(e) NULL
    )

    if (is.null(m) || !(g %in% names(coef(m)))) {
      out[[j]] <- tibble(
        hypothesis_id = hid,
        omitted_country = cc,
        nobs = nrow(d2),
        clusters = n_distinct(d2[[country_var]]),
        beta = NA_real_,
        clustered_p = NA_real_,
        beta_change = NA_real_,
        abs_beta_change = NA_real_,
        sign_same = NA
      )
      next
    }

    b <- unname(coef(m)[g])

    out[[j]] <- tibble(
      hypothesis_id = hid,
      omitted_country = cc,
      nobs = nobs(m),
      clusters = n_distinct(d2[[country_var]]),
      beta = b,
      clustered_p = safe_cluster_p(m, g),
      beta_change = b - full_beta,
      abs_beta_change = abs(b - full_beta),
      sign_same = sign(b) == sign(full_beta)
    )
  }

  bind_rows(out) %>%
    arrange(desc(abs_beta_change))
}

leave_one_year_influence <- function(dd, f, g, hid, full_beta) {
  years <- sort(unique(dd[[year_var]]))
  out <- vector("list", length(years))

  for (j in seq_along(years)) {
    yy <- years[j]
    d2 <- dd %>% filter(.data[[year_var]] != yy)
    d2 <- drop_singletons(d2, country_var, year_var)

    if (nrow(d2) == 0 ||
        n_distinct(d2[[country_var]]) < 2 ||
        n_distinct(d2[[year_var]]) < 2) {
      out[[j]] <- tibble(
        hypothesis_id = hid,
        omitted_year = yy,
        nobs = nrow(d2),
        clusters = n_distinct(d2[[country_var]]),
        beta = NA_real_,
        clustered_p = NA_real_,
        beta_change = NA_real_,
        abs_beta_change = NA_real_,
        sign_same = NA
      )
      next
    }

    m <- tryCatch(
      feols(f, data = d2, cluster = country_var, fixef.rm = "none"),
      error = function(e) NULL
    )

    if (is.null(m) || !(g %in% names(coef(m)))) {
      out[[j]] <- tibble(
        hypothesis_id = hid,
        omitted_year = yy,
        nobs = nrow(d2),
        clusters = n_distinct(d2[[country_var]]),
        beta = NA_real_,
        clustered_p = NA_real_,
        beta_change = NA_real_,
        abs_beta_change = NA_real_,
        sign_same = NA
      )
      next
    }

    b <- unname(coef(m)[g])

    out[[j]] <- tibble(
      hypothesis_id = hid,
      omitted_year = yy,
      nobs = nobs(m),
      clusters = n_distinct(d2[[country_var]]),
      beta = b,
      clustered_p = safe_cluster_p(m, g),
      beta_change = b - full_beta,
      abs_beta_change = abs(b - full_beta),
      sign_same = sign(b) == sign(full_beta)
    )
  }

  bind_rows(out) %>%
    arrange(desc(abs_beta_change))
}

all_summary <- list()
all_country_var <- list()
all_year_var <- list()
all_loo_country <- list()
all_loo_year <- list()

for (i in seq_len(nrow(targets))) {
  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  d <- if (block == "financial") fin else legal

  comp <- build_sample_and_formula(d, block, y, g)
  raw <- comp$data
  dd <- drop_singletons(raw, country_var, year_var)

  full_model <- feols(
    comp$formula,
    data = dd,
    cluster = country_var,
    fixef.rm = "none"
  )
  full_beta <- unname(coef(full_model)[g])

  vdec <- variance_decomp(dd, g)
  cvar <- country_variation_table(dd, g, hid)
  yvar <- year_variation_table(dd, g, hid)
  loc <- leave_one_country_influence(dd, comp$formula, g, hid, full_beta)
  loy <- leave_one_year_influence(dd, comp$formula, g, hid, full_beta)

  main_row <- main %>% filter(hypothesis_id == hid)

  summary_row <- tibble(
    hypothesis_id = hid,
    block = block,
    outcome = y,
    gpr_measure = g,
    nobs = nobs(full_model),
    country_clusters = n_distinct(dd[[country_var]]),
    years = n_distinct(dd[[year_var]]),
    full_beta = full_beta,
    full_clustered_p = safe_cluster_p(full_model, g),
    total_variance = vdec$total_variance,
    within_country_variance = vdec$within_country_variance,
    within_share_of_total = vdec$within_share_of_total,
    countries_with_within_variation = sum(cvar$has_within_variation),
    countries_without_within_variation = sum(!cvar$has_within_variation),
    max_country_abs_beta_change = max(loc$abs_beta_change, na.rm = TRUE),
    country_sign_stability = mean(loc$sign_same, na.rm = TRUE),
    max_year_abs_beta_change = max(loy$abs_beta_change, na.rm = TRUE),
    year_sign_stability = mean(loy$sign_same, na.rm = TRUE)
  )

  if (nrow(main_row)) {
    if ("nobs" %in% names(main_row)) {
      summary_row$stage3g_expected_n <- main_row$nobs[1]
      summary_row$n_match <- summary_row$nobs == main_row$nobs[1]
    }
  }

  all_summary[[hid]] <- summary_row
  all_country_var[[hid]] <- cvar
  all_year_var[[hid]] <- yvar
  all_loo_country[[hid]] <- loc
  all_loo_year[[hid]] <- loy

  write_csv(cvar, file.path(audit_dir, paste0(hid, "_country_gpr_variation.csv")))
  write_csv(yvar, file.path(audit_dir, paste0(hid, "_year_gpr_variation.csv")))
  write_csv(loc, file.path(audit_dir, paste0(hid, "_leave_one_country_influence.csv")))
  write_csv(loy, file.path(audit_dir, paste0(hid, "_leave_one_year_influence.csv")))

  p1 <- ggplot(cvar, aes(x = reorder(country, gpr_var), y = gpr_var)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0(hid, ": within-country GPR variation proxy"),
      x = NULL,
      y = "Country-level variance of GPR in estimation sample"
    )
  ggsave(
    file.path(audit_dir, paste0(hid, "_country_gpr_variation.png")),
    p1, width = 8, height = 7, dpi = 180
  )

  p2 <- ggplot(loc, aes(x = reorder(omitted_country, abs_beta_change), y = abs_beta_change)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0(hid, ": coefficient sensitivity to dropping one country"),
      x = NULL,
      y = "|Beta_without_country - Beta_full|"
    )
  ggsave(
    file.path(audit_dir, paste0(hid, "_country_influence.png")),
    p2, width = 8, height = 7, dpi = 180
  )
}

summary_df <- bind_rows(all_summary)
country_var_df <- bind_rows(all_country_var)
year_var_df <- bind_rows(all_year_var)
loo_country_df <- bind_rows(all_loo_country)
loo_year_df <- bind_rows(all_loo_year)

write_csv(
  summary_df,
  file.path(audit_dir, "00_cluster_influence_summary.csv")
)
write_csv(
  country_var_df,
  file.path(audit_dir, "01_all_country_gpr_variation.csv")
)
write_csv(
  year_var_df,
  file.path(audit_dir, "02_all_year_gpr_variation.csv")
)
write_csv(
  loo_country_df,
  file.path(audit_dir, "03_all_leave_one_country_influence.csv")
)
write_csv(
  loo_year_df,
  file.path(audit_dir, "04_all_leave_one_year_influence.csv")
)

report <- c(
  "# Stage 3G Cluster Influence + Within-GPR Variation Audit",
  "",
  "This audit does not change the main specification.",
  "It diagnoses how much identifying variation remains after country fixed effects",
  "and whether individual countries or years materially drive the GPR coefficient.",
  "",
  "## Summary"
)

for (i in seq_len(nrow(summary_df))) {
  z <- summary_df[i, ]
  report <- c(
    report,
    sprintf(
      "- %s: N=%s, clusters=%s, within-share=%.3f, countries with variation=%s/%s, country sign stability=%.3f, year sign stability=%.3f",
      z$hypothesis_id,
      z$nobs,
      z$country_clusters,
      z$within_share_of_total,
      z$countries_with_within_variation,
      z$country_clusters,
      z$country_sign_stability,
      z$year_sign_stability
    )
  )
}

writeLines(
  report,
  file.path(audit_dir, "AUDIT_SUMMARY.md")
)

message("57_cluster_influence_gpr_audit.R completed.")
print(summary_df)
