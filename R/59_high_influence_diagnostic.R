suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(ggplot2)
  library(purrr)
  library(stringr)
})

base_dir <- "outputs/stage3g_final_inference"
out_dir  <- "outputs/stage3i_high_influence_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

req <- c(
  file.path(base_dir, "01_financial_final_data.csv"),
  file.path(base_dir, "02_legal_final_data.csv"),
  file.path(base_dir, "03_final_hypotheses.csv"),
  file.path(base_dir, "04_final_clustered_results.csv")
)
if (any(!file.exists(req))) {
  stop(
    paste(
      "Missing Stage 3G base outputs:",
      paste(req[!file.exists(req)], collapse = ", ")
    ),
    call. = FALSE
  )
}

fin     <- read_csv(file.path(base_dir, "01_financial_final_data.csv"), show_col_types = FALSE)
legal   <- read_csv(file.path(base_dir, "02_legal_final_data.csv"), show_col_types = FALSE)
targets <- read_csv(file.path(base_dir, "03_final_hypotheses.csv"), show_col_types = FALSE)
main    <- read_csv(file.path(base_dir, "04_final_clustered_results.csv"), show_col_types = FALSE)

country_var <- "borrower_country_final"
year_var    <- "year"

focus_ids <- intersect(c("F1", "F2", "L2", "L3", "L4"), targets$hypothesis_id)
targets <- targets %>% filter(hypothesis_id %in% focus_ids)

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

safe_coef_stats <- function(model, g) {
  ct <- coeftable(model)
  if (!(g %in% rownames(ct))) {
    return(tibble(beta = NA_real_, se = NA_real_, p = NA_real_))
  }

  se_col <- grep("Std", colnames(ct), value = TRUE)[1]
  p_col  <- grep("Pr",  colnames(ct), value = TRUE)[1]

  tibble(
    beta = as.numeric(ct[g, 1]),
    se   = if (!is.na(se_col)) as.numeric(ct[g, se_col]) else NA_real_,
    p    = if (!is.na(p_col))  as.numeric(ct[g, p_col])  else NA_real_
  )
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

fit_clustered <- function(f, data, g) {
  m <- tryCatch(
    feols(f, data = data, cluster = country_var, fixef.rm = "none"),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(list(model = NULL, stats = tibble(beta = NA_real_, se = NA_real_, p = NA_real_)))
  }
  list(model = m, stats = safe_coef_stats(m, g))
}

make_formula <- function(y, g, controls) {
  rhs <- paste(c(g, controls), collapse = " + ")
  as.formula(
    paste0(y, " ~ ", rhs, " | ", country_var, " + ", year_var)
  )
}

residualized_gpr_country_share <- function(dd, g, controls, hid) {
  rhs <- if (length(controls)) paste(controls, collapse = " + ") else "1"
  f_g <- as.formula(
    paste0(g, " ~ ", rhs, " | ", country_var, " + ", year_var)
  )

  m_g <- feols(f_g, data = dd, fixef.rm = "none")
  r_g <- resid(m_g)

  tmp <- dd %>%
    mutate(.g_resid = as.numeric(r_g))

  denom <- sum(tmp$.g_resid^2, na.rm = TRUE)

  tmp %>%
    group_by(.data[[country_var]]) %>%
    summarise(
      hypothesis_id = hid,
      n_obs = n(),
      n_years = n_distinct(.data[[year_var]]),
      residualized_gpr_ss = sum(.g_resid^2, na.rm = TRUE),
      max_abs_residualized_gpr = max(abs(.g_resid), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      residualized_gpr_share = ifelse(
        is.finite(denom) && denom > 0,
        residualized_gpr_ss / denom,
        NA_real_
      )
    ) %>%
    rename(country = all_of(country_var)) %>%
    arrange(desc(residualized_gpr_share))
}

country_year_profile <- function(dd, y, g, hid) {
  dd %>%
    group_by(.data[[country_var]], .data[[year_var]]) %>%
    summarise(
      hypothesis_id = hid,
      n_obs = n(),
      outcome_mean = mean(.data[[y]], na.rm = TRUE),
      outcome_sd = sd(.data[[y]], na.rm = TRUE),
      outcome_min = min(.data[[y]], na.rm = TRUE),
      outcome_max = max(.data[[y]], na.rm = TRUE),
      gpr_mean = mean(.data[[g]], na.rm = TRUE),
      gpr_sd = sd(.data[[g]], na.rm = TRUE),
      gpr_min = min(.data[[g]], na.rm = TRUE),
      gpr_max = max(.data[[g]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      country = all_of(country_var),
      year = all_of(year_var)
    ) %>%
    arrange(country, year)
}

top_extreme_observations <- function(dd, y, g, hid, n_top = 30) {
  y_sd <- sd(dd[[y]], na.rm = TRUE)
  g_sd <- sd(dd[[g]], na.rm = TRUE)

  dd %>%
    mutate(
      hypothesis_id = hid,
      outcome_z = ifelse(
        is.finite(y_sd) && y_sd > 0,
        (.data[[y]] - mean(.data[[y]], na.rm = TRUE)) / y_sd,
        NA_real_
      ),
      gpr_z = ifelse(
        is.finite(g_sd) && g_sd > 0,
        (.data[[g]] - mean(.data[[g]], na.rm = TRUE)) / g_sd,
        NA_real_
      ),
      combined_extreme_score = abs(outcome_z) + abs(gpr_z)
    ) %>%
    arrange(desc(combined_extreme_score)) %>%
    select(
      hypothesis_id,
      all_of(country_var),
      all_of(year_var),
      all_of(y),
      all_of(g),
      outcome_z,
      gpr_z,
      combined_extreme_score,
      everything()
    ) %>%
    slice_head(n = n_top)
}

leave_one_country_standardized <- function(dd, f, g, hid, full_beta, full_se) {
  countries <- sort(unique(dd[[country_var]]))

  map_dfr(countries, function(cc) {
    d2 <- dd %>%
      filter(.data[[country_var]] != cc) %>%
      drop_singletons(country_var, year_var)

    if (nrow(d2) == 0 ||
        n_distinct(d2[[country_var]]) < 2 ||
        n_distinct(d2[[year_var]]) < 2) {
      return(tibble(
        hypothesis_id = hid,
        omitted_country = cc,
        nobs = nrow(d2),
        clusters = n_distinct(d2[[country_var]]),
        beta = NA_real_,
        se = NA_real_,
        clustered_p = NA_real_,
        beta_change = NA_real_,
        abs_beta_change = NA_real_,
        relative_abs_change = NA_real_,
        dfbeta_like = NA_real_,
        abs_dfbeta_like = NA_real_,
        sign_same = NA
      ))
    }

    fit <- fit_clustered(f, d2, g)
    st <- fit$stats

    b <- st$beta[1]

    tibble(
      hypothesis_id = hid,
      omitted_country = cc,
      nobs = if (!is.null(fit$model)) nobs(fit$model) else nrow(d2),
      clusters = n_distinct(d2[[country_var]]),
      beta = b,
      se = st$se[1],
      clustered_p = st$p[1],
      beta_change = b - full_beta,
      abs_beta_change = abs(b - full_beta),
      relative_abs_change = ifelse(
        is.finite(full_beta) && abs(full_beta) > 0,
        abs(b - full_beta) / abs(full_beta),
        NA_real_
      ),
      dfbeta_like = ifelse(
        is.finite(full_se) && full_se > 0,
        (b - full_beta) / full_se,
        NA_real_
      ),
      abs_dfbeta_like = ifelse(
        is.finite(full_se) && full_se > 0,
        abs(b - full_beta) / full_se,
        NA_real_
      ),
      sign_same = sign(b) == sign(full_beta)
    )
  }) %>%
    arrange(desc(abs_dfbeta_like), desc(abs_beta_change))
}

leave_two_out <- function(dd, f, g, hid, full_beta, influential_countries) {
  influential_countries <- unique(influential_countries)
  if (length(influential_countries) < 2) return(tibble())

  pairs <- combn(influential_countries, 2, simplify = FALSE)

  map_dfr(pairs, function(pair) {
    d2 <- dd %>%
      filter(!(.data[[country_var]] %in% pair)) %>%
      drop_singletons(country_var, year_var)

    fit <- fit_clustered(f, d2, g)
    st <- fit$stats
    b <- st$beta[1]

    tibble(
      hypothesis_id = hid,
      omitted_country_1 = pair[1],
      omitted_country_2 = pair[2],
      nobs = if (!is.null(fit$model)) nobs(fit$model) else nrow(d2),
      clusters = n_distinct(d2[[country_var]]),
      beta = b,
      se = st$se[1],
      clustered_p = st$p[1],
      beta_change = b - full_beta,
      abs_beta_change = abs(b - full_beta),
      sign_same = sign(b) == sign(full_beta)
    )
  }) %>%
    arrange(desc(abs_beta_change))
}

winsor_sensitivity <- function(dd, y, g, controls, hid) {
  specs <- list(
    baseline = NULL,
    winsor_1_99 = c(0.01, 0.99),
    winsor_5_95 = c(0.05, 0.95)
  )

  map_dfr(names(specs), function(spec_name) {
    d2 <- dd

    if (!is.null(specs[[spec_name]])) {
      d2[[y]] <- winsorize_vec(d2[[y]], specs[[spec_name]])
    }

    f2 <- make_formula(y, g, controls)
    fit <- fit_clustered(f2, d2, g)
    st <- fit$stats

    tibble(
      hypothesis_id = hid,
      specification = spec_name,
      nobs = if (!is.null(fit$model)) nobs(fit$model) else nrow(d2),
      clusters = n_distinct(d2[[country_var]]),
      beta = st$beta[1],
      se = st$se[1],
      clustered_p = st$p[1]
    )
  })
}

all_main <- list()
all_country_inf <- list()
all_fwl_share <- list()
all_cy <- list()
all_extreme <- list()
all_leave2 <- list()
all_winsor <- list()
all_focus_country_year <- list()

for (i in seq_len(nrow(targets))) {
  hid   <- targets$hypothesis_id[i]
  block <- targets$block[i]
  y     <- targets$outcome[i]
  g     <- targets$gpr_measure[i]
  d     <- if (block == "financial") fin else legal

  comp <- build_sample_and_formula(d, block, y, g)
  dd <- drop_singletons(comp$data, country_var, year_var)

  full_fit <- fit_clustered(comp$formula, dd, g)
  if (is.null(full_fit$model)) {
    warning(paste("Full model failed for", hid))
    next
  }

  full_stats <- full_fit$stats
  full_beta <- full_stats$beta[1]
  full_se   <- full_stats$se[1]
  full_p    <- full_stats$p[1]

  inf <- leave_one_country_standardized(
    dd, comp$formula, g, hid, full_beta, full_se
  )

  fwl_share <- residualized_gpr_country_share(
    dd, g, comp$controls, hid
  )

  cy <- country_year_profile(dd, y, g, hid)
  extreme <- top_extreme_observations(dd, y, g, hid)

  top5 <- inf %>%
    filter(is.finite(abs_dfbeta_like)) %>%
    slice_head(n = 5) %>%
    pull(omitted_country)

  leave2 <- leave_two_out(
    dd, comp$formula, g, hid, full_beta, top5
  )

  wins <- winsor_sensitivity(dd, y, g, comp$controls, hid)

  top_country <- if (nrow(inf)) inf$omitted_country[1] else NA_character_
  top_fwl_country <- if (nrow(fwl_share)) fwl_share$country[1] else NA_character_

  focus_countries <- unique(na.omit(c(
    "Sri Lanka",
    top_country,
    top_fwl_country
  )))

  focus_cy <- cy %>%
    filter(country %in% focus_countries) %>%
    mutate(
      is_top_influence_country = country == top_country,
      is_top_fwl_share_country = country == top_fwl_country,
      is_sri_lanka = country == "Sri Lanka"
    )

  main_row <- tibble(
    hypothesis_id = hid,
    block = block,
    outcome = y,
    gpr_measure = g,
    nobs = nobs(full_fit$model),
    clusters = n_distinct(dd[[country_var]]),
    years = n_distinct(dd[[year_var]]),
    full_beta = full_beta,
    full_se = full_se,
    full_clustered_p = full_p,
    top_influence_country = top_country,
    top_abs_dfbeta_like = if (nrow(inf)) inf$abs_dfbeta_like[1] else NA_real_,
    top_relative_abs_change = if (nrow(inf)) inf$relative_abs_change[1] else NA_real_,
    top_country_beta_without = if (nrow(inf)) inf$beta[1] else NA_real_,
    top_country_p_without = if (nrow(inf)) inf$clustered_p[1] else NA_real_,
    top_country_sign_flip = if (nrow(inf)) !isTRUE(inf$sign_same[1]) else NA,
    top_fwl_share_country = top_fwl_country,
    top_fwl_residualized_gpr_share = if (nrow(fwl_share)) fwl_share$residualized_gpr_share[1] else NA_real_,
    sri_lanka_fwl_share = {
      z <- fwl_share %>% filter(country == "Sri Lanka")
      if (nrow(z)) z$residualized_gpr_share[1] else NA_real_
    },
    sri_lanka_abs_dfbeta_like = {
      z <- inf %>% filter(omitted_country == "Sri Lanka")
      if (nrow(z)) z$abs_dfbeta_like[1] else NA_real_
    },
    sri_lanka_sign_flip = {
      z <- inf %>% filter(omitted_country == "Sri Lanka")
      if (nrow(z)) !isTRUE(z$sign_same[1]) else NA
    }
  )

  all_main[[hid]] <- main_row
  all_country_inf[[hid]] <- inf
  all_fwl_share[[hid]] <- fwl_share
  all_cy[[hid]] <- cy
  all_extreme[[hid]] <- extreme
  all_leave2[[hid]] <- leave2
  all_winsor[[hid]] <- wins
  all_focus_country_year[[hid]] <- focus_cy

  write_csv(
    inf,
    file.path(out_dir, paste0(hid, "_country_standardized_influence.csv"))
  )
  write_csv(
    fwl_share,
    file.path(out_dir, paste0(hid, "_residualized_gpr_country_share.csv"))
  )
  write_csv(
    focus_cy,
    file.path(out_dir, paste0(hid, "_focus_country_year_profile.csv"))
  )
  write_csv(
    wins,
    file.path(out_dir, paste0(hid, "_winsor_sensitivity.csv"))
  )
  if (nrow(leave2)) {
    write_csv(
      leave2,
      file.path(out_dir, paste0(hid, "_leave_two_out.csv"))
    )
  }

  p1 <- ggplot(
    fwl_share %>% slice_head(n = min(15, n())),
    aes(x = reorder(country, residualized_gpr_share), y = residualized_gpr_share)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0(hid, ": residualized GPR identifying-variation share"),
      x = NULL,
      y = "Share of residualized GPR sum of squares"
    )
  ggsave(
    file.path(out_dir, paste0(hid, "_residualized_gpr_share.png")),
    p1, width = 8, height = 6, dpi = 180
  )

  p2 <- ggplot(
    inf %>% slice_head(n = min(15, n())),
    aes(x = reorder(omitted_country, abs_dfbeta_like), y = abs_dfbeta_like)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0(hid, ": standardized country influence"),
      x = NULL,
      y = "|Beta_without_country - Beta_full| / SE_full"
    )
  ggsave(
    file.path(out_dir, paste0(hid, "_standardized_country_influence.png")),
    p2, width = 8, height = 6, dpi = 180
  )
}

main_df <- bind_rows(all_main)
country_inf_df <- bind_rows(all_country_inf)
fwl_share_df <- bind_rows(all_fwl_share)
cy_df <- bind_rows(all_cy)
extreme_df <- bind_rows(all_extreme)
leave2_df <- bind_rows(all_leave2)
winsor_df <- bind_rows(all_winsor)
focus_cy_df <- bind_rows(all_focus_country_year)

write_csv(main_df,       file.path(out_dir, "00_stage3i_diagnostic_summary.csv"))
write_csv(country_inf_df,file.path(out_dir, "01_all_standardized_country_influence.csv"))
write_csv(fwl_share_df,  file.path(out_dir, "02_all_residualized_gpr_country_share.csv"))
write_csv(cy_df,         file.path(out_dir, "03_all_country_year_profiles.csv"))
write_csv(extreme_df,    file.path(out_dir, "04_top_extreme_observations.csv"))
write_csv(winsor_df,     file.path(out_dir, "05_winsor_sensitivity.csv"))
write_csv(focus_cy_df,   file.path(out_dir, "06_focus_country_year_profiles.csv"))
if (nrow(leave2_df)) {
  write_csv(leave2_df, file.path(out_dir, "07_leave_two_out.csv"))
}

flag_df <- main_df %>%
  mutate(
    flag_sign_flip = top_country_sign_flip %in% TRUE,
    flag_large_standardized_influence = top_abs_dfbeta_like >= 1,
    flag_fwl_concentration_25pct = top_fwl_residualized_gpr_share >= 0.25,
    flag_sri_lanka_sign_flip = sri_lanka_sign_flip %in% TRUE,
    diagnostic_status = case_when(
      flag_sign_flip ~ "RED: sign flips when top country is removed",
      flag_large_standardized_influence | flag_fwl_concentration_25pct ~
        "AMBER: high influence/concentrated identifying variation",
      TRUE ~ "GREEN: no major country-influence flag"
    )
  )

write_csv(flag_df, file.path(out_dir, "08_diagnostic_flags.csv"))

md <- c(
  "# Stage 3I High-Influence Country Diagnostic",
  "",
  "This stage does not alter the baseline specification.",
  "It diagnoses whether the GPR coefficient is driven by a small number of countries,",
  "whether effective GPR identifying variation is concentrated after controls and fixed effects,",
  "and whether extreme continuous outcomes explain the result.",
  "",
  "## Interpretation rules",
  "",
  "- RED: removing the most influential country flips the coefficient sign.",
  "- AMBER: no sign flip, but standardized influence >= 1 full-sample SE or residualized GPR share >= 25%.",
  "- GREEN: no major country-level influence flag under these thresholds.",
  "",
  "## Summary",
  ""
)

for (i in seq_len(nrow(flag_df))) {
  r <- flag_df[i, ]
  md <- c(
    md,
    paste0(
      "- ", r$hypothesis_id,
      ": beta=", signif(r$full_beta, 4),
      ", p=", signif(r$full_clustered_p, 4),
      ", top country=", r$top_influence_country,
      ", |DFBETA-like|=", signif(r$top_abs_dfbeta_like, 4),
      ", top residualized-GPR share=", signif(r$top_fwl_residualized_gpr_share, 4),
      ", Sri Lanka share=", signif(r$sri_lanka_fwl_share, 4),
      ", status=", r$diagnostic_status
    )
  )
}

writeLines(md, file.path(out_dir, "STAGE3I_SUMMARY.md"))

cat(paste(md, collapse = "\n"), "\n")
