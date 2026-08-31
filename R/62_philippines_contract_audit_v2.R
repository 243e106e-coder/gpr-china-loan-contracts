suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

# Stage 3K v2: audit the Philippines observations in the exact L3/L4 sample.
base_dir <- "outputs/stage3g_final_inference"
out_dir <- "outputs/stage3k_philippines_contract_audit_v2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

country_var <- "borrower_country_final"
year_var <- "year"
focus_country <- "Philippines"

legal_path <- file.path(base_dir, "02_legal_final_data.csv")
targets_path <- file.path(base_dir, "03_final_hypotheses.csv")
if (!file.exists(legal_path) || !file.exists(targets_path)) {
  stop("Required Stage 3G legal inputs are missing.", call. = FALSE)
}

legal <- read_csv(legal_path, show_col_types = FALSE)
targets <- read_csv(targets_path, show_col_types = FALSE) %>%
  filter(hypothesis_id %in% c("L3", "L4"))

if (!all(c("L3", "L4") %in% targets$hypothesis_id)) {
  stop("The Stage 3G hypothesis table must contain L3 and L4.", call. = FALSE)
}

outcome_l3 <- targets$outcome[match("L3", targets$hypothesis_id)]
outcome_l4 <- targets$outcome[match("L4", targets$hypothesis_id)]
gpr_var <- targets$gpr_measure[match("L3", targets$hypothesis_id)]

first_existing <- function(candidates, available) {
  hit <- intersect(candidates, available)
  if (length(hit)) hit[1] else NA_character_
}

creditor_col <- first_existing(
  c("creditor_name_final", "creditor_name", "creditor", "lender_name", "lender"),
  names(legal)
)
project_col <- first_existing(
  c("project_name", "project_title", "project", "purpose", "sector"),
  names(legal)
)
contract_col <- first_existing(
  c("contract_id", "loan_id", "record_id", "uid", "id"),
  names(legal)
)

controls <- unique(c(
  intersect(c("log_loan_amount", "creditor_type"), names(legal)),
  intersect(c("maturity_years", "grace_period_years"), names(legal))
))
required <- unique(c(
  "main_sample", country_var, year_var, outcome_l3, outcome_l4, gpr_var, controls
))

sample <- legal %>%
  filter(main_sample == 1) %>%
  filter(if_all(all_of(required), ~ !is.na(.x)))
philippines <- sample %>% filter(.data[[country_var]] == focus_country)

if (!nrow(philippines)) {
  stop("No Philippines contracts occur in the exact L3/L4 estimation sample.", call. = FALSE)
}

audit_columns <- unique(na.omit(c(
  contract_col, creditor_col, project_col,
  country_var, year_var, gpr_var, outcome_l3, outcome_l4,
  "creditor_type", "log_loan_amount", "maturity_years", "grace_period_years"
)))

contract_level <- philippines %>%
  mutate(row_in_philippines_sample = row_number()) %>%
  select(any_of(audit_columns), everything())

has_log_amount <- "log_loan_amount" %in% names(sample)
year_comparison <- sample %>%
  mutate(country_group = if_else(.data[[country_var]] == focus_country, focus_country, "All other countries")) %>%
  group_by(country_group, .data[[year_var]]) %>%
  summarise(
    n_contracts = n(),
    gpr_mean = mean(.data[[gpr_var]]),
    mainland_china_arbitration_share = mean(.data[[outcome_l3]]),
    international_third_arbitration_share = mean(.data[[outcome_l4]]),
    mean_log_loan_amount = if (has_log_amount) mean(log_loan_amount) else NA_real_,
    .groups = "drop"
  ) %>%
  rename(year = all_of(year_var)) %>%
  arrange(year, country_group)

group_profile <- function(data, group_column, group_label) {
  if (is.na(group_column)) {
    return(tibble(
      group_variable = group_label,
      group_value = "<unavailable>",
      n_contracts = NA_integer_,
      contract_share = NA_real_,
      first_year = NA_real_,
      last_year = NA_real_,
      mainland_china_arbitration_share = NA_real_,
      international_third_arbitration_share = NA_real_
    ))
  }

  data %>%
    mutate(.group = as.character(.data[[group_column]])) %>%
    mutate(.group = if_else(is.na(.group) | .group == "", "<missing>", .group)) %>%
    group_by(.group) %>%
    summarise(
      n_contracts = n(),
      contract_share = n() / nrow(data),
      first_year = min(.data[[year_var]]),
      last_year = max(.data[[year_var]]),
      mainland_china_arbitration_share = mean(.data[[outcome_l3]]),
      international_third_arbitration_share = mean(.data[[outcome_l4]]),
      .groups = "drop"
    ) %>%
    transmute(
      group_variable = group_label,
      group_value = .group,
      n_contracts = n_contracts,
      contract_share = contract_share,
      first_year = first_year,
      last_year = last_year,
      mainland_china_arbitration_share = mainland_china_arbitration_share,
      international_third_arbitration_share = international_third_arbitration_share
    ) %>%
    arrange(desc(n_contracts), group_value)
}

concentration <- bind_rows(
  group_profile(philippines, creditor_col, "creditor"),
  group_profile(philippines, project_col, "project_or_sector")
)

concentration_metrics <- concentration %>%
  filter(is.finite(contract_share)) %>%
  group_by(group_variable) %>%
  summarise(
    n_groups = n(),
    top_group = group_value[which.max(n_contracts)],
    top_group_contracts = max(n_contracts),
    top_group_share = max(contract_share),
    hhi = sum(contract_share ^ 2),
    .groups = "drop"
  )

year_outcome_cells <- philippines %>%
  count(
    .data[[year_var]],
    .data[[outcome_l3]],
    .data[[outcome_l4]],
    name = "n_contracts"
  ) %>%
  rename(
    year = all_of(year_var),
    arb_mainland_china = all_of(outcome_l3),
    arb_international_third = all_of(outcome_l4)
  ) %>%
  arrange(year, desc(n_contracts))

write_csv(contract_level, file.path(out_dir, "01_philippines_contract_level_audit.csv"))
write_csv(year_comparison, file.path(out_dir, "02_philippines_vs_other_countries_by_year.csv"))
write_csv(concentration, file.path(out_dir, "03_philippines_creditor_project_concentration.csv"))
write_csv(concentration_metrics, file.path(out_dir, "04_philippines_concentration_metrics.csv"))
write_csv(year_outcome_cells, file.path(out_dir, "05_philippines_year_outcome_cells.csv"))

summary_lines <- c(
  "# Stage 3K v2 Philippines Contract Audit",
  "",
  paste0("Exact L3/L4 estimation sample: ", nrow(sample), " contracts."),
  paste0("Philippines observations: ", nrow(philippines), " contracts."),
  paste0("Philippines sample years: ", min(philippines[[year_var]]), "–", max(philippines[[year_var]]), "."),
  "",
  "Interpretation: a credible mechanism requires observations across multiple years and no overwhelming single-creditor or single-project concentration."
)
writeLines(summary_lines, file.path(out_dir, "STAGE3K_V2_SUMMARY.md"))
cat(paste(summary_lines, collapse = "\n"), "\n")
