suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3g_final_inference"

main <- read_csv(file.path(outdir,"04_final_clustered_results.csv"),
                 show_col_types=FALSE)
wild <- read_csv(file.path(outdir,"06_final_wild_cluster_results.csv"),
                 show_col_types=FALSE)
audit <- read_csv(file.path(outdir,"06a_wildcluster_sample_audit.csv"),
                  show_col_types=FALSE)
loo_c <- read_csv(file.path(outdir,"10_final_leave_one_country_summary.csv"),
                  show_col_types=FALSE)
loo_y <- read_csv(file.path(outdir,"11_final_leave_one_year_summary.csv"),
                  show_col_types=FALSE)

final <- main %>%
  left_join(
    wild %>%
      select(
        hypothesis_id,
        wild_status=status,
        p_value_wild_cluster,
        wild_nobs=nobs,
        wild_n_clusters=n_clusters
      ),
    by="hypothesis_id"
  ) %>%
  left_join(
    audit %>%
      select(
        hypothesis_id,
        bootstrap_n_match=n_match,
        bootstrap_year_fe=year_fe,
        singleton_iterations
      ),
    by="hypothesis_id"
  ) %>%
  left_join(
    loo_c %>%
      select(
        hypothesis_id,
        loo_country_sign_share=share_expected_sign
      ),
    by="hypothesis_id"
  ) %>%
  left_join(
    loo_y %>%
      select(
        hypothesis_id,
        loo_year_sign_share=share_expected_sign
      ),
    by="hypothesis_id"
  ) %>%
  mutate(
    final_strength=case_when(
      wild_status=="OK" &
        p.value<0.05 &
        p_value_wild_cluster<0.10 &
        loo_country_sign_share>=0.95 &
        loo_year_sign_share>=0.95 ~ "strong",

      wild_status=="OK" &
        p.value<0.10 &
        p_value_wild_cluster<0.15 &
        loo_country_sign_share>=0.90 &
        loo_year_sign_share>=0.90 ~ "moderate",

      TRUE ~ "weak_or_unconfirmed"
    )
  )

write_csv(
  final,
  file.path(outdir,"12_final_results_table.csv")
)

message("58_build_final_results_table_v2.R completed.")
