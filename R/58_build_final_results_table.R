suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

outdir <- "outputs/stage3g_final_inference"

main <- read_csv(file.path(outdir,"04_final_clustered_results.csv"),show_col_types=FALSE)
wild <- read_csv(file.path(outdir,"06_final_wild_cluster_results.csv"),show_col_types=FALSE)
loo_c <- read_csv(file.path(outdir,"10_final_leave_one_country_summary.csv"),show_col_types=FALSE)
loo_y <- read_csv(file.path(outdir,"11_final_leave_one_year_summary.csv"),show_col_types=FALSE)

final <- main %>%
  left_join(
    wild %>% select(hypothesis_id,status,p_value_wild_cluster),
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
    final_strength = case_when(
      p.value<0.05 &
        p_holm_all<0.10 &
        loo_country_sign_share>=0.95 &
        loo_year_sign_share>=0.95 ~ "strong",
      p.value<0.10 &
        loo_country_sign_share>=0.90 &
        loo_year_sign_share>=0.90 ~ "moderate",
      TRUE ~ "weak"
    )
  )

write_csv(
  final,
  file.path(outdir,"12_final_results_table.csv")
)

message("58_build_final_results_table.R completed.")
