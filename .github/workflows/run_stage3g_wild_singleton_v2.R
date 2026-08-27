name: Paper1 Stage3G Wild Cluster Singleton Fix V2

on:
  workflow_dispatch:

jobs:
  wildcluster-v2:
    runs-on: ubuntu-latest
    timeout-minutes: 120

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: "4.4.2"
          use-public-rspm: true

      - name: Setup dependencies
        uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::readr
            any::dplyr
            any::fixest
            any::broom
            any::haven
            any::stringr
            any::tidyr
            any::purrr
            any::janitor

      - name: Install fwildclusterboot
        run: |
          Rscript -e '
          options(
            repos=c(
              s3alfisc="https://s3alfisc.r-universe.dev",
              CRAN="https://cloud.r-project.org"
            )
          )
          install.packages("fwildclusterboot", dependencies=TRUE)
          stopifnot(requireNamespace("fwildclusterboot", quietly=TRUE))
          cat(
            "fwildclusterboot version:",
            as.character(packageVersion("fwildclusterboot")),
            "\n"
          )
          '

      - name: Check Stage3G runner files
        run: |
          set -euo pipefail

          echo "===== CHECK R FILES ====="

          test -f R/run_stage3g_wild_singleton_v2.R
          test -f R/56_run_wild_cluster_final_v2.R
          test -f R/58_build_final_results_table_v2.R
          test -f R/59_stage3g_summary_v2.R

          echo "Singleton V2 scripts found."

          if [ -f R/run_stage3g.R ]; then
            echo "Full Stage3G runner found: R/run_stage3g.R"
          else
            echo "WARNING: R/run_stage3g.R not found."
            echo "The V2 runner can only continue if Stage3G base outputs already exist."
          fi

      - name: Run Stage3G and singleton-safe bootstrap V2
        run: |
          set -euo pipefail

          Rscript R/run_stage3g_wild_singleton_v2.R

      - name: Verify required outputs
        run: |
          set -euo pipefail

          OUT="outputs/stage3g_final_inference"

          echo "===== VERIFY OUTPUTS ====="

          test -f "$OUT/06a_wildcluster_sample_audit.csv"
          test -f "$OUT/06_final_wild_cluster_results.csv"
          test -f "$OUT/12_final_results_table.csv"

          echo ""
          echo "===== SAMPLE AUDIT ====="
          cat "$OUT/06a_wildcluster_sample_audit.csv"

          echo ""
          echo "===== WILD CLUSTER RESULTS ====="
          cat "$OUT/06_final_wild_cluster_results.csv"

          echo ""
          echo "===== FINAL RESULTS ====="
          cat "$OUT/12_final_results_table.csv"

      - name: Show final summary
        if: always()
        run: |
          OUT="outputs/stage3g_final_inference"

          echo "===== STAGE3G SUMMARY ====="

          if [ -f "$OUT/STAGE3G_SUMMARY.md" ]; then
            cat "$OUT/STAGE3G_SUMMARY.md"
          else
            echo "STAGE3G_SUMMARY.md was not generated."
          fi

      - name: Upload artifact
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: paper1-stage3g-wild-singleton-v2
          path: outputs/stage3g_final_inference/
          if-no-files-found: warn
          retention-days: 30
