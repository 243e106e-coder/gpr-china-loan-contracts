# Paper 1 — Stage 1 Data Audit

**Working title:** Pricing or Contracting? Geopolitical Risk and Risk Allocation in China’s Overseas Lending

This starter pack does **not** run the final econometric models yet. It first verifies the real HCL 2.0 file structure, column names, missingness, and GPR coverage so that later code does not rely on guessed variable names.

## Stage 1 goals

1. Download AidData **How China Lends 2.0**.
2. Download the official Caldara–Iacoviello GPR files and the 2026 AI-GPR country file.
3. Inventory every tabular file and sheet in HCL 2.0.
4. Produce:
   - file inventory;
   - table/sheet inventory;
   - column dictionary;
   - missingness report;
   - candidate columns for country/date/lender/interest/maturity/collateral/guarantee/etc.;
   - GPR column dictionary.
5. Upload all audit outputs as a GitHub Actions artifact.

## Official data sources

- AidData HCL 2.0:
  https://www.aiddata.org/data/how-china-lends-dataset-version-2-0
- Direct HCL 2.0 ZIP:
  https://docs.aiddata.org/ad4/datasets/How_China_Lends_Dataset_Version_2_0.zip
- Caldara–Iacoviello GPR:
  https://www.matteoiacoviello.com/gpr.htm
- Country-specific GPR:
  https://www.matteoiacoviello.com/gpr_country.htm
- AI-GPR:
  https://www.matteoiacoviello.com/ai_gpr.html

## Run locally

Requires R >= 4.3.

```bash
Rscript R/run_stage1.R
```

## Run on GitHub Actions

Copy this folder into a GitHub repository, commit, then:

1. Open **Actions**.
2. Select **Paper1 Stage1 Data Audit**.
3. Click **Run workflow**.
4. When it finishes, download the artifact named:
   `paper1-stage1-audit`

## What to send back for Stage 2

The most important files are:

- `outputs/stage1_audit/hcl_table_inventory.csv`
- `outputs/stage1_audit/hcl_column_dictionary.csv`
- `outputs/stage1_audit/hcl_column_candidates.csv`
- `outputs/stage1_audit/hcl_missingness.csv`
- `outputs/stage1_audit/gpr_column_dictionary.csv`

Stage 2 will then use the **actual HCL column names** to build:

- borrower-country mapping;
- contract signing month;
- 3/6/12-month lagged GPR;
- pricing outcomes;
- legal-protection outcomes;
- baseline FE regressions;
- Joint Risk Allocation model.

## Important

Do not manually rename HCL variables before Stage 1 finishes. The audit is intended to preserve the original variable names and document them.
