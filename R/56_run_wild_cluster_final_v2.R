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
if(!requireNamespace("fwildclusterboot",quietly=TRUE)) stop("fwildclusterboot is not installed.", call.=FALSE)

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

    bad_c <- x %>% count(.data[[country_var]], name="n") %>% filter(n <= 1) %>% pull(1)
    if(length(bad_c)) x <- x %>% filter(!(.data[[country_var]] %in% bad_c))

    bad_y <- x %>% count(.data[[year_var]], name="n") %>% filter(n <= 1) %>% pull(1)
    if(length(bad_y)) x <- x %>% filter(!(.data[[year_var]] %in% bad_y))

    if(nrow(x)==n0) break
    if(iter > 100L) stop("Singleton pruning failed to converge.")
  }
  attr(x,"iterations") <- iter
  x
}

build_sample_and_formula <- function(d, block, y, g) {
  controls <- intersect(c("log_loan_amount","creditor_type"), names(d))
  if(block=="financial" && y=="pricing_rate_t0")
    controls <- c(controls, intersect(c("maturity_years","grace_period_years"), names(d)))
  if(block=="financial" && y=="maturity_years")
    controls <- c(controls, intersect("grace_period_years", names(d)))
  if(block=="legal")
    controls <- c(controls, intersect(c("maturity_years","grace_period_years"), names(d)))
  controls <- unique(controls)

  vars_needed <- unique(c("main_sample",country_var,year_var,y,g,controls))
  dd <- d %>% filter(main_sample==1) %>% filter(if_all(all_of(vars_needed), ~ !is.na(.x)))

  rhs <- paste(c(g,controls), collapse=" + ")
  f <- as.formula(paste0(y," ~ ",rhs," | ",country_var," + ",year_var))
  list(data=dd,formula=f)
}

extract_p <- function(bt) {
  for(nm in c("p_val","p.value","pvalue","p")) {
    if(!is.null(bt[[nm]])) {
      val <- suppressWarnings(as.numeric(bt[[nm]][1]))
      if(length(val)==1 && is.finite(val)) return(val)
    }
  }
  txt <- capture.output(print(bt))
  hit <- grep("p[- ]?value|p_val|Pr\\(", txt, ignore.case=TRUE, value=TRUE)
  if(length(hit)) {
    nums <- regmatches(hit, gregexpr("[0-9]*\\.?[0-9]+", hit))
    nums <- suppressWarnings(as.numeric(unlist(nums)))
    nums <- nums[is.finite(nums) & nums>=0 & nums<=1]
    if(length(nums)) return(tail(nums,1))
  }
  NA_real_
}

set.seed(20260828)
B <- 9999L
boot_res <- list()
audit_res <- list()

for(i in seq_len(nrow(targets))) {
  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  d <- if(block=="financial") fin else legal

  comp <- build_sample_and_formula(d,block,y,g)
  raw <- comp$data
  pruned <- drop_singletons(raw,country_var,year_var)

  min_c <- min(table(pruned[[country_var]]))
  min_y <- min(table(pruned[[year_var]]))
  if(min_c < 2 || min_y < 2) stop(paste0(hid,": singleton remains after pruning."))

  m <- feols(comp$formula, data=pruned, fixef.rm="none")
  expected_n <- main %>% filter(hypothesis_id==hid) %>% pull(nobs)
  if(length(expected_n)==0) expected_n <- NA_integer_

  audit_res[[hid]] <- tibble(
    hypothesis_id=hid,
    n_raw_complete=nrow(raw),
    n_after_singleton_pruning=nrow(pruned),
    n_feols=nobs(m),
    stage3g_expected_n=expected_n,
    n_match=ifelse(is.na(expected_n),NA,nobs(m)==expected_n),
    country_clusters=n_distinct(pruned[[country_var]]),
    year_fe=n_distinct(pruned[[year_var]]),
    min_country_cell=min_c,
    min_year_cell=min_y,
    singleton_iterations=attr(pruned,"iterations")
  )

  bt <- tryCatch(
    fwildclusterboot::boottest(
      m, param=g, clustid=country_var, B=B,
      type="rademacher", impose_null=TRUE
    ),
    error=function(e) e
  )

  if(inherits(bt,"error")) {
    msg1 <- conditionMessage(bt)
    bt2 <- tryCatch(
      fwildclusterboot::boottest(
        m, param=g, clustid=pruned[[country_var]], B=B,
        type="rademacher", impose_null=TRUE
      ),
      error=function(e) e
    )
    if(inherits(bt2,"error")) {
      boot_res[[hid]] <- tibble(
        hypothesis_id=hid,status="BOOTTEST_FAILED",
        p_value_wild_cluster=NA_real_,
        nobs=nobs(m),n_clusters=n_distinct(pruned[[country_var]]),
        B=B,seed=20260828L,
        reason=paste0("String clustid error: ",msg1," | Vector clustid error: ",conditionMessage(bt2))
      )
      next
    } else bt <- bt2
  }

  pboot <- extract_p(bt)
  capture.output(print(bt), file=file.path(outdir,paste0("wildcluster_",hid,"_print.txt")))
  capture.output(summary(bt), file=file.path(outdir,paste0("wildcluster_",hid,"_summary.txt")))

  boot_res[[hid]] <- tibble(
    hypothesis_id=hid,
    status=ifelse(is.finite(pboot),"OK","P_EXTRACT_FAILED"),
    p_value_wild_cluster=pboot,
    nobs=nobs(m),
    n_clusters=n_distinct(pruned[[country_var]]),
    B=B,seed=20260828L,
    reason=ifelse(is.finite(pboot),NA_character_,"Inspect print/summary files.")
  )
}

write_csv(bind_rows(audit_res), file.path(outdir,"06a_wildcluster_sample_audit.csv"))
write_csv(bind_rows(boot_res), file.path(outdir,"06_final_wild_cluster_results.csv"))
message("56_run_wild_cluster_final_v2.R completed.")
