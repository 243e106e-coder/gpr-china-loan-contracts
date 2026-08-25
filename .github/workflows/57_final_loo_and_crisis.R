suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
})

outdir <- "outputs/stage3g_final_inference"

fin <- read_csv(file.path(outdir,"01_financial_final_data.csv"),show_col_types=FALSE,progress=FALSE)
legal <- read_csv(file.path(outdir,"02_legal_final_data.csv"),show_col_types=FALSE,progress=FALSE)
targets <- read_csv(file.path(outdir,"03_final_hypotheses.csv"),show_col_types=FALSE)

country_var <- "borrower_country_final"

fit_one <- function(d,y,g,block,drop_country=NULL,drop_year=NULL,exclude_years=NULL) {

  controls <- intersect(c("log_loan_amount","creditor_type"),names(d))

  if(block=="financial" && y=="pricing_rate_t0") {
    controls <- c(controls,intersect(c("maturity_years","grace_period_years"),names(d)))
  }

  if(block=="financial" && y=="maturity_years") {
    controls <- c(controls,intersect(c("grace_period_years"),names(d)))
  }

  if(block=="legal") {
    controls <- c(controls,intersect(c("maturity_years","grace_period_years"),names(d)))
  }

  controls <- unique(controls)

  dd <- d %>%
    filter(
      main_sample==1,
      !is.na(.data[[y]]),
      !is.na(.data[[g]]),
      !is.na(.data[[country_var]]),
      !is.na(year)
    )

  if(!is.null(drop_country)) dd <- dd %>% filter(.data[[country_var]]!=drop_country)
  if(!is.null(drop_year)) dd <- dd %>% filter(year!=drop_year)
  if(!is.null(exclude_years)) dd <- dd %>% filter(!(year %in% exclude_years))

  if(nrow(dd)<40) return(NULL)
  if(block=="legal" && n_distinct(dd[[y]],na.rm=TRUE)<2) return(NULL)

  rhs <- paste(c(g,controls),collapse=" + ")
  f <- as.formula(
    paste0(
      y," ~ ",rhs,
      " | ",country_var," + year"
    )
  )

  tryCatch(
    feols(f,data=dd,vcov=as.formula(paste0("~",country_var))),
    error=function(e) NULL
  )
}

loo_country_res <- list()
loo_year_res <- list()
crisis_res <- list()

crisis_windows <- list(
  exclude_2001=2001,
  exclude_gfc_2008_2009=c(2008,2009),
  exclude_2014_2015=c(2014,2015),
  exclude_covid_2020_2021=c(2020,2021),
  exclude_ukraine_2022_2023=c(2022,2023)
)

for(i in seq_len(nrow(targets))) {

  block <- targets$block[i]
  hid <- targets$hypothesis_id[i]
  y <- targets$outcome[i]
  g <- targets$gpr_measure[i]
  expected <- targets$expected_sign[i]

  d <- if(block=="financial") fin else legal

  countries <- sort(unique(d[[country_var]][!is.na(d[[country_var]])]))
  years <- sort(unique(d$year[!is.na(d$year)]))

  for(cc in countries) {
    m <- fit_one(d,y,g,block,drop_country=cc)
    if(is.null(m) || !(g %in% names(coef(m)))) next

    loo_country_res[[paste(hid,cc,sep="__")]] <- tibble(
      hypothesis_id=hid,
      dropped_country=cc,
      estimate=as.numeric(coef(m)[g]),
      sign_ok=if(expected=="+") coef(m)[g]>0 else coef(m)[g]<0
    )
  }

  for(yy in years) {
    m <- fit_one(d,y,g,block,drop_year=yy)
    if(is.null(m) || !(g %in% names(coef(m)))) next

    loo_year_res[[paste(hid,yy,sep="__")]] <- tibble(
      hypothesis_id=hid,
      dropped_year=yy,
      estimate=as.numeric(coef(m)[g]),
      sign_ok=if(expected=="+") coef(m)[g]>0 else coef(m)[g]<0
    )
  }

  for(ex_name in names(crisis_windows)) {
    yrs <- crisis_windows[[ex_name]]
    m <- fit_one(d,y,g,block,exclude_years=yrs)
    if(is.null(m) || !(g %in% names(coef(m)))) next

    loo <- coef(m)[g]
    se <- sqrt(vcov(m)[g,g])
    tval <- loo/se
    df <- max(1,n_distinct(model.frame(m)[[country_var]])-1)
    pval <- 2*pt(abs(tval),df=df,lower.tail=FALSE)

    crisis_res[[paste(hid,ex_name,sep="__")]] <- tibble(
      hypothesis_id=hid,
      exclusion=ex_name,
      estimate=as.numeric(loo),
      std_error=as.numeric(se),
      p_value=as.numeric(pval),
      sign_ok=if(expected=="+") loo>0 else loo<0
    )
  }
}

loo_c <- bind_rows(loo_country_res)
loo_y <- bind_rows(loo_year_res)
crisis <- bind_rows(crisis_res)

write_csv(loo_c,file.path(outdir,"07_final_leave_one_country_out.csv"))
write_csv(loo_y,file.path(outdir,"08_final_leave_one_year_out.csv"))
write_csv(crisis,file.path(outdir,"09_final_crisis_exclusions.csv"))

loo_c_sum <- loo_c %>%
  group_by(hypothesis_id) %>%
  summarise(
    n_runs=n(),
    min_beta=min(estimate,na.rm=TRUE),
    max_beta=max(estimate,na.rm=TRUE),
    share_expected_sign=mean(sign_ok,na.rm=TRUE),
    .groups="drop"
  )

loo_y_sum <- loo_y %>%
  group_by(hypothesis_id) %>%
  summarise(
    n_runs=n(),
    min_beta=min(estimate,na.rm=TRUE),
    max_beta=max(estimate,na.rm=TRUE),
    share_expected_sign=mean(sign_ok,na.rm=TRUE),
    .groups="drop"
  )

write_csv(loo_c_sum,file.path(outdir,"10_final_leave_one_country_summary.csv"))
write_csv(loo_y_sum,file.path(outdir,"11_final_leave_one_year_summary.csv"))

message("57_final_loo_and_crisis.R completed.")
