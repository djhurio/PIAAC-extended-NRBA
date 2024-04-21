# Extended NRBA Analyses (without scores) LVA

# ANALYSIS 1: Estimates Before and After Weighting

## Options
options(max.print = 10e3)

## Reset
rm(list = ls())
gc()

## Libs
library(purrr)
library(data.table)
library(survey)
library(nrba) # For running NR bias analysis

## Data & weights

### SDIF - final
dat_sdif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/SAS/SAS7BDAT/psdlvams.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_sdif)

### WIF
dat_wif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_wif)

# ### SPRWT0
# dat_sprwt <- haven::read_sas(
#   data_file = "../PIAAC-data-2023/data-weights/sprwt.sas7bdat"
# ) |> setDT()

# hh_repwt
# HHUEWT: Screener unknown eligibility adjusted weight
dat_hh_repwt <- haven::read_sas(
  data_file = "analyses/data/qc_hh_repwt_lva.sas7bdat"
) |> setDT(key = "CASEID") |> setcolorder()
names(dat_hh_repwt)
# Remove variables overlapping with WIF
x <- grep("WT0$", names(dat_hh_repwt), value = TRUE)
dat_hh_repwt[, c(x) := NULL]
rm(x)

# sp_repwt
# SPBWT: Sample person base weight
# SPLNRWT: Sample person literacy-related nonresponse adjusted weight
dat_sp_repwt <- haven::read_sas(
  data_file = "analyses/data/qc_sp_repwt_lva.sas7bdat"
) |> setDT(key = "PERSID") |> setcolorder()
names(dat_sp_repwt)
# Remove variables overlapping with WIF
x <- grep("WT0$", names(dat_sp_repwt), value = TRUE)
dat_sp_repwt[, c(x) := NULL]
rm(x)


# # Test of overlapping variables
# varnames <- intersect(names(dat_wif), names(dat_hh_repwt))
# tmp <- merge(
#   x = dat_wif[, ..varnames],
#   y = dat_hh_repwt[, ..varnames],
#   by = key(dat_hh_repwt)
# )
# tmp[, all.equal(HHUEWT0.x, HHUEWT0.y, check.attributes = FALSE)]
# # equal
# 
# varnames <- intersect(names(dat_wif), names(dat_sp_repwt))
# tmp <- merge(
#   x = dat_wif[, ..varnames],
#   y = dat_sp_repwt[, ..varnames],
#   by = key(dat_sp_repwt)
# )
# tmp[, all.equal(SPBWT0.x, SPBWT0.y, check.attributes = FALSE)]
# tmp[, all.equal(SPLNRWT0.x, SPLNRWT0.y, check.attributes = FALSE)]
# # equal
# 
# rm(varnames, tmp)


### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()


### External_Estimates_LVA
sheet_names <- openxlsx2::wb_load(
  file = "results/External_Estimates_LVA.xlsx"
) |> openxlsx2::wb_get_sheet_names()

dat_ext_est <- purrr::map(
  .x = sheet_names,
  .f = \(x) {
    openxlsx2::read_xlsx(
      file = "results/External_Estimates_LVA.xlsx",
      sheet = x
    )
  }
)

length(dat_ext_est)
names(dat_ext_est)


## Combine
key(dat_sdif)
key(dat_wif)
key(dat_nrba)
key(dat_hh_repwt)
key(dat_sp_repwt)

names(dat_wif)
names(dat_hh_repwt)
names(dat_sp_repwt)

da <- Reduce(f = merge, x = list(dat_sdif, dat_wif, dat_nrba)) |>
  merge(y = dat_hh_repwt, by = "CASEID") |>
  merge(y = dat_sp_repwt, by = "PERSID")

setkeyv(da, key(dat_sdif)) |> setcolorder()
key(da)
names(da) |> first(10)


# Age groups
grep("CI_AGE", names(da), value = TRUE)
da[, range(CI_AGE)]
da[, CI_AGE_CAT := trunc((CI_AGE - min(CI_AGE)) / 5 + 1)]
da[, as.list(range(CI_AGE)), keyby = .(CI_AGE_CAT)]


# ANALYSIS 1: Estimates Before and After Weighting

# Notes: 
# - A t-test is used for this analysis. PROBF1 is the resulting p-value from the t-test.
# - Significant differences are those with PROBF1 < Bonferroni Alpha. Bonferroni Alpha is calculated as alpha(0.05)/(number of categories).
# - "Eligible SPs" are all sample persons, excluding ineligible cases (those with DISP_CIBQ=18 or 25).

# Base Weight All Eligible SPs (1)
# SPBWT0

da[!is.na(PERSID), .N, keyby = .(DISP_CIBQ)]
da[!is.na(PERSID), .N]
da[!is.na(PERSID) & !DISP_CIBQ %in% c(18, 25), .N]

da[
  !is.na(PERSID) & !DISP_CIBQ %in% c(18, 25),
  .(prop1 = sum(SPBWT0)),
  keyby = "NRBAVAR1"
][, prop1 := round(100 * prop.table(prop1), 2)][]


# - "Respondents" are sampled persons with WEIGHTFLG=1 (that is, with DISP_CIBQ=1 or (DISP_CIBQ=7 and DISP_DS=1)).

# Base Weight Respondents (2)
# SPBWT0

da[!is.na(PERSID) & WEIGHTFLG == 1, .N]
da[!is.na(PERSID) & (DISP_CIBQ == 1 | DISP_CIBQ == 7 & DISP_DS == 1), .N]

da[
  !is.na(PERSID) & WEIGHTFLG == 1,
  .(prop1 = sum(SPBWT0)),
  keyby = "NRBAVAR1"
][, prop1 := round(100 * prop.table(prop1), 2)][]

# NR Adj Weight Respondents (3)
# SPLNRWT0

da[
  !is.na(PERSID) & WEIGHTFLG == 1,
  .(prop1 = sum(SPLNRWT0)),
  keyby = "NRBAVAR1"
][, prop1 := round(100 * prop.table(prop1), 2)][]



# - Cases with missing values for an analysis variable were excluded from the analysis for that variable.


## Analysis 1 - Wgting Adjustments

tab_analysis_1 <- openxlsx2::read_xlsx(
  file = "analyses/CY2_Extended_NRBA_Analyses_(without scores)_LVA.xlsx",
  sheet = 1,
  start_row = 4,
  skip_empty_rows = TRUE
) |> setDT()

### Data

vars_analysis_1 <- unique(tab_analysis_1$VARIABLE)
all(vars_analysis_1 %in% names(da))

da[, map(.SD, class), .SDcols = vars_analysis_1]
da[, c(vars_analysis_1) := map(.SD, as.factor), .SDcols = vars_analysis_1]
da[, map(.SD, class), .SDcols = vars_analysis_1]


# Eligible SPs
da_allSP <- da[!is.na(PERSID) & !DISP_CIBQ %in% c(18, 25)]

# Respondents
da_resp <- da[!is.na(PERSID) & WEIGHTFLG == 1]


### Designs

grep("SPBWT[1-n]+", names(da_allSP), value = TRUE) |> length() == 80
grep("SPLNRWT[1-n]+", names(da_allSP), value = TRUE) |> length() == 80

da_allSP_design <- svrepdesign(
  data = da_allSP,
  weights = ~SPBWT0,
  repweights = "SPBWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

da_R_design <- svrepdesign(
  data = da_resp,
  weights = ~SPBWT0,
  repweights = "SPBWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

da_NR_adj_design <- svrepdesign(
  data = da_resp,
  weights = ~SPLNRWT0,
  repweights = "SPLNRWT[1-n]+",
  type = "Fay",
  rho = 0.3
)


analysis_1 <- function(x, digits = 4, alpha = 0.05) {
  
  if (
    !is.factor(da_allSP[[x]]) | !is.factor(da_resp[[x]])
  ) stop("x should be a factor")
  
  tab_t_test_1 <- t_test_of_weight_adjustment(
    orig_design = da_allSP_design,
    updated_design = da_R_design,
    y_vars = x
  ) |> setDT()
  
  tab_t_test_2 <- t_test_of_weight_adjustment(
    orig_design = da_allSP_design,
    updated_design = da_NR_adj_design,
    y_vars = x
  ) |> setDT()

  tab_t_test_1 <- tab_t_test_1[, .(
    VARIABLE = outcome,
    VALUE = outcome_category,
    `Base Weight All Eligible SPs (1)` = round(Original_mean * 100, digits = digits),
    `Base Weight Respondents (2)` = round(Adjusted_mean * 100, digits = digits),
    `PROBF1 (1) v (2)` = round(p_value, digits = digits)
  )]
  
  tab_t_test_2 <- tab_t_test_2[, .(
    VARIABLE = outcome,
    VALUE = outcome_category,
    `NR Adj Weight Respondents (3)` = round(Adjusted_mean * 100, digits = digits),
    `PROBF1 (1) v (3)` = round(p_value, digits = digits)
  )]
  
  tab_t_test <- merge(
    x = tab_t_test_1,
    y = tab_t_test_2,
    by = c("VARIABLE", "VALUE")
  )
  
  tab_t_test[, `Bonferroni Alpha` := alpha / .N]
  
  tab_t_test[, VALUE := as.numeric(VALUE)]
  
  setcolorder(
    x = tab_t_test,
    neworder = c(
      "VARIABLE",
      "VALUE",
      "Base Weight All Eligible SPs (1)",
      "Base Weight Respondents (2)",
      "NR Adj Weight Respondents (3)"
    )
  )
  
  return(
    tab_t_test[]
  )

}

# Cashing of the function results
analysis_1 <- memoise::memoise(analysis_1)

vars_analysis_1

analysis_1("NRBAVAR1")
analysis_1("NRBAVAR2")

# is.factor(da[["NRBAVAR1"]])

tab_analysis_1_test <- map(
  .x = x,
  .f = analysis_1
) |> rbindlist()

tab_analysis_1_melt <- melt.data.table(
  data = tab_analysis_1,
  id.vars = c("VARIABLE", "LABEL", "VALUE")
)

tab_analysis_1_test_melt <- melt.data.table(
  data = tab_analysis_1_test,
  id.vars = c("VARIABLE", "VALUE")
)

tab_analysis_1_melt[, class(VALUE)]
tab_analysis_1_test_melt[, class(VALUE)]

tab_analysis_1_test_diff <- merge(
  x = tab_analysis_1_melt,
  y = tab_analysis_1_test_melt,
  by = c("VARIABLE", "VALUE", "variable"),
  suffixes = c(".orig", ".test")
)

setorder(tab_analysis_1_test_diff, VARIABLE, variable, VALUE)

tab_analysis_1_test_diff

tab_analysis_1_test_diff[is.na(value.orig) | is.na(value.test)]
tab_analysis_1_test_diff[is.na(value.orig), value.orig := 0]
tab_analysis_1_test_diff[is.na(value.test), value.test := 0]

tab_analysis_1_test_diff[
  ,
  all.equal(value.orig, value.test) |> as.character(),
  keyby = .(variable)
]

tab_analysis_1_test_diff[, diff := abs(value.orig - value.test)]

tab_analysis_1_test_diff[
  order(-diff),
  .(VARIABLE, VALUE, variable, value.orig, value.test)
] |> first(10)

tab_analysis_1_test_diff[value.orig < 1][
  order(-diff),
  .(VARIABLE, VALUE, variable, value.orig, value.test)
] |> first(10)

tab_analysis_1


# Significant differences are those with PROBF1 < Bonferroni Alpha. Bonferroni Alpha is calculated as alpha(0.05)/(number of categories).

tab_analysis_1[, .N, keyby = .(sign = `PROBF1 (1) v (2)` < `Bonferroni Alpha`)]
tab_analysis_1_test[, .N, keyby = .(sign = `PROBF1 (1) v (2)` < `Bonferroni Alpha`)]

tab_analysis_1[, .N, keyby = .(sign = `PROBF1 (1) v (3)` < `Bonferroni Alpha`)]
tab_analysis_1_test[, .N, keyby = .(sign = `PROBF1 (1) v (3)` < `Bonferroni Alpha`)]

# Significant differences between the base-weighted estimates and all eligible SPs existed for all variables in the analysis
# except Number of declared residents aged 16-65 and Indicator if a child (aged 0-14) is in a household.

tab_analysis_1[
  ,
  mean(`PROBF1 (1) v (2)` < `Bonferroni Alpha`),
  keyby = .(VARIABLE, LABEL)
][order(V1)][V1 == 0]

tab_analysis_1_test[
  ,
  mean(`PROBF1 (1) v (2)` < `Bonferroni Alpha`),
  keyby = .(VARIABLE)
][order(V1)][V1 == 0]

# Correct

# In general, the differences in the estimates are smaller after the nonresponse adjustment, indicating that the BQ nonresponse adjustment was effective in reducing the nonresponse bias in these estimates.

tab_analysis_1[
  is.na(`Base Weight Respondents (2)`),
  `Base Weight Respondents (2)` := 0
]
tab_analysis_1[
  is.na(`NR Adj Weight Respondents (3)`),
  `NR Adj Weight Respondents (3)` := 0
]

tab_analysis_1[
  ,
  diff1 := abs(`Base Weight All Eligible SPs (1)` - `Base Weight Respondents (2)`),
]

tab_analysis_1[
  ,
  diff2 := abs(`Base Weight All Eligible SPs (1)` - `NR Adj Weight Respondents (3)`),
]

tab_analysis_1[, map(.SD, mean), .SDcols = patterns("diff")]
tab_analysis_1[
  ,
  map(.SD, mean), .SDcols = patterns("diff"),
  keyby = .(VARIABLE)
][diff2 > diff1]


tab_analysis_1_test[
  ,
  diff1 := abs(`Base Weight All Eligible SPs (1)` - `Base Weight Respondents (2)`),
]

tab_analysis_1_test[
  ,
  diff2 := abs(`Base Weight All Eligible SPs (1)` - `NR Adj Weight Respondents (3)`),
]

tab_analysis_1_test[, map(.SD, mean), .SDcols = patterns("diff")]
tab_analysis_1_test[
  ,
  map(.SD, mean), .SDcols = patterns("diff"),
  keyby = .(VARIABLE)
][diff2 > diff1]

# Correct



# Significant differences remain in
# Household size from Screener,
# Share of one person households by JURISDICTION and cities,
# Unemployment level in age group 15-64 by JURISDICTION and cities.

tab_analysis_1[, .N, keyby = .(sign = `PROBF1 (1) v (3)` < `Bonferroni Alpha`)]
tab_analysis_1_test[, .N, keyby = .(sign = `PROBF1 (1) v (3)` < `Bonferroni Alpha`)]

tab_analysis_1[
  `PROBF1 (1) v (3)` < `Bonferroni Alpha`,
  .(VARIABLE, LABEL, `NR Adj Weight Respondents (3)`)
][order(-`NR Adj Weight Respondents (3)`)]

tab_analysis_1[
  `PROBF1 (1) v (3)` < `Bonferroni Alpha`,
  .(VARIABLE, `NR Adj Weight Respondents (3)`)
][order(-`NR Adj Weight Respondents (3)`)]

tab_analysis_1_test[
  `PROBF1 (1) v (3)` < `Bonferroni Alpha`,
  .(VARIABLE, `NR Adj Weight Respondents (3)`)
][order(-`NR Adj Weight Respondents (3)`)]


# Significant differences in very small categories (<1% of total) of
# 
# Share of population aged 15 and over with upper secondary education or higher (ISCED 3-8) by cities, towns, rural territories and neighbourhoods,
# Share of owner occupied dwellings by JURISDICTION-2021 and neighbourhoods,
# Share of occupied dwellings by JURISDICTION-2021 and neighbourhoods,
# Share of citizens of Latvia by cities, towns, rural territories and neighbourhoods
# 
# should be interpreted with caution.
# 
# Working status, age, education, and region were used in calibration,
# which would have addressed the remaining bias in these variables.

# Correct



da_finalwt_design <- svrepdesign(
  data = da_resp,
  weights = ~SPFWT0,
  repweights = "SPFWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

prop.table(dat_ext_est$NRBAVAR8$EXTEST)

dat_ext_est$NRBAVAR8$NRBAVAR8
prop.table(dat_ext_est[["NRBAVAR8"]]$EXTEST)

ext_ests <- map(
  .x = dat_ext_est,
  .f = \(x) {
    y <- x$EXTEST / sum(x$EXTEST)
    names(y) <- x[[1]]
    return(y)
  }
)
ext_ests

ext_std_errors <- map(
  .x = dat_ext_est,
  .f = \(x) {
    y <- x$EXTSE / sum(x$EXTEST)
    names(y) <- x[[1]]
    return(y)
  }
)
ext_std_errors

t_test_vs_external_estimate(
  survey_design = da_finalwt_design,
  y_var = "NRBAVAR8",
  ext_ests = ext_ests[["NRBAVAR8"]],
  ext_std_errors = ext_std_errors[["NRBAVAR8"]]
)

da_resp[, .N]
da_resp[, .N, keyby = "NRBAVAR8"]

