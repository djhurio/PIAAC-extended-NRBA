# Extended NRBA Analyses (without scores) LVA

# ANALYSIS 1: Estimates Before and After Weighting

## Options
options(max.print = 10e3)
options(openxlsx.numFmt = "0.0000")
options(survey.replicates.mse = TRUE)

## Reset
rm(list = ls())
gc()

## Libs
library(purrr)
library(data.table)
library(survey)
library(nrba) # For running NR bias analysis
library(ggplot2)

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


### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_nrba)


## Combine
key(dat_sdif)
key(dat_wif)
key(dat_nrba)
key(dat_sp_repwt)

names(dat_wif)
names(dat_sp_repwt)

da <- Reduce(f = merge, x = list(dat_sdif, dat_wif, dat_nrba)) |>
  merge(y = dat_sp_repwt, by = "PERSID")

setkeyv(da, key(dat_sdif)) |> setcolorder()
key(da)
names(da) |> first(10)


# Age groups
grep("CI_AGE", names(da), value = TRUE)
da[, range(CI_AGE)]
da[, CI_AGE_CAT := trunc((CI_AGE - min(CI_AGE)) / 5 + 1)]
da[, as.list(range(CI_AGE)), keyby = .(CI_AGE_CAT)]


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

sheet.names <- openxlsx2::wb_load(file = "analyses/LVA_NRBA_ANALYSIS_1.xlsx") |>
  openxlsx2::wb_get_sheet_names()

tab_analysis_1_orig <- map(
  .x = sheet.names,
  .f = \(x) {
    openxlsx2::read_xlsx(
      file = "analyses/LVA_NRBA_ANALYSIS_1.xlsx",
      sheet = x
    )
  }
) |> rbindlist(idcol = "test")

tab_analysis_1_orig[, c("LABEL", "FMTVAL") := NULL]
tab_analysis_1_orig[, VALUE := as.integer(VALUE)]


### Data

vars_analysis_1 <- unique(tab_analysis_1_orig$TABLE)
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


# x <- vars_analysis_1[1]
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
  
  tab_t_test_1[, test := sheet.names[1]]
  tab_t_test_2[, test := sheet.names[2]]
  
  tab_t_test <- rbindlist(
    l = list(tab_t_test_1, tab_t_test_2)
  )
  
  tab_t_test <- tab_t_test[, .(
    test,
    TABLE      = outcome,
    VALUE      = as.integer(outcome_category),
    PERCENT1   = round(100 * Original_mean, digits = digits),
    STDERROR1  = round(100 * Original_se,   digits = digits),
    PERCENT2   = round(100 * Adjusted_mean, digits = digits),
    STDERROR2  = round(100 * Adjusted_se,   digits = digits),
    ESTDIFF    = round(100 * difference,    digits = digits),
    STDERRDIFF = round(100 * std_error,     digits = digits),
    PROBF      = round(p_value,             digits = digits)
  )]
  
  tab_t_test[, BONF_ALPHA := alpha / .N, by = .(test)]
  
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

t1 <- Sys.time()
tab_analysis_1_test <- map(
  .x = vars_analysis_1,
  .f = analysis_1
) |> rbindlist()
t2 <- Sys.time()
t2 - t1
# Time difference of 5.030634 mins

rm(t1, t2)

tab_analysis_1_orig_melt <- melt.data.table(
  data = tab_analysis_1_orig,
  id.vars = c("test", "TABLE", "VALUE")
)

tab_analysis_1_test_melt <- melt.data.table(
  data = tab_analysis_1_test,
  id.vars = c("test", "TABLE", "VALUE")
)

# tab_analysis_1_orig_melt[, class(VALUE)]
# tab_analysis_1_test_melt[, class(VALUE)]

# key(tab_analysis_1_orig_melt)
# key(tab_analysis_1_test_melt)

tab_analysis_1_test_diff <- merge(
  x = tab_analysis_1_orig_melt,
  y = tab_analysis_1_test_melt,
  by = c("test", "TABLE", "VALUE", "variable"),
  suffixes = c(".orig", ".test")
)

tab_analysis_1_test_diff[, test := factor(x = test, levels = sheet.names)]
tab_analysis_1_test_diff[, TABLE := factor(x = TABLE, levels = vars_analysis_1)]

setorder(tab_analysis_1_test_diff, test, TABLE, variable, VALUE)

tab_analysis_1_test_diff

tab_analysis_1_test_diff[is.na(value.orig) | is.na(value.test)]
# tab_analysis_1_test_diff[is.na(value.orig), value.orig := 0]
# tab_analysis_1_test_diff[is.na(value.test), value.test := 0]

tab_analysis_1_test_diff[
  ,
  all.equal(value.orig, value.test) |> as.character(),
  keyby = .(variable, test)
]

tab_analysis_1_test_diff[, diff := value.orig - value.test]

tab_analysis_1_test_diff[
  order(-abs(diff)),
  .(test, TABLE, VALUE, variable, value.orig, value.test)
] |> first(10)

tab_analysis_1_test_diff[value.orig < 1][
  order(-abs(diff)),
  .(test, TABLE, VALUE, variable, value.orig, value.test)
] |> first(10)



# Save

tab_analysis_1_test_diff[, map(.SD, class)]
# tab_analysis_1_test_diff[, VALUE := factor(VALUE)]

openxlsx::write.xlsx(
  x = tab_analysis_1_test_diff,
  file = "analyses/LVA_NRBA_ANALYSIS_1_test.xlsx",
  firstRow = TRUE,
  colWidths = 20
)



# Significant differences are those with PROBF1 < Bonferroni Alpha. Bonferroni Alpha is calculated as alpha(0.05)/(number of categories).

tab_analysis_1_orig[, .N, keyby = .(test, sign = PROBF < BONF_ALPHA)]
tab_analysis_1_test[, .N, keyby = .(test, sign = PROBF < BONF_ALPHA)]


# Significant differences between the base-weighted estimates and all eligible SPs existed for all variables in the analysis
# except Number of declared residents aged 16-65 and Indicator if a child (aged 0-14) is in a household.

# DUVAR_ALL2
# Number of declared residents aged 16-65
# 
# DUVAR_SCRRESP2
# Indicator if a child (aged 0-14) is in a household

tab_analysis_1_orig[
  test == "BaseAll_v_BaseResp",
  mean(PROBF < BONF_ALPHA),
  keyby = .(test, TABLE)
][order(V1)][V1 == 0][, TABLE %in% c("DUVAR_ALL2", "DUVAR_SCRRESP2")]

tab_analysis_1_test[
  test == "BaseAll_v_BaseResp",
  mean(PROBF < BONF_ALPHA),
  keyby = .(test, TABLE)
][order(V1)][V1 == 0][, TABLE %in% c("DUVAR_ALL2", "DUVAR_SCRRESP2")]

# Correct



# In general, the differences in the estimates are smaller after the nonresponse adjustment, indicating that the BQ nonresponse adjustment was effective in reducing the nonresponse bias in these estimates.

tab_analysis_1_orig[is.na(PERCENT2), PERCENT2 := 0]
tab_analysis_1_orig[, diff := abs(PERCENT1 - PERCENT2)]
tab_analysis_1_orig[, map(.SD, mean), .SDcols = patterns("diff"), keyby = .(test)]

tab_analysis_1_test[is.na(PERCENT2), PERCENT2 := 0]
tab_analysis_1_test[, diff := abs(PERCENT1 - PERCENT2)]
tab_analysis_1_test[, map(.SD, mean), .SDcols = patterns("diff"), keyby = .(test)]

# Correct



# Significant differences remain in
# DUVAR_SCRRESP1: Household size from Screener,
# DUVAR_SCRRESP3: Share of one person households by JURISDICTION and cities,
# AREAVAR5: Unemployment level in age group 15-64 by JURISDICTION and cities.

tab_analysis_1_orig[, .N, keyby = .(test, sign = PROBF < BONF_ALPHA)]
tab_analysis_1_test[, .N, keyby = .(test, sign = PROBF < BONF_ALPHA)]

tab_analysis_1_orig[
  test == "BaseAll_v_NRAdjResp" & PROBF < BONF_ALPHA & PERCENT2 > 1,
  .(TABLE)
] |> unique()

tab_analysis_1_test[
  test == "BaseAll_v_NRAdjResp" & PROBF < BONF_ALPHA & PERCENT2 > 1,
  .(TABLE)
] |> unique()


# Significant differences in very small categories (<1% of total) of
# 
# PERSVAR2: Share of population aged 15 and over with upper secondary education or higher (ISCED 3-8) by cities, towns, rural territories and neighbourhoods,
# PERSVAR3: Share of owner occupied dwellings by JURISDICTION-2021 and neighbourhoods,
# PERSVAR4: Share of occupied dwellings by JURISDICTION-2021 and neighbourhoods,
# PERSVAR5: Share of citizens of Latvia by cities, towns, rural territories and neighbourhoods
# 
# should be interpreted with caution.
# 
# Working status, age, education, and region were used in calibration,
# which would have addressed the remaining bias in these variables.

tab_analysis_1_orig[
  test == "BaseAll_v_NRAdjResp" & PROBF < BONF_ALPHA & PERCENT2 < 1,
  .(TABLE)
] |> unique()

tab_analysis_1_test[
  test == "BaseAll_v_NRAdjResp" & PROBF < BONF_ALPHA & PERCENT2 < 1,
  .(TABLE)
] |> unique()


# Correct
