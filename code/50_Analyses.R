# Extended NRBA Analyses (without scores) LVA

## Options
options(max.print = 10e3)
getOption("max.print")

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
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID"))

### WIF
dat_wif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID"))

# ### SPRWT0
# dat_sprwt <- haven::read_sas(
#   data_file = "../PIAAC-data-2023/data-weights/sprwt.sas7bdat"
# ) |> setDT()

### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID"))

intersect(names(dat_sdif), names(dat_wif)) |> rev()


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
da <- Reduce(f = merge, x = list(dat_sdif, dat_wif, dat_nrba))

nrbavarnames <- grep("NRBAVAR", names(da), value = TRUE)

da[, map(.SD, class), .SDcols = nrbavarnames]
da[, c(nrbavarnames) := map(.SD, as.factor), .SDcols = nrbavarnames]
da[, map(.SD, class), .SDcols = nrbavarnames]


# ANALYSIS 1: Estimates Before and After Weighting

# Notes: 
# - A t-test is used for this analysis. PROBF1 is the resulting p-value from the t-test.
# - Significant differences are those with PROBF1 < Bonferroni Alpha. Bonferroni Alpha is calculated as alpha(0.05)/(number of categories).
# - "Eligible SPs" are all sample persons, excluding ineligible cases (those with DISP_CIBQ=18 or 25).

da[!is.na(PERSID), .N, keyby = .(DISP_CIBQ)]
da[!is.na(PERSID), .N]
da[!is.na(PERSID) & !DISP_CIBQ %in% c(18, 25), .N]

da[
  !is.na(PERSID) & !DISP_CIBQ %in% c(18, 25),
  .(prop1 = sum(SPBWT0)),
  keyby = "NRBAVAR1"
][, prop1 := round(100 * prop.table(prop1), 2)][]


# - "Respondents" are sampled persons with WEIGHTFLG=1 (that is, with DISP_CIBQ=1 or (DISP_CIBQ=7 and DISP_DS=1)).

da[!is.na(PERSID) & WEIGHTFLG == 1, .N]
da[!is.na(PERSID) & (DISP_CIBQ == 1 | DISP_CIBQ == 7 & DISP_DS == 1), .N]

da[
  !is.na(PERSID) & WEIGHTFLG == 1,
  .(prop1 = sum(SPBWT0)),
  keyby = "NRBAVAR1"
][, prop1 := round(100 * prop.table(prop1), 2)][]

# - Cases with missing values for an analysis variable were excluded from the analysis for that variable.

# Eligible SPs
da_allSP <- da[!is.na(PERSID) & !DISP_CIBQ %in% c(18, 25)]

# Respondents
da_resp <- da[!is.na(PERSID) & WEIGHTFLG == 1]

names(dat_wif)
grep("SPBWT[1-n]+", names(da_allSP), value = TRUE)
grep("SPFWT[1-n]+", names(da_allSP), value = TRUE)

# da_allSP_design <- svrepdesign(
#   data = da_allSP,
#   weights = ~SPBWT0,
#   repweights = "SPBWT[1-n]+",
#   type = "Fay",
#   rho = 0.3
# )


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

