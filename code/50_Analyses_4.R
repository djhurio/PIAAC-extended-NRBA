# 3.2.2 Comparison of estimates from alternative weighting adjustments

## Options
options(max.print = 10e3)
options(openxlsx.numFmt = "0.0000")
options(survey.replicates.mse = TRUE)

## Reset
rm(list = ls())
gc()

# load essential packages. Please download and install them if they are not available.
library(survey)
library(haven) # For reading in sas dataset
library(tidyverse) # For data manipulation
library(nrba) # For running NR bias analysis
library(srvyr) # For dplyr-style syntax with survey designs
library(reldist) # For calculating weighted quantile
library(Hmisc) # For weighted quantiles and means

library(purrr)
library(data.table)

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

### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_nrba)

### Survey data
dat_surv <- haven::read_spss(
  file = "../PIAAC-data-2023/data/Survey-Data/PRGLVAP2.sav",
  col_select = c("CNTRYID", "CASEID", "PERSID", matches("^PV(LIT|NUM|APS)1$"))
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_surv) |> first(10)


# replace with your data and format analysis variables as needed
da <- Reduce(
  f = merge,
  x = list(dat_sdif, dat_wif, dat_nrba, dat_surv)
)
key(da)
names(da) |> first(10)

da[, .N, keyby = .(DISP_CIBQ)]
da[, .N, keyby = .(NRBAVAR8)]

# CI_AGE_CAT
# Person 5-year age group (Case Initialization)
da[, .N, keyby = .(CI_AGE)]
da[, CI_AGE_CAT := (CI_AGE - 11) %/% 5]
da[, as.list(range(CI_AGE)), keyby = .(CI_AGE_CAT)]


# Specify population benchmarks
population_benchmarks <- openxlsx2::read_xlsx(
  file = "results/Alternative_Totals_LVA.xlsx"
) |> setDT() |> setnames(c("ALT_RAKEDIM", "ALT_RAKEDIM_BENCHMARK"))

# Add the population benchmarks as a variable in the data
da[, .N, keyby = .(WEIGHTFLG)]
da_w_benchmark <- merge(
  x = da[WEIGHTFLG == 1L],
  y = population_benchmarks,
  by = "ALT_RAKEDIM"
)

# da_w_benchmark[, .N, keyby = .(NRBAVAR8, NRBAVAR8_f)]

# Create a survey design object
da_design_w_benchmark <- svrepdesign(
  data = da_w_benchmark,
  weights = ~SPFWT0, 
  repweights = "SPFWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

# Subset data to only include respondents
rep_svy_respondents <- subset(
  x = da_design_w_benchmark,
  subset = WEIGHTFLG == 1
)

# Apply raking adjustment 
raked_rep_svy_respondents <- rake_to_benchmarks(
  survey_design = rep_svy_respondents,
  group_vars = c("ALT_RAKEDIM"),
  group_benchmark_vars = c("ALT_RAKEDIM_BENCHMARK")
)

# Compare estimates from respondents in original vs. adjusted design 
t_test_of_weight_adjustment(
  orig_design = rep_svy_respondents,
  updated_design = raked_rep_svy_respondents,
  y_vars = c("PVLIT1", "PVNUM1", "PVAPS1")
)

t_test_of_weight_adjustment(
  orig_design = subset(rep_svy_respondents, NRBAVAR8 == 1),
  updated_design = subset(raked_rep_svy_respondents, NRBAVAR8 == 1),
  y_vars = c("PVLIT1", "PVNUM1", "PVAPS1")
)

t_test_of_weight_adjustment(
  orig_design = subset(rep_svy_respondents, NRBAVAR8 == 2),
  updated_design = subset(raked_rep_svy_respondents, NRBAVAR8 == 2),
  y_vars = c("PVLIT1", "PVNUM1", "PVAPS1")
) |> class()


# Function for one value
analysis_4_single_value <- function(
    orig_design = rep_svy_respondents,
    updated_design = raked_rep_svy_respondents,
    y_vars   = c("PVLIT1", "PVNUM1", "PVAPS1"),
    variable = "OVERALL",
    label    = variable,
    value    = 1L,
    digits   = 4
) {
  
  if (variable == "OVERALL") {
    tab_t_test <- t_test_of_weight_adjustment(
      orig_design = orig_design,
      updated_design = updated_design,
      y_vars = y_vars
    )
  } else {
    stopifnot(variable %in% names(orig_design$variables))
    tab_t_test <- t_test_of_weight_adjustment(
      orig_design = subset(orig_design, get(variable) == value),
      updated_design = subset(updated_design, get(variable) == value),
      y_vars = y_vars
    )
  }
  
  setDT(tab_t_test)
  
  return(
    tab_t_test[, .(
      VARIABLE = variable,
      LABEL = label,
      VALUE = value,
      YVAR    = factor(outcome, levels = y_vars),
      MEAN1   = round(Original_mean, digits = digits),
      STDERR1 = round(Original_se,   digits = digits),
      MEAN2   = round(Adjusted_mean, digits = digits),
      STDERR2 = round(Adjusted_se,   digits = digits),
      ESTDIFF = difference,
      STDERRDIFF = round(std_error, 6),
      PROBF = round(p_value, digits = digits)
    )]
  )
  
}

analysis_4_single_value()
analysis_4_single_value(variable = "NRBAVAR8", label = "Labour status", value = 1L)
analysis_4_single_value(variable = "NRBAVAR12", value = 5L)
analysis_4_single_value(variable = "CI_AGE_CAT", value = 5L)


# Function for all values
analysis_4_all_values <- function(
    orig_design = rep_svy_respondents,
    updated_design = raked_rep_svy_respondents,
    y_vars   = c("PVLIT1", "PVNUM1", "PVAPS1"),
    variable = "OVERALL",
    label    = variable,
    digits   = 4,
    alpha    = 0.05
) {
  
  if (variable == "OVERALL") {
    values <- 1L
  } else {
    values <- sort(unique(orig_design$variables[[variable]]))
  }
  
  tab_t_test <- purrr::map(
    .x = values,
    .f = \(x) analysis_4_single_value(
      orig_design = orig_design,
      updated_design = updated_design,
      y_vars = y_vars,
      variable = variable,
      label = label,
      value = x,
      digits = digits
    )
  ) |> rbindlist()
  
  tab_t_test[, `Bonferroni Alpha` := alpha / length(values)]
  
  setorder(tab_t_test, YVAR, VALUE)
  
  return(tab_t_test[])
  
}

analysis_4_all_values()
analysis_4_all_values(variable = "NRBAVAR8", label = "Labour status")
analysis_4_all_values(
  variable = "NRBAVAR12",
  label = "Highest education level (B2_Q01LV)"
)
analysis_4_all_values(variable = "CI_AGE_CAT")

da[, .N, keyby = .(CI_AGE_CAT)]


# Run for all variables
tab_analysis_4_test <- map2(
  .x = c(
    "OVERALL",
    "NRBAVAR8",
    "NRBAVAR12",
    "NRBAVAR13",
    "CI_AGE_CAT",
    "CI_GENDER"
  ),
  .y = c(
    "OVERALL",
    "Labour status",
    "Highest education level (B2_Q01LV)",
    "Region (NUTS-3)",
    "Person 5-year age group (Case Initialization)",
    "Person gender (Case Initialization)"
  ),
  .f = \(x, y) analysis_4_all_values(
    orig_design = rep_svy_respondents,
    updated_design = raked_rep_svy_respondents,
    y_vars = c("PVLIT1", "PVNUM1", "PVAPS1"),
    variable = x,
    label = y
  ),
  .progress = TRUE
) |> rbindlist()

tab_analysis_4_test


# Read orig value computed by Consortium
tab_analysis_4_orig <- openxlsx2::read_xlsx(
  file = "Extended_NRBA_Analyses/CY2_Extended_NRBA_Analyses_(with scores)_LVA.xlsx",
  sheet = "Analysis 4 - Alt Control Tots",
  start_row = 4,
  skip_empty_rows = TRUE
) |> setDT()

tab_analysis_4_orig[, VARIABLE := factor(VARIABLE, unique(VARIABLE))]
tab_analysis_4_test[, VARIABLE := factor(VARIABLE, unique(VARIABLE))]
tab_analysis_4_orig[, YVAR := factor(YVAR, unique(YVAR))]





# Melt and merge orig with test
tab_analysis_4_orig_melt <- melt.data.table(
  data = tab_analysis_4_orig,
  id.vars = c("VARIABLE", "LABEL", "VALUE", "YVAR"),
  na.rm = TRUE
)

tab_analysis_4_test_melt <- melt.data.table(
  data = tab_analysis_4_test,
  id.vars = c("VARIABLE", "LABEL", "VALUE", "YVAR"),
  na.rm = TRUE
)

tab_analysis_4 <- merge(
  x = tab_analysis_4_orig_melt,
  y = tab_analysis_4_test_melt,
  by = c("VARIABLE", "LABEL", "VALUE", "YVAR", "variable"),
  suffixes = c(".orig", ".test"),
  sort = FALSE
)

tab_analysis_4[
  ,
  all.equal(value.orig, value.test) |> as.character(),
  keyby = .(variable)
]

tab_analysis_4[, diff := abs(value.orig - value.test)]

# The largest differences
tab_analysis_4[diff > 1e-3]


# Notes: 

# - A t-test is used for this analysis.
# PROBF is the resulting p-value from the t-test.
tab_analysis_4_orig[, summary(PROBF)]
tab_analysis_4_test[, summary(PROBF)]

# - Significant differences are those with PROBF < Bonferroni Alpha.
# Bonferroni Alpha is calculated as alpha(0.05)/(number of categories).
tab_analysis_4_orig[, signif := PROBF < `Bonferroni Alpha`]
tab_analysis_4_test[, signif := PROBF < `Bonferroni Alpha`]

tab_analysis_4_orig[
  ,
  .(n = .N, n_signif = sum(signif)),
  keyby = .(VARIABLE, LABEL)
]
tab_analysis_4_test[
  ,
  .(n = .N, n_signif = sum(signif)),
  keyby = .(VARIABLE, LABEL)
]

# - 'Notable' differences are significant differences for which
# the absolute value of ESTDIFF (i.e., MEAN1-MEAN2) is greater than
# the standard error of MEAN1 (STDERR1).
# Only 'notable' differences are mentioned here.
tab_analysis_4_orig[, notable := signif & abs(ESTDIFF) > STDERR1]
tab_analysis_4_test[, notable := signif & abs(ESTDIFF) > STDERR1]

tab_analysis_4_orig[
  ,
  .(n = .N, n_signif = sum(signif), n_notable = sum(notable)),
  keyby = .(VARIABLE, LABEL)
]
tab_analysis_4_test[
  ,
  .(n = .N, n_signif = sum(signif), n_notable = sum(notable)),
  keyby = .(VARIABLE, LABEL)
]

# For Latvia, final weights were recalibrated to population totals for gender by highest education level which came from the official education statistics.
# The original calibration used gender by age groups and two other unrelated raking dimensions.
# Also, the alternative control totals came from a different source. 
# Some differences between the mean proficiency scores calculated using final weights and the re-weighted mean proficiency scores were significant, with absolute differences ranging from 1.44 to 4.63.
tab_analysis_4_orig[(signif), summary(ESTDIFF) |> round(2)]
tab_analysis_4_test[(signif), summary(ESTDIFF) |> round(2)]
# OK

# Notable significant differences were observed in for the following variables:
# - overall and by gender for all three proficiency scores;
tab_analysis_4_orig[
  notable & grepl("OVERALL|CI_GENDER", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]
tab_analysis_4_test[
  notable & grepl("OVERALL|CI_GENDER", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]
# OK

# - NRBAVAR8 (labor force status): for category 1 (employed) for all three proficiency scores;
tab_analysis_4_orig[
  notable & grepl("NRBAVAR8", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]
tab_analysis_4_test[
  notable & grepl("NRBAVAR8", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]

# - NRBAVAR13 (region): for all regions and all three proficiency scores
# except for
# regions 4 (Kurzeme) and 6 (Latgale) for PVLIT1,
# region 6 for PVNUM1, and
# regions 1 (Riga), 3 (Vidzeme), 4, and 6 for PVAPS1.
tab_analysis_4_orig[
  !notable & grepl("NRBAVAR13", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]
tab_analysis_4_test[
  !notable & grepl("NRBAVAR13", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, MEAN1, MEAN2, ESTDIFF, STDERRDIFF)
]
# OK

# - CI_AGE_CAT (age categories):
# for ages 36-45 and 51-55 for PVLIT1,
# for ages 31-45 and 51-55 for PVNUM1,
# and ages 36-45 for PVAPS1; 
tab_analysis_4_orig_age <- tab_analysis_4_orig[VARIABLE == "CI_AGE_CAT"]
tab_analysis_4_test_age <- tab_analysis_4_test[VARIABLE == "CI_AGE_CAT"]

tab_analysis_4_orig_age[, VALUE_LABEL := factor(VALUE, labels = c(
  "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65"
))]
tab_analysis_4_test_age[, VALUE_LABEL := factor(VALUE, labels = c(
  "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65"
))]

tab_analysis_4_orig_age[, .N, keyby = .(VARIABLE, VALUE, VALUE_LABEL)]
tab_analysis_4_test_age[, .N, keyby = .(VARIABLE, VALUE, VALUE_LABEL)]

tab_analysis_4_orig_age[
  notable & grepl("CI_AGE_CAT", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, VALUE_LABEL, MEAN1, MEAN2, ESTDIFF, STDERR1)
]
tab_analysis_4_test_age[
  notable & grepl("CI_AGE_CAT", VARIABLE),
  .(VARIABLE, LABEL, YVAR, VALUE, VALUE_LABEL, MEAN1, MEAN2, ESTDIFF, STDERR1)
]
# OK

######
# OK #
######
