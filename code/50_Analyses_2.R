# Extended NRBA Analyses (without scores) LVA

# ANALYSIS 2: Comparison to External Estimates

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


### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_nrba)


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
    ) |> setDT()
  }
)

length(dat_ext_est)
names(dat_ext_est)


## Combine
key(dat_sdif)
key(dat_wif)
key(dat_nrba)

names(dat_wif)

da <- Reduce(f = merge, x = list(dat_sdif, dat_wif, dat_nrba))

setkeyv(da, key(dat_sdif)) |> setcolorder()
key(da)
names(da) |> first(10)


# Age groups
grep("CI_AGE", names(da), value = TRUE)
da[, range(CI_AGE, na.rm = TRUE)]
da[, CI_AGE_CAT := trunc((CI_AGE - min(CI_AGE, na.rm = TRUE)) / 5 + 1)]
da[!is.na(CI_AGE), as.list(range(CI_AGE)), keyby = .(CI_AGE_CAT)]




# Variables as factors
vars_analysis <- names(dat_ext_est)
all(vars_analysis %in% names(da))

da[, map(.SD, class), .SDcols = vars_analysis]
da[, c(vars_analysis) := map(.SD, as.factor), .SDcols = vars_analysis]
da[, map(.SD, class), .SDcols = vars_analysis]




# Respondents
da_resp <- da[!is.na(PERSID) & WEIGHTFLG == 1]


# Design
da_finalwt_design <- svrepdesign(
  data = da_resp,
  weights = ~SPFWT0,
  repweights = "SPFWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

da_resp[
  ,
  .(`Unweighted FREQ` = .N,
    `Weighted FREQ` = sum(SPFWT0)),
  keyby = .(NRBAVAR8)
][, PERCENT := (prop.table(`Weighted FREQ`) * 100) |> round(2)][]


da_resp[, sum(SPFWT0)]

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

svymean(
  x = ~NRBAVAR8,
  design = da_finalwt_design
) |> as.data.table()

t_test_vs_external_estimate(
  survey_design = da_finalwt_design,
  y_var = "NRBAVAR8",
  ext_ests = ext_ests[["NRBAVAR8"]]
)

t_test_vs_external_estimate(
  survey_design = da_finalwt_design,
  y_var = "NRBAVAR8",
  ext_ests = ext_ests[["NRBAVAR8"]],
  ext_std_errors = ext_std_errors[["NRBAVAR8"]]
) |> as.data.table()

da_resp[, .(NRBAVAR8)]

# qnorm(p = 1 - 0.05 / 2)
# survey::degf(da_finalwt_design) - 1
# qt(p = 1 - 0.05 / 2, df = survey::degf(da_finalwt_design) - 1)
# qt(p = 1 - 0.05 / 2, df = Inf)

# qt(p = 1 - 0.05 / 2, df = 1000)
# 
# df <- 1000
# x <- qt(p = 1 - 0.05 / 2, df = df)
# round(x, 6)
# while(x > 1.9623) {
#   df <- df + 1L
#   x <- qt(p = 1 - 0.05 / 2, df = df)
# }

analysis_2 <- function(x, design = da_finalwt_design, alpha = 0.05) {
  
  tab1 <- da_resp[
    ,
    .(VARIABLE = x,
      `Unweighted FREQ` = as.numeric(.N),
      `Weighted FREQ` = sum(SPFWT0)),
    keyby = .(VALUE = get(x))
  ]
  
  tab2 <- svymean(
    x = ~get(x),
    design = design
  ) |> as.data.table()
  
  tab2[, c(names(tab2)) := map(.SD, \(x) x * 100)]
  setnames(tab2, c("PERCENT", "STDERR"))
  
  z_alpha <- qt(p = 1 - alpha / 2, df = survey::degf(design) - 1)
  tab2[, LOWERCL := PERCENT - STDERR * z_alpha]
  tab2[, UPPERCL := PERCENT + STDERR * z_alpha]
  
  tab3 <- data.table(
    TESTPCT = dat_ext_est[[x]][, EXTEST / sum(EXTEST) * 100],
    TESTSE  = dat_ext_est[[x]][, EXTSE  / sum(EXTEST) * 100]
  )
  
  z_alpha <- qt(p = 1 - alpha / 2, df = 1000)
  tab3[, TESTLOCL := TESTPCT - TESTSE * z_alpha]
  tab3[, TESTUPLC := TESTPCT + TESTSE * z_alpha]
  
  tab4 <- t_test_vs_external_estimate(
    survey_design = design,
    y_var = x,
    ext_ests = ext_ests[[x]],
    ext_std_errors = ext_std_errors[[x]]
  ) |> setDT()
  
  tab4 <- tab4[, .(
    DIFF = difference * 100,
    TVAL = t_statistic,
    PROB_T = p_value
  )]
  
  tab <- cbind(tab1, tab2, tab3, tab4)
  setcolorder(tab, c("VARIABLE", "VALUE"))
  
  return(tab)
  
}

analysis_2("NRBAVAR8")
analysis_2("NRBAVAR9")
analysis_2("NRBAVAR10")
analysis_2("NRBAVAR11")


tab_analysis_2_test <- map(
  .x = names(dat_ext_est),
  .f = analysis_2
) |> rbindlist() |> melt.data.table(
  id.vars = c("VARIABLE", "VALUE")
)

tab_analysis_2_test[, VALUE := as.integer(VALUE)]


tab_analysis_2_orig <- openxlsx2::read_xlsx(
  file = "analyses/CY2_Extended_NRBA_Analyses_(without scores)_LVA.xlsx",
  sheet = 2,
  start_row = 3,
  cols = 1:16,
  skip_empty_rows = TRUE
) |> setDT() |> melt.data.table(
  id.vars = c("VARIABLE", "LABEL", "VALUE"),
  na.rm = TRUE
)

tab_analysis_2_orig[, VALUE := as.integer(VALUE)]


tab_analysis_2 <- merge(
  x = tab_analysis_2_orig,
  y = tab_analysis_2_test,
  by = c("VARIABLE", "VALUE", "variable"),
  suffixes = c(".orig", ".test")
)

tab_analysis_2[, VARIABLE := factor(
  x = VARIABLE,
  levels = names(ext_ests),
  labels = names(ext_ests)
)]

setcolorder(tab_analysis_2, c("VARIABLE", "LABEL", "VALUE"))
setorder(tab_analysis_2, VARIABLE, VALUE)

tab_analysis_2[
  ,
  all.equal(value.orig, value.test) |> as.character(),
  keyby = .(variable)
]

tab_analysis_2[
  variable == "PROB_T",
  .N,
  keyby = .(value.orig < 0.05, value.test < 0.05)
]
tab_analysis_2[
  variable == "PROB_T",
  .N,
  keyby = .(value.orig < 0.05, value.test < 0.05, VARIABLE)
]
tab_analysis_2[variable == "PROB_T" & value.orig < 0.05]
tab_analysis_2[variable == "PROB_T" & value.test < 0.05]

tab_analysis_2[, diff := value.orig - value.test]

# Save

tab_analysis_2[, map(.SD, class)]
# tab_analysis_1_test_diff[, VALUE := factor(VALUE)]

setorder(tab_analysis_2, VARIABLE, variable, VALUE)

openxlsx::write.xlsx(
  x = tab_analysis_2,
  file = "analyses/LVA_NRBA_ANALYSIS_2_test.xlsx",
  firstRow = TRUE,
  colWidths = 20
)



# Plot

tab_analysis_2_plot <- melt.data.table(
  data = tab_analysis_2,
  id.vars = c("VARIABLE", "LABEL", "VALUE", "variable"),
  variable.name = "origin"
)

ggplot(
  data = tab_analysis_2_plot[grep("PERCENT|TESTPCT", variable)],
  mapping = aes(
    x = as.integer(VALUE) |> factor(),
    y = value,
    fill = variable
  )
) +
  geom_col(position = "dodge") +
  facet_wrap(facets = vars(VARIABLE), scales = "free") +
  theme_bw()



# The Rao-Scott Chi-square test

chisq_test_vs_external_estimate(
  survey_design = da_finalwt_design,
  y_var = "NRBAVAR8",
  ext_ests = ext_ests[["NRBAVAR8"]]
)

map(
  .x = names(ext_ests),
  .f = \(x) {
    chisq_test_vs_external_estimate(
      survey_design = da_finalwt_design,
      y_var = x,
      ext_ests = ext_ests[[x]]
    )
  }
) |> rbindlist()
