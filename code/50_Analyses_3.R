# ANALYSIS 5: BQ LRNR compared to other BQ NR

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
library(ggplot2)
library(glue)

## Params

alpha <- 0.05
z_alpha <- qnorm(1 - alpha / 2)

## Data & weights

### SDIF - final
dat_sdif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/SAS/SAS7BDAT/psdlvams.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_sdif)

dat_sdif[, .N, keyby = .(DISP_SCR)]

### WIF
dat_wif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_wif)

# ### SPRWT0
# dat_sprwt <- haven::read_sas(
#   data_file = "../PIAAC-data-2023/data-weights/sprwt.sas7bdat"
# ) |> setDT()

# # hh_repwt
# # HHUEWT: Screener unknown eligibility adjusted weight
# dat_hh_repwt <- haven::read_sas(
#   data_file = "analyses/data/qc_hh_repwt_lva.sas7bdat"
# ) |> setDT(key = "CASEID") |> setcolorder()
# names(dat_hh_repwt)
# # Remove variables overlapping with WIF
# x <- grep("WT0$", names(dat_hh_repwt), value = TRUE)
# dat_hh_repwt[, c(x) := NULL]
# rm(x)

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


## Combine
key(dat_sdif)
key(dat_wif)
key(dat_nrba)
# key(dat_hh_repwt)
key(dat_sp_repwt)

names(dat_wif)
# names(dat_hh_repwt)
# names(dat_sp_repwt)

da <- Reduce(f = merge, x = list(dat_sdif, dat_wif, dat_nrba)) |>
  merge(y = dat_sp_repwt, by = "PERSID")

setkeyv(da, key(dat_sdif)) |> setcolorder()
key(da)
names(da) |> first(10)


# Age groups
grep("CI_AGE", names(da), value = TRUE)
da[, range(CI_AGE, na.rm = TRUE)]
da[, CI_AGE_CAT := trunc((CI_AGE - min(CI_AGE, na.rm = TRUE)) / 5 + 1)]
da[!is.na(CI_AGE), as.list(range(CI_AGE)), keyby = .(CI_AGE_CAT)]



# Consorcium results
tab_analysis_3_orig <- openxlsx2::read_xlsx(
  file = "analyses/CY2_Extended_NRBA_Analyses_(without scores)_LVA.xlsx",
  sheet = 3,
  start_row = 4,
  skip_empty_cols = TRUE,
  skip_empty_rows = TRUE
) |> setDT()


vars_analysis_3 <- unique(tab_analysis_3_orig$VARIABLE)
all(vars_analysis_3 %in% names(da))

da[, map(.SD, class), .SDcols = vars_analysis_3]
da[, c(vars_analysis_3) := map(.SD, as.factor), .SDcols = vars_analysis_3]
da[, map(.SD, class), .SDcols = vars_analysis_3]


da[, comp.group := NA_integer_]
da[DISP_CIBQ %in% c(7, 8, 9) == 1, comp.group := 1L]
da[DISP_CIBQ %in% c(1, 7, 8, 9, 18, 25) == 0, comp.group := 2L]
da[, .N, keyby = .(comp.group)]

map(
  .x = vars_analysis_3,
  .f = \(x) da[!is.na(comp.group), .N, keyby = c("comp.group", x)]
)


# Eligible SPs
da[!is.na(PERSID), .N, keyby = .(DISP_CIBQ)]
da_allSP <- da[!is.na(PERSID)]


# Sampling design
da_allSP_design <- svrepdesign(
  data = da_allSP,
  weights = ~SPBWT0,
  repweights = "SPBWT[1-n]+",
  type = "Fay",
  rho = 0.3
)

# BQ Literacy-related NR
svymean(
  x = ~CI_AGE_CAT,
  design = subset(da_allSP_design, comp.group == 1L)
)

# other BQ NR
svymean(
  x = ~factor(CI_AGE_CAT),
  design = subset(da_allSP_design, comp.group == 2L)
)

analyses_3 <- function(x) {
  
  # BQ Literacy-related NR
  tab1 <- svymean(
    x = ~get(x),
    design = subset(da_allSP_design, comp.group == 1L)
  ) |> as.data.table()
  tab1[, comp.group := 1L]

  # other BQ NR
  tab2 <- svymean(
    x = ~get(x),
    design = subset(da_allSP_design, comp.group == 2L)
  ) |> as.data.table()
  tab2[, comp.group := 2L]
  
  tab <- rbindlist(l = list(tab1, tab2))
  tab[, VARIABLE := x]
  tab[, VALUE := (1:.N) - (x == "DUVAR_SCRRESP2"), by = .(comp.group)]
  tab[, comp.group := factor(comp.group)]
  setcolorder(tab, c("VARIABLE", "comp.group", "VALUE"))
  
  return(tab[])
  
}

analyses_3(vars_analysis_3[1])

tab_analysis_3_test <- map(
  .x = vars_analysis_3,
  .f = analyses_3
) |> rbindlist()

tab_analysis_3_test[, VARIABLE := factor(
  x = VARIABLE, levels = vars_analysis_3
)]

tab_analysis_3_test[, VALUE := factor(VALUE)]

tab_analysis_3_test[, mean := mean * 100]
tab_analysis_3_test[, SE := SE * 100]

tab_analysis_3_test[, LOWERCL := mean - SE * z_alpha]
tab_analysis_3_test[, UPPERCL := mean + SE * z_alpha]

ggplot(
  data = tab_analysis_3_test,
  mapping = aes(
    x = VALUE, y = mean, ymin = LOWERCL, ymax = UPPERCL, fill = comp.group
  )
) +
  geom_col(position = "dodge", alpha = 0.5) +
  geom_errorbar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(facets = vars(VARIABLE), scales = "free") +
  theme_bw() +
  ggtitle("ANALYSIS 5: BQ LRNR compared to other BQ NR")
ggsave(filename = "analyses/plot3.pdf", scale = 2)


comp.group <- NULL
comp.group[da_allSP$comp.group == 1L] <- 1L
comp.group[da_allSP$comp.group == 2L] <- 2L

table(comp.group, useNA = "ifany")
table(comp.group, da_allSP$DISP_CIBQ, useNA = "ifany")

da_allSP_design_comp <- update(
  object = da_allSP_design,
  comp.group = comp.group
)

# chi-square test
svychisq(
  formula = as.formula("~CI_AGE_CAT + comp.group"),
  design = da_allSP_design_comp
)

map(
  .x = vars_analysis_3,
  .f = \(x) {
    svychisq(
      formula = as.formula(glue("~{x} + comp.group")),
      design = da_allSP_design_comp
    )
  }
)
