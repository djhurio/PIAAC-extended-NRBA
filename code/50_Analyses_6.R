# 3.2.3 Comparison of proficiency estimates by level-of-effort

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

### Survey data
da <- haven::read_spss(
  file = "../PIAAC-data-2023/data/Survey-Data/PRGLVAP2.sav"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()

key(da)
names(da) |> first(10)


# Setting up the design with final weights
da_resp <- da[WEIGHTFLG == 1]

da_resp[, .N, keyby = .(contactTotal)]
da_resp[, LEVEL_EFFORT := contactTotal + 1L]
da_resp[, .N, keyby = .(contactTotal, LEVEL_EFFORT)]

class(da_resp)

# Function to estimate mean values for selected respondents
get_estimates <- function(contacts = max(data$LEVEL_EFFORT), data = da_resp) {
  da_resp[
    LEVEL_EFFORT <= contacts,
    c(
      .(
        contacts = contacts,
        respondents = as.numeric(sum(LEVEL_EFFORT == contacts)),
        cum_resp = as.numeric(.N)
      ),
      map(.SD, \(x) weighted.mean(x, w = SPFWT0))
    ),
    .SDcols = patterns("^PV(LIT|NUM|APS)1$")
  ]
}

get_estimates()
get_estimates(1)

attempts <- seq(from = 1, to = max(da_resp$LEVEL_EFFORT), by = 1)

tab_analysis_6_test <- map(
  .x = attempts,
  .f = get_estimates
) |> rbindlist() |> setnames(
  c(
    "Number of Contacts",
    "Number of respondents",
    "Cumulative number of respondents",
    "Cumulative Mean of PVLIT1",
    "Cumulative Mean of PVNUM1",
    "Cumulative Mean of PVAPS1"
  )
)

tab_analysis_6_test

ggplot(
  data = melt(
    data = tab_analysis_6_test,
    id.vars = c(
      "Number of Contacts",
      "Number of respondents",
      "Cumulative number of respondents"
    ),
    variable.name = "variable",
    value.name = "Cumulative mean scores"
  ),
  mapping = aes(
    x = `Number of Contacts`,
    y = `Cumulative mean scores`
  )
) +
  geom_point(
    mapping = aes(size = `Number of respondents`),
    colour = "#5B90A0"
  ) +
  geom_line(colour = "#BEBEBE") +
  scale_x_continuous(breaks = attempts) +
  facet_grid(cols = vars(variable)) +
  theme_bw() +
  theme(legend.position = "none")


# Original values
tab_analysis_6_orig <- openxlsx2::read_xlsx(
  file = "Extended_NRBA_Analyses/CY2_Extended_NRBA_Analyses_(with scores)_LVA.xlsx",
  sheet = "Analysis 6 - Level of Effort",
  start_row = 3,
  skip_empty_rows = TRUE
) |> setDT()


# Compare

tab_analysis_6_orig_long <- melt(
  data = tab_analysis_6_orig,
  id.vars = "Number of Contacts",
  variable.name = "variable",
  value.name = "value"
)
tab_analysis_6_test_long <- melt(
  data = tab_analysis_6_test,
  id.vars = "Number of Contacts",
  variable.name = "variable",
  value.name = "value"
)

setkey(tab_analysis_6_orig_long, variable, `Number of Contacts`)
setkey(tab_analysis_6_test_long, variable, `Number of Contacts`)

tab_analysis_6 <- merge(
  x = tab_analysis_6_orig_long,
  y = tab_analysis_6_test_long,
  all = TRUE,
  suffixes = c(".orig", ".test")
)

tab_analysis_6[is.na(value.orig)]
tab_analysis_6[is.na(value.test)]

tab_analysis_6[
  !is.na(value.orig) & !is.na(value.test),
  all.equal(value.orig, value.test)
]

# This analysis shows the cumulative mean proficiency scores by the number of contact attempts. The cumulative mean scores by number of contacts for literacy, numeracy, and adaptive problem solving show a steady increase with the number of contacts in the first few visits, indicating that respondents who are harder to contact score higher on average than lower level-of-effort respondents. Beyond the first few visits, there was very little change in the mean scores. The harder to reach cases score differently than the completes with fewer contacts, therefore we assume that field efforts were reaching cases with different characteristics, cases similar to nonrespondents, and as a result were effective in reducing potential bias.

# OK
# Entry with 15 attempts is missing at table and plot

