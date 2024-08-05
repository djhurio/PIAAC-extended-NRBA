#############################################################
# PIAAC Cycle II
# EVO calculations for outcome variables PVLIT, PVNUM, PVAPS
# Westat, May 2024
#############################################################

# This program has to be run for three outcome variables: PVLIT, PVNUM, PVAPS
# In addition, for each of these outcome variables, it should be run twice:
# - using the cell indicator variable only in the regression (a combined indicator for non-literacy related and literacy-related nonresponse)
# - using both cell indicator variable and all raking variables in the regression

# The model used in EVO calculations is determining the association between the PVs (proficiency scores) and the auxiliary variables.
# These auxiliary variables are:
# - Screener stage: HHNRCELL or HHNRCELL together with raking variables
# - BQ stage: a combination variable of SPNRCELL and SPLNRCELL alone, or together with raking variables.

# SCREENER countries:
# The stage 1 model is looking at the association between the PVs and the variables that are used to reduce bias 
# due to screener nonresponse, and the stage 2 model is looking at the association between the PVs and the variables 
# that are used to reduce bias due to BQ nonresponse. 

# REGISTRY countries:
# Only stage 1 model is considered. It is looking at the association between the PVs and the variables 
# that are used to reduce bias due to BQ nonresponse. 


# Since PV-values are only available for the respondents, the model will be fitted using those cases only
# (hence subsetting to WEIGHTFLG = 1 in the script, i.e., DISP_CIBQ = 1 or DISP_DS = 1 - these are cases with available PVs).

#####################################################################
# Parameters
#####################################################################

# nrba_data:
#  A dataframe of respondents and nonrespondents with
#  the following variables
#
#  - RESP_STAT:
#           "R" for respondents,
#           "NR_1" for first stage nonrespondents,
#           "NR_2" for second stage nonrespondents - only applicable to SCREENER countries
#           "I" for ineligibles (although these can be deleted beforehand)
#           These are the only acceptable values.
#  Derivation of RESP_STAT for SCREENER countries:
# RESP_STAT = "R" - for "R" and "L1" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#             "NR_1" - for "NR", "U", "L" from Table 1B in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#             "NR_2" - for "NR", "D", "L2" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#             "I"  - for "I" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#                      

#  Derivation of RESP_STAT for REGISTRY countries:
# RESP_STAT = "R" - for "R" and "L1" from Table 1A in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#             "NR_1" - for "NR", "D", "U", "L2" from Table 1A in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#             "I"  - for "I" from Table 1A in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
# 
#
# prediction_variables_1: should be non-missing for all cases where outcome values (PV) are present.
#   It's a character vector of the prediction variable names to be used in the first stage NR model:
# - For screener countries, it is: HHNRCELL or HHNRCELL together with raking variables
# - For registry countries, this is BQ stage NR adjustment cell variable (a COMBINATION variable of SPNRCELL 
#   and SPLNRCELL) alone, or together with raking variables.
#   To create a COMBINATION variable of SPNRCELL and SPLNRCELL, use the literacy-related adjustment cells (SPLNRCELL) for L1 and L2 cases
#   and the non-literacy-related cells (SPNRCELL) otherwise, making sure to assign a unique number to every cell sequencing to the two sets 
#   of cells (e.g., add 999 to SPLNRCELL).

# prediction_variables_2: Only for SCREENER countries. Should be non-missing for all cases where outcome values (PV) are present.
#   It's a character vector of the prediction variable names to be used in the second stage NR model: 
#         BQ stage NR adjustment cell variable (a COMBINATION variable of SPNRCELL 
#         and SPLNRCELL) alone, or together with raking variables.
# prediction_variables_2 should be set to NULL for REGISTRY countries.
#
#  - WEIGHTFLG:
#           Overall combined BQ and DS disposition code.
#           Has value '1' for all cases with proficiency scores, i.e. when DISP_CIBQ=1 OR (DISP_CIBQ=7 and DISP_DS = 1),
#           reflecting completion of the BQ or completion of doorstep interview when BQ is not completed due to literacy-related reasons.
#           Has value '0' for all other cases.

#  - PVVAR1-10: - parameter PVVAR stands for PVLIT, PVNUM, PVAPS.
#           Ten plausible values for outcome variables (PVLIT, PVNUM, PVAPS).
#           They are multiple imputations that are generated from ETS' item response theory models.
#           Should be non-missing for all cases where WEIGHTFLG == 1
#
# For REGISTRY countries:
#  - SPUEWT0: person-level weights adjusted for unknown eligibility
#  - SPUEWT1-SPUEWT[nrep]:  Replicates of SPUEWT0.
# For SCREENER countries:
#  - SP_HHUEWT0: approximation for person-level weights adjusted for unknown eligibility. See instructions below.
#  - SP_HHUEWT1-SP_HHUEWT[nrep]:  Replicates of SP_HHUEWT0.
# 
# Instructions for SCREENER countries on approximation of person-level weights:
#
# Since the EVO is calculated at the person-level, we need to derive eligible person base weights for all cases, i.e., respondents, non-responding persons, and non-responding households. 
# We call them SP_HHUEWT.
# SP_HHUEWT for the records coming from the person-level file (with R (respondents) + NR2 (BQ-stage nonrespondents))
# are calculated as:
# SP_HHUEWT = HHUEWT/PROB_PERS - do this for SP_HHUEWT0 and for all the replicates of SP_HHUEWT1-SP_HHUEWT[nrep] (using HHUEWT0 and HHUEWT1-HHUEWT[nrep]).
# (this is equivalent to the theoretical person base weight which uses HHBWT instead of HHUEWT; HHBWT and HHUEWT are the same for screener respondents).
# For the HH-level file: in order to approximate the sum of person weights from nonresponding households,
# determine the average number of eligible persons per eligible HH - mean_NUMELG1 - as a weighted average (by HHUEWT0) of NUMELG1 over cases with DISP_SCR in (1,2).
# Derive SP_HHUEWT for nonresponding HH as:
# SP_HHUEWT0 = HHUEWT0 * mean_NUMELG1 - do this for SP_HHUEWT0 and for all the replicates of SP_HHUEWT1-SP_HHUEWT[nrep] (using HHUEWT0 and HHUEWT1-HHUEWT[nrep]).
# Stack the HH file with nonresponding households under the person-level file.
#####################################################################


gc()
rm(list = ls())

# Install missing packages, if any, and load them:

# unavailable <- setdiff(c("survey", "tidyverse", "srvyr", "mitools", "haven", "openxlsx"), rownames(installed.packages()))
# install.packages(unavailable)

library(survey)
library(dplyr) # For data manipulation
library(srvyr) # For dplyr-style syntax with survey designs
library(tidyverse) # For data manipulation
library(mitools) # For working with multiple imputations and plausible values
library(haven) #for reading in SAS files
library(openxlsx)

#################################################
# PARAMETERS
#################################################

DESIGN <- "HH" #"RGSTRY" or "HH"
rep_method <- "Fay" # "Fay" or "JK2"
#################################################

stopifnot(DESIGN %in% c("HH", "RGSTRY"))


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

# hh_repwt
# HHUEWT: Screener unknown eligibility adjusted weight
dat_hh_repwt <- haven::read_sas(
  data_file = "Extended_NRBA_Analyses/data/qc_hh_repwt_lva.sas7bdat"
) |> setDT(key = "CASEID") |> setcolorder()
names(dat_hh_repwt)
# Remove variables overlapping with WIF
x <- grep("WT0$", names(dat_hh_repwt), value = TRUE)
dat_hh_repwt[, c(x) := NULL]
rm(x)

### Extended_NRBA_LVA
dat_nrba <- openxlsx2::read_xlsx(
  file = "results/Extended_NRBA_LVA.xlsx"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_nrba)

### Survey data
dat_surv <- haven::read_spss(
  file = "../PIAAC-data-2023/data/Survey-Data/PRGLVAP2.sav",
  col_select = c(
    "CNTRYID", "CASEID", "PERSID",
    matches("^PV(LIT|NUM|APS)([1-9]|10)$")
  )
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_surv)
names(dat_surv) |> length() == 3 + 3 * 10


# replace with your data and format analysis variables as needed
nrba_data <- Reduce(
  f = \(x, y) merge(x, y, all = TRUE),
  x = list(dat_sdif, dat_wif, dat_nrba, dat_surv)
) |> merge(dat_hh_repwt, all = TRUE) |>
  setkey(CNTRYID, CASEID, PERSID) |> setcolorder()
key(nrba_data)
names(nrba_data) |> head(10)

names(nrba_data)
grep("RESP_STAT", names(nrba_data), ignore.case = TRUE, value = TRUE)

# Table 1B.
# Screener Disposition Code Categories for Basic NRBA (Countries with Screeners)
nrba_data[, .N, keyby = .(DISP_SCR)]
nrba_data[, DISP_SCR_CAT := factor(
  x = case_match(
    DISP_SCR,
    c(1:2) ~ "R",
    c(7) ~ "L",
    c(3, 9, 12:16) ~ "NR",
    c(19, 22, 26, 28) ~ "I",
    c(4:5, 17, 20:21, 24) ~ "U"
  ),
  levels = c("R", "L", "NR", "I", "U")
)]
nrba_data[, .N, keyby = .(DISP_SCR_CAT)]
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_SCR)]

# Table 1C.
# BQ Disposition Code Categories for Basic NRBA (Countries with Screeners)
nrba_data[, .N, keyby = .(DISP_CIBQ)]
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ)]

nrba_data[, DISP_CIBQ_CAT := factor(
  x = case_when(
    DISP_CIBQ %in% 1 & !DISP_MAIN %in% 7:9 ~ "R",
    DISP_CIBQ %in% 7 & DISP_DS %in% 1 ~ "L1",
    DISP_CIBQ %in% 1 & DISP_MAIN %in% 7:9 ~ "L1",
    DISP_CIBQ %in% 7:9 & !DISP_DS %in% 1 ~ "L2",
    DISP_CIBQ %in% c(3:5, 14, 17, 21, 23:24, 90) ~ "NR",
    DISP_CIBQ %in% c(12:13, 15:16) ~ "D",
    DISP_CIBQ %in% c(18, 25) ~ "I"
  ),
  levels = c("R", "L1", "L2", "NR", "D", "I")
)]
nrba_data[, .N, keyby = .(DISP_CIBQ_CAT)]
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ_CAT)]


#  - RESP_STAT:
#           "R" for respondents,
#           "NR_1" for first stage nonrespondents,
#           "NR_2" for second stage nonrespondents - only applicable to SCREENER countries
#           "I" for ineligibles (although these can be deleted beforehand)
#           These are the only acceptable values.
#  Derivation of RESP_STAT for SCREENER countries:
#  RESP_STAT = "R" - for "R" and "L1" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#              "NR_1" - for "NR", "U", "L" from Table 1B in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#              "NR_2" - for "NR", "D", "L2" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx
#              "I"  - for "I" from Table 1C in PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx

# Drop "I"
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ_CAT)]
nrba_data <- nrba_data[!DISP_SCR_CAT %in% "I" & !DISP_CIBQ_CAT %in% "I"]
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ_CAT)]

nrba_data[, RESP_STAT := case_when(
  DISP_CIBQ_CAT %in% c("R", "L1") ~ "R",
  DISP_SCR_CAT  %in% c("NR", "U", "L") ~ "NR_1",
  DISP_CIBQ_CAT %in% c("NR", "D", "L2") ~ "NR_2"
)]
nrba_data[, .N, keyby = .(RESP_STAT)]
nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ_CAT, RESP_STAT)]


nrba_data[, .N, keyby = .(WEIGHTFLG)]


# Check that the input data contains necessary variables in expected format ----
if (!all(c(
  "RESP_STAT", "WEIGHTFLG",
  paste0(rep(c("PVLIT", "PVNUM", "PVAPS"), each = 10), 1:10)
) %in% names(nrba_data))) {
  stop("Some of the necessary variables are not available in `nrba_data`")
}

# prediction_variables_1: should be non-missing for all cases where outcome values (PV) are present.
#   It's a character vector of the prediction variable names to be used in the first stage NR model:
# - For screener countries, it is: HHNRCELL or HHNRCELL together with raking variables

prediction_variables_1 <- c("HHNRCELL", "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")

if (sum(prediction_variables_1 %in% names(nrba_data)) <
    length(prediction_variables_1)) {
  stop("Some of the first stage prediction variables are not available in `nrba_data`")
}

#   To create a COMBINATION variable of SPNRCELL and SPLNRCELL,
#   use the literacy-related adjustment cells (SPLNRCELL) for L1 and L2 cases
#   and the non-literacy-related cells (SPNRCELL) otherwise, making sure to assign a unique number to every cell sequencing to the two sets 
#   of cells (e.g., add 999 to SPLNRCELL).

nrba_data[, .N, keyby = .(SPNRCELL)]
nrba_data[, .N, keyby = .(SPLNRCELL)]

nrba_data[, COMBINATION := case_when(
  DISP_CIBQ_CAT %in% c("L1", "L2") ~ SPLNRCELL + 999,
  TRUE ~ SPNRCELL
)]

nrba_data[, .N, keyby = .(COMBINATION)]

# prediction_variables_2: Only for SCREENER countries. Should be non-missing for all cases where outcome values (PV) are present.
#   It's a character vector of the prediction variable names to be used in the second stage NR model: 
#         BQ stage NR adjustment cell variable (a COMBINATION variable of SPNRCELL 
#         and SPLNRCELL) alone, or together with raking variables.
# prediction_variables_2 should be set to NULL for REGISTRY countries.

prediction_variables_2 <- c("COMBINATION", "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")

if (sum(prediction_variables_2 %in% names(nrba_data)) <
    length(prediction_variables_2)) {
  stop("Some of the second stage prediction variables are not available in `nrba_data`")
}

if (!all((unique(nrba_data$RESP_STAT) %in% c("I", "NR_1", "NR_2", "R")))) {
  stop("Incorrect values of variable `RESP_STAT`")
}

if (!(is.numeric(nrba_data$WEIGHTFLG))) {
  stop("Variable `WEIGHTFLG` should be numeric")
}



# For SCREENER countries:
#  - SP_HHUEWT0: approximation for person-level weights adjusted for unknown eligibility. See instructions below.
#  - SP_HHUEWT1-SP_HHUEWT[nrep]:  Replicates of SP_HHUEWT0.
# 
# Instructions for SCREENER countries on approximation of person-level weights:
#
# Since the EVO is calculated at the person-level, we need to derive eligible person base weights for all cases, i.e., respondents, non-responding persons, and non-responding households. 
# We call them SP_HHUEWT.

# SP_HHUEWT for the records coming from the person-level file (with R (respondents) + NR2 (BQ-stage nonrespondents))
# are calculated as:
# SP_HHUEWT = HHUEWT/PROB_PERS - do this for SP_HHUEWT0 and for all the replicates of SP_HHUEWT1-SP_HHUEWT[nrep] (using HHUEWT0 and HHUEWT1-HHUEWT[nrep]).
# (this is equivalent to the theoretical person base weight which uses HHBWT instead of HHUEWT; HHBWT and HHUEWT are the same for screener respondents).

nrba_data[, .N, keyby = .(DISP_SCR_CAT, DISP_CIBQ_CAT, RESP_STAT)]
nrba_data[
  RESP_STAT %in% c("R", "NR_2"),
  .(HHUEWT0, HHUEWT1, HHUEWT80, PROB_PERS)
]

nrba_data[
  RESP_STAT %in% c("R", "NR_2"),
  c(paste0("SP_HHUEWT", 0:80)) := map(.SD, \(x) x / PROB_PERS),
  .SDcols = paste0("HHUEWT", 0:80)
]

# For the HH-level file: in order to approximate the sum of person weights from nonresponding households,
# determine the average number of eligible persons per eligible HH - mean_NUMELG1 - as a weighted average (by HHUEWT0) of NUMELG1 over cases with DISP_SCR in (1,2).
# Derive SP_HHUEWT for nonresponding HH as:
# SP_HHUEWT0 = HHUEWT0 * mean_NUMELG1 - do this for SP_HHUEWT0 and for all the replicates of SP_HHUEWT1-SP_HHUEWT[nrep] (using HHUEWT0 and HHUEWT1-HHUEWT[nrep]).
# Stack the HH file with nonresponding households under the person-level file.

nrba_data[DISP_SCR %in% 1:2, sum(HHUEWT0), keyby = .(NUMELG1)]
mean_NUMELG1 <- nrba_data[DISP_SCR %in% 1:2, weighted.mean(NUMELG1, HHUEWT0)]

nrba_data[
  RESP_STAT %in% c("NR_1"),
  c(paste0("SP_HHUEWT", 0:80)) := map(.SD, \(x) x * mean_NUMELG1),
  .SDcols = paste0("HHUEWT", 0:80)
]

nrba_data[, map(.SD, sum), .SDcols = paste0("SP_HHUEWT", 0:80)]


# Prepare the survey design objects for statistical modeling ----

if (DESIGN == "HH") {
  rep_design <- survey::svrepdesign(
    data = nrba_data,
    variables = nrba_data |> select(-matches("SP_HHUEWT")),
    weights = ~ SP_HHUEWT0,
    repweights = "SP_HHUEWT[1-9][0-9]{0,1}",
    type = rep_method, #Fay or JK2 -  This argument depends on the replication method
    rho = 0.3, #rho will be ignored for "JK2"
    combined.weights = TRUE 
  ) |> srvyr::as_survey()
} else {# for REG countries:
  rep_design <- survey::svrepdesign(
    data = nrba_data,
    variables = nrba_data |> select(-matches("SPUEWT")),
    weights = ~ SPUEWT0,
    repweights = "SPUEWT[1-9][0-9]{0,1}",
    type = rep_method, # Fay or JK2 -  This argument depends on the replication method
    rho = 0.3, # rho will be ignored for "JK2"
    combined.weights=TRUE  
  ) |> srvyr::as_survey()
}



# PVVAR <- "PVLIT"
# prediction_variables_1 <- c("HHNRCELL", "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")
# prediction_variables_2 <- c("COMBINATION", "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")

analyses_3_EVO <- function(PVVAR = "PVLIT", rake = FALSE) {
  
  prediction_variables_1 <- "HHNRCELL"
  prediction_variables_2 <- "COMBINATION"
  
  if (rake) {
    prediction_variables_1 <- c(prediction_variables_1, "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")
    prediction_variables_2 <- c(prediction_variables_2, "RAKEDIM1", "RAKEDIM2", "RAKEDIM3")
  }
  
  rep_design_for_model <- rep_design |>
    subset(WEIGHTFLG == 1) |>
    mutate(across(all_of(c(prediction_variables_1, prediction_variables_2)),
                  factor))
  
  # Get estimates of base-weighted rates, overall and by replicate ----
  
  # Rate of loss to first stage nonresponse
  rate_estimate_rep1 <- withReplicates(
    ## Provide the survey design object with base weights and response codes
    design = rep_design,
    ## Declare a function to estimate the sum of first stage nonrespondent
    ## base weights over the sum of all base weights,
    ## given a set of weights and the data frame of data
    theta = function(wts, data) {
      
      data$wts <- wts
      
      resp_dat <- data |> filter(RESP_STAT == "NR_1")
      resp_nr_dat <- data |> filter(RESP_STAT %in% c("R", "NR_1", "NR_2"))
      
      rate_estimate <- sum(resp_dat$wts) / sum(resp_nr_dat$wts)
      return(rate_estimate)
    },
    return.replicates = TRUE
  )
  
  if (DESIGN == "HH") {
    # Rate of loss to second stage nonresponse
    rate_estimate_rep2 <- withReplicates(
      ## Provide the survey design object with base weights
      design = rep_design,
      ## Declare a function to estimate the sum of second stage nonrespondent
      ## base weights over the sum of all base weights,
      ## given a set of weights and the data frame of data
      theta = function(wts, data) {
        
        data$wts <- wts
        
        resp_dat <- data |> filter(RESP_STAT == "NR_2")
        resp_nr_dat <- data |> filter(RESP_STAT %in% c("R", "NR_1", "NR_2"))
        
        rate_estimate <- sum(resp_dat$wts) / sum(resp_nr_dat$wts)
        return(rate_estimate)
      },
      return.replicates = TRUE
    )
  } else rate_estimate_rep2 <- NULL
  
  
  if (DESIGN == "HH") {
    overall_rr_est <- 1 - (rate_estimate_rep1[['theta']] + rate_estimate_rep2[['theta']])
    overall_rr_rep_estimates <- 1 - (rate_estimate_rep1[['replicates']] + rate_estimate_rep2[['replicates']])
  } else{
    overall_rr_est <- 1 - (rate_estimate_rep1[['theta']] )
    overall_rr_rep_estimates <- 1 - (rate_estimate_rep1[['replicates']])
  } 
  
  vhat_overall_rr <- svrVar(
    overall_rr_rep_estimates,
    scale = rep_design$scale,
    rscales = rep_design$rscales,
    mse = FALSE
  )
  
  class(overall_rr_est) <- 'svystat'
  attr(overall_rr_est, 'var') <- vhat_overall_rr
  attr(overall_rr_est, 'statistic') <- 'overall_rr'
  
  # Produce estimates for each PV, overall and by replicate ----
  # For each PV, fit a screener and BQ model and estimate R^2_1 and R^2_2.
  # Combine with base-weighted rate estimates to estimate EVO.
  # Use replicates to estimate variance
  
  pv_var_names <- paste0(PVVAR, 1:10)
  names(pv_var_names) <- pv_var_names
  
  estimates_by_pv_rep <- lapply(pv_var_names, function(pv_var_name) {
    
    r2_estimate_rep1 <- withReplicates(
      ## Provide the survey design object with final weights
      design = rep_design_for_model,
      ## Declare a function to estimate the R^2 statistic,
      ## given a set of weights and the data frame of data
      theta = function(wts, data) {
        
        model_formula <- reformulate(response = pv_var_name,
                                     termlabels = prediction_variables_1)
        
        ## Fit the model using the weights
        lit_model <- glm(
          formula = model_formula,
          weights = wts,
          data = data
        )
        
        
        ## Estimate the population R^2 for the fitted model
        r2_estimate <- 1 - (lit_model$deviance / lit_model$null.deviance)
        return(r2_estimate)
      },
      return.replicates = TRUE
    )
    
    if(DESIGN == "HH"){
      r2_estimate_rep2 <- withReplicates(
        ## Provide the survey design object with final weights
        design = rep_design_for_model,
        ## Declare a function to estimate the R^2 statistic,
        ## given a set of weights and the data frame of data
        theta = function(wts, data) {
          
          model_formula <- reformulate(response = pv_var_name,
                                       termlabels = prediction_variables_2)
          
          ## Fit the model using the weights
          lit_model <- glm(
            formula = model_formula,
            weights = wts,
            data = data
          )
          
          ## Estimate the population R^2 for the fitted model
          r2_estimate <- 1 - (lit_model$deviance / lit_model$null.deviance)
          return(r2_estimate)
        },
        return.replicates = TRUE
      )
    }
    
    # Use rate and R^2 estimates to estimate EVO,
    r2_1 <- r2_estimate_rep1[['theta']][1]
    
    evo <- overall_rr_est +
      rate_estimate_rep1[['theta']][1] * r2_1
    
    # Repeat the estimation for each replicate,
    # and use the replicate estimates to produce variance estimates
    rr_rep <- 1 - rate_estimate_rep1[['replicates']] 
    
    evo_rep <- overall_rr_rep_estimates +
      rate_estimate_rep1[['replicates']] * r2_estimate_rep1[['replicates']] +
      
      
      if(DESIGN == "HH"){
        
        r2_2 <- r2_estimate_rep2[['theta']][1]
        
        evo <- overall_rr_est +
          rate_estimate_rep1[['theta']][1] * r2_1 +
          rate_estimate_rep2[['theta']][1] * r2_2
        
        # Repeat the estimation for each replicate,
        # and use the replicate estimates to produce variance estimates
        rr_rep <- 1 - rate_estimate_rep1[['replicates']] - rate_estimate_rep2[['replicates']]
        
        evo_rep <- overall_rr_rep_estimates +
          rate_estimate_rep1[['replicates']] * r2_estimate_rep1[['replicates']] +
          rate_estimate_rep2[['replicates']] * r2_estimate_rep2[['replicates']]
      }
    
    
    v_hat_evo <- svrVar(
      evo_rep,
      scale = rep_design_for_model$scale,
      rscales = rep_design_for_model$rscales,
      mse = FALSE
    )
    
    vhat_r2_1 <- svrVar(r2_estimate_rep1[['replicates']],
                        scale = rep_design_for_model$scale,
                        rscales = rep_design_for_model$rscales,
                        mse = FALSE)
    
    if(DESIGN == "HH"){
      
      vhat_r2_2 <- svrVar(r2_estimate_rep2[['replicates']],
                          scale = rep_design_for_model$scale,
                          rscales = rep_design_for_model$rscales,
                          mse = FALSE)
    }
    
    # To ensure output works with 'survey' and 'mitools' functions,
    # wrap up the results as a 'svystat' object
    class(evo) <- 'svystat'
    attr(evo, 'var') <- v_hat_evo
    attr(evo, 'statistic') <- 'evo'
    
    class(r2_1) <- 'svystat'
    attr(r2_1, 'var') <- vhat_r2_1
    attr(r2_1, 'statistic') <- 'r2_1'
    
    if(DESIGN == "HH"){
      class(r2_2) <- 'svystat'
      attr(r2_2, 'var') <- vhat_r2_2
      attr(r2_2, 'statistic') <- 'r2_2'
    }
    
    # Export evo, r2_1, and r2_2
    
    
    
    
    if(DESIGN == "HH"){
      return(list("evo_estimate" = evo,
                  "r2_estimate1" = r2_1,
                  'r2_estimate2' = r2_2))
      
    } else{
      return(list("evo_estimate" = evo,
                  "r2_estimate1" = r2_1))
    }
  })
  
  
  # Extract the three sets of estimates
  evo_estimates <- lapply(estimates_by_pv_rep,"[[",1)
  r2_1_estimates <- lapply(estimates_by_pv_rep,"[[",2)
  
  # Combine results across PVs ----
  combined_r2_1_estimate_rep <- r2_1_estimates |> MIcombine()
  combined_evo_estimate_rep <- evo_estimates |> MIcombine()
  
  # Print estimates ----
  summary_table <- data.frame(
    'Statistic' = c("EVO",
                    "Combined Weighted Response Rate",
                    "Rate of Loss to First Stage Nonresponse",
                    "R^2 of First Stage"),
    'Estimate' = c(coef(combined_evo_estimate_rep),
                   coef(overall_rr_est),
                   coef(rate_estimate_rep1),
                   coef(combined_r2_1_estimate_rep)),
    'Standard Error' = c(SE(combined_evo_estimate_rep),
                         SE(overall_rr_est),
                         SE(rate_estimate_rep1), 
                         SE(combined_r2_1_estimate_rep))
  )
  
  if (DESIGN == "HH") {
    
    # Extract the three sets of estimates
    evo_estimates <- lapply(estimates_by_pv_rep,"[[",1)
    r2_1_estimates <- lapply(estimates_by_pv_rep,"[[",2)
    r2_2_estimates <- lapply(estimates_by_pv_rep,"[[",3)
    
    # Combine results across PVs ----
    combined_r2_1_estimate_rep <- r2_1_estimates |> MIcombine()
    combined_r2_2_estimate_rep <- r2_2_estimates |> MIcombine()
    combined_evo_estimate_rep <- evo_estimates |> MIcombine()
    
    # Print estimates ----
    summary_table <- data.frame(
      'Statistic' = c("EVO",
                      "Combined Weighted Response Rate",
                      "Rate of Loss to First Stage Nonresponse",
                      "Rate of Loss to Second Stage Nonresponse",
                      "R^2 of First Stage", "R^2 of Second Stage"),
      'Estimate' = c(coef(combined_evo_estimate_rep),
                     coef(overall_rr_est),
                     coef(rate_estimate_rep1), coef(rate_estimate_rep2),
                     coef(combined_r2_1_estimate_rep), coef(combined_r2_2_estimate_rep)),
      'Standard Error' = c(SE(combined_evo_estimate_rep),
                           SE(overall_rr_est),
                           SE(rate_estimate_rep1), SE(rate_estimate_rep2),
                           SE(combined_r2_1_estimate_rep), SE(combined_r2_2_estimate_rep))
    )
    
    
  }
  
  print(summary_table)
  
  setDT(summary_table)
  
  summary_table <- melt(
    data = summary_table,
    id.vars = "Statistic"
  )
  
  summary_table[
    variable == "Standard.Error",
    Statistic := paste0("SE (", Statistic, ")")
  ]
  
  summary_table[, item := paste0(PVVAR, "1")]
  summary_table[, cell := ifelse(rake, "cell + rake", "cell only")]
  
  return(
    dcast(
      data = summary_table,
      formula = item + cell ~ Statistic
    )
  )

}

# analyses_3_EVO("PVLIT", rake = FALSE)

t1 <- Sys.time()
tab_analysis_3_test <- map2(
  .x = rep(c("PVLIT", "PVNUM", "PVAPS"), each = 2),
  .y = rep(c(FALSE, TRUE), times = 3),
  .f = analyses_3_EVO,
  .progress = TRUE
)
t2 <- Sys.time()
t2 - t1
# Time difference of 30.28152 mins

tab_analysis_3_test <- rbindlist(tab_analysis_3_test)

tab_analysis_3_test <- tab_analysis_3_test[
  ,
  .(
    item,
    cell,
    EVO,
    `Combined Weighted Response Rate`,
    `Rate of Loss to First Stage Nonresponse`,
    `Rate of Loss to Second Stage Nonresponse`,
    `R^2 of First Stage`,
    `R^2 of Second Stage`,
    `SE (EVO)`,
    `SE (Combined Weighted Response Rate)`,
    `SE (Rate of Loss to First Stage Nonresponse)`,
    `SE (Rate of Loss to Second Stage Nonresponse)`,
    `SE (R^2 of First Stage)`,
    `SE (R^2 of Second Stage)`
  )
]

tab_analysis_3_test

clipr::write_clip(tab_analysis_3_test)
