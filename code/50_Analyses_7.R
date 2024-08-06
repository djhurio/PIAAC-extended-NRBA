# 3.4.7 Calculation of the range of potential bias

## Options
options(max.print = 10e3)
options(openxlsx.numFmt = "0.0000")
options(survey.replicates.mse = TRUE)

## Reset
rm(list = ls())
gc()

library(purrr)
library(data.table)

# load essential packages. Please download and install them if they are not available.
library(survey)
library(haven) # For reading in sas dataset
library(tidyverse) # For data manipulation
library(nrba) # For running NR bias analysis
library(srvyr) # For dplyr-style syntax with survey designs
library(reldist) # For calculating weighted quantile
library(Hmisc) # For weighted quantiles and means

### SDIF - final
dat_sdif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/SAS/SAS7BDAT/psdlvams.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_sdif)
"SPLNRCELL" %in% names(dat_sdif)

### BQ data
dat_bq <- fread(file = "../PIAAC-data-2023/data/BQ/BQ_3NOV2023.csv")
names(dat_bq)
"SPLNRCELL" %in% names(dat_bq)

### WIF
dat_wif <- haven::read_sas(
  data_file = "../PIAAC-data-2023/data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
names(dat_wif)
"SPLNRCELL" %in% names(dat_wif)

### SPFWT
dat_spfwt <- haven::read_sas(
  data_file = "Extended_NRBA_Analyses/data/_14_spfwt_lva.sas7bdat",
  col_select = c("CNTRYID", "CASEID", "PERSID", "BQ_STAT")
) |> setDT(key = c("CNTRYID", "CASEID", "PERSID")) |> setcolorder()
dat_spfwt[, .N, keyby = .(BQ_STAT)]


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
  f = \(x, y) merge(x, y),
  x = list(dat_sdif, dat_wif, dat_nrba, dat_surv, dat_spfwt)
) |> setkey(CNTRYID, CASEID, PERSID) |> setcolorder()
key(nrba_data)
names(nrba_data) |> head(10)


# read in test file of a screener country
nrba_data[, .N, keyby = .(DISP_CIBQ)]
nrba_data[, .N, keyby = .(WEIGHTFLG)]


analysis_7 <- function(
    pv_var_name = "PVLIT1",
    pv_var_label = pv_var_name
) {
  
  da <- nrba_data |> dplyr::mutate(
    PVVAR = get(pv_var_name),
    RESP_STAT = case_when(
      DISP_CIBQ %in% c(18, 25) ~ "I",
      DISP_CIBQ == 22 ~ "U",
      WEIGHTFLG == 0 ~ "NR",
      WEIGHTFLG == 1 ~ "R"
    )
  ) |>
    # create weighting cells where
    # BQ_STAT is the BQ Nonliteracy-Related Nonresponse Status,
    # where BQ_STAT=3 for literacy-related nonrespondents and ineligible
    dplyr::mutate(
      BQ_CELL_COMBINED = ifelse(
        (BQ_STAT == 3 & !(DISP_CIBQ %in% c(18, 25))),
        SPLNRCELL + 900,
        SPNRCELL
      ),
      # create base weights
      THEOR_PBWT_NEW = THEOR_HBWT / PROB_PERS
    ) |>
    # remove ineligibles and unknown eligibility
    dplyr::filter(RESP_STAT %in% c("R", "NR")) |>
    # remove respondents with missing scores - shouldn't be any
    dplyr::filter(!(RESP_STAT == "R" & is.na(PVVAR))) # or PVNUM or PVAPS
  
  names(da) |> tail(10)
  
  da[, .N, keyby = .(RESP_STAT)]
  da[, .N, keyby = .(BQ_CELL_COMBINED)]
  da[, summary(THEOR_PBWT_NEW)]
  
  # screener response rate
  SCR_RR <- 0.373 # insert screener response rate from the final Sample Design Summary
  # CY2_MS_Sample_Design_Summary_final_LVA.docx
  
  # Sum THEOR_PBWT_NEW over all records and call this SUMWT_O
  SUMWT_O <- sum(da$THEOR_PBWT_NEW)
  # Compute the sum of THEOR_PBWT_NEW for respondents, and call this SUMWT_R_O
  SUMWT_R_O <- sum(da[da$RESP_STAT == "R", "THEOR_PBWT_NEW"])
  # PCT_R_O
  PCT_R_O <- (SCR_RR * SUMWT_R_O / SUMWT_O) * 100
  
  # design with THEOR_PBWT_NEW
  da_design<-svydesign(ids=~1,
                       data = da,
                       weights=~THEOR_PBWT_NEW)
  
  # design with final weights
  da_design_finalwts<-svydesign(ids=~1,
                                data = da |>
                                  dplyr::filter(WEIGHTFLG == 1),
                                weights=~SPFWT0)
  
  # Average score using final weights and using THEOR_PBWT_NEW
  
  # Y_F_O = average score with final weights
  Y_F_O <- svymean(~PVVAR, da_design_finalwts)[1] # or PVNUM or PVAPS
  
  # Y_R_O is the mean score for BQ/doorstep respondents (i.e., WEIGHTFLG = 1 cases) based on the 1st plausible values, using THEOR_PBWT_NEW
  Y_R_O <- svymean(~PVVAR, da_design, na.rm=TRUE)[1] # or PVNUM or PVAPS
  
  # for each cell, get the total THEOR_PBWT_NEW, sum of respondent THEOR_PBWT_NEW for R and NR, mean, and quantiles
  nr_tbl <- da|>
    dplyr::group_by(BQ_CELL_COMBINED) %>%
    dplyr::summarize(
      #Sample size: NO_SP = number of records (n)
      NO_SP = n(), 
      #Sum of weights: SUMWT = sum of THEOR_PBWT_NEW
      SUMWT = sum(THEOR_PBWT_NEW), 
      #Sum of respondent weights: SUMWT_R = sum of THEOR_PBWT_NEW for GROUP = 1
      SUMWT_R = sum(THEOR_PBWT_NEW*(RESP_STAT == "R")), 
      #Proportion of total SPs that are in the cell: P_SP = SUMWT/SUMWT_O
      P_SP = SUMWT/SUMWT_O, 
      # Proportion of SPs in the cell that are respondents
      P_R = SCR_RR*SUMWT_R/SUMWT,
      #Estimate for respondents: Y_R = mean of PV value (Y) for GROUP = 1,
      # with weight THEOR_PBWT_NEW
      Y_R = Hmisc::wtd.mean(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"],
        na.rm = TRUE),# or PVNUM or PVAPS
      # If Y is continuous, calculate the 40th and 60th percentile of Y
      # for cases with GROUP = 1, using weight THEOR_PBWT_NEW,
      # and call them Y_NR_40 and Y_NR_60, respectively.
      Y_NR_10 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .1,
        na.rm = TRUE),
      Y_NR_25 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .25,
        na.rm = TRUE),
      Y_NR_40 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .4,
        na.rm = TRUE),
      Y_NR_60 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .6,
        na.rm = TRUE),
      Y_NR_75 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .75,
        na.rm = TRUE),
      Y_NR_90 = Hmisc::wtd.quantile(
        PVVAR[RESP_STAT == "R"],
        weights = THEOR_PBWT_NEW[RESP_STAT == "R"], # or PVNUM or PVAPS
        probs = .9,
        na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Alternative estimates: Y_k = P_R*Y_R + (1 – P_R)*Y_NR_k,
      # where k = 40 and 60 if Y is continuous
      Y_10 = P_R * Y_R + (1 - P_R) * Y_NR_10,
      Y_25 = P_R * Y_R + (1 - P_R) * Y_NR_25,
      Y_40 = P_R * Y_R + (1 - P_R) * Y_NR_40,
      Y_60 = P_R * Y_R + (1 - P_R) * Y_NR_60,
      Y_75 = P_R * Y_R + (1 - P_R) * Y_NR_75,
      Y_90 = P_R * Y_R + (1 - P_R) * Y_NR_90
    )
  
  print(nr_tbl)
  
  # Calculate the mean of Y_R over all values of BQ_CELL_COMBINED, using the weight SUMWT, and call the result Y_O.  This is the overall estimate of Y based on the assumption that respondents and nonrespondents within the same cell have the same mean.
  Y_O <- Hmisc::wtd.mean(nr_tbl$Y_R, weights = nr_tbl$SUMWT)
  
  # For each value of k, calculate the mean of Y_k over all values of BQ_CELL_COMBINED, using the weight SUMWT, and call the result Y_k_O.  This is the overall estimate of Y based on the assumption for nonrespondents.
  Y_10_O <- Hmisc::wtd.mean(nr_tbl$Y_10, weight = nr_tbl$SUMWT)
  Y_25_O <- Hmisc::wtd.mean(nr_tbl$Y_25, weight = nr_tbl$SUMWT)
  Y_40_O <- Hmisc::wtd.mean(nr_tbl$Y_40, weight = nr_tbl$SUMWT)
  Y_60_O <- Hmisc::wtd.mean(nr_tbl$Y_60, weight = nr_tbl$SUMWT)
  Y_75_O <- Hmisc::wtd.mean(nr_tbl$Y_75, weight = nr_tbl$SUMWT)
  Y_90_O <- Hmisc::wtd.mean(nr_tbl$Y_90, weight = nr_tbl$SUMWT)
  
  # Bias (low) is Y_F_O – Y_60_O; Bias (high) is Y_F_O – Y_40_O; Range of bias is Bias (high) – Bias (low)
  Bias_low <- Y_F_O - Y_60_O
  Bias_high <- Y_F_O - Y_40_O
  Range_of_bias <- Bias_high - Bias_low
  
  return(
    data.table(
      VARIABLE = pv_var_name,
      LABEL = pv_var_label,
      PCT_R_O = PCT_R_O,
      Y_R_O = Y_R_O,
      Y_10_O = Y_10_O,
      Y_25_O = Y_25_O,
      Y_40_O = Y_40_O,
      Y_O = Y_O,
      Y_60_O = Y_60_O,
      Y_75_O = Y_75_O,
      Y_90_O = Y_90_O,
      Y_F_O = Y_F_O,
      BIAS_LOW = Bias_low,
      BIAS_HIGH = Bias_high,
      `Range of Bias` = Range_of_bias
    )
  )
  
}

analysis_7()
analysis_7(pv_var_name = "PVNUM1", pv_var_label = "Numeracy score - Plausible value 1")

tab_analysis_7 <- map2(
  .x = c("PVLIT1", "PVNUM1", "PVAPS1"),
  .y = c(
    "Literacy score - Plausible value 1",
    "Numeracy score - Plausible value 1",
    "Adaptive problem solving score - Plausible value 1"
  ),
  .f = analysis_7
) |> rbindlist()

# tab_analysis_7 |> clipr::write_clip()

tab_analysis_7 |> fwrite(file = "/tmp/temp.csv")
system("xclip -selection clipboard < /tmp/temp.csv")

