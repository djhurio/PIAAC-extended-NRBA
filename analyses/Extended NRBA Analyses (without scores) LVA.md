# Extended NRBA Analyses (without scores) LVA

- Step: Review
- Due Date: April 3, 2024

## Description:

The Consortium has conducted the Extended Nonresponse Bias Analysis (NRBA) for the analyses that do not require proficiency scores. This spreadsheet contains the following sheets:

    Analysis 1 - Wgting Adjustments. This sheet provides a comparison of estimates before and after weighting adjustments, for key characteristics.
    Analysis 2 - External Estimates. This sheet provides a comparison of population estimates from PIAAC and external sources.
    Analysis 5 - BQ LRNR vs oth BQ NR. This sheet provides a comparison of the distribution of literacy-related BQ nonrespondents versus other BQ nonrespondents, for key characteristics.
    Analysis 5 - SCR LRNR vs oth SCR NR. (For Screener countries only). This sheet provides a comparison of the distribution of literacy-related Screener nonrespondents versus other Screener nonrespondents, for key characteristics.

There is also a Comments sheet which provides some observations made by the Consortium about each of the analyses.

Please review the NRBA and send any comments and questions to the Westat Sampling email (PIAACC2-Sampling@westat.com) by 29th March. If changes are required, please "Return" the task on the Portal. A Westat statistician will update the file and resubmit it to the Portal for your review. When all questions are resolved, please click the "Finish Task" button to mark the task as Complete on the Portal.

For background information on these analyses, you can refer to the following documents: PIAAC_CY2(2023_01)Sampling Plan Part III NRBA.docx, and NRBA Information Template_Extended_CCC.docx.

As a reminder, the second part of the NRBA, the analyses that use proficiency scores, will be sent to countries by 15th July and you will have until 29th July to complete your review.


## 2024-04-04

Thanks for the feedback! I have uploaded two datasets (qc_hh_repwt_lva.sas7bdat, qc_sp_repwt_lva.sas7bdat) containing the relevant replicate weights to the Documents\Exchange\LVA - Latvia\Sampling\Weighting folder on the Portal: Documents – PIAAC Portal (ets.org) Please note the household unknown eligibility weights are used for the Analysis5-SCR LRNR vs oth SCRNR.

Regarding Analysis 2, there was an error in our calculations and we are working on a revised analysis which we will send soon. Could you please return the task to us so that we can upload a new version once ready?



## 2024-04-10

Just to let you know that the updated NRBA result has been submitted to the Portal. Could you please provide feedback by April 17th?

Here is a summary of changes to the ‘Comments’ worksheet:

- No changes were made to A1 or A2. They both still seemed fine as is.
- For A5 (BQ), we added age group into the description.
- For A5 (SCR), we dropped reference to ‘Share of population with ISCED 0-1’ because that variable no longer appears.
    

## 2024-04-20

### Analysis 1 - Wgting Adjustments

```
                           variable                                   V1
                             <fctr>                               <char>
1: Base Weight All Eligible SPs (1)                                 TRUE
2:      Base Weight Respondents (2)                                 TRUE
3:    NR Adj Weight Respondents (3)                                 TRUE
4:                 PROBF1 (1) v (2)  Mean relative difference: 0.1293481
5:                 PROBF1 (1) v (3) Mean relative difference: 0.05842444
6:                 Bonferroni Alpha                                 TRUE
```

```
      VARIABLE VALUE         variable value.orig value.test
        <char> <num>           <fctr>      <num>      <num>
 1:   PERSVAR3    93 PROBF1 (1) v (2)          1     0.0000
 2:   PERSVAR3    95 PROBF1 (1) v (2)          1     0.0000
 3:   PERSVAR3    93 PROBF1 (1) v (3)          1     0.0030
 4: DUVAR_ALL1    24 PROBF1 (1) v (2)          1     0.3115
 5: DUVAR_ALL2    22 PROBF1 (1) v (2)          1     0.3115
 6: DUVAR_ALL1    16 PROBF1 (1) v (2)          1     0.3159
 7: DUVAR_ALL2    11 PROBF1 (1) v (2)          1     0.3159
 8:   PERSVAR2     9 PROBF1 (1) v (2)          1     0.3173
 9:   PERSVAR2    28 PROBF1 (1) v (2)          1     0.3205
10:   PERSVAR2    28 PROBF1 (1) v (3)          1     0.3205
```


```
    VARIABLE VALUE         variable value.orig value.test
      <char> <num>           <fctr>      <num>      <num>
 1: PERSVAR5    99 PROBF1 (1) v (3)     0.5548     0.3193
 2: PERSVAR2    52 PROBF1 (1) v (3)     0.5807     0.4655
 3: PERSVAR5    99 PROBF1 (1) v (2)     0.8424     0.7705
 4: NRBAVAR3     2 PROBF1 (1) v (3)     0.6441     0.5997
 5: PERSVAR4    28 PROBF1 (1) v (2)     0.3064     0.2665
 6: AREAVAR5    45 PROBF1 (1) v (3)     0.1680     0.1333
 7: NRBAVAR1     9 PROBF1 (1) v (3)     0.4557     0.4218
 8: PERSVAR4    28 PROBF1 (1) v (3)     0.0496     0.0176
 9: PERSVAR2    52 PROBF1 (1) v (2)     0.3218     0.2928
10: PERSVAR3    55 PROBF1 (1) v (3)     0.2145     0.1879
```

