##### This folder includes R code used to create Figure 3 and 4.


Note: Another input data, serosurvey data (*"NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.xlsx"*), is locates at /Table1_TableS4/data/ folder

##### R folder:
- `KWpostst5_SelMdl.R` --- R code to produce post-stratified KW weights and conduct backward model selection, and to generate data `covid2020_x.sel.rds`. 
- `ForestPlots.R` --- R code to create Figure 3 and Figure 4. 
- `WprevSeSp_MF.R` (Author: Michael Fay) --- R function sourced by `ForestPlots.R` to perform sensitivity and specificity adjustment.
- `PSwtEst_0.1.0.tar.gz` (Author: Yan Li) --- R package to compute prevalence estimate (called by ForestPlots.R).

##### data folder (not included):
 The R code calls 2 .rds data files that are not included in this repository:
- `lgtreg.x.sel.rds` ---It stores output objects from survey::svyglm(), which estimates the propensity of being included in the target data. `/Table1_TableS4/R/PaperTable1_SupplTable4.R` contains the code to generate this data. 
- `covid2020_x.sel.rds` ---Survey data with pseudo-weights added. The file  `/Figure3_Figure4/R/KWpostst5_SelMdl.R` contains the code to generate this data.

##### output data folder:
This folder includes data generated from `ForestPlots.R` and further formatted by hand in excel file (e.g. label editing, adding space) to create Figure 3 and 4. 
Data list:
- *Modification of Adjusted_Prevalence_KWmethod_SixQuotaVariable_09Dec2020.xlsx*  --- to create Figure 3
- *RemoveItemWithSmallSampleSize \_Adjusted_Prevalence_KWmethod_NotSixQuotaVariable_09Dec2020.xlsx*  --- to create Figure 4
- *Modification of PropPositive_SimpleMethod_AllVariables_03Dec2020.xlsx*  --- to calculate positive numbers appeared in Figure 3 and 4




