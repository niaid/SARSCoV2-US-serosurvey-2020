#### This folder included data and R codes used to create Figure 3 and 4.

##### data folder:
- `lgtreg.x.sel.rds` --- can be opened in R/Rstuido. It stores output objects from survey::svyglm(), which estimates the propensity of being included in the target data. `/Table1_TableS4/R/PaperTable1_SupplTable4.R` contains the code to generate this data. 
- `covid2020_x.sel.rds` --- can be opened in R/Rstudio. `/Figure3_Figure4/R/KWpostst5_SelMdl.R` contains the code to generate this data.


Note: Another input data, serosurvey data (*"NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.xlsx"*), is locates at /Table1_TableS4/data/ folder

##### R folder:
- `KWpostst5_SelMdl.R` --- R code to produce post-stratified KW weights and backward model selection, and to generate data `covid2020_x.sel.rds`. 
- `ForestPlots.R` --- R code to create Figure 3 and Figure 4. 
- `WprevSeSp_MF.R` (Author: Mike Fay) --- R function sourced by `ForestPlots.R` to perform sensitivity and specificity adjustment.
- `PSwtEst_0.1.0.tar.gz` (Author: Yan Li) --- R package to compute prevalence estimate

##### output data folder:
This folder includes data generated from `ForestPlots.R` and further formatted in excel file (e.g. label editing, adding space) to create Figure 3 and 4. 
Data list:
- RemoveItemWithSmallSampleSize \_Adjusted_Prevalence_KWmethod_NotSixQuotaVariable_09Dec2020.xlsx  --- to create Figure 3
- Copy of Copy of Adjusted_Prevalence_KWmethod_SixQuotaVariable_09Dec2020.xlsx  --- to create Figure 4
- Copy of PropPositive_SimpleMethod_AllVariables_03Dec2020  --- to calculate positive numbers appeared in Figure 3 and 4




