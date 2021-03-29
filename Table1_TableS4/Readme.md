This folder includes data and R codes used to generate Table 1 and Supplemental Table 4. 

##### data folder:
- The Serosurvey data (*“NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.xlsx”*) can be found in /Table1_TableS4/data/ folder
-	BRFSS data (*“LLCP2018.xpt”*) can be downloaded from CDC website: https://www.cdc.gov/brfss/annual_data/annual_2018.html
-	US Census data (*“Census3AgeGroupsWithNames_UrbanRural.csv"*) can be found in /Table1_TableS4/data folder 

##### R folder:
-	`merge data CovidBrfss.R` --- clean and merge the BRFSS data with the Serosurvey data. The merged output is called “BRFSS2018COVID9028.csv”
-	`PaperTable1_SupplTable4.R` --- R codes to construct Table 1 and Table S4. The input data include “BRFSS2018COVID9028.csv” and the Census data, “Census3AgeGroupsWithNames_UrbanRural.csv”. 
