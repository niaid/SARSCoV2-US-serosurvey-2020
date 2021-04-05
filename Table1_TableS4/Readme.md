This folder includes R code used to generate Table 1 and Supplemental Table 4. 


##### R folder:
- `Add_urbalrural_seropositivity.R` --- extract urbal/rural variable from the Census data; define NCHS urban/rural variable based on NCHS classification scheme; add seropositivity outcomes by evaluating 300 negative and 56 positive samples from the "finaldataforEUA.xlsx" data (not included); create output data "NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.csv" (not included).
-	`merge data CovidBrfss.R` --- clean and merge the BRFSS data with the "NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.csv" data. The merged output is called “BRFSS2018COVID9028.csv” (not included)
-	`PaperTable1_SupplTable4.R` --- R code to construct Table 1 and Table S4. The input data include “BRFSS2018COVID9028.csv” and the Census data, “Census3AgeGroupsWithNames_UrbanRural.csv” (not included).

##### data folder (not included):
- The Serosurvey data called “NIHCOVID19AntibodySt_DATA_CODES_2020-10-22_1119_NP.xlsx”. 
-	BRFSS data called “LLCP2018.xpt” can be downloaded from CDC website: https://www.cdc.gov/brfss/annual_data/annual_2018.html
-	US Census data called “Census3AgeGroupsWithNames_UrbanRural.csv" created from Census files.  
-	NCHS Urban-Rural Classification Scheme of Counties can be downloaded from CDC website:https://www.cdc.gov/nchs/data_access/urban_rural.html
-	Data used to define seropositivity threshold, called "finaldataforEUA.xlsx".
