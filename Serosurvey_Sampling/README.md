# Serosurvey_Sampling
 
This serves as a respository of code used to sample a volunteer roster of ~300K to match (as closely as possible) the demographic characteristics captured in the U.S. Census, across 6 categories: Age, Sex, Race, Ethnicity, State, and Urban/Rural environment. Please note that data were obfuscated to ensure sufficient de-identification of volunteers and enrolled individuals. Since files are updated daily, the code and data provided cannot fully replicate the process of identifying the target smaple. We only aim to illustrate how the process was implemented in code. 

See the sampling_strategy_share.Rmd file for complete code and discussion. Supporting de-identified data are provided in the data folder, but not all the data are given (e.g., the Census data is over 25 Mb, so is not included).  

The main code relies on several supporting R files: 
-	`functions.R` --- contains utility functions that analzye demographic characteristics and support the sampling process 
-	`clean_sf_data.R` --- cleans up roster of volunteers, by excluding people who have not completed the demographic survey or who have expressed that they are no longer interested in being contacted; and reclassifies demographic information to match target requirements 
-	`REDCap Cleaning.R` --- cleans up the deidentified .csv file of enrolled participants to extract state and county; and  calculates age and categorizes into age groups based on date of birth
-	`zip_county_lookup.R` --- extract and apply rural / urban classificatoin based on county from RedCap enrollment file 
-	`tdat_clean.R` --- clean up and summarize targets file to establish expected sample sizes for each combined level of race, ethnicity, sex, age, state, and urbanicity. 
