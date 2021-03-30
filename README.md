# SARSCoV2-US-serosurvey-2020 (draft version) 
This repository copntains R code for manuscript "Mapping a Pandemic: SARS-CoV-2 Seropositivity in the United States" by Kalish, et al
https://www.medrxiv.org/content/10.1101/2021.01.27.21250570v1

The main data collected from the study is not available at this time; therefore the R code is provided only to give a detailed record of how the main analyses were done, and without the data file that code cannot be run to recreate tables and figures in the paper. 



## Raw Data

1. 2018 Behavioral Risk Factor Surveillance (BRFSS) data. The data used are very large, and may be retrieved from the CDC website: 
 (source: https://www.cdc.gov/brfss/annual_data/annual_2018.html). 
2. U.S. Census data. The url addresses for the source data, and the R code used to combined different source files is described in the US Census folder. 
3. Serosurvey data. This is the data collected specifically in this study. Because of privacy issues, care must be made in how it is released. At this time, the data are not part of this repository. 



## R codes
R codes to generate the following tables/figures are included:

1. **Table 1**: Characteristics of serosurvey population in comparison to United States population
2. **Supplemental Table 4**: Comparison of weighed proportions from the BRFSS to the proportions from the US Census
3. **Figure 3**: Seroprevalence estimates of health and behavioral traits
4. **Figure 4**: Characteristics of serosurvey population in comparison to United States population


## Note
Each of the main folder contains one data folder, one R folder, and one Readme file. The excel file *"Folder_ReferenceChart.xlsx"* list the structure of this site. 

