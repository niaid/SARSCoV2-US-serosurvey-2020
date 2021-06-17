# SARSCoV2-US-serosurvey-2020 
This repository copntains R code for manuscript "Mapping a Pandemic: SARS-CoV-2 Seropositivity in the United States" by Kalish, et al
https://www.medrxiv.org/content/10.1101/2021.01.27.21250570v1

The main data collected from the study is not available at this time; therefore the R code is provided only to give a detailed record of how the main analyses were done, and without the data file that code cannot be run to recreate tables and figures in the paper. Other data that is freely available on the internet (e.g., US Census data, CDC BRFSS data) will not be additionally stored here because the files are very large.

## Overview

There are many analyses in the paper, that use several data sets used (see especially Figure 1 of the paper). This repository contains R code for the main analyses in several different folders (each with its own Readme.md file). 

1. US_Census: contains R markdown that tells the URL addresses from where the US census data files were gotten, and it gives the R code used to combine the several different US census files.
2. Serosurvey_Sampling: contains R markdown that explains the algorithm to create the invitations to join the study for the quota sampling.
3. Figure3_Figure4: contains R code to create Figures 3 and 4 of the paper. 
4. Table1_Tables4: contains code to create Tables 1 and S4 of the paper.
5. Ratio_of_Counts: contains R markdown to detail the estimate of the ratio of undiagnosed infections over diagnoses cases from the study. 


https://zenodo.org/badge/349515159.svg
