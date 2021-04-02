
# THIS DOCUMENT REQUIRES THAT YOU FIRST RUN THE 
#   "get_sf_data.R" SCRIPT TO LOAD THE SALESFORCE DATA 

# CALCULATE AGE
edat$Birthdate <- as.character(edat$Birthdate)
edat$age <- calc_age(edat$Birthdate)
hist(edat$age[edat$age>=18], 
     breaks = seq(min(edat$age), max(edat$age), 1))
edat <- edat[edat$age >= 18, ]


# CATEGORIZE AGE: 1 [18-44], 2 [45-69], 3[70+]
edat$AGEGRP[edat$age>=18 & edat$age<=44]  <- 1
edat$AGEGRP[edat$age>=45 & edat$age<=69]  <- 2
edat$AGEGRP[edat$age>=70 & edat$age<=120] <- 3 

# SEX 
edat$sex <- substr(edat$Sex__c, 1, 1)

# ETHNICITY
edat$hispanic[edat$Hispanic__c=="true"] <- "H" 
edat$hispanic[edat$Hispanic__c=="false"] <- "NH" 

# RACE 
edat$race[grepl(";", edat$Race__c)] <- "TOM" 
edat$race[edat$Race__c == "White"]  <- "WA"
edat$race[edat$Race__c == "Black or African American"]  <- "BA"
edat$race[edat$Race__c == "Asian"]  <- "AA"
edat$race[edat$Race__c == "Pacific Islander"]  <- "PA"
edat$race[edat$Race__c == "American Indian or Alaska Native"]  <- "IA"

# CALCULATE COUNTY FROM ZIP 
edat$ZIP <- substr(edat$MailingPostalCode, 1, 5)
edat <- edat %>% left_join(geo, by = "ZIP") 

nrow(edat[is.na(edat$urban.rural), ]) # how many bad zip codes do we have

edat <- edat %>% 
  filter(!is.na(urban.rural)) %>% 
  filter(STNAME == as.character(MailingState) | 
           Abbreviation == as.character(MailingState)) 

nrow(edat) # 223185

# KEEP ONLY NECESSARY FIELDS
# edat <- edat %>%
#   select(Id, AGEGRP, sex, hispanic, race, StateFIPS, STNAME, urban.rural)

