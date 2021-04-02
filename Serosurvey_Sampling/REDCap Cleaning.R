# PROCESS REDCAP FILE 
str(rdat)

rdat.names <- c("RC.Id","County", "sex", "Age", "hispanic"
                , "White", "Black", "AmIndian","Asian", "PIslands", "Other")
length(rdat.names) == length(names(rdat))

names(rdat) <- rdat.names 

unique(rdat$County)

# TEST FIRST VALUE TO MAKE SURE THIS APPROACH WILL WORK
unlist(strsplit(rdat$County[1], " - "))[1]

for (i in 1:nrow(rdat)){ 
  rdat$CTYNAME[i] <- 
    unlist(strsplit(rdat$County[i], " - "))[1]
  rdat$STNAME[i] <- 
    unlist(strsplit(rdat$County[i], " - "))[2]
}

geonames <- read_xlsx(path = "data/all-geocodes-v2018.xlsx", skip = 4)

countycodes <- geonames %>% 
  filter(`Summary Level` == "050") %>% 
  rename(CTYNAME = `Area Name (including legal/statistical area description)`)
countycodes$County <- paste(countycodes$`State Code (FIPS)`
                            , countycodes$`County Code (FIPS)`
                            , sep = "") %>% as.data.frame()
countycodes <- countycodes[ , c("County", "CTYNAME")]

for (f in 
     c("White", "Black", "AmIndian", "Asian", "PIslands", "Other")){
  rdat[rdat[, f]=="Checked", f] <- "1"
  rdat[rdat[, f]=="Unchecked", f] <- "0"
  rdat[, f] <- as.numeric(rdat[, f])
}

rdat$nraces <- rdat$White + rdat$Black + 
  rdat$AmIndian + rdat$Asian + rdat$PIslands + rdat$Other

rdat$race[rdat$nraces > 1] <- "TOM"
rdat$race[rdat$nraces == 1 & rdat$White==1] <- "WA"
rdat$race[rdat$nraces == 1 & rdat$Black==1] <- "BA"
rdat$race[rdat$nraces == 1 & rdat$Asian==1] <- "AA"
rdat$race[rdat$nraces == 1 & rdat$AmIndian==1] <- "IA"
rdat$race[rdat$nraces == 1 & rdat$PIslands==1] <- "PA"
rdat$race[rdat$nraces == 1 & rdat$Other==1] <- "OA"

rdat$hispanic[rdat$hispanic=="Yes"] <- "H"
rdat$hispanic[rdat$hispanic=="No"] <- "NH"

rdat$AGEGRP[rdat$Age >= 18 & rdat$Age < 45] <- 1
rdat$AGEGRP[rdat$Age >= 45 & rdat$Age < 70] <- 2
rdat$AGEGRP[rdat$Age >=70] <- 3 

rdat$sex <- substr(rdat$sex, 1, 1)


