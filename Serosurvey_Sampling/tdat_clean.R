# CLEAN-UP TARGETS FILE TO TRANSLATE COUNTY FIPS 
# FROM NUMERIC VALUES TO STANDARDIZED 5-DIGIT TEXT WITH LEADING 0'S 


### Convert state codes to 2-digit codes (add leading 0's if needed)
tdat$StateFIPS[tdat$STATE<10] <- 
  paste("0", tdat$STATE[tdat$STATE<10], sep="")
tdat$StateFIPS[tdat$STATE>=10] <- 
  as.character(tdat$STATE[tdat$STATE>=10]) 


### COnvert county code to GEOID [concatenated state + county ids]
###   convert county codes to 3-digit counties (adding leading 0's as needed) 
tdat$County[tdat$COUNTY<10] <- 
  paste(tdat$StateFIPS[tdat$COUNTY<10], "00"
        , tdat$COUNTY[tdat$COUNTY<10], sep="")
tdat$County[tdat$COUNTY>=10 & tdat$COUNTY<100] <-   
  paste(tdat$StateFIPS[tdat$COUNTY>=10 & tdat$COUNTY<100], "0"
        , tdat$COUNTY[tdat$COUNTY>=10 & tdat$COUNTY<100], sep="")
tdat$County[tdat$COUNTY>=100] <-   
  paste(tdat$StateFIPS[tdat$COUNTY>=100]
        , tdat$COUNTY[tdat$COUNTY>=100], sep="")
tdat$race[tdat$RACENAME == 'Pacific Islander Alone'] <- "PA"


### Assign regions [1-6] based on state assignments 
tdat$region[
  tdat$STNAME %in% c( 
    'Connecticut','Maine','Massachusetts'
    ,'New Hampshire','Rhode Island','Vermont'
    ,'New Jersey','New York','Pennsylvania')] <- 1 

tdat$region[
  tdat$STNAME %in% c( 
    'Illinois','Indiana','Michigan','Ohio' 
    ,'Wisconsin','Iowa','Minnesota')] <- 2

tdat$region[
  tdat$STNAME %in% c('Delaware' 
                     ,'District of Columbia','Georgia','Maryland'
                     ,'North Carolina','South Carolina','Virginia' 
                     ,'West Virginia','Kentucky','Tennessee' )] <- 3

tdat$region[
  tdat$STNAME %in% c('Kansas'
                     , 'Missouri','Florida','Alabama','Mississippi'
                     ,'Arkansas','Louisiana','Oklahoma')] <-4  

tdat$region[
  tdat$STNAME %in% c( 
    'Nebraska','North Dakota','South Dakota'
    ,'Texas','Arizona','Colorado','Idaho' 
    ,'Montana','New Mexico','Utah','Wyoming' )] <- 5

tdat$region[
  tdat$STNAME %in% c('Nevada','Alaska','California',
                     'Hawaii','Oregon','Washington')] <- 6