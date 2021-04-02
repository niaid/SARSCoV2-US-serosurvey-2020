# CREATE COUNTY-->URBAN/RURAL LOOKUP TABLE 
#  FROM THE TARGETS FILE THAT MICHAEL FAY SENT SO THAT
#  COUNTY <--> RURAL/URBAN DESIGNATION ARE CONSISTENT ACROSS ALL DATA SETS 

# READ-IN ZIP TO COUNTY MATCHING FILE 
# using 2018 version to map correctly to county names  
gdat <- read_xlsx("data/ZIP_COUNTY_122018.xlsx", 
                  col_types = c(rep("text",2), rep("numeric", 4)))
# READ-IN MOST RECENT ZIP TO COUNTY FILE TO ADD 
#   ANY MISSING ZIPS ADDED BETWEEN 2018 AND 2020
gdat2 <- read_xlsx("data/ZIP_COUNTY_032020.xlsx", 
                   col_types = c(rep("text",2), rep("numeric", 4)))

stnames <- read_xlsx("data/State_FIPS_Codes.xlsx")

# READ-IN FILE OF POPULATION TARGETS IF IT DOESN'T EXIST 
if (sum(ls() %in% "tdat")==0) {
  tdat <- read.csv("data/Census3AgeGroupsWithNames_UrbanRural.csv"
                   , stringsAsFactors = FALSE)
}


urban <- tdat %>% 
  distinct(StateFIPS, County, STNAME, CTYNAME, urban.rural) %>% 
  as.data.frame()
  
# CLEAN UP ZIP TO COUNTY MATCHING FILE 
# FOR NOW, WE WILL TAKE THE COUNTY THAT 
#   HAS THE HIGHEST PROPORTION OF RESIDENCES 
# ZIP TO COUNTY AT THE CENSUS LEVEL IS FROM 2010 
#   SO IS OUTDATED DUE TO FREQUENT CHANGES IN ZIP CODES 
# [CHECKS ON SOME OF THE DISCREPANCIES FOUND 
#   THAT MANY ZIP CODES FROM THE CENSUS WERE IN THE WRONG STATE]

# IDENTIFY ZIP CODES THAT MAY HAVE BEEN ADDED IN THE LAST 2 YEARS
names(gdat2) <- names(gdat)
gdat <- gdat2 %>% anti_join(gdat, by='zip') %>% 
  bind_rows(gdat) %>% as.data.frame()

geo <- gdat %>% inner_join(
  (gdat %>% group_by(zip) %>% 
     summarise(maxRES = max(res_ratio), 
               count = n())), "zip") 

# DECIDED TO JUST TAKE WHICHEVER COUNTY HAS THE HIGHER RES_RATIO
geo <- as.data.frame(geo[geo$res_ratio == geo$maxRES, ])
geo <- geo[!duplicated(geo$zip), ]

# CHECK: WE SHOULD HAVE ONE ROW PER ZIP CODE 
nrow(geo)
length(unique(gdat$zip))

geo$STATE <- substr(geo$county, 1, 2)

geo <- geo %>% 
  filter(!(STATE %in% c("72", "78" ,"60","66", "69"))) %>% 
  rename(County = county, ZIP = zip) %>% 
  select(ZIP, County, res_ratio) %>% 
  left_join(urban, "County") %>% 
  inner_join(stnames %>% rename(
    STNAME = Name, StateFIPS = FIPS), by = c("STNAME", "StateFIPS"))

nrow(geo[is.na(geo$urban.rural), ])
# missing on 19 rows [2020] - added 'Shannon County' 
# [2018] version is missing on 0 rows 



