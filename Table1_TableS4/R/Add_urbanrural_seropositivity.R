###########################################################
### Project: seroprev
### Purpose: 1) extract rural/urban from Census Data and merge with serosurvey data
###          3) define urban rural using NCHS classification scheme
###          2) Add columns of pos/neg test results based on different cutoff rules, where rules are:
###########################################################
# 1)	Any positive= positive using 4 std
# 2)	IgG RBD  and IgG Spike both pos=pos. using  2 std
# 3)	1 spike and 1 rbd pos=pos using 2 and 3 std (3std as the primary one)
# 4)	Any 2 positives= pos using 2 and 3 std
###########################################################
###
### Date: Oct 22nd 2020
### Programmer: Jing Wang
###########################################################


library(readxl)
library(tidyverse)

indir <- "Table1_TableS4\\data\\"


######################################
###
###  extract Rural/urban column from census data 
### "Census3AgeGroupsWithNames_UrbanRural.csv"
###  
######################################
state_data <- data.frame(Abb = state.abb, STNAME = state.name, stringsAsFactors = FALSE)%>%
  bind_rows(data.frame(cbind(Abb="DC",STNAME="District of Columbia"), stringsAsFactors = FALSE))

census0 <- read.csv(paste0(indir,"Census3AgeGroupsWithNames_UrbanRural.csv"), header = TRUE, stringsAsFactors = FALSE)
census1<- census0%>%
  select(STNAME, CTYNAME, State_county=X2015.Geography.Name, urban.rural)%>%
  distinct(State_county, .keep_all = TRUE)%>%
  left_join(state_data, by="STNAME")



#########################
### add NCHS urban-rural definition
### 1. large central metro
### 2. large fringe metro
### 3. medium metro
### 4. small metro
### 5. Micropolitan
### 6. Non-core
###
### data source: 2013 NCHS Urban-Rural Classification Scheme for Counties
##############################
NCHS0 <- read_excel(paste0(indir,"NCHSurbruralcodes.xlsx")) ##N=3149, has 7 more records compared to the census data

NCHS <- NCHS0%>%
  select(Abb = `State Abr.`, CTYNAME=`County name`,
         NCHS_UrbanRural = `2013 code`)%>%
  mutate(CTYNAME=ifelse(Abb=="IL" & CTYNAME=="La Salle County","LaSalle County",CTYNAME),
         CTYNAME=ifelse(Abb=="NM" & CTYNAME=="Dona Ana County","Doña Ana County", CTYNAME),
         CTYNAME=ifelse(Abb=="AK" & CTYNAME=="Petersburg Census Area", "Petersburg Borough", CTYNAME))%>%
  mutate(NCHS_UrbanRuralChar= ifelse(NCHS_UrbanRural==1,"Large central metro",
                                     ifelse(NCHS_UrbanRural==2, "Large fringe metro",
                                            ifelse(NCHS_UrbanRural==3,"Medium metro",
                                                   ifelse(NCHS_UrbanRural==4,"Small metro",
                                                          ifelse(NCHS_UrbanRural==5,"Micropolitan",
                                                                 ifelse(NCHS_UrbanRural==6,"Non-core","Need to check!")))))),
         NCHS_Metro.Micro=ifelse(NCHS_UrbanRural %in% c(1,2,3,4),"Metro","Micro"),
         NCHS_Urban.Rural=ifelse(NCHS_UrbanRural %in% c(1,2,3,4,5),"Urban","Rural"))


census <- census1%>%
  left_join(NCHS, by=c("Abb","CTYNAME"))


census<-census%>%
  mutate(CTY = gsub(" County","",CTYNAME),
         CTY = gsub(" ","",CTY),
         CTY = gsub("\\'","",CTY),
         CTY = gsub("\\.","",CTY),
         CTY = gsub("\\-","",CTY),
         county =paste0(Abb, "_",CTY),
         county_fromCensus=county)%>%
  select(newcounty=county, county_fromCensus, urban.rural, NCHS_UrbanRuralChar,
         NCHS_Metro.Micro, NCHS_Urban.Rural,STNAME, Abb)%>%
  ### manually update county name to match the county in the serosurvey data set
  mutate(newcounty=ifelse(newcounty=="DC_DistrictofColumbia","DC_DC",newcounty),
         newcounty=ifelse(newcounty=="VA_Richmondcity","VA_RichmondC",newcounty),
         newcounty=ifelse(newcounty=="VA_Roanokecity","VA_RoanokeC",newcounty),
         newcounty=ifelse(newcounty=="VA_Fairfaxcity","VA_FairfaxC",newcounty),
         newcounty=ifelse(newcounty=="MD_Baltimorecity","MD_BaltimoreC",newcounty),
         newcounty=ifelse(newcounty=="MO_StLouiscity","MO_StLouisC",newcounty),
         newcounty=ifelse(newcounty=="NV_CarsonCity","NV_CarsonC",newcounty),
         newcounty=ifelse(newcounty=="VA_CharlesCity","VA_CharlesC",newcounty),
         newcounty=ifelse(newcounty=="VA_Franklincity","VA_FranklinC",newcounty),
         
         newcounty=ifelse(newcounty=="AK_AnchorageMunicipality","AK_Anchorage",newcounty),
         newcounty=ifelse(str_detect(newcounty,"MatanuskaSusitna"),"AK_Matanuska",newcounty),
         newcounty=ifelse(newcounty=="AK_ValdezCordovaCensusArea","AK_Valdez",newcounty),
         newcounty=ifelse(newcounty=="AK_NomeCensusArea","AK_Nome",newcounty),
         newcounty=ifelse(str_detect(newcounty,"AK_Juneau"),"AK_Juneau",newcounty),
         newcounty=ifelse(str_detect(newcounty,"Kenai"),"AK_Kenai",newcounty),
         newcounty=ifelse(newcounty=="AK_SoutheastFairbanksCensusArea","AK_Southeast",newcounty),
         newcounty=ifelse(str_detect(newcounty,"FairbanksNorthStar"),"AK_Fairbanks",newcounty),
         newcounty=ifelse(str_detect(newcounty,"Kodiak"),"AK_Kodiak",newcounty),
         
         newcounty=ifelse(newcounty=="VA_IsleofWight","VA_IsleOfWight",newcounty),
         newcounty=ifelse(newcounty=="MT_LewisandClark","MT_LewisClark",newcounty),
         newcounty=ifelse(str_detect(newcounty,"LA_StJohn"),"LA_StJohnBaptist",newcounty),
         newcounty=ifelse(newcounty=="MN_LakeoftheWoods","MN_LakeofWoods",newcounty),
         newcounty=ifelse(newcounty=="NM_DoñaAna","NM_DonaAna",newcounty)
         )%>%
  mutate(newcounty=gsub("city","",newcounty),
         newcounty=gsub("City","",newcounty),
         newcounty=gsub("Parish","",newcounty),
         newcounty=gsub("Borough","",newcounty))


###  import Serosurvey data
covid0 <- read_excel(paste0(indir,"NIHCOVID19AntibodySt_DATA_CODES_2020-10-22_1119_NP.xlsx"))
covid <- covid0%>%
  mutate(state=ifelse(participant_id==22122,"AL",
                      ifelse(participant_id==52502,"VA",state)))%>%
  mutate(CTY=substr(county, 4, nchar(county)),
         CTY = gsub("_","",CTY),
         newcounty=paste0(state,"_",CTY))%>%
  mutate(newcounty=ifelse(str_detect(newcounty,"MD_Baltimorecity"),"MD_BaltimoreC",
                          ifelse(str_detect(newcounty,"MO_StLouiscity"),"MO_StLouisC",
                                 ifelse(str_detect(newcounty,"NV_CarsonCity"),"NV_CarsonC",
                                        ifelse(str_detect(newcounty,"VA_CharlesCity"),"VA_CharlesC",newcounty)))))%>%
  mutate(newcounty=gsub("City","",newcounty),
         newcounty=gsub("city","",newcounty))


combine <- covid%>%
  left_join(census, by="newcounty")%>%
  select(-county_fromCensus,-Abb, -CTY, -newcounty, -STNAME)


### input data used to defined seropositivity cutoffs
dta0<-read_excel(paste0(indir,"finaldataforEUA.xlsx"),skip=2)

TruePos<-dta0%>%
  select(IgG_Spike = `Spike...3`, IgG_RBD = `RBD...4`,
         IgM_Spike = `Spike...5`, IgM_RBD = `RBD...6`)%>%
  .[1:56,]%>%
  mutate(subjid = paste0("TP",1:n()),
         IgG_Spike = as.numeric(IgG_Spike),
         Real="Pos")

TrueNeg<-dta0%>%
  select(IgG_Spike = `Spike...12`, IgG_RBD = `RBD...13`,
         IgM_Spike = `Spike...14`, IgM_RBD = `RBD...15`)%>%
  .[1:300,]%>%
  mutate(subjid = paste0("NP",1:n()),
         IgG_Spike = as.numeric(IgG_Spike),
         Real="Neg")

CutOff <- TrueNeg%>%
  summarise_at(.vars=vars(IgG_Spike:IgM_RBD), .funs = list(MEAN =mean, STD=sd))
CutOffMean <- select(CutOff, contains("MEAN"))
CutOffSTD <- select(CutOff, contains("STD"))


CP_2SD <- data.frame(Ig = c("IgG Spike","IgG RBD","IgM Spike","IgM RBD"),
                     CP = as.numeric(c(CutOffMean[1]+2*CutOffSTD[1],
                                       CutOffMean[2]+2*CutOffSTD[2],
                                       CutOffMean[3]+2*CutOffSTD[3],
                                       CutOffMean[4]+2*CutOffSTD[4])),
                     stringsAsFactors = FALSE)

CP_3SD <- data.frame(Ig = c("IgG Spike","IgG RBD","IgM Spike","IgM RBD"),
                     CP = as.numeric(c(CutOffMean[1]+3*CutOffSTD[1],
                                       CutOffMean[2]+3*CutOffSTD[2],
                                       CutOffMean[3]+3*CutOffSTD[3],
                                       CutOffMean[4]+3*CutOffSTD[4])),
                     stringsAsFactors = FALSE)

CP_4SD <- data.frame(Ig = c("IgG Spike","IgG RBD","IgM Spike","IgM RBD"),
                     CP = as.numeric(c(CutOffMean[1]+4*CutOffSTD[1],
                                       CutOffMean[2]+4*CutOffSTD[2],
                                       CutOffMean[3]+4*CutOffSTD[3],
                                       CutOffMean[4]+4*CutOffSTD[4])),
                     stringsAsFactors = FALSE)


AB <- combine%>%
  select(participant_id, IgG_Spike=spike_igg, IgG_RBD=rbd_igg, 
         IgM_Spike=spike_igm, IgM_RBD=rbd_igm)%>%
  mutate(AnySinglePositive_4SD = ifelse(IgG_Spike> as.numeric(CP_4SD$CP[1]) |
                                          IgG_RBD> as.numeric(CP_4SD$CP[2])|
                                          IgM_Spike> as.numeric(CP_4SD$CP[3]) |
                                          IgM_RBD> as.numeric(CP_4SD$CP[4]),"Pos","Neg"),
         
         IgG.RBD_IgG.Spike_2SD=ifelse(IgG_RBD> as.numeric(CP_2SD$CP[2]) & IgG_Spike> as.numeric(CP_2SD$CP[1]),"Pos","Neg"),
         IgG.RBD_IgM.Spike_2SD=ifelse(IgG_RBD> as.numeric(CP_2SD$CP[2]) & IgM_Spike> as.numeric(CP_2SD$CP[3]),"Pos","Neg"),
         IgM.RBD_IgG.Spike_2SD=ifelse(IgM_RBD> as.numeric(CP_2SD$CP[4]) & IgG_Spike> as.numeric(CP_2SD$CP[1]),"Pos","Neg"),
         IgM.RBD_IgM.Spike_2SD=ifelse(IgM_RBD> as.numeric(CP_2SD$CP[4]) & IgM_Spike> as.numeric(CP_2SD$CP[3]),"Pos","Neg"),
         IgG.RBD_IgG.Spike_3SD=ifelse(IgG_RBD> as.numeric(CP_3SD$CP[2]) & IgG_Spike> as.numeric(CP_3SD$CP[1]),"Pos","Neg"),
         IgG.RBD_IgM.Spike_3SD=ifelse(IgG_RBD> as.numeric(CP_3SD$CP[2]) & IgM_Spike> as.numeric(CP_3SD$CP[3]),"Pos","Neg"),
         IgM.RBD_IgG.Spike_3SD=ifelse(IgM_RBD> as.numeric(CP_3SD$CP[4]) & IgG_Spike> as.numeric(CP_3SD$CP[1]),"Pos","Neg"),
         IgM.RBD_IgM.Spike_3SD=ifelse(IgM_RBD> as.numeric(CP_3SD$CP[4]) & IgM_Spike> as.numeric(CP_3SD$CP[3]),"Pos","Neg"),
         
         IgM.RBD_IgG.RBD_2SD=ifelse(IgM_RBD> as.numeric(CP_2SD$CP[4]) & IgG_RBD> as.numeric(CP_2SD$CP[2]),"Pos","Neg"),
         IgM.Spike_IgG.Spike_2SD=ifelse(IgM_Spike> as.numeric(CP_2SD$CP[3]) & IgG_Spike> as.numeric(CP_2SD$CP[1]),"Pos","Neg"),
         IgM.RBD_IgG.RBD_3SD=ifelse(IgM_RBD> as.numeric(CP_3SD$CP[4]) & IgG_RBD> as.numeric(CP_3SD$CP[2]),"Pos","Neg"),
         IgM.Spike_IgG.Spike_3SD=ifelse(IgM_Spike> as.numeric(CP_3SD$CP[3]) & IgG_Spike> as.numeric(CP_3SD$CP[1]),"Pos","Neg"),
         
         AnyTwoPos_2SD=ifelse(IgG.RBD_IgG.Spike_2SD=="Pos" | IgG.RBD_IgM.Spike_2SD=="Pos" |
                                IgM.RBD_IgG.Spike_2SD=="Pos" | IgM.RBD_IgM.Spike_2SD=="Pos" |
                                IgM.RBD_IgG.RBD_2SD=="Pos" | IgM.Spike_IgG.Spike_2SD=="Pos","Pos","Neg"),
         AnyTwoPos_3SD=ifelse(IgG.RBD_IgG.Spike_3SD=="Pos" | IgG.RBD_IgM.Spike_3SD=="Pos" |
                                IgM.RBD_IgG.Spike_3SD=="Pos" | IgM.RBD_IgM.Spike_3SD=="Pos" |
                                IgM.RBD_IgG.RBD_3SD=="Pos" | IgM.Spike_IgG.Spike_3SD=="Pos","Pos","Neg"),
         
         
         OneSpike_OneRBD_2SD=ifelse(IgG.RBD_IgG.Spike_2SD=="Pos" | IgG.RBD_IgM.Spike_2SD=="Pos" |
                                      IgM.RBD_IgG.Spike_2SD=="Pos" | IgM.RBD_IgM.Spike_2SD=="Pos","Pos","Neg"),
         
         OneSpike_OneRBD_3SD=ifelse(IgG.RBD_IgG.Spike_3SD=="Pos" | IgG.RBD_IgM.Spike_3SD=="Pos" |
                                      IgM.RBD_IgG.Spike_3SD=="Pos" | IgM.RBD_IgM.Spike_3SD=="Pos","Pos","Neg")
  )%>%
  select(participant_id, 
         AnySinglePositive_4SD,                     # 1)	Any positive= positive using 4 std
         IgG.RBD_IgG.Spike_2SD,                     # 2)	IgG RBD  and IgG Spike both pos=pos. using  2 std
         OneSpike_OneRBD_2SD,OneSpike_OneRBD_3SD,  # 3)	1 spike and 1 rbd pos=pos using 2 and 3 std (3std as the primary one)
         AnyTwoPos_2SD,AnyTwoPos_3SD               # 4)	Any 2 positives= pos using 2 and 3 std
  )


final.code <- combine%>%
  left_join(AB, by="participant_id")


write.csv(final.code, paste0(indir,"NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.csv"),
          row.names = FALSE,na="")

