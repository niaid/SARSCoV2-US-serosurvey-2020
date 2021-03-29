######################################################################################################################################
# programmer: Yan Li
# Merge data BRFSS2018 and Covid survey data 2020
# date: 03/23/2021
#######################################################################################################################################rm(list = ls())
rm(list = ls())
library(survey)
library(Hmisc)
library(mvtnorm)
library(dplyr)

setwd("/.../")

#-----------read BRFSS data
mydata <- sasxport.get("data/LLCP2018.xpt")

#-----------data massage: age->x.age80, hispanc3->x.hispanc, mrace1-> x.mrace1, in addition, x.ststr, x.psu, x.state, x.llcpwt
BRFSS.varnames=c("sex1","x.age80","x.ageg5yr","x.age65yr","x.age.g", "x.hispanc","x.mrace1","children","educa","renthom1",
             "employ1","hlthpln1", "flushot6","pneuvac4",
             "cvdinfr4","cvdcrhd4","cvdstrk3","asthma3","asthnow","chcscncr","chcocncr","chccopd1",
             "havarth3","addepev2","chckdny1","diabete3","diabage2","x.llcpwt","x.ststr","x.state","x.psu",
             "x.metstat","x.urbstat")
BRFSS_2018 = mydata[,BRFSS.varnames]
brfss0=BRFSS_2018; colnames(brfss0);n.brfss0=nrow(brfss0); sum(brfss0$x.llcpwt)
#---rename with covid variable names
colnames(brfss0)=c("dmsex","dmage","x.ageg5yr","x.age65yr","x.age.g", "dmethnic_y_n","x.mrace1","children_lt_18_house","education_level","homeowner",
                   "employ_educ_status","health_care_coverage", "flu_shot_past_12_most", "pneumococcal_vaccine",
                   "had_mi","had_angina_cad","had_stroke",
                   "had_asthma", "had_astma_still","had_skin_ca","had_any_ca","had_copd_bronchitis",
                   "had_arthritis_etc","had_depression","had_kidney_issues","had_diabetes","had_diabetes_age",
                   "x.llcpwt","x.ststr","x.state","x.psu",
                   "NCHS_Metro.Micro","NCHS_Urban.Rural")

#--BRFSS recode missing to NA and new dummy dmrace___
brfss0$dmsex[brfss0$dmsex==7|brfss0$dmsex==9]<-NA
brfss0$dmethnic_y_n[brfss0$dmethnic_y_n==9]<-NA

brfss0$x.mrace1[brfss0$x.mrace1==77|brfss0$x.mrace1==99]<-NA;
brfss0$children_lt_18_house[brfss0$children_lt_18_house==99]<-NA; brfss0$children_lt_18_house[brfss0$children_lt_18_house==88]=0
brfss0$education_level[brfss0$education_level==9]<-NA
brfss0$homeowner[brfss0$homeowner==7|brfss0$homeowner==9]<-NA
brfss0$employ_educ_status[brfss0$employ_educ_status==9]<-NA
brfss0$health_care_coverage[brfss0$health_care_coverage==7|brfss0$health_care_coverage==9]<-NA

brfss0$flu_shot_past_12_most[brfss0$flu_shot_past_12_most==7|brfss0$flu_shot_past_12_most==9]<-NA
brfss0$pneumococcal_vaccine[brfss0$pneumococcal_vaccine==7|brfss0$pneumococcal_vaccine==9]<-NA

brfss0$had_mi[brfss0$had_mi==7|brfss0$had_mi==9]<-NA
brfss0$had_angina_cad[brfss0$had_angina_cad==7|brfss0$had_angina_cad==9]<-NA
brfss0$had_stroke[brfss0$had_stroke==7|brfss0$had_stroke==9]<-NA
brfss0$had_asthma[brfss0$had_asthma==7|brfss0$had_asthma==9]<-NA
brfss0$had_astma_still[brfss0$had_astma_still==7|brfss0$had_astma_still==9]<-NA
brfss0$had_astma_still[brfss0$had_asthma==2]<-2

brfss0$had_skin_ca[brfss0$had_skin_ca==7|brfss0$had_skin_ca==9]<-NA
brfss0$had_any_ca[brfss0$had_any_ca==7|brfss0$had_any_ca==9]<-NA
brfss0$had_copd_bronchitis[brfss0$had_copd_bronchitis==7|brfss0$had_copd_bronchitis==9]<-NA
brfss0$had_arthritis_etc[brfss0$had_arthritis_etc==7|brfss0$had_arthritis_etc==9]<-NA
brfss0$had_depression[brfss0$had_depression==7|brfss0$had_depression==9]<-NA
brfss0$had_kidney_issues[brfss0$had_kidney_issues==7|brfss0$had_kidney_issues==9]<-NA
brfss0$had_diabetes[brfss0$had_diabetes==7|brfss0$had_diabetes==9]<-NA
brfss0$had_diabetes_age[brfss0$had_diabetes_age==98|brfss0$had_diabetes_age==99]<-NA

brfss0$agegr3=cut(brfss0$dmage, breaks=c(18, 45, 70, 95), right = FALSE);table(brfss0$dmage);table(brfss0$agegr3)
brfss0$urban.rural<-NA
sum(is.na(brfss0$NCHS_Metro.Micro))
sum(is.na(brfss0$NCHS_Urban.Rural))

######################------------------
#-----------read covid data
covid.mis=read.csv(file="data/NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.csv",header = TRUE, sep=",");colnames(covid.mis)

covid=covid.mis;n.covid = nrow(covid); n.covid

#--COVID recode missing to NA, text to number, 0/1 <- 2/1, new x.dmrace1
covid$dmethnic_y_n[covid$dmethnic_y_n==0]<-2;

race.mat=cbind(covid$dmrace___w,covid$dmrace___b,covid$dmrace___ai,covid$dmrace___as,covid$dmrace___pi,covid$dmrace___o)#[1:100,]
covid$x.mrace1=ifelse(apply(race.mat,1,sum)>1,7,123456)
covid$x.mrace1[covid$x.mrace1==123456]=(covid$dmrace___w*1+covid$dmrace___b*2+covid$dmrace___ai*3+covid$dmrace___as*4+covid$dmrace___pi*5+covid$dmrace___o*6)[covid$x.mrace1==123456]
covid$x.mrace1[apply(race.mat,1,sum)==0]=NA
covid$dmrace___w[covid$dmrace___w==0]<-2;covid$dmrace___b[covid$dmrace___b==0]<-2;covid$dmrace___ai[covid$dmrace___ai==0]<-2;covid$dmrace___as[covid$dmrace___as==0]<-2;covid$dmrace___pi[covid$dmrace___pi==0]<-2;covid$dmrace___o[covid$dmrace___o==0]<-2;

covid$health_care_coverage[covid$health_care_coverage==88]<-NA;
covid$health_care_coverage[covid$health_care_coverage==0]<-2

#--recode text to numbers for the state variable
covid$x.state = recode(covid$state, "AK"=2,"AL"=1,  "AR"=5, "AZ"=4,   "CA"=6,  "CO"=8,  "CT"=9 , "DC"=11,  "DE"=10,  "FL"=12,  "GA"=13,  "HI"=15,
       "IA"=19,  "ID"=16,  "IL"=17,  "IN"=18,  "KS"=20,  "KY"=21,  "LA"=22,  "MA"=25,  "MD"=24,  "ME"=23,  "MI"=26,  "MN"=27,  "MO"=29,  "MS"=  28,
       "MT"=30,  "NC"=37,  "ND"=38,  "NE"=31 , "NH"=33,  "NJ"=34, "NM"=35,  "NV"=32 , "NY"=36 , "OH"=39,  "OK"=40 , "OR"=41 , "PA"=42 , "PR"= 72 ,
       "RI"=44 , "SC" =45, "SD"= 46, "TN"=47 , "TX"=48 , "UT"=49 , "VA"=51 , "VT"=50,  "WA"=53,  "WI"=55 , "WV"=54,  "WY"=56 )
sum(table(covid$state));sum(table(covid$x.state)); table(covid$dmsex)

covid$dmsex<-recode(covid$dmsex, 'M'=1, 'F'=2)
covid$NCHS_Metro.Micro<-recode(covid$NCHS_Metro.Micro,'Metro'=1,'Micro'=2)
covid$NCHS_Urban.Rural<-recode(covid$NCHS_Urban.Rural,'Urban'=1,'Rural'=2)

covid$education_level<-recode(covid$education_level, 'N'=1, 'E'=2, 'HS'=3, 'GHS'=4, 'C'=5, 'GC'=6)
covid$homeowner<-recode(covid$homeowner,'OWN'=1,'RENT'=2,'OTHER'=3)
covid$agegr3=cut(covid$dmage, breaks=c(18, 45, 70, 95), right = FALSE);table(covid$dmage);table(covid$agegr3)

covid$flu_shot_past_12_most[covid$flu_shot_past_12_most==88]<-NA;covid$flu_shot_past_12_most[covid$flu_shot_past_12_most==0]<-2
covid$pneumococcal_vaccine[covid$pneumococcal_vaccine==88]<-NA;covid$pneumococcal_vaccine[covid$pneumococcal_vaccine==0]<-2

covid$had_mi[covid$had_mi==99]<-NA;covid$had_mi[covid$had_mi==0]<-2
covid$had_angina_cad[covid$had_angina_cad==99]<-NA;covid$had_angina_cad[covid$had_angina_cad==0]<-2
covid$had_stroke[covid$had_stroke==99]<-NA;covid$had_stroke[covid$had_stroke==0]<-2
covid$had_asthma[covid$had_asthma==99]<-NA;covid$had_asthma[covid$had_asthma==0]<-2

covid$had_astma_still<-2; covid$had_astma_still[covid$had_astma_still___1==1]<-1;covid$had_astma_still[covid$had_astma_still___99==1]<-NA;covid$had_astma_still[covid$had_asthma==NA]<-NA;

covid$had_skin_ca[covid$had_skin_ca==99]<-NA;covid$had_skin_ca[covid$had_skin_ca==0]<-2
covid$had_any_ca[covid$had_any_ca==99]<-NA;covid$had_any_ca[covid$had_any_ca==0]<-2
covid$had_copd_bronchitis[covid$had_copd_bronchitis==99]<-NA; covid$had_copd_bronchitis[covid$had_copd_bronchitis==0]<-2
covid$had_arthritis_etc[covid$had_arthritis_etc==99]<-NA;covid$had_arthritis_etc[covid$had_arthritis_etc==0]<-2
covid$had_depression[covid$had_depression==99]<-NA;covid$had_depression[covid$had_depression==0]<-2
covid$had_kidney_issues[covid$had_kidney_issues==99]<-NA;covid$had_kidney_issues[covid$had_kidney_issues==0]<-2
covid$had_diabetes[covid$had_diabetes==99]<-NA; covid$had_diabetes[covid$had_diabetes==0]<-2

#################--------Combining two datasets ------------- #################
A=c(rep(1,n.covid),rep(0,n.brfss0))
covid.y=c("AnySinglePositive_4SD","IgG.RBD_IgG.Spike_2SD","OneSpike_OneRBD_2SD",
          "OneSpike_OneRBD_3SD",	"AnyTwoPos_2SD",	"AnyTwoPos_3SD")

brfss.y=matrix(NA,n.brfss0,length(covid.y))
colnames(brfss.y)=covid.y
y=rbind(covid[,covid.y],brfss.y)

wt=c(rep(1,n.covid),brfss0$x.llcpwt)
psu=c(seq(1,n.covid),brfss0$x.psu)
str =c(rep(1,n.covid),brfss0$x.ststr)
dsgn.vars = cbind(wt,psu,str)

#---common covariates
com.covx=c("dmsex", "dmethnic_y_n", "x.mrace1",
           "children_lt_18_house","education_level","homeowner","employ_educ_status", "health_care_coverage",
           "flu_shot_past_12_most", "pneumococcal_vaccine",
           "had_mi","had_angina_cad","had_stroke",
           "had_asthma", "had_astma_still", 
           "had_skin_ca","had_any_ca","had_copd_bronchitis","had_arthritis_etc","had_depression","had_kidney_issues","had_diabetes",
           "agegr3","x.state","dmage","NCHS_Metro.Micro", "NCHS_Urban.Rural","urban.rural") 
num.covx=length(com.covx); num.covx
x.mat<-rbind(covid[,com.covx],brfss0[,com.covx])
data.prpn <- data.frame(A,x.mat,dsgn.vars, y)
colnames(data.prpn);dim(data.prpn)

#========== create new covariates (region, cvd, immun, pulg, vaccine) from existing common variables
id.1=which(data.prpn$x.state %in% c(9, 23, 25, 33, 44, 50, 34, 36, 42))
id.2=which(data.prpn$x.state %in% c(17,18,26,39,55,19,27))
id.3=which(data.prpn$x.state %in% c(10,11,13,24,37,45,51,54,21,47))
id.4=which(data.prpn$x.state %in% c(20,29,12,1,28,5,22,40))
id.5=which(data.prpn$x.state %in% c(31,38,46,48,4,8,16,30,35,49,56))
id.6=which(data.prpn$x.state %in% c(32,2,6,15,41,53))
data.prpn$region=NA;data.prpn$region[id.1]=1;data.prpn$region[id.2]=2
data.prpn$region[id.3]=3;data.prpn$region[id.4]=4;data.prpn$region[id.5]=5;data.prpn$region[id.6]=6
#data.prpn <- data.prpn[!is.na(data.prpn$region),]
dim(data.prpn)

data.prpn$cvd=NA; data.prpn$cvd[which(data.prpn$had_mi==1 | data.prpn$had_angina_cad==1 | data.prpn$had_stroke==1)]=1
data.prpn$cvd[which(data.prpn$had_mi==2 & data.prpn$had_angina_cad==2 & data.prpn$had_stroke==2)]=2

data.prpn$pulg=NA;data.prpn$pulg[which(data.prpn$had_asthma==1| data.prpn$had_astma_still==1| data.prpn$had_copd_bronchitis==1)]=1
data.prpn$pulg[which(data.prpn$had_asthma==2 & data.prpn$had_astma_still==2 & data.prpn$had_copd_bronchitis==2)]=2
#mean(data.prpn$pulg[data.prpn$A==0],weights=data.prpn$wt[data.prpn$A==0],na.rm = T)

data.prpn$immun=NA;data.prpn$immun[which(data.prpn$had_any_ca==1 | data.prpn$had_arthritis_etc==1)]=1
data.prpn$immun[which(data.prpn$had_any_ca==2 & data.prpn$had_arthritis_etc==2)]=2

data.prpn$vaccine=NA;data.prpn$vaccine[which(data.prpn$flu_shot_past_12_most==1 | data.prpn$pneumococcal_vaccine==1)]=1
data.prpn$vaccine[which(data.prpn$flu_shot_past_12_most==2 & data.prpn$pneumococcal_vaccine==2)]=2

dim(data.prpn)

write.csv(data.prpn,file="data/BRFSS2018COVID9028.csv")







