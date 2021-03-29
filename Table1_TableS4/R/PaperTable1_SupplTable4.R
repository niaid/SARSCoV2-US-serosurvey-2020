######################################################################################################################################
# programmer: Yan Li
# Generate paper Table 1 and suppl. Table 4
# date: 03/23/2021
######################################################################################################################################

rm(list=ls())
library(survey)
library(Hmisc)
library(mvtnorm)
library(dplyr)
library(meanEst)
setwd("/.../")

data.prpn = read.table(file="real data analysis/data/BRFSS2018COVID9028.csv",header = T,sep=',')

#---Outcome of COVID---
covid.y=c("AnySinglePositive_4SD","IgG.RBD_IgG.Spike_2SD","OneSpike_OneRBD_2SD",
          "OneSpike_OneRBD_3SD",	"AnyTwoPos_2SD",	"AnyTwoPos_3SD")
y.mat = sapply(1:length(covid.y), function(i) recode(data.prpn[,covid.y[i]],'Neg'=0,'Pos'=1))
colnames(y.mat)=covid.y
data.prpn$yy = y.mat

data.prpn$RaceGrp3 = recode(data.prpn$x.mrace1, "1"=1,  "2"=2, "4"= 3,"3"=3,"5"=3,"6"=3,"7"=3)
data.prpn$agegr3 = recode(data.prpn$agegr3, "[18,45)"=1, "[45,70)"=2, "[70,95)"=3)
data.prpn$children3 = NA; data.prpn$children3[data.prpn$children_lt_18_house==0]=0; data.prpn$children3[data.prpn$children_lt_18_house==1|data.prpn$children_lt_18_house==2]="1-2";data.prpn$children3[data.prpn$children_lt_18_house>=3]=">=3";
data.prpn$children2 = NA; data.prpn$children2[data.prpn$children_lt_18_house==0]=2; data.prpn$children2[data.prpn$children_lt_18_house>0]=1;
data.prpn$children7 = data.prpn$children_lt_18_house; data.prpn$children7[data.prpn$children_lt_18_house>5]=6
data.prpn$educ3 =NA; data.prpn$educ3[data.prpn$education_level<=4]="<=HS" ; data.prpn$educ3[data.prpn$education_level==5]="College";data.prpn$educ3[data.prpn$education_level==6]="College Graduate"
data.prpn$employ3 =NA; data.prpn$employ3[data.prpn$employ_educ_status<3]= "emp or self-emp"; data.prpn$employ3[data.prpn$employ_educ_status==3|data.prpn$employ_educ_status==4|data.prpn$employ_educ_status==5]="unemp"; data.prpn$employ3[data.prpn$employ_educ_status>5]="NLF"
data.prpn$urban.rural2 = recode(data.prpn$urban.rural, "mostly urban"=2, "mostly rural"=1, "completely rural"=1)

n0.tot=nrow(data.prpn)

###---delete observations with missing outcome or in singltons------------------------

mis.y= matrix(is.na(data.prpn$yy),nrow(data.prpn),)
dat.covidbrfss = data.prpn[!(mis.y[,1])| data.prpn$A==0,] 

str.1=which(table(data.prpn$str)==1)
if(length(names(str.1))>0) dat.covidbrfss = dat.covidbrfss[(dat.covidbrfss$str!=names(str.1)),]

#----
covid0=dat.covidbrfss[dat.covidbrfss$A==1,];n.covid0=nrow(covid0)
brfss0=dat.covidbrfss[dat.covidbrfss$A==0,];n.brfss0=nrow(brfss0)

brfss0$had_diabetes = recode(brfss0$had_diabetes, "1"=1,  "2"=2, "4"= 2,"3"=2)
dat.covidbrfss[dat.covidbrfss$A==0,'had_diabetes']=brfss0$had_diabetes
table(covid0$had_diabetes)/n.covid0;table(brfss0$had_diabetes)/n.brfss0

###---
x1.var<-c("region","agegr3","dmsex","NCHS_Urban.Rural","RaceGrp3","dmethnic_y_n")
x2.var<-c("children2","educ3","homeowner","employ3",
          "health_care_coverage","vaccine","cvd","pulg","immun","had_diabetes")
x.var <- c(x1.var, x2.var)

# ################### DESIGNS of dat.covidbrfss data ##################################
prpn.dsgn=svydesign(data=dat.covidbrfss, ids = ~psu, strata = ~str, weights=~wt, nest=TRUE)
no.dsgn = svydesign(ids=~1, strata = NULL, weights = ~1, data = prpn.dsgn$variables)

########################### Table 1: compare x dist in brfss and covid ###################
dist.x.y0 <- vector(mode = "list", length = length(x.var))
for (i in c(1:length(x.var))){
  cnt=svytable(as.formula(paste0("~",x.var[i],"+","A")),  no.dsgn)
  pct0=(prop.table(svytable(as.formula(paste0("~",x.var[i],"+","A")),  no.dsgn),2)*100)
  pctw=(prop.table(svytable(as.formula(paste0("~",x.var[i],"+","A")),  prpn.dsgn),2)*100)
  dist.x.y0[[i]]=cbind(cnt.brfss=cnt[,1],
                 pct0.brfss=pct0[,1],
                 pctw.brfss=pctw[,1],
                 cnt.covid=cnt[,2],
                 pct.covid=pctw[,2])
}
names(dist.x.y0)=x.var
dist.x.y0

########################## Supplemental Table 4: x dist in Census   #######################
#---------read census data
census=read.csv(file="real data analysis/data/Census3AgeGroupsWithNames_UrbanRural.csv",header = TRUE, sep=",")
colnames(census)=c("seqn","x.state","county","state","ctyname",
                   "agegr3","dmsex","dmethnic_y_n","race","popsize",
                   "R10.000","AGEGRPNAME", "RACENAME","HISPNAME","X2015.Geography.Name","urban.rural" )

census$dmsex = recode(census$dmsex,"M"=1,"F"=2);
census$dmethnic_y_n = recode(census$dmethnic_y_n, "H"=1, "NH"=2);
census$RaceGrp3 = recode(census$RACENAME,"White Alone"=1, "Black Alone" =2, "Asian Alone"=3,
                         "American Indian Alone"=3,  "Pacific Islander Alone"=3, "Two or More"=3)
census$urban.rural2 = recode(census$urban.rural,'completely rural'=1,'mostly rural'=1, 'mostly urban'=2)

#---------create new region variables in census
id.r1=which(census$state %in% c("Connecticut","Maine", "Massachusetts", "New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania"))
id.r2=which(census$state %in% c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Minnesota"))
id.r3=which(census$state %in% c("Delaware","District of Columbia","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Kentucky","Tennessee"))
id.r4=which(census$state %in% c("Kansas","Missouri", "Florida", "Alabama","Mississippi", "Arkansas","Louisiana","Oklahoma"))
id.r5=which(census$state %in% c("Nebraska", "North Dakota", "South Dakota","Texas", "Arizona","Colorado", "Idaho", "Montana", "New Mexico","Utah","Wyoming"))
id.r6=which(census$state %in% c("Nevada", "Alaska", "California", "Hawaii", "Oregon", "Washington"))
census$region=NA;census$region[id.r1]=1;census$region[id.r2]=2
census$region[id.r3]=3;census$region[id.r4]=4;census$region[id.r5]=5;census$region[id.r6]=6

varname.census = c("region", "agegr3", "dmsex","urban.rural2","RaceGrp3","dmethnic_y_n")
census.dsgn=svydesign(data=census, ids = ~1,  weights=~popsize)
mdist.N.census = sapply(1:length(varname.census), function (i)
  rbind(
        N=svytable(as.formula(paste0("~",varname.census[i])), design=census.dsgn),
        prop= round(prop.table(svytable(as.formula(paste0("~",varname.census[i])),  design=census.dsgn))*100,2)
        )
)
names(mdist.N.census) = varname.census
mdist.N.census

###################------- construct svyglm() object saved as RDS data âlgtreg.x.sel.rds"--------################################

fm.sel=as.formula("A ~ factor(region) + factor(agegr3) + dmsex + factor(RaceGrp3) +
                  dmethnic_y_n + NCHS_Urban.Rural + children2 + factor(educ3) +
                  homeowner + factor(employ3) + health_care_coverage + vaccine +
                  cvd + pulg + immun + had_diabetes + factor(region):factor(agegr3) +
                  factor(region):dmsex + factor(region):factor(RaceGrp3) +
                  factor(region):dmethnic_y_n + factor(region):NCHS_Urban.Rural +
                  factor(region):homeowner + factor(region):factor(employ3) +
                  factor(region):cvd + factor(agegr3):dmsex + factor(agegr3):factor(RaceGrp3) +
                  factor(agegr3):dmethnic_y_n + factor(agegr3):NCHS_Urban.Rural +
                  factor(agegr3):factor(educ3) + factor(agegr3):homeowner +
                  factor(agegr3):factor(employ3) + factor(agegr3):vaccine +
                  factor(agegr3):immun + factor(agegr3):had_diabetes + dmsex:factor(RaceGrp3) +
                  dmsex:children2 + dmsex:factor(employ3) + dmsex:vaccine +
                  factor(RaceGrp3):dmethnic_y_n + factor(RaceGrp3):NCHS_Urban.Rural +
                  factor(RaceGrp3):children2 + factor(RaceGrp3):factor(educ3) +
                  factor(RaceGrp3):factor(employ3) + factor(RaceGrp3):vaccine +
                  factor(RaceGrp3):pulg + factor(RaceGrp3):immun + dmethnic_y_n:NCHS_Urban.Rural +
                  dmethnic_y_n:children2 + dmethnic_y_n:factor(educ3) + dmethnic_y_n:homeowner +
                  dmethnic_y_n:health_care_coverage + dmethnic_y_n:vaccine +
                  NCHS_Urban.Rural:immun + NCHS_Urban.Rural:had_diabetes +
                  children2:homeowner + children2:factor(employ3) + factor(educ3):factor(employ3) +
                  factor(educ3):health_care_coverage + factor(educ3):vaccine +
                  factor(educ3):pulg + factor(educ3):immun + factor(educ3):had_diabetes +
                  homeowner:immun + factor(employ3):pulg + health_care_coverage:vaccine +
                  health_care_coverage:pulg + cvd:immun")

lgtreg.x.sel = svyglm(fm.sel, family = binomial, design = prpn.dsgn)
               
