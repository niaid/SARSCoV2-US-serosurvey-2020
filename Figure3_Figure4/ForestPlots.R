### Purpose: Create Figure 3 and Figure 4
### Date: Dec 8th 2020
### Programmer: Jing Wang

# install package PSwtEst
install.packages("PSwtEst_0.1.0.tar.gz", repos = NULL, type="source")

# import Fay's function used to adjust sensitivity and specificity
source("WprevSeSp_MF.R")


library(dplyr)
library(tidyr)
library(readxl)


####################################
###
###  Input Data
###
####################################

# input svyglm() object which estimates the propensity of being included in the target data
lgt.objs = readRDS(file="lgtreg.x.sel.rds") 

# create alp and kw pseudoweights
covid.wt.x.sel<- pseudoweights(lgt.objs, mth='kw', TD = TRUE)
#covid.wt.x.sel <- readRDS(file="covid2020_x.sel.rds")  


### input covid survey data
dta_code <- read_excel("NIHCOVID19AntibodySt_DATA_CODES_2020-10-27.xlsx",sheet=1)%>%
  mutate(X=1:n())%>%
  rowwise()%>% 
  mutate(sumRace = sum(c_across(dmrace___w:dmrace___o)),
         sumWork = sum(c_across(work_site___h:work_site___o)))

#########################################
###
###   data manipulation and data merge
###
##########################################
dta_code_final <- dta_code%>%
  mutate(State_final=state,
         Age_final=dmage,
         Sex_final=ifelse(dmsex=="F","Female",
                          ifelse(dmsex=="M","Male",NA)),
         Race_final = ifelse(sumRace<2 & dmrace___b==1, "Black Only",
                             ifelse(sumRace<2 & dmrace___w==1, "White Only",
                                    ifelse(sumRace<2 & dmrace___ai==1, "American Indian or Alaska Native Only",
                                           ifelse(sumRace<2 & dmrace___as==1, "Asian Only",
                                                  ifelse(sumRace<2 & dmrace___pi==1, "Pacific Islander Only",
                                                         ifelse(sumRace<2 & dmrace___o==1, "Others",
                                                                ifelse(sumRace>=2, "Multiple Races",NA))))))),
         Ethnicity_final = ifelse(dmethnic_y_n==1,"Yes",
                                  ifelse(dmethnic_y_n==0,"No",NA)),
         Had_covid_exposure_final = ifelse(covid19_known_exposure==1,"Yes",
                                           ifelse(covid19_known_exposure==0, "No",NA)),
         Sick_since_Jan1_final = ifelse(sick_since_jan_1_2020==1,"Yes",
                                        ifelse(sick_since_jan_1_2020==0,"No",NA)),
         
         Travelled_out_final = ifelse(travelled_out_since_oct_19==1,"Yes",
                                      ifelse(travelled_out_since_oct_19==0,"No",NA)),
         Direct_contact_final = ifelse(direct_contact==1,"Yes",
                                       ifelse(direct_contact==0,"No",NA)),
         Work_site_final = ifelse(work_site___h==1 & sumWork==1,"At home only",
                                  ifelse(work_site___w==1 & sumWork==1, "At work only",
                                         ifelse(work_site___h==1 & work_site___w==1, "At home or at work",
                                                ifelse(work_site___99==1,"Not currently working",
                                                       ifelse(work_site___o==1,"Other",NA))))),
         
         Had_children_lt18yr_final = ifelse(children_lt_18_house>0, "Yes","No"),
         Education_final = case_when(
           education_level=="GC" ~ "College graduate",
           education_level=="C" ~ "Some college or technical school",
           education_level %in% c("E","HS","GHS","N") ~ "<= High school"
         ),
         Homeowner_final=homeowner,
         Employ_final = case_when(
           employ_educ_status %in% c(1,2) ~ "Employed",
           employ_educ_status %in% c(3,4,5) ~ "Unemployed",
           employ_educ_status %in% c(6,7,8) ~ "Other"   
         ),
         HealthCare_final = case_when(
           health_care_coverage==1 ~ "Yes",
           health_care_coverage==0 ~ "No",
           health_care_coverage==88 ~ "Don't know"
         ),
         Flushot_final = case_when(
           flu_shot_past_12_most==1 ~ "Yes",
           flu_shot_past_12_most==0 ~ "No",
           flu_shot_past_12_most==88 ~ "Don't know"
         ),
         Pneumo_final = case_when(
           pneumococcal_vaccine==1 ~ "Yes",
           pneumococcal_vaccine==0 ~ "No",
           pneumococcal_vaccine==88 ~ "Don't know"
         ),
         BCG_final = case_when(
           bcg_vaccine==1 ~ "Yes",
           bcg_vaccine==0 ~ "No",
           bcg_vaccine==88 ~ "Don't know"
         ),
         Had_heartAttack_final = case_when(
           had_mi==1 ~ "Yes",
           had_mi==0 ~ "No",
           had_mi==99 ~ "Not Sure"
         ),
         Had_Angina_coronary_final = case_when(
           had_angina_cad==1 ~ "Yes",
           had_angina_cad==0 ~ "No",
           had_angina_cad==99 ~ "Not Sure"
         ),
         Had_stroke_final = case_when(
           had_stroke==1 ~ "Yes",
           had_stroke==0 ~ "No",
           had_stroke==99 ~ "Not Sure"
         ),
         Had_asthma_final = case_when(
           had_asthma==1 ~ "Yes",
           had_asthma==0 ~ "No",
           had_asthma==99 ~ "Not Sure"
         ),
         Asthma_still_final = case_when(
           had_astma_still___1==1 ~ "Yes",
           had_astma_still___0==1 ~ "No",
           had_astma_still___99==1 ~ "Not Sure"
         ),
         Had_skinCancer_final = case_when(
           had_skin_ca==1 ~ "Yes",
           had_skin_ca==0 ~ "No",
           had_skin_ca==99 ~ "Not Sure"
         ),
         Had_anyCancer_final = case_when(
           had_any_ca==1 ~ "Yes",
           had_any_ca==0 ~ "No",
           had_any_ca==99 ~ "Not Sure"
         ),
         Had_copd_bronchitis_final=case_when(
           had_copd_bronchitis==1 ~ "Yes",
           had_copd_bronchitis==0 ~ "No",
           had_copd_bronchitis==99 ~ "Not Sure"
         ), 
         Had_arthritis_etc_final =case_when(
           had_arthritis_etc==1 ~ "Yes",
           had_arthritis_etc==0 ~ "No",
           had_arthritis_etc==99 ~ "Not Sure"
         ), 
         Had_depression_final=case_when(
           had_depression==1 ~ "Yes",
           had_depression==0 ~ "No",
           had_depression==99 ~ "Not Sure"
         ),
         Had_kidney_final =case_when(
           had_kidney_issues==1 ~ "Yes",
           had_kidney_issues==0 ~ "No",
           had_kidney_issues==99 ~ "Not Sure"
         ),
         Had_diabetes_final =case_when(
           had_diabetes==1 ~ "Yes",
           had_diabetes==0 ~ "No",
           had_diabetes==99 ~ "Not Sure"
         ),
         Had_hiv_weakImmune_final = case_when(
           had_hiv_or_med_wkns==1 ~ "Yes",
           had_hiv_or_med_wkns==0 ~ "No",
           had_hiv_or_med_wkns==99 ~ "Not Sure"
         )
  )%>%
  select(X, contains("final"),survey_taken_date)%>%
  full_join(dta_label, by="X")


covid.wt.x.sel<-covid.wt.x.sel%>%
  mutate(AnySinglePositive_4SD01=case_when(
    AnySinglePositive_4SD=="Pos" ~ 1,
    AnySinglePositive_4SD=="Neg" ~0
  ),
  IgG.RBD_IgG.Spike_2SD01=case_when(
    IgG.RBD_IgG.Spike_2SD=="Pos" ~ 1,
    IgG.RBD_IgG.Spike_2SD=="Neg" ~0
  ),
  OneSpike_OneRBD_2SD01=case_when(
    OneSpike_OneRBD_2SD=="Pos" ~ 1,
    OneSpike_OneRBD_2SD=="Neg" ~ 0
  ),
  AnyTwoPos_2SD01=case_when(
    AnyTwoPos_2SD=="Pos" ~ 1,
    AnyTwoPos_2SD=="Neg" ~0
  ),
  AnyTwoPos_3SD01=case_when(
    AnyTwoPos_3SD=="Pos" ~1,
    AnyTwoPos_3SD=="Neg" ~0
  )
  )%>%
  left_join(dta_code_final, by="X")


#######################################################
###
### calculate prop of positive samples --- overall and by group
###
#########################################################
# YVarList <- c("OneSpike_OneRBD_3SD01","AnySinglePositive_4SD01","OneSpike_OneRBD_2SD01",
#               "IgG.RBD_IgG.Spike_2SD01","AnyTwoPos_2SD01","AnyTwoPos_3SD01")
# Se_num <- c(1,1,1,1,1,1)
# Sp_num <- c(1,0.967, 0.993, 1, 0.99,1)


### focus on the primary outcome, "OneSpike_OneRBD_3SD01"
YVarList<-"OneSpike_OneRBD_3SD01"
Se_num <- Sp_num <- 1

ByVarList <- covid.wt.x.sel%>%
  select( region, agegr3, Sex_final, Race_final,Ethnicity_final, urban.rural2,
          Had_covid_exposure_final:Had_hiv_weakImmune_final,
          State_final)%>%
  names()


XY <- covid.wt.x.sel%>%
  select(ByVarList, YVarList)

groups <- c(quo(region), quo(agegr3),quo(Sex_final),quo(Race_final),quo(Ethnicity_final),
            quo(urban.rural2),
            quo(Had_covid_exposure_final),quo(Sick_since_Jan1_final), quo(Travelled_out_final),      
            quo(Direct_contact_final),      quo(Work_site_final),          quo(Had_children_lt18yr_final),
            quo(Education_final),           quo(Homeowner_final),           quo(Employ_final),            
            quo(HealthCare_final),          quo(Flushot_final),            quo(Pneumo_final),         
            quo(BCG_final),                 quo(Had_heartAttack_final),     quo(Had_Angina_coronary_final),
            quo(Had_stroke_final),         quo(Had_asthma_final),          quo(Asthma_still_final),     
            quo(Had_skinCancer_final),      quo(Had_anyCancer_final),       quo(Had_copd_bronchitis_final),
            quo(Had_arthritis_etc_final),   quo(Had_depression_final),      quo(Had_kidney_final),         
            quo(Had_diabetes_final),        quo(Had_hiv_weakImmune_final), quo(State_final))

Ygroup <- c(quo(OneSpike_OneRBD_3SD01),quo(AnySinglePositive_4SD01),
            quo(OneSpike_OneRBD_2SD01),quo(IgG.RBD_IgG.Spike_2SD01),
            quo(AnyTwoPos_2SD01),quo(AnyTwoPos_3SD01))


for (j in 1:length(YVarList)){
  
  ### calculate prop of positive samples by groups
  temp1<-temp<-NULL
  for(i in 1:length(ByVarList)){
    
    temp<-XY%>%
      group_by(!!groups[[i]])%>%
      summarise(n=n(),
                est=mean(!!Ygroup[[j]]),
                se = sd(!!Ygroup[[j]])/sqrt(n))%>%
      mutate(Variable = ByVarList[i])%>%
      rename(Subgroup=!!groups[[i]])%>%
      select(Variable, everything())
    
    temp1 <- rbind(temp1, temp)
  }
  
  ### add overall
  tempAll<-XY%>%
    summarise(n=n(),
              est=mean(!!Ygroup[[j]]),
              se = sd(!!Ygroup[[j]])/sqrt(n))%>%
    mutate(Variable = "Overall",
           Subgroup=NA)%>%
    select(Variable, Subgroup, everything())
  
  out_simple<-as.data.frame(rbind(tempAll, temp1))
  rownames(out_simple)<-NULL
  
  out_simple$Variable <- gsub("_final","",out_simple$Variable)

}

### data frame out_simple was exported and edited in Excel. 
### Excel file name "Copy of PropPositive_SimpleMethod_AllVariables_03Dec2020.xlsx"

############################
###
### calculate mean estimate of seroprev overall and by group
###
##############################
ByVarList <- covid.wt.x.sel%>%
  select( region, agegr3, Sex_final, Race_final,Ethnicity_final, urban.rural2,
          Had_covid_exposure_final:Had_hiv_weakImmune_final,
          State_final)%>%
  names()

out<-out1<-NULL
for(i in 1:length(ByVarList)){

  ### se.est.mean() takes hours to complete the execution for each demographic variables
  ### To save the time, this part was ran on high-throughput computing platform 
  out <- se.est.mean(lgreg = lgt.objs, wtdat=covid.wt.x.sel, YVarList, 
                       mth='kw.postr5.mat', var.PS=T, ByVarList[i])
  out1<-rbind(out1, out)
}


all<-dta0[1,]
dta<-all%>%
  bind_rows(all)%>%
  bind_rows(dta0)%>%
  mutate_at(.vars = vars(n:se.PS),.funs = as.numeric)%>%
  .[,c(1,2,4,6)]%>%
  filter(!is.na(n))

## set parameters
nSe_num=56
nSp_num=300

### KW Method:
out2<-out1<-out <-NULL
for(i in 1:nrow(dta)){
  out<-WprevSeSp(AP=as.numeric(dta[i,3]),
                 nP=as.numeric(dta[i,2]),
                 Se=Se_num,nSe=nSe_num,Sp=Sp_num, nSp=nSp_num, 
                 conf.level=0.95, neg.to.zero=TRUE, 
                 stdErrPrev=as.numeric(dta[i,4]))
  out1<- round(c(as.numeric(out$estimate),out$conf.int[1],out$conf.int[2]),3)
  
  out2<-rbind(out2, out1)
}
out_kw <- as.data.frame(cbind(dta,est=out2[,1],LCL=out2[,2],UCL=out2[,3]))

out_kw[2,]<-NA
out_kw$X[1]<-"Overall"

out_kw[which(out_kw$X=="full_sample"),2:7]<-NA
out_kw$X[which(out_kw$X=="full_sample")]<-c("Region","Age","Sex","Race","Ethnicity",
                                            "Urban/Rural","Direct contact with patients",
                                            "Work site","Have children <18yr live in the household",
                                            "Education level","Own or rent your home","Employment status","Have health care coverage",
                                            "Had flu shot","Had Pheunomia vaccine","Had BCG vaccine","Had a heart attack",
                                            "Had angina or coronary heart disease","Had a stroke","Had asthma","Still have asthma","Had skin cancer",
                                            "Had any other types of cancer",
                                            "Have chronic obstructive pulmonary disease, COPD, emphysema or chronic bronchitis",
                                            "Have some form of arthritis, rheumatoid arthritis, gout, lupus, or fibromyalgia",
                                            "Have a depressive disorder","Have kidney disease","Have diabetes","Have HIV or weakened immune system",
                                            "State","Had a known COVID19 exposure","Sick since 01Jan2020","Traveled outside of the US since Oct 2019"
                                            
                                            
)

final<-out_kw%>%
  mutate(Variables = ifelse(is.na(n),X,""),
         Subgroup=ifelse(!is.na(n),X,""))%>%
  select(Variables, Subgroup, n, EST=OneSpike_OneRBD_3SD01, SE=se.PS, AdjEST=est,LCL, UCL)%>%
  mutate(EST=round(EST,3),
         SE=round(SE,3))

### data frame final was exported and edited in Excel. 
### Excel file name "Copy of Copy of Adjusted_Prevalence_KWmethod_SixQuotaVariable_09Dec2020.xlsx"
###             and "RemoveItemWithSmallSampleSize _Adjusted_Prevalence_KWmethod_NotSixQuotaVariable_09Dec2020.xlsx"



################
### Figure 3
################
dta0 <- read_excel("Copy of Copy of Adjusted_Prevalence_KWmethod_SixQuotaVariable_09Dec2020.xlsx",
                   sheet="OneSpike_OneRBD_3SD")

names(final)<-c("Index","subgroup","n","est_Yan","se_Yan","est","LCL","UCL")
dta0<-final[1:30,1:8]

simple <- read_excel("Copy of PropPositive_SimpleMethod_AllVariables_03Dec2020.xlsx",
                     sheet="OneSpike_OneRBD_3SD01")
names(simple)<-c("Index","subgroup","n","est_Yan","se_Yan","est","LCL","UCL")
simple_6<-simple[1:30,1:8]


dta <- dta0%>%
  mutate(LCL_fmt = sprintf("%.3f", LCL),
         LCL_fmt=ifelse(LCL_fmt=="NA","",LCL_fmt),
         UCL_fmt = sprintf("%.3f", UCL),
         UCL_fmt=ifelse(UCL_fmt=="NA","",UCL_fmt),
         `95% CI` = paste0("(",LCL_fmt,", ",UCL_fmt,")"),
         `95% CI` = ifelse(`95% CI`=="(, )", "",`95% CI`),
         est_fmt = sprintf("%.3f", est),
         est_fmt=ifelse(est_fmt=="NA","",est_fmt))%>%
  bind_cols(Num_pos=simple_6$n*simple_6$est)


axes <- c(0,5,10,15)
Num_row <- nrow(dta)
loc <- c(-46, -44, -16,-5, 35, 47)

attach(dta)

pdf("ForestPlot_KW_OneSpike_OneRBD_3SD_SixQuota_09Dec2020.pdf",width = 16, height = 18)

par(mar=(c(5,1,1,1)))

plot(NA, xlim=c(-45,50), ylim=c(0, nrow(dta0)+2), xlab="", 
     ylab="", main="", axes = FALSE)

for(i in 1:Num_row){
  
  ### 95% bars
  if(i==Num_row){
    points(est[Num_row+1-i]*100, i, pch=19, cex=1.5)
    text(LCL[Num_row+1-i]*100, i, "(")
    text(UCL[Num_row+1-i]*100, i, ")")
    segments(LCL[Num_row+1-i]*100, i, UCL[Num_row+1-i]*100, i,lwd=2)
  }else{
    points(est[Num_row+1-i]*100, i, pch=19, cex=1)
    text(LCL[Num_row+1-i]*100, i, "(")
    text(UCL[Num_row+1-i]*100, i, ")")
    segments(LCL[Num_row+1-i]*100, i, UCL[Num_row+1-i]*100, i)
  }
  
  
  text(loc[1], i, dta$Index[Num_row+1-i], adj=0, font=2)
  text(loc[2], i, dta$subgroup[Num_row+1-i], adj=0)
  text(loc[3], i, dta$Num_pos[Num_row+1-i], adj=0)
  text(loc[4], i, dta$n[Num_row+1-i])
  
  text(loc[5], i, dta$est_fmt[Num_row+1-i])
  text(loc[6], i, dta$`95% CI`[Num_row+1-i])
  
}


segments(est[1]*100, -2, est[1]*100, Num_row, lty=2)

segments(loc[3]-4, nrow(dta0)+1.3, loc[3]+4,nrow(dta0)+1.3)
segments(loc[4]-4, nrow(dta0)+1.3, loc[4]+4,nrow(dta0)+1.3)
segments(loc[5]-3, nrow(dta0)+1.3, loc[5]+3,nrow(dta0)+1.3)
segments(loc[6]-6, nrow(dta0)+1.3, loc[6]+6,nrow(dta0)+1.3)

text(loc[3], nrow(dta0)+2, labels = "Pos No.", font=2)
text(loc[4], nrow(dta0)+2, labels = "Total No.", font=2)
text(loc[5], nrow(dta0)+2, labels = "Estimate", font=2)
text(loc[6], nrow(dta0)+2, labels = "95% CI", font=2)

axis(1, at=c(0,5,10,15,20,25,30), labels=seq(0,30,5)/100)

par(xpd=NA)
text(15, -5, labels = "Estimated prevalence")

detach(dta)

dev.off()


################
### Figure 4
################
### forest plot
dta0 <- read_excel("RemoveItemWithSmallSampleSize _Adjusted_Prevalence_KWmethod_NotSixQuotaVariable_09Dec2020.xlsx",
                   sheet="OneSpike_OneRBD_3SD")

simple_not6<-simple[c(1:2,31:nrow(simple)),1:8]


names(dta0)<-c("Index","subgroup","n","est_Yan","se_Yan","est","LCL","UCL")
dta0<-dta0[,1:8]

dta <- dta0%>%
  mutate(LCL_fmt = sprintf("%.3f", LCL),
         LCL_fmt=ifelse(LCL_fmt=="NA","",LCL_fmt),
         UCL_fmt = sprintf("%.3f", UCL),
         UCL_fmt=ifelse(UCL_fmt=="NA","",UCL_fmt),
         `95% CI` = paste0("(",LCL_fmt,", ",UCL_fmt,")"),
         `95% CI` = ifelse(`95% CI`=="(, )", "",`95% CI`),
         est_fmt = sprintf("%.3f", est),
         est_fmt=ifelse(est_fmt=="NA","",est_fmt))%>%
  bind_cols(Num_pos=simple_not6$n*simple_not6$est_Yan)


axes <- c(0,5,10,15)
Num_row <- nrow(dta)
loc <- c(-46, -44, -16,-5, 40, 52)

pdf("ForestPlot_KW_OneSpike_OneRBD_3SD_Others_09Dec2020.pdf",width = 16, height = 18)


attach(dta)

par(mar=(c(5,1,1,1)))

plot(NA, xlim=c(-45,55), ylim=c(0, nrow(dta0)+2), xlab="", 
     ylab="", main="", axes = FALSE)

for(i in 1:Num_row){
  
  ### 95% bars
  if(i==Num_row){
    points(est[Num_row+1-i]*100, i, pch=19, cex=1.5)
    text(LCL[Num_row+1-i]*100, i, "(")
    text(UCL[Num_row+1-i]*100, i, ")")
    segments(LCL[Num_row+1-i]*100, i, UCL[Num_row+1-i]*100, i,lwd=2)
  }else{
    points(est[Num_row+1-i]*100, i, pch=19, cex=1)
    text(LCL[Num_row+1-i]*100, i, "(")
    text(UCL[Num_row+1-i]*100, i, ")")
    segments(LCL[Num_row+1-i]*100, i, UCL[Num_row+1-i]*100, i)
  }
  
  
  text(loc[1], i, dta$Index[Num_row+1-i], adj=0, font=2)
  text(loc[2], i, dta$subgroup[Num_row+1-i], adj=0)
  text(loc[3], i, dta$Num_pos[Num_row+1-i], adj=0)
  text(loc[4], i, dta$n[Num_row+1-i])
  
  text(loc[5], i, dta$est_fmt[Num_row+1-i])
  text(loc[6], i, dta$`95% CI`[Num_row+1-i])
  
}


segments(est[1]*100, -2, est[1]*100, Num_row, lty=2)

segments(loc[3]-4, nrow(dta0)+1.3, loc[3]+4,nrow(dta0)+1.3)
segments(loc[4]-4, nrow(dta0)+1.3, loc[4]+4,nrow(dta0)+1.3)
segments(loc[5]-3, nrow(dta0)+1.3, loc[5]+3,nrow(dta0)+1.3)
segments(loc[6]-6, nrow(dta0)+1.3, loc[6]+6,nrow(dta0)+1.3)

text(loc[3], nrow(dta0)+2, labels = "Pos No.", font=2)
text(loc[4], nrow(dta0)+2, labels = "Total No.", font=2)
text(loc[5], nrow(dta0)+2, labels = "Estimate", font=2)
text(loc[6], nrow(dta0)+2, labels = "95% CI", font=2)

axis(1, at=c(0,5,10,15,20,25,30), labels=seq(0,30,5)/100)

par(xpd=NA)
text(15, -8, labels = "Estimated prevalence")

dev.off()

detach(dta)







