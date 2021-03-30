############------Generate postratified KW and the selected model----#####################
# Programer: Yan Li                                                                      #
##########################################################################################

##########################################################################################
# TD.ybar.w:  Derivative of HT mean esitmator wrt weights                                #
# Input:      y: a vector of outcome  1Xn                                                #
#             w: a vector of weights  1Xn                                                #
# Output:     a 1Xn vector of Taylor Deviates (i.e. deriv of ybar wrt each weight)       #
##########################################################################################
TD.ybar.w <- function(y,w){
  y.bar = sum(y*w)/sum(w)
  TD=(y-y.bar)/sum(w)
  TD
}
###############################################################################################
# TD.y.postwt.wt: Derivative of HT mean esitmator ybar wrt original weights                   #
# Input:      y: a vector of outcome  1Xn                                                     #
#             postwt: a vector of poststratified weights  1Xn                                                #
#             wt0: a vector of original weights  1Xn                                          #
#             poststrt.h: a vector of catogorical poststrata IDs  1Xn                         #
#             pop.totals.h: a vector of population totals in each stratum  1XH
#                           --of each corrsponding to the order of names(table(poststrt.h))   #
# Output:     a 1Xn vector of Taylor Deviates (i.e. deriv of ybar wrt original weight)        #
###############################################################################################
TD.y.postwt.wt <- function(y, postwt, wt0, poststrt.h, pop.totals.h){
  n=length(wt0);n
  id.h = model.matrix(~poststrt.h-1)                      # n * G groups
  sam.totals.h = apply(as.matrix(id.h*wt0),2,sum)         # sample totals in each group
  fg.vec=id.h%*%(pop.totals.h/sam.totals.h); dim(fg.vec)  # a vector of n of each Ng/Ng.hat
  fwg.vec=as.matrix(id.h*wt0)%*%((pop.totals.h/sam.totals.h)/sam.totals.h); dim(fwg.vec)
                                                          # a vector of n of each Ng*wt0/Ng.hat^2
  TD.0 = id.h*as.vector(0-fwg.vec);                       # a MATRIX of n by G of each col 0 or Ng/Ng.hat*(-w0/Ng.hat)
  
  tmp=matrix(TD.ybar.w(y, post.wt)); (dim(tmp))           # n by 1
  TD.g1 = t(tmp)%*%(TD.0);                                # group totals of size G
  TD.g = (id.h)%*%c(TD.g1); 
  
  TD.i=t(tmp)%*%diag(c(fg.vec)); dim(TD.i)                # group totals of size G
  c(TD.i)+c(TD.g)
}

###############################################################################################
# TD.postwt.wt: Derivative of poststratified weights wrt original weights                     #
# Input:      wt0: a vector of original weights  1Xn                                          #
#             poststrt.h: a vector of catogorical poststrata IDs  1Xn                         #
#             pop.totals.h: a vector of population totals in each stratum  1XH                #
#                           -- corrsponds to the order of names(table(poststrt.h))            #
# Output:     a nXn matrix of deriv of postwts wrt original weights w/ col-wt.post.i; row-wt.j#
###############################################################################################

TD.postwt.wt <- function(wt0, poststrt.h, pop.totals.h){
  n=length(wt0); n
  id.h = model.matrix(~poststrt.h-1); dim(id.h) # n * G groups
  sam.totals.h = apply(as.matrix(id.h*wt0),2,sum); length(sam.totals.h)
  # wted sample totals in each group
  fwg.vec=as.matrix(id.h*wt0)%*%((pop.totals.h/sam.totals.h)/sam.totals.h); dim(fwg.vec)
  # a nX1 vector of each Ng*wt0/Ng.hat^2
  fwg.nG = -(id.h)*c(fwg.vec);table(fwg.nG[,1]) # MATRIX of n by G of each col 0 or Ng/Ng.hat*(-w0/Ng.hat)
  fwg.mat = id.h%*%t(fwg.nG); dim(fwg.mat)      # matrix of n by n of each row 0 or Ng/Ng.hat*(-w0/Ng.hat)
  # table(fwg.mat[1,])
  
  fg.vec=id.h%*%(pop.totals.h/sam.totals.h)    # a vector of n of each Ng/Ng.hat
  fg.mat=diag(as.vector(fg.vec)); dim(fg.mat)  # n by n with diag = Ng/Ng.hat
  return(fg.mat+fwg.mat) # row-wt.post.i; col-wt.j; n by n matrix
}
###############################################################################################
######--------poststratified estimates by using svy package (svymean with postStratify design #
######        and svydesign) for SE                                                           #
###############################################################################################
est.postrwt <- function(by.var, y.var, post.dsgn){
  wtdat<-post.dsgn$variables
  pwt = weights(post.dsgn)
  n.cmplt = nrow(wtdat); id.subgrp = matrix(rep(1,n.cmplt))
  id.mis=F
  cv.wt = sd(pwt)/mean(pwt)
  if(!is.null(by.var)) {
    id.mis = is.na(wtdat[,by.var])
    n.cat = nlevels(factor(wtdat[,by.var]))
    id.subgrps <- matrix(0,n.cmplt,n.cat)
    id.subgrps[!id.mis,]=model.matrix(as.formula(paste0("~factor(",by.var,")-1")),wtdat)
    cv.wt = c(cv.wt,sapply(1:n.cat, function(i) sd(pwt[id.subgrps[,i]==1])/mean(pwt[id.subgrps[,i]==1])))
    id.subgrp <- cbind(1, id.subgrps) # assign values of zero if byvar = missing
  }
  n.subgrp = apply(id.subgrp,2,sum)
  
  dsgn.pw <-svydesign(data=wtdat[!id.mis,], ids=~1, weights = pwt[!id.mis])
  est.full.wt = svymean(as.formula(paste("~",y.var)), dsgn.pw, na.rm=T)
  est.full.postwt = svymean(as.formula(paste("~",y.var)), post.dsgn, na.rm=T)
  est.full.v = c(est.full.wt, sqrt(diag(vcov(est.full.wt))), est.full.postwt, sqrt(diag(vcov(est.full.postwt))))
  
  if (!is.null(by.var)) {
    ests.subgrp.wt = svyby(as.formula(paste("~",y.var)),by=as.formula(paste0("~",by.var)),dsgn.pw,svymean)
    ests.subgrp.postwt = svyby(as.formula(paste("~",y.var)),by=as.formula(paste0("~",by.var)),post.dsgn,svymean)
    rslt.svy= data.frame(n=n.subgrp, cv.pwt=cv.wt, wt=rbind(est.full.v[1:2], ests.subgrp.wt[,-1]), dsgn.se = c(est.full.v[4], ests.subgrp.postwt[,-(1:2)]))
    rownames(rslt.svy) = c("full_sample",paste0(by.var,levels(factor(wtdat[,by.var]))))
  }
  rslt.svy
}

#################################################################################
#-Model-selection: delete the interaction term with max pvalue one at a time    #
#---keep the all main effects, sig or not                                       #
#################################################################################
mdl.selection <- function(model0, covx.names.main,covx.names.int, outcome, dsgn, pvalue_cut){
 p.value0 = sapply(1:length(covx.names.int), function(i) regTermTest(model0,covx.names.int[i])$p)
  names(p.value0)=covx.names.int
  terms.sig.int0 = names(which(p.value0<.5))
  
  flag=1
  terms.sig.int=terms.sig.int0
  while(flag>0){
    terms.sig=c(covx.names.main,terms.sig.int)
    fm=as.formula(paste(outcome, "~", paste(terms.sig,collapse = "+")))
    model1<-svyglm(fm, design=dsgn, family=quasibinomial())                            # pavlue of the new model 
    pvalue.1=sapply(1:length(terms.sig.int), function(i) regTermTest(model1,terms.sig.int[i])$p)
    
    if (max(pvalue.1)>=pvalue_cut) {terms.sig.int = terms.sig.int[-which.max(pvalue.1)];flag=flag+1}
    if (max(pvalue.1) <pvalue_cut) {flag=0}
    print(date());print(max(pvalue.1));print(length(terms.sig.int));print(flag)
  } #end while
  return(list(model=model1, terms.sig.int=terms.sig.int, terms.sig.int0=terms.sig.int0, fm.sel=fm, covx.names=terms.sig, flag=flag))
} 

########################################
#-backward selection only              #
########################################
bckSel<-function(outcome, lgreg, pvl_cut=.1){
  string = gsub(toString(lgreg$formula), pattern = c('\n    '), replacement  = "")
  var.names = str_split(string, c(" \\+ "))[[1]]
  covx.names0 = gsub(var.names,pattern = paste0('~, ',outcome,', '), replacement = '')
  id.inter = which(str_detect(covx.names0, pattern = ":"))
  covx.names.main = covx.names0[-id.inter]; covx.names.int = covx.names0[id.inter]
  dsgn=lgreg$survey.design
  
  #---keep all main effects, call mdl.selection()
  Formula_fit.x.bckSel = mdl.selection(lgreg, covx.names.main, covx.names.int, outcome, dsgn,
                                       pvalue_cut=pvl_cut)
  Formula_fit.x.bckSel
}

######################################################################################
library(survey)
library(Hmisc)
library(mvtnorm)
library(dplyr)
library(tidyverse)
library(stringi)
library(PSwtEst)

lgt.objs = readRDS(file="lgtreg.x.sel.rds")
names(lgt.objs)=c("lgtreg.x.sel")

cht.wt.x.sel = pseudoweights(lgreg = lgt.objs$lgtreg.x.sel,  mth='kw')

cht.wt.x.sel$OneSpike_OneRBD_3SD01 = recode(cht.wt.x.sel$OneSpike_OneRBD_3SD, 'Neg'=0, 'Pos'=1)

y.var="OneSpike_OneRBD_3SD01"

###---simple proportions by subgroups
no.dsgn = svydesign(ids=~1, strata = NULL, weights = ~1, data = cht.wt.x.sel)

x.var<-c("region","agegr3","dmsex","RaceGrp3","dmethnic_y_n","urban.rural2")

n.full = length(weights(no.dsgn))

est.full= svymean(as.formula(paste("~",y.var)),no.dsgn)

est0 =est.y0.subgrp= c(0, est.full,sqrt(vcov(est.full)), n.full)

names=c("full.sample")
for (i in 1:length(x.var)){
  n.cnt = (cbind(svytable(as.formula(paste0("~",x.var[i])),  no.dsgn),2))
  est.g = (svyby(as.formula(paste("~",y.var)),by=as.formula(paste0("~",x.var[i])),no.dsgn,svymean))
  tmp.names=paste0(x.var[i],rownames(est.g))
  
  est.y0.subgrp = rbind(est.y0.subgrp,as.matrix(unname(cbind(est.g,n.cnt[,1]))))
  names = c(names,tmp.names)
}
rownames(est.y0.subgrp) = names
colnames(est.y0.subgrp) = c("levels", "est", "se", "n")

est.y0.subgrp

################### Construct Poststratification  Variable in census and covid data ######################
#---------read/clean census data----------#
census=read.csv(file="real data analysis/data/Census3AgeGroupsWithNames_UrbanRural.csv",header = TRUE, sep=",")

colnames(census)=c("seqn","x.state","county","state","ctyname",
                   "agegr3","dmsex","dmethnic_y_n","race","popsize",
                   "R10.000","AGEGRPNAME", "RACENAME","HISPNAME","X2015.Geography.Name","urban.rural" )

census$dmsex = recode(census$dmsex,"M"=1,"F"=2)

census$dmethnic_y_n = recode(census$dmethnic_y_n, "H"=1, "NH"=2)

census$RaceGrp3 = recode(census$RACENAME,"White Alone"=1, "Black Alone" =2, "Asian Alone"=3,
                         "American Indian Alone"=3,  "Pacific Islander Alone"=3, "Two or More"=3)

census$urban.rural2 = recode(census$urban.rural,'completely rural'=1,'mostly rural'=1, 'mostly urban'=2)

#---------create new region & poststr variables in census
id.r1=which(census$state %in% c("Connecticut","Maine", "Massachusetts", "New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania"))
id.r2=which(census$state %in% c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Minnesota"))
id.r3=which(census$state %in% c("Delaware","District of Columbia","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Kentucky","Tennessee"))
id.r4=which(census$state %in% c("Kansas","Missouri", "Florida", "Alabama","Mississippi", "Arkansas","Louisiana","Oklahoma"))
id.r5=which(census$state %in% c("Nebraska", "North Dakota", "South Dakota","Texas", "Arizona","Colorado", "Idaho", "Montana", "New Mexico","Utah","Wyoming"))
id.r6=which(census$state %in% c("Nevada", "Alaska", "California", "Hawaii", "Oregon", "Washington"))

census$region=NA;census$region[id.r1]=1;census$region[id.r2]=2;census$region[id.r3]=3;census$region[id.r4]=4;census$region[id.r5]=5;census$region[id.r6]=6

census.dsgn=svydesign(data=census, ids = ~1,  weights=~popsize)

###############-----Census counts------#################
varname.census = c("region", "agegr3", "dmsex","RaceGrp3","dmethnic_y_n","urban.rural2") #"popsize","R10.000","RACENAME"

census.prpn.N = sapply(1:length(varname.census), function (i)
  rbind(census.prpn= prop.table(svytable(
    as.formula(paste0("~",varname.census[i])),  design=census.dsgn)),
        N=round(svytable(
          as.formula(paste0("~",varname.census[i])), design=census.dsgn),
          2)
    )
)
names(census.prpn.N) = varname.census

#############

census.p.x1 = as.data.frame(census.prpn.N)[1,]
census.N.x1 = as.data.frame(census.prpn.N)[2,]

pop.totals.region <- svytable(~region ,census.dsgn)
pop.totals.dmsex  <- svytable(~dmsex,census.dsgn)
pop.totals.agegr3 <- svytable(~agegr3,census.dsgn)
pop.totals.dmethnic_y_n <- svytable(~dmethnic_y_n,census.dsgn)
pop.totals.RaceGrp3     <- svytable(~RaceGrp3,census.dsgn)
pop.totals.urban.rural2 <- svytable(~urban.rural2,census.dsgn)

####----- create poststrata in covid data and census
census$poststr5 = as.factor(   as.numeric(census$region)*100000+
                                 as.numeric(census$dmethnic_y_n)*10000+
                                 as.numeric(census$agegr3)*1000+
                                 as.numeric(census$dmsex)*100+
                                 as.numeric(census$RaceGrp3)*10)

census.dsgn=svydesign(data=census, ids = ~1,  weights=~popsize)

census.N.5postr01 <- svytable(~poststr5,census.dsgn)

cht.wt.x.sel$poststr5 = as.factor(as.numeric(cht.wt.x.sel$region)*100000+
                                    as.numeric(cht.wt.x.sel$dmethnic_y_n)*10000+
                                    as.numeric(cht.wt.x.sel$agegr3)*1000+
                                    as.numeric(cht.wt.x.sel$dmsex)*100+
                                    as.numeric(cht.wt.x.sel$RaceGrp3)*10)

tmp = table(cht.wt.x.sel$poststr5)

census.N.5postr1 <- census.N.5postr01[names(census.N.5postr01)%in%names(tmp)]

####-------- Construct poststratification KW weights--------############

dsgn.kw <- svydesign(ids = ~1, weights = ~kw[,1], data = cht.wt.x.sel)

dsgn.kw.postr5=postStratify(design=dsgn.kw, ~poststr5, census.N.5postr1)

####---------Construct poststr KW pseudoweights & TDs --------#####
y=cht.wt.x.sel[,y.var]

wt0=as.vector(cht.wt.x.sel$kw[,1])
w0.beta=cht.wt.x.sel$kw[,-1]

post.wt = weights(dsgn.kw.postr5)
poststrt.h=cht.wt.x.sel$poststr5

pop.totals.h=census.N.5postr1

wtTD0=wt0*TD.ybar.w(y, wt0); sum(wtTD0)         # kw

wtTD1=post.wt*TD.ybar.w(y, post.wt); sum(wtTD1) # poststr5.kw

wtTD =wt0*TD.y.postwt.wt(y, post.wt, wt0, poststrt.h, pop.totals.h); sum(wtTD) # y.postwt.kw

wtTD.star =wt0*t(TD.ybar.w(y, post.wt))%*%
  t(TD.postwt.wt(wt0, poststrt.h, pop.totals.h))

#--- Revise the Taylor Deviates by multiplying deriv.postwt.wt to deriv.wt.beta

wpost.w0=t(TD.postwt.wt(wt0, poststrt.h, pop.totals.h))

wpost.beta <- as.matrix(wpost.w0)%*%(w0.beta); dim(wpost.beta)

############# Update covid2020_x.sel with kw.postr5.mat pseudoweights #########

cht.wt.x.sel$kw.postr5.mat = cbind(post.wt, wpost.beta) 

saveRDS(cht.wt.x.sel,file = "covid2020_x.sel.rds")
# # OR use the package PSwtEst funcitons of (poststrPSwt and var.TL.cmplx) 
# kw.postr5.mat = poststrPSwt(datPSwt=cht.wt.x.sel, pswtVar='kw', postStrVar='poststr5', 
#                              pop.totals.h=data.frame(census.N.5postr1))

############# Select the propensity model by backward selection ###########################

Formula_fit.x =  as.formula("A ~ factor(region)+factor(agegr3)+dmsex+factor(RaceGrp3)+dmethnic_y_n +NCHS_Urban.Rural+
                            children2+factor(educ3)+homeowner+factor(employ3)+
                            health_care_coverage+vaccine+cvd+pulg+immun+had_diabetes")  

Formula_fit.x.all = update(Formula_fit.x, ~.^2)
lgtreg.x.all = svyglm(Formula_fit.x.all, family = binomial, design = prpn.dsgn)

outcome="A"; lgreg=lgtreg.x.all

mdl.sel=bckSel(outcome, lgreg, pvl_cut=.1)
fm.sel <- mdl.sel$fm.sel
fm.sel

