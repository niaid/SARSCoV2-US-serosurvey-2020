setwd("~/box/Share")
lgt.objs = readRDS(file="lgtreg.x.sel.rds")
covid.wt.x.sel <- readRDS(file="covid2020_x.sel.rds")
y.var="OneSpike_OneRBD_3SD01"

y.rgn.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="region")
y.age.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="agegr3")
y.sex.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="dmsex")
y.rac.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="RaceGrp3")
y.his.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="dmethnic_y_n")
y.urb.x.sel=se.est(lgreg = lgt.objs, wtdat=covid.wt.x.sel, y.var, mth='kw.postr5.mat', var.PS=T, by.var="urban.rural2")

ymeans.x.sel = rbind(y.rgn.x.sel, y.age.x.sel, y.sex.x.sel, y.rac.x.sel, y.his.x.sel, y.urb.x.sel)
ymeans.x.sel
