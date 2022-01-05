#setwd("C:/LocalData/ovaskain/all stuff/manuscripts/InPreparation/Elie Gaget birds")
library(Hmsc)
load("List_AllData.RData")

Y = List_AllData$Y

Y.pa = Y
Y.pa[Y>1] = 1

Y.abu.class = Y
Y.abu.class[Y>=1&Y<10] = 1
Y.abu.class[Y>=10&Y<100] = 2
Y.abu.class[Y>=100&Y<1000] = 3
Y.abu.class[Y>=1000&Y<10000] = 4

Y.abu.c.pres = Y
Y.abu.c.pres[Y==0] = NA
Y.abu.c.pres = log(Y.abu.c.pres,base = 10)

studyDesign = List_AllData$studyDesign

XData = List_AllData$XData
XData$log.NoCells.eq.ha. = log(XData$NoCells.eq.ha.+1)

TrData = List_AllData$traits

phyloTree = List_AllData$phy

XFormula = ~ log.NoCells.eq.ha.*Tmin_precis +
  prop_ArtSurf*Tmin_precis +
  prop_AgrArea*Tmin_precis +
  PA*Tmin_precis

TrFormula = ~ Weight +STI+ hab_pref 

plots = levels(studyDesign$plot)
sData = matrix(NA,nrow = length(plots),ncol=2)
for(i in 1:length(plots)){
  sData[i,1] = mean(XData[studyDesign$plot==plots[i],]$latitude)
  sData[i,2] = mean(XData[studyDesign$plot==plots[i],]$longitude)
}
rownames(sData) = plots
rL.plot = HmscRandomLevel(sData = sData)

rL.year = HmscRandomLevel(units = levels(studyDesign$year))

m1 = Hmsc(Y=Y.pa,
         XData=XData,
         XFormula=XFormula,
         TrData = TrData,
         TrFormula=TrFormula,
         phyloTree = phyloTree,
         studyDesign=studyDesign,
         ranLevels=list("plot"=rL.plot,"year"=rL.year),
         distr="probit")


m2 = Hmsc(Y=Y.abu.c.pres,
          XData=XData,
          XFormula=XFormula,
          TrData = TrData,
          TrFormula=TrFormula,
          phyloTree = phyloTree,
          studyDesign=studyDesign,
          ranLevels=list("plot"=rL.plot,"year"=rL.year),
          distr="normal")

m3 = Hmsc(Y=Y.abu.class,
          XData=XData,
          XFormula=XFormula,
          TrData = TrData,
          TrFormula=TrFormula,
          phyloTree = phyloTree,
          studyDesign=studyDesign,
          ranLevels=list("plot"=rL.plot,"year"=rL.year),
          distr="poisson")


sampleMcmc(m1,samples = 5)
sampleMcmc(m2,samples = 5)
sampleMcmc(m3,samples = 5)

models = list(m1,m2,m3)
modelnames = c("presence-absence","abundance-conditional-on-presence","abundance-class")
save(models,modelnames,file = "unfitted_models")
