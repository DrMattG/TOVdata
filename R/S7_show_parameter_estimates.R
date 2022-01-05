setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")
storeToLocalFolder = TRUE

library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

localDir = "."
folders = list.dirs(full.names=TRUE, recursive = FALSE)

samples_list = c(5,250,250,250,250)
thin_list = c(1,1,10,100,1000)
nst = length(thin_list)
nChains = 4

#for(nf in 1:length(folders)) {
for(nf in 4){
  for (Lst in nst:1) {
    thin = thin_list[Lst]
    samples = samples_list[Lst]
    
    
    filename = file.path(folders[nf], paste("models/models_thin_", as.character(thin),
                                            "_samples_", as.character(samples),
                                            "_chains_",as.character(nChains),
                                            ".Rdata",sep = ""))
    if(file.exists(filename)){break}
  }
  if(file.exists(filename)){
    load(filename)
    nm = length(models)
    
    if(storeToLocalFolder){
      filename = file.path(folders[nf], paste("panels/parameter_estimates.pdf"))
    } else {
      filename = file.path(localDir,"ZZ7_parameter_estimates",paste0(substr(folders[nf],start=3,stop=nchar(folders[nf])),".pdf"))
    }
    pdf(file = filename)
    for(j in 1:nm){
      m = models[[j]]
      covariates = attr(terms(m$XFormula),"term.labels")
      if(m$nr+length(covariates)>1){
        preds = computePredictedValues(m)
        VP = computeVariancePartitioning(m)
        vals = VP$vals
        mycols = rainbow(nrow(VP$vals))
        plotVariancePartitioning(hM=m, VP=VP, main = paste0("Proportion of explained variance, ",modelnames[[j]]), cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7))
        MF = evaluateModelFit(hM=m, predY=preds)
        R2 = NULL
        if(!is.null(MF$TjurR2)){
          TjurR2 = MF$TjurR2
          vals = rbind(vals,TjurR2)
          R2=TjurR2
        }
        if(!is.null(MF$R2)){
          R2=MF$R2
          vals = rbind(vals,R2)
        }
        if(!is.null(MF$SR2)){
          R2=MF$SR2
          vals = rbind(vals,R2)
        }
        if(storeToLocalFolder){
          filename = file.path(folders[nf], paste("panels/parameter_estimates_VP_",modelnames[[j]],".csv"))
        } else {
          filename = file.path(localDir,"ZZ7_parameter_estimates","numerical values",
                               paste0("VP_",substr(folders[nf],start=3,stop=nchar(folders[nf])),
                                      "_model_",modelnames[[j]],".csv"))
        }
        write.csv(vals,file=filename)
        if(!is.null(VP$R2T$Beta)){
          if(storeToLocalFolder){
            filename = file.path(folders[nf], paste("panels/parameter_estimates_VP_R2T_Beta",modelnames[[j]],".csv"))
          } else {
            filename = file.path(localDir,"ZZ7_parameter_estimates","numerical values",
                                 paste0("VP_R2T_Beta",substr(folders[nf],start=3,stop=nchar(folders[nf])),
                                        "_model_",modelnames[[j]],".csv"))
          }
          write.csv(VP$R2T$Beta,file=filename)
        }
        if(!is.null(VP$R2T$Y)){
          if(storeToLocalFolder){
            filename = file.path(folders[nf], paste("panels/parameter_estimates_VP_R2T_Y",modelnames[[j]],".csv"))
          } else {
            filename = file.path(localDir,"ZZ7_parameter_estimates","numerical values",
                                 paste0("VP_R2T_Y",substr(folders[nf],start=3,stop=nchar(folders[nf])),
                                        "_model_",modelnames[[j]],".csv"))
          }
          write.csv(VP$R2T$Y,file=filename)
        }
        if(!is.null(R2)){
          VPr = VP
          for(k in 1:m$ns){
            VPr$vals[,k] = R2[k]*VPr$vals[,k]
          }
          VPr$vals = VPr$vals[,order(R2, decreasing = TRUE)]
          plotVariancePartitioning(hM=m, VP=VPr,main=paste0("Proportion of raw variance, ",modelnames[[j]]),cex.main=0.8, cols = mycols, args.leg=list(bg="white",cex=0.7),ylim=c(0,1))
        }
      }
    }
    for(j in 1:nm){
      m = models[[j]]
      if(m$nc>1){
        postBeta = getPostEstimate(m, parName="Beta")
        if(storeToLocalFolder){
          filename = file.path(folders[nf], paste("panels/parameter_estimates_Beta_",modelnames[j],".xlsx"))
        } else {
          filename = file.path(localDir,"ZZ7_parameter_estimates","numerical values",
                               paste0("Beta_",substr(folders[nf],start=3,stop=nchar(folders[nf])),
                                      "_model_",modelnames[[j]],".xlsx"))
        }
        me = as.data.frame(t(postBeta$mean))
        me = cbind(m$spNames,me)
        colnames(me) = c("Species",m$covNames)
        po = as.data.frame(t(postBeta$support))
        po = cbind(m$spNames,po)
        colnames(po) = c("Species",m$covNames)
        ne = as.data.frame(t(postBeta$supportNeg))
        ne = cbind(m$spNames,ne)
        colnames(ne) = c("Species",m$covNames)
        vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
        writexl::write_xlsx(vals,path = filename)
        show.sp.names = (is.null(m$phyloTree) && m$ns<=30) 
        plotBeta(m, post=postBeta, supportLevel = 0.95,param="Sign",
                 plotTree = !is.null(m$phyloTree),
                 covNamesNumbers = c(TRUE,FALSE),
                 spNamesNumbers=c(show.sp.names,FALSE),
                 cex=c(0.6,0.6,0.8))
        mymain = paste0("BetaPlot, ",modelnames[[j]])
        if(!is.null(m$phyloTree)){
          mpost = convertToCodaObject(m)
          rhovals = unlist(poolMcmcChains(mpost$Rho))
          mymain = paste0(mymain,", E[rho] = ",round(mean(rhovals),2),", Pr[rho>0] = ",round(mean(rhovals>0),2))
        }
        title(main=mymain, line=2.5, cex.main=0.8)
      }
    }
    for(j in 1:nm){
      m = models[[j]]      
      if(m$nt>1 & m$nc>1){
        postGamma = getPostEstimate(m, parName="Gamma")
        plotGamma(m, post=postGamma, supportLevel = 0.9, param="Sign",
                  covNamesNumbers = c(TRUE,FALSE),
                  trNamesNumbers=c(m$nt<21,FALSE),
                  cex=c(0.6,0.6,0.8))
        title(main=paste0("GammaPlot ",modelnames[[j]]), line=2.5,cex.main=0.8)
      }
    }
    for(j in 1:nm){
      m = models[[j]]
      if(m$nr>0){
        OmegaCor = computeAssociations(m)
        supportLevel = 0.75
        for (r in 1:m$nr){
          plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
          #plotOrder = 1:m$ns
          toPlot = ((OmegaCor[[r]]$support>supportLevel) + (OmegaCor[[r]]$support<(1-supportLevel))>0)*sign(OmegaCor[[r]]$mean)
          if(m$ns>10){
            colnames(toPlot)=rep("",m$ns)
            rownames(toPlot)=rep("",m$ns)
          }
          mymain = paste0("Associations, ",modelnames[[j]], ": ",names(m$ranLevels)[[r]])
          if(m$ranLevels[[r]]$sDim>0){
            mpost = convertToCodaObject(m)
            alphavals = unlist(poolMcmcChains(mpost$Alpha[[1]][,1]))
            mymain = paste0(mymain,", E[alpha1] = ",round(mean(alphavals),2),", Pr[alpha1>0] = ",round(mean(alphavals>0),2))
          }
          corrplot(toPlot[plotOrder,plotOrder], method = "color",
                   col=colorRampPalette(c("blue","white","red"))(3),
                   mar=c(0,0,1,0),
                   main=mymain,cex.main=0.8)
          me = as.data.frame(OmegaCor[[r]]$mean)
          me = cbind(m$spNames,me)
          colnames(me)[1] = ""
          po = as.data.frame(OmegaCor[[r]]$support)
          po = cbind(m$spNames,po)
          colnames(po)[1] = ""
          ne = as.data.frame(1-OmegaCor[[r]]$support)
          ne = cbind(m$spNames,ne)
          colnames(ne)[1] = ""
          vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
          if(storeToLocalFolder){
            filename = file.path(folders[nf], paste("panels/parameter_estimates_Omega_",modelnames[[j]],"_",names(m$ranLevels)[[r]],".xlsx"))
          } else {
            filename = file.path(localDir,"ZZ7_parameter_estimates","numerical values",
                                 paste0("Omega_",substr(folders[nf],start=3,stop=nchar(folders[nf])),
                                        "_model_",modelnames[[j]],"_",names(m$ranLevels)[[r]],".xlsx"))
          }
          writexl::write_xlsx(vals,path = filename)
        }
      }
      #phyloTree=m$phyloTree
      #C = m$C
      #Tr = m$Tr
      #TrData = m$TrData
      #save(postBeta, phyloTree, C, Tr, TrData, file = file.path(folders[nf], paste0(modelnames[[j]],".RData")))
    }
    dev.off()
  }
}
