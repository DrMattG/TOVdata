setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")
storeToLocalFolder = TRUE

library(Hmsc)
library(ggplot2)

localDir = "."
folders = list.dirs(full.names=TRUE, recursive = FALSE)

samples_list = c(5,250,250,250,250)
thin_list = c(1,1,10,100,1000)
nst = length(thin_list)
nChains = 4

#for(nf in 1:length(folders)) {
for(nf in 4){
  print(nf)
  if(TRUE){
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
        filename = file.path(folders[nf], paste("panels/predictions.pdf"))
      } else {
        filename = file.path(localDir,"ZZ8_predictions",paste0(substr(folders[nf],start=3,stop=nchar(folders[nf])),".pdf"))
      }
      pdf(file = filename)
      for(j in 1:nm){
        m = models[[j]]
        covariates = all.vars(m$XFormula)
        ex.sp = which.max(colMeans(m$Y,na.rm = TRUE)) #most common species as example species
        if(m$distr[1,1]==2){
          ex.sp = which.min(abs(colMeans(m$Y,na.rm = TRUE)-0.5))
        }
        for(k in 1:(length(covariates))){
          covariate = covariates[[k]]
          Gradient = constructGradient(m,focalVariable = covariate)
          Gradient2 = constructGradient(m,focalVariable = covariate,non.focalVariables = 1)
          predY = predict(m, Gradient=Gradient, expected = TRUE)  
          predY2 = predict(m, Gradient=Gradient2, expected = TRUE)  
          par(mfrow=c(2,1))
          pl = plotGradient(m, Gradient, pred=predY, yshow = 0, measure="S", showData = TRUE, 
                            main = paste0(modelnames[[j]],": summed response (total effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[[j]],": summed response (total effect)")))
          }
          pl = plotGradient(m, Gradient2, pred=predY2, yshow = 0, measure="S", showData = TRUE, 
                             main = paste0(modelnames[[j]],": summed response (marginal effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[[j]],": summed response (marginal effect)")))
          }
          par(mfrow=c(2,1))
          pl = plotGradient(m, Gradient, pred=predY, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp, showData = TRUE, 
                       main = paste0(modelnames[[j]],": example species (total effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[[j]],": example species (total effect)")))
          }
          pl = plotGradient(m, Gradient2, pred=predY2, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp, showData = TRUE, 
                       main = paste0(modelnames[[j]],": example species (marginal effect)"))
          if(inherits(pl, "ggplot")){
            print(pl + labs(title=paste0(modelnames[[j]],": example species (marginal effect)")))
          }
          if(m$nt>1){
            for(l in 2:m$nt){
              par(mfrow=c(2,1))
              pl = plotGradient(m, Gradient, pred=predY, measure="T",index=l, showData = TRUE,yshow = 0,
                           main = paste0(modelnames[[j]],": community weighted mean trait (total effect)"))
              if(inherits(pl, "ggplot")){
                print(pl + labs(title=paste0(modelnames[[j]],": community weighted mean trait (total effect)")))
              }
              pl = plotGradient(m, Gradient2, pred=predY2, measure="T",index=l, showData = TRUE, yshow = 0,
                           main = paste0(modelnames[[j]],": community weighted mean trait (marginal effect)"))
              if(inherits(pl, "ggplot")){
                print(pl + labs(title=paste0(modelnames[[j]],": community weighted mean trait (marginal effect)")))
              }
            }
          }
        }
      }
      dev.off()
    }
  }
}
