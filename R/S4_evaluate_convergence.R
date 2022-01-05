#setwd("Q:/ovaskainen/O2/hmsc_pipeline")
setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")
storeToLocalFolder = TRUE

library(Hmsc)
library(colorspace)
library(vioplot)

localDir = "."
folders = list.dirs(full.names=TRUE, recursive = FALSE)
folders

#samples_list = c(5)
#thin_list = c(1)
samples_list = c(5,250,250,250,250)
thin_list = c(1,1,10,100,1000)
nst = length(thin_list)
nChains = 4


#for(nf in 1:length(folders)) {
for(nf in 4){
  ma = NULL
  na = NULL
  for (Lst in 1:nst) {
    thin = thin_list[Lst]
    samples = samples_list[Lst]
    
    
    filename = file.path(folders[nf], paste("models/models_thin_", as.character(thin),
                                            "_samples_", as.character(samples),
                                            "_chains_",as.character(nChains),
                                            "_runtimes.Rdata",sep = ""))
    if(file.exists(filename)){
      load(filename)
      out = paste0("project = ",folders[nf],"; thin = ",as.character(thin),
                   "; samples = ",as.character(samples),"; runtimes = ")
      nm = length(runtimes)
      for(j in 1:nm){
        out = paste0(out,runtimes[j]," ")
      }
      print(out)
      filename = file.path(folders[nf], paste("models/models_thin_", as.character(thin),
                                              "_samples_", as.character(samples),
                                              "_chains_",as.character(nChains),
                                              ".Rdata",sep = ""))
      load(filename)
#      if(thin==1000) models[[2]]=models[[1]]
      for(j in 1:nm){
        mpost = convertToCodaObject(models[[j]], spNamesNumbers = c(T,F), covNamesNumbers = c(T,F))
        psrf.beta = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
        tmp = summary(psrf.beta)
        if(is.null(ma)){
          ma=psrf.beta[,1]
          na = paste0(as.character(thin),",",as.character(samples))
        } else {
          ma = cbind(ma,psrf.beta[,1])
          if(j==1){
            na = c(na,paste0(as.character(thin),",",as.character(samples)))
          } else {
            na = c(na,"")
          }
        }
      }
    }
  }
  if(!is.null(ma)){
    if(length(na)>nm){
      na = na[-(1:nm)]
      ma = ma[,-(1:nm)]
    }
    if(storeToLocalFolder){
      filename = file.path(folders[nf], paste("panels/MCMC_convergence.pdf"))
    } else {
      filename = file.path(localDir,"ZZ4_MCMC_convergence",paste0(substr(folders[nf],start=3,stop=nchar(folders[nf])),".pdf"))
    }
    pdf(file = filename)
    par(mfrow=c(2,1))
    vioplot(ma,col=rainbow_hcl(nm),names=na,ylim=c(0,max(ma)),main="psrf(beta)")
    vioplot(ma,col=rainbow_hcl(nm),names=na,ylim=c(0.9,1.1),main="psrf(beta)")
    dev.off()
  }
}
