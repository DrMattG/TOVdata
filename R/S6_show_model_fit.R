setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")
storeToLocalFolder = TRUE

library(Hmsc)

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
    
    filename = file.path(folders[nf], paste("models/MF_thin_", as.character(thin),
                                            "_samples_", as.character(samples),
                                            "_chains_",as.character(nChains),
                                            ".Rdata",sep = ""))
    if(file.exists(filename)){break}
  }
  if(file.exists(filename)){
    load(filename)
    nm = length(MF)
    if(storeToLocalFolder){
      filename = file.path(folders[nf], paste("panels/model_fit.pdf"))
    } else {
      filename = file.path(localDir,"ZZ6_model_fits",paste0(substr(folders[nf],start=3,stop=nchar(folders[nf])),".pdf"))
    }
    pdf(file = filename)
    for(j in 1:nm){
      cMF = MF[[j]]
      cMFCV = MFCV[[j]]
      if(!is.null(cMF$TjurR2)){
        plot(cMF$TjurR2,cMFCV$TjurR2,xlim=c(-1,1),ylim=c(-1,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",
                         as.character(thin),
                         ", samples = ",as.character(samples),
                         ": Tjur R2.\n",
                         "mean(MF) = ",as.character(mean(cMF$TjurR2,na.rm=TRUE)),
                         ", mean(MFCV) = ",as.character(mean(cMFCV$TjurR2,na.rm=TRUE))))
        abline(0,1)
        abline(v=0)
        abline(h=0)
      }
      if(!is.null(cMF$R2)){
        plot(cMF$R2,cMFCV$R2,xlim=c(-1,1),ylim=c(-1,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                         ", samples = ",as.character(samples),
                         ": R2. \n",
                         "mean(MF) = ",as.character(mean(cMF$R2,na.rm=TRUE)),
                         ", mean(MFCV) = ",as.character(mean(cMFCV$R2,na.rm=TRUE))))
        abline(0,1)
        abline(v=0)
        abline(h=0)
      }
      if(!is.null(cMF$AUC)){
        plot(cMF$AUC,cMFCV$AUC,xlim=c(0,1),ylim=c(0,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                         ", samples = ",as.character(samples),
                         ": AUC. \n",
                         "mean(MF) = ",as.character(mean(cMF$AUC,na.rm=TRUE)),
                         ", mean(MFCV) = ",as.character(mean(cMFCV$AUC,na.rm=TRUE))))
        abline(0,1)
        abline(v=0.5)
        abline(h=0.5)
      }
      if(FALSE && !is.null(cMF$O.TjurR2)){
        plot(cMF$O.TjurR2,cMFCV$O.TjurR2,xlim=c(-1,1),ylim=c(-1,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": O.Tjur R2"))
        abline(0,1)
        abline(v=0)
        abline(h=0)
      }
      if(FALSE && !is.null(cMF$O.AUC)){
        plot(cMF$O.AUC,cMFCV$O.AUC,xlim=c(0,1),ylim=c(0,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": O.AUC"))
        abline(0,1)
        abline(v=0.5)
        abline(h=0.5)
      }      
      if(!is.null(cMF$SR2)){
        plot(cMF$SR2,cMFCV$SR2,xlim=c(-1,1),ylim=c(-1,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),
                         ", samples = ",as.character(samples),
                         ": SR2. \n",
                         "mean(MF) = ",as.character(mean(cMF$SR2,na.rm=TRUE)),
                         ", mean(MFCV) = ",as.character(mean(cMFCV$SR2,na.rm=TRUE))))
        abline(0,1)
        abline(v=0)
        abline(h=0)
      }    
      if(FALSE && !is.null(cMF$C.SR2)){
        plot(cMF$C.SR2,cMFCV$C.SR2,xlim=c(-1,1),ylim=c(-1,1),
             xlab = "explanatory power",
             ylab = "predictive power",
             main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": C.SR2"))
        abline(0,1)
        abline(v=0)
        abline(h=0)
      }  
    }
    dev.off()
  }
}

