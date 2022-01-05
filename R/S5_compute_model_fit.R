setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")

library(Hmsc)
library(timeR)

localDir = "."
folders = list.dirs(full.names=TRUE, recursive = FALSE)

#samples_list = c(5)
#thin_list = c(1)
samples_list = c(5,250,250,250,250)
thin_list = c(1,1,10,100,1000)
nChains = 4

timer <- createTimer(precision = "s") 


for (Lst in 1:length(thin_list)) {
  
 # for(nf in 4:length(folders)) {
    for(nf in 4) {
      
    print(paste0("project = ",folders[nf]))
   
    thin = thin_list[Lst]
    samples = samples_list[Lst]
    
    print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
    
    filename_in = file.path(folders[nf], paste("models/models_thin_", as.character(thin),
                                                "_samples_", as.character(samples),
                                                "_chains_",as.character(nChains),
                                                ".Rdata",sep = ""))
    
    filename_out = file.path(folders[nf], paste("models/xx_MF_thin_", as.character(thin),
                                               "_samples_", as.character(samples),
                                               "_chains_",as.character(nChains),
                                               ".Rdata",sep = ""))
    
    filename_out_TEMP = paste(gsub(".Rdata", "_TEMP.Rdata", filename_out))
    
    if(file.exists(filename_in) == TRUE & file.exists(filename_out) == FALSE & file.exists(filename_out_TEMP) == FALSE) {
      
      save(nf,file= filename_out_TEMP)
      
      load(file = filename_in) #models, modelnames
      
      nm = length(models)
      runtimes = vector(length=nm)
      
      MF = list()
      MFCV = list()
      WAIC = list()
      
      for(model in 1:nm){
        print(paste0("model = ",as.character(model)))
        timer$start(paste0("McMc_timer", nm))
        m = models[[model]]
        preds = computePredictedValues(m)
        MF[[model]] = evaluateModelFit(hM=m, predY=preds)
        if(folders[nf]=="./jokikokko_mika"){
          partition = createPartition(m, nfolds = 38, column = "place")
        } else {
        partition = createPartition(m, nfolds = 2)
        }
        preds = computePredictedValues(m,partition=partition,nParallel = nChains)
        MFCV[[model]] = evaluateModelFit(hM=m, predY=preds)
        WAIC[[model]] = computeWAIC(m)
        timer$stop(paste0("McMc_timer", nm))
        runtimes[[model]] = timer$getTimeElapsed(paste0("McMc_timer", nm))        
      }
      save(MF,MFCV,WAIC,modelnames,file = filename_out)
      file.remove(filename_out_TEMP)
      save(runtimes,file=gsub(".Rdata", "_runtimes.Rdata", filename_out))
    }
  }
}
