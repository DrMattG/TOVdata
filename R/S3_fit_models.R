setwd("P:/h570/ovaskainen/O2/hmsc_pipeline")
#setwd("Q:/ovaskainen/O2/hmsc_pipeline")

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
  
  for(nf in 4) {
    #for(nf in 4:length(folders)) {
    
    print(paste0("project = ",folders[nf]))
    
    filename_in = file.path(folders[nf], "models/unfitted_models")
    
    
    thin = thin_list[Lst]
    samples = samples_list[Lst]
    
    print(paste0("thin = ",as.character(thin),"; samples = ",as.character(samples)))
    
    filename_out = file.path(folders[nf], paste("models/models_thin_", as.character(thin),
                                                "_samples_", as.character(samples),
                                                "_chains_",as.character(nChains),
                                                ".Rdata",sep = ""))
    filename_out_TEMP = paste(gsub(".Rdata", "_TEMP.Rdata", filename_out))
    
    if(file.exists(filename_in) == TRUE & file.exists(filename_out) == FALSE & file.exists(filename_out_TEMP) == FALSE) {
      
      save(nf,file= filename_out_TEMP)
      
      load(file = filename_in) #models, modelnames
      
      nm = length(models)
      runtimes = vector(length=nm)
      no.error = TRUE
      for (model in 1:nm) {
        print(paste0("model = ",as.character(model)))
        timer$start(paste0("McMc_timer", nm))
        m = models[[model]]
        xx=try(sampleMcmc(m, samples = samples, thin=thin,
                          adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                          transient = ceiling(0.5*samples*thin),
                          nChains = nChains, nParallel = nChains))
        timer$stop(paste0("McMc_timer", nm))
        if(class(xx)=="try-error"){
          print(paste0("ERROR with: ",filename_in))
          file.remove(filename_out_TEMP)
          no.error = FALSE
        } else {
          models[[model]] = xx
          runtimes[[model]] = timer$getTimeElapsed(paste0("McMc_timer", nm))
        }
      }
      if(no.error){
        save(models,modelnames,file=filename_out)
        file.remove(filename_out_TEMP)
        save(runtimes,file=gsub(".Rdata", "_runtimes.Rdata", filename_out))
      }
    }
  }
}