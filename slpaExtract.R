source("slpaRead.R")

slpaExtract <- function (rootDir, batchTag) {
  
  
  fullNames <- list.files(rootDir, recursive = TRUE)
  fullNames <- fullNames[sapply(fullNames, function (fn) grepl("icpm", fn))]
  sbmStems <- sapply(fullNames, function (fn) strsplit(fn, "SLPA")[[1]][1])
  names(sbmStems) <- NULL

  for (i in 1:length(fullNames)) {
      
    fn <- file.path(rootDir, fullNames[i])
    slpa_fn <- file.path(rootDir, paste0(sbmStems[i], "slpa.RData"))
    sbm_fn <- file.path(rootDir, paste0(sbmStems[i], "sbm.RData"))
    
    # Loading sbm
    load(sbm_fn)
    
    cat("Saving", slpa_fn,"\n")
    
    # Getting results
    results <- slpaRead(fn)
    
    # Finding background, if any
    bg <- setdiff(1:(sbm$param_list$N + sbm$param_list$hv),
                  unique(unlist(results$communities)))
    results$background <- bg
    
    # Saving
    save(results, file = slpa_fn)
  }
  return("finished")
}