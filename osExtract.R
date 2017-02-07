source("osRead.R")

osExtract <- function (rootDir, batchTag, do_slow = FALSE) {
  
  
  load(file.path(rootDir, "OSLOM2", paste0(batchTag,".RData")))

  for (i in 1:length(fullNames)) {
      
    fn <- fullNames[i]
    datName <- datNames[i]
    
    # Finding results folder
    fn_parts <- strsplit(fn, "/", fixed = TRUE)[[1]]
    fn_dir <- file.path(fn_parts[1], fn_parts[2])
    
    cat("Saving", datName,"\n")
    
    # Getting results
    results <- osRead(file.path(rootDir, "OSLOM2",
                                paste0(datName, "_oslo_files"), "tp"))
    
    # Saving
    save(results, 
         file = file.path(rootDir, fn_dir,
                          "oslom.RData"))
  }
  
  if (do_slow) {
    load(file.path(rootDir, "OSLOM2_slow", paste0(batchTag,".RData")))
    
    for (i in 1:length(fullNames)) {
      
      fn <- fullNames[i]
      datName <- datNames[i]
      
      # Finding results folder
      fn_parts <- strsplit(fn, "/", fixed = TRUE)[[1]]
      fn_dir <- file.path(fn_parts[1], fn_parts[2])
      
      cat("Saving", datName,"\n")
      
      # Getting results
      results <- osRead(file.path(rootDir, "OSLOM2",
                                  paste0(datName, "_oslo_files"), "tp"))
      
      # Saving
      save(results, 
           file = file.path(rootDir, fn_dir,
                            "oslom_slow.RData"))
    }
  }
  return("finished")
}