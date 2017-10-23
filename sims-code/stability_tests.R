Args = commandArgs(TRUE)
library(igraph)
library(Matrix)
library(Rcpp)
source("sims-code/sbm_funs3.R")
sourceCpp("methodFiles/CCME/new_funs.cpp")
source("methodFiles/CCME/CCME.R")
total_expers <- readLines("sims-results/exper-names.txt")

if (length(Args) < 2) {
  batch_name <- "0"
  first_exper <- 1
  last_exper <- 9
  runCCME <- TRUE
  runIGRAPH <- TRUE
} else {
  batch_name <- Args[1]
  first_exper <- as.numeric(Args[2])
  last_exper <- as.numeric(Args[3])
  runCCME <- TRUE
  runIGRAPH <- TRUE
}

run_expers <- first_exper:last_exper

# This should consistent throughout the experiments
# (and match the same variable in sims/lfr/make_lfr_sims.R)
nreps <- 20
ndups <- 30
tmpdir <- tempdir()
mutual_dir <- "methodFiles/mutual3/mutual"

set.seed(12345)

for (exper in run_expers) {
    
  exper_string <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", exper_string)

  # Loading parameters
  load(paste0(file.path("sims-results/sbm-par-lists", exper_string),
              ".RData"))
  
  onmi_stab_means <- onmi_stab_sds <- matrix(0, par_divs, nreps)
  
  for (p in 1:par_divs) {
    
    curr_dir_p <- file.path(root_dir, par_dirs[p])
    
    for (rep in 1:nreps) {
      
      cat("exper", exper, "p", p, "rep", rep, "\n")
      
      if (rep == 1) {
      
        # Loading data
        curr_dir_p_rep <- file.path(curr_dir_p, rep)
        load(file.path(curr_dir_p_rep, "sbm.RData"))
        
        for (j in 1:ndups) {
          
          cat("--duplication", j, "\n")
        
          # Getting results
          results <- CCME(sbm$edge_list, 
                          updateOutput = FALSE, 
                          generalOutput = FALSE,
                          loopOutput = FALSE)
          
          # Converting to ONMI format and writing
          comms_w <- unlist(lapply(results$communities,
                                   function (C) paste(C, collapse = " ")))
          writeLines(comms_w, file.path(tmpdir, paste0("comms_", j, ".dat")))
          
        }
        
        cat("--calculating onmis...\n")
        onmiMat <- matrix(0, ndups, ndups)
        
        for (j1 in 1:(ndups - 1)) {
          
          for (j2 in (j1 + 1):ndups) {
            
            fn1 <- file.path(tmpdir, paste0("comms_", j1, ".dat"))
            fn2 <- file.path(tmpdir, paste0("comms_", j2, ".dat"))
          
            onmiCalc <- system(paste(mutual_dir, fn1, fn2, ">",
                                     file.path(tmpdir, "mut.dat")))
            onmiText <- readLines(file.path(tmpdir, "mut.dat"))
            onmiMat[j1, j2] <- as.numeric(strsplit(onmiText, "\t")[[1]][2])
            
          }
          
        }
        
        onmi_stab_means[p, rep] <- mean(onmiMat[upper.tri(onmiMat)])
        onmi_stab_sds[p, rep]   <- sd(onmiMat[upper.tri(onmiMat)])
        
      }
        
    }
    
  }
  
  write.table(onmi_stab_means, 
              quote = FALSE, row.names = FALSE, col.names = FALSE,
              file = file.path(root_dir, "onmi_stab_means.dat"))
  
  write.table(onmi_stab_sds, 
              quote = FALSE, row.names = FALSE, col.names = FALSE,
              file = file.path(root_dir, "onmi_stab_sds.dat"))
  
}
