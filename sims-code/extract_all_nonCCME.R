# Extracting

source("osExtract.R")
source("slpaExtract.R")

total_expers <- readLines("sims-results/exper-names.txt")

# Sbm or ccm?
sbm <- TRUE

do_os <- TRUE
do_slpa <- TRUE


# Set expers
run_expers <- c(1, 4:length(total_expers))

for (i in run_expers) {
  
  cat("#-------------------------------------\n")
  cat("# Experiment", i, "\n")
  cat("#-------------------------------------\n")
  
  
  if (sbm) {
    exper_string <- paste0("experiment", i)
    rootDir <- file.path("sims-results", exper_string)
  } else {
    exper_string <- paste0("experiment", i)
    rootDir <- file.path("sims-results", exper_string)
  }
  batchTag <- paste0("sims_", exper_string)
  
  if (do_os) {
    osExtract(rootDir, batchTag)
  }
  
  if (do_slpa) {
    slpaExtract(rootDir, batchTag)
  }
  
}
