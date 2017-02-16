# Specify whether or not to write for OSLOM
writeOSLOM <- FALSE



saveDir <- "applications-results/airports/results"
source("saveit.R")

ccmeDir <- "methodFiles/ccme"
source(file.path(ccmeDir, "CCME.R"))

library(igraph)

monthNames <- c("jan", "feb", "mar", "apr", 
                "may", "jun", "jul", "aug", 
                "sep", "oct", "nov", "dec")

fnList <- c(monthNames, "year")
load("applications-results/airports/data/adjList_cleaned.RData")
startYear <- as.numeric(names(adjList)[1])
endYear <- as.numeric(names(adjList)[length(adjList)])
yL <- endYear - startYear + 1

# Initializing run lines for SLPAw
slpa_run_lines <- character(0)
oslom_run_lines <- character(0)
fullNames <- character(0)
datNames <- character(0)


for (y in 1:yL) {
    
  yearNum <- startYear + y - 1
  curr_dir <- file.path(saveDir, yearNum)
  fnList_y <- c(monthNames[1:length(adjList[[y]]$months)], "year")
  
  for (fn in fnList_y) {
      
    load(file.path(curr_dir, paste0(fn, ".RData")))
  
    # Formatting data for CCME run
    colnames(data) <- NULL
    rownames(data) <- NULL
    G <- graph.adjacency(data, mode = "undirected", weighted = "weight")
    edge_list <- get.edgelist(G)
    edge_list <- cbind(edge_list, E(G)$weight)
    edge_list <- as.data.frame(edge_list)
    names(edge_list) <- c("node1", "node2", "weight")
    
    dat_fn <- paste0(yearNum, "_", fn, ".dat")
    
    if (writeOSLOM) {
    
      write.table(edge_list,
                  file = file.path("airports/oslom/OSLOM2",
                                   dat_fn),
                  row.names = FALSE,
                  col.names = FALSE)
      
    }
    
    # Draw random seed and save
    if (redraw_CCME_seeds) {
      seed_draw <- sample(1e6, 1)
      writeLines(as.character(seed_draw), 
                 con = file.path(curr_dir, 
                                 paste0("seed_ccme_", fn, ".txt")))
      set.seed(seed_draw)
    } else {
      seed_lines <- readLines(file.path(curr_dir, 
                                        paste0("seed_ccme_", fn, ".txt")))
      seed_draw <- as.numeric(seed_lines)
      set.seed(seed_draw)
    }
    


    # Running  
    Timer <- proc.time()[3]
    results <- CCME(edge_list)
    Timer <- proc.time()[3] - Timer
    results$time <- Timer
    save(results, file = file.path(curr_dir, 
                                   paste0("output_ccme_",fn,".RData")))
  
    
    # Making an ipairs file and saving
    write.table(edge_list, 
                file = file.path(curr_dir, paste0("network_", fn, ".ipairs")), 
                row.names = FALSE, 
                col.names = FALSE)
    
    # Make SLPA seed
    if (redraw_SLPA_seeds) {
      seed_draw <- sample(1e6, 1)
      writeLines(as.character(seed_draw), 
                 con = file.path(curr_dir, paste0("seed_slpa_", 
                                                  fn, ".txt")
                                 )
                 )
    } else {
      seed_draw <- as.numeric(readLines(file.path(curr_dir, 
                                                  paste0("seed_slpa_", 
                                                         fn, ".txt")
                                                  )
                                        )
                              )
    }
      
    
    # Adding line to SLPA script
    slpa_addline <- paste0("java -jar ",
                           file.path(getwd(), 
                                     "methodFiles/GANXiS_v3.0.2/GANXiSw.jar"),
                           " -i ",
                           file.path(getwd(), curr_dir, 
                                     paste0("network_", fn, ".ipairs")),
                           " -Sym 1 -r 0.1 -d ",
                           file.path(getwd(), curr_dir),
                           " -seed ",
                           seed_draw)
    slpa_run_lines <- c(slpa_run_lines,
                        slpa_addline)
    
    # Adding OSLOM line to script
    inside_addline <- paste0("./oslom_undir -f ",
                             dat_fn,
                             " -w -singlet")
    oslom_run_lines <- c(oslom_run_lines,
                         inside_addline)
    
    fullNames <- c(fullNames, file.path(curr_dir, fn))
    datNames <- c(datNames, dat_fn)
    
      
  }

}

# Saving slpa lines
writeLines(slpa_run_lines, con = "airports/SLPA_run_script.bat")




writeLines(oslom_run_lines, file.path("airports/oslom/OSLOM2/aports.txt"))
save(datNames, fullNames, 
     file = file.path("airports/oslom/OSLOM2/aports.RData"))
