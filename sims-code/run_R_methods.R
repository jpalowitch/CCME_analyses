Args = commandArgs(TRUE)
library(igraph)
library(Matrix)
source("sims-code/sbm_funs3.R")
source("methodFiles/ccme/CCME.R")

total_expers <- readLines("sims-results/exper-names.txt")

if (length(Args) < 2) {
  batch_name <- "0"
  first_exper <- 1
  last_exper <- 9
  redrawSeeds <- FALSE
  runCCME <- FALSE
  runIGRAPH <- FALSE
} else {
  batch_name <- Args[1]
  first_exper <- as.numeric(Args[2])
  last_exper <- as.numeric(Args[3])
  redrawSeeds <- TRUE
  runCCME <- TRUE
  runIGRAPH <- TRUE
}

run_expers <- first_exper:last_exper

# This should consistent throughout the experiments
# (and match the same variable in sims/lfr/make_lfr_sims.R)
nreps <- 20

for (exper in run_expers) {
    
  exper_string <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", exper_string)

  # Loading parameters
  load(paste0(file.path("sims-results/sbm-par-lists", exper_string),
              ".RData"))
  
  for (p in 1:par_divs) {
    
    curr_dir_p <- file.path(root_dir, par_dirs[p])
    
    for (rep in 1:nreps) {

      cat("exper", exper, "p", p, "rep", rep, "\n")
      
      curr_dir_p_rep <- file.path(curr_dir_p, rep)
      load(file.path(curr_dir_p_rep, "sbm.RData"))
    
      if (runCCME) {
        # Draw random seed and save
        if (redrawSeeds) {
          seed_draw <- sample(1e6, 1)
          writeLines(as.character(seed_draw), 
                     con = file.path(curr_dir_p_rep, "ccme_seed.txt"))
          set.seed(seed_draw)
        } else {
          set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                  "ccme_seed.txt")
                                        )))
        }
         
        results <- CCME(sbm$edge_list, updateOutput = TRUE)
        save(results, file = file.path(curr_dir_p_rep, "ccme.RData"))
        
      }
      
      if (runIGRAPH) {
        
        # Formatting the SBM
        G <- graph.edgelist(as.matrix(sbm$edge_list[ , c(1:2)]), directed = FALSE)
        E(G)$weight <- sbm$edge_list$weight
        results0 <- list("communities" = NULL,
                         "background" = NULL)
        node_list <- unique(as.vector(unlist(sbm$edge_list[ , c(1:2)])))
        
        # WALKTRAP
        
          # Draw random seed and save
          if (redrawSeeds) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), 
                       con = file.path(curr_dir_p_rep, "walktrap_seed.txt"))
            set.seed(seed_draw)
          } else {
            set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                    "walktrap_seed.txt")
            )))
          }
          
          # Formatting and saving results
          ig_results <- cluster_walktrap(G)
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, file = file.path(curr_dir_p_rep, "walktrap.RData"))
        
        # INFOMAP
          
          # Draw random seed and save
          if (redrawSeeds) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), 
                       con = file.path(curr_dir_p_rep, "infomap_seed.txt"))
            set.seed(seed_draw)
          } else {
            set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                    "infomap_seed.txt")
            )))
          }
          
          # Formatting and saving results
          ig_results <- cluster_infomap(G)
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, file = file.path(curr_dir_p_rep, "infomap.RData"))
        
        # FAST GREEDY
          
          # Draw random seed and save
          if (redrawSeeds) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), 
                       con = file.path(curr_dir_p_rep, "fast_greedy_seed.txt"))
            set.seed(seed_draw)
          } else {
            set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                    "fast_greedy_seed.txt")
            )))
          }
          
          # Formatting and saving results
          ig_results <- cluster_fast_greedy(G)
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, file = file.path(curr_dir_p_rep, "fast_greedy.RData"))
        
      }
      
    }
    
  }
  
}
