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

set.seed(12345)

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

      cat("exper", exper_string, "p", p, "rep", rep, "\n")
      
      curr_dir_p_rep <- file.path(curr_dir_p, rep)
      load(file.path(curr_dir_p_rep, "sbm.RData"))
    
      if (runCCME) {
        # Draw random seed and save
        seedfn <- file.path(curr_dir_p_rep, "ccme_seed.txt")
        if (!file.exists(seedfn)) {
          seed_draw <- sample(1e6, 1)
          writeLines(as.character(seed_draw), con = seedfn)
        } else {
          seed_draw <- as.integer(readLines(seedfn))
        }
        set.seed(seed_draw)
        timer <- proc.time()[3] 
        results <- CCME(sbm$edge_list, updateOutput = TRUE)
        timer <- proc.time()[3] - timer
        save(results, timer, file = file.path(curr_dir_p_rep, "ccme.RData"))
        
        #results <- CCME(sbm$edge_list, updateOutput = TRUE, fastInitial = TRUE)
        #save(results, file = file.path(curr_dir_p_rep, "ccme_fast.RData"))
        
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
          seedfn <- file.path(curr_dir_p_rep, "walktrap_seed.txt")
          if (!file.exists(seedfn)) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), con = seedfn)
          } else {
            seed_draw <- as.integer(readLines(seedfn))
          }
          set.seed(seed_draw)
            
          # Formatting and saving results
          timer <- proc.time()[3]
          ig_results <- cluster_walktrap(G)
          timer <- proc.time()[3] - timer
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, timer, file = file.path(curr_dir_p_rep, "walktrap.RData"))
        
        # INFOMAP
          
          # Draw random seed and save
          seedfn <- file.path(curr_dir_p_rep, "infomap_seed.txt")
          if (!file.exists(seedfn)) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), con = seedfn)
          } else {
            seed_draw <- as.integer(readLines(seedfn))
          }
          set.seed(seed_draw)
          
          # Formatting and saving results
          timer <- proc.time()[3]
          ig_results <- cluster_infomap(G)
          timer <- proc.time()[3] - timer
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, timer, file = file.path(curr_dir_p_rep, "infomap.RData"))
        
        # FAST GREEDY
          
          # Draw random seed and save
          seedfn <- file.path(curr_dir_p_rep, "fast_greedy_seed.txt")
          if (!file.exists(seedfn)) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), con = seedfn)
          } else {
            seed_draw <- as.integer(readLines(seedfn))
          }
          set.seed(seed_draw)
          
          # Formatting and saving results
          timer <- proc.time()[3]
          ig_results <- cluster_fast_greedy(G)
          timer <- proc.time()[3] - timer
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, timer, file = file.path(curr_dir_p_rep, "fast_greedy.RData"))
          
        # LOUVAIN
          
          # Draw random seed and save
          seedfn <- file.path(curr_dir_p_rep, "louvain_seed.txt")
          if (!file.exists(seedfn)) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), con = seedfn)
          } else {
            seed_draw <- as.integer(readLines(seedfn))
          }
          set.seed(seed_draw)
          
          # Formatting and saving results
          timer <- proc.time()[3]
          ig_results <- cluster_louvain(G)
          timer <- proc.time()[3] - timer
          results <- results0
          results$communities <- lapply(unique(ig_results$membership),
                                        function (i) 
                                          which(ig_results$membership == i))
          results$background <- setdiff(node_list, unlist(results$communities))
          save(results, timer, file = file.path(curr_dir_p_rep, "louvain.RData"))
          
        # GRAPHTOOL
          
          # Draw random seed and save
          seedfn <- file.path(curr_dir_p_rep, "graphtool_seed.txt")
          if (!file.exists(seedfn)) {
            seed_draw <- sample(1e6, 1)
            writeLines(as.character(seed_draw), con = seedfn)
          } else {
            seed_draw <- as.integer(readLines(seedfn))
          }
          set.seed(seed_draw)
          
          # Formatting and saving results
          timer <- proc.time()[3]
          system(paste("/usr/bin/python",
                       "fit_sbm.py",
                       file.path(curr_dir_p_rep, "network.gml"),
                       file.path(curr_dir_p_rep, "network_gtMemship.dat")))
          timer <- proc.time()[3] - timer
          membership <- as.integer(readLines(file.path(curr_dir_p_rep, 
                                                       "network_gtMemship.dat"))) + 1
          results <- results0
          results$communities <- lapply(1:max(membership), function (j) which(membership == j))
          results$background <- integer(0)
          save(results, timer, file = file.path(curr_dir_p_rep, "graphtool.RData"))
        
      }
      
    }
    
  }
  
}
