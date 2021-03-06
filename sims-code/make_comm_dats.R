source("sims-code/sbm_funs3.R")


total_expers <- readLines("sims-results/exper-names.txt")

# Do saving or just script write?
doSave <- TRUE

# The choices below correspond to the positions in the total_expers vector,
# NOT the actual experiment names
run_expers <- c(1:length(total_expers))

# This should consistent throughout the experiments
nreps <- 20

# Set method names:
methNames = c("ccme", "oslom", "slpa", "fast_greedy", "infomap", "walktrap", "louvain", "graphtool")

# Fixed variable to call the "mutual" function

mutual <- paste0("./../../../../methodFiles/mutual3/mutual ")

run_calc_scripts <- character(0)
null_expers <- integer(0)
options(warn = 2)


for (exper in run_expers) {
  
  expString <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", expString)
  
  # Getting par values
  load(file.path("sims-results/sbm-par-lists", paste0(expString, ".RData")))
  
  calc_script_lines <- character(0)
  
  for (p in 1:par_divs) {
    
    cat("########\n")
    cat("exper =", exper, "p =", p, "\n")
    cat("########\n")
    
    curr_dir_p <- file.path(root_dir, par_dirs[p])
    
    for (rep in 1:nreps) {
        
      cat("rep =",rep,"\n")
      curr_dir_p_rep <- file.path(curr_dir_p, rep)
      
      if (doSave) {
        # Getting truth
        load(file.path(curr_dir_p_rep, "sbm.RData"))
      }
              
      for (meth in setdiff(methNames, "truth")) {
        
        
        if (sbm$param_list$N > 0) {
          
          if (doSave) {
          
            # Finding nmi nodes
            load(file.path(curr_dir_p_rep, paste0(meth, ".RData")))
            found_nodes <- unique(unlist(results$communities))
            
            if (length(found_nodes) > 0) {
              
              truth_list <- sbm$truth$communities
              if (meth == "graphtool") {
                load(file.path(curr_dir_p_rep, "ig_lookup.RData"))
                truth_list <- lapply(truth_list, function (C) ig_lookup[C])
                rm(ig_lookup)
              }
            
              comm_nodes <- unique(unlist(truth_list))
              nmi_nodes <- intersect(comm_nodes, found_nodes)
              
              # Getting nmi results
              filter_nmi <- function (comm){intersect(comm, nmi_nodes)}
              nmi_truth <- lapply(truth_list, filter_nmi)
              nmi_found <- lapply(results$communities, filter_nmi)
              
              # saving
              save_result_dat(nmi_truth, curr_dir_p_rep, paste0("truth_", meth))
              save_result_dat(nmi_found, curr_dir_p_rep, meth)
              
            } else {
              
              # saving
              save_result_dat(sbm$truth$communities, curr_dir_p_rep, paste0("truth_", meth))
              writeLines("", file.path(curr_dir_p_rep, paste0(meth, "_comms.dat")))
              
            }
            
          }

          next_calc_line1 <- paste0("cd ", file.path(curr_dir_p_rep))
          
          # This chunk was from when I was using my modified mutual3, for Windows
          # Added functionality to save within the c++ function
          #next_calc_line2 <- paste0(mutual,
          #                          paste0(meth, "_comms.dat "),
          #                          paste0("truth_", meth, "_comms.dat "),
          #                          paste0(meth, "_mutual.txt"))
          
          # This uses a kosherized iOS/Linux approach
          # Basic mutual3 just prints the score
          # The following saves the print to a .txt file through terminal
          next_calc_line2 <- paste0(mutual,
                                    paste0(meth, "_comms.dat "),
                                    paste0("truth_", meth, "_comms.dat "),
                                    paste0("> ", meth, "_mutual.txt"))
          
          next_calc_line3 <- paste0("cd ", "../../../../")
          calc_script_lines <- c(calc_script_lines,
                                 next_calc_line1,
                                 next_calc_line2,
                                 next_calc_line3)
          
        }
        
      }
        
    }
      
  }
  
    
    
  writeLines(calc_script_lines,
             file.path(root_dir, "mutual_calc.txt"))
      
}

# Making all_calc script

run_calc_scripts <- character(0)

for (exper in run_expers) {
  
  expString <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", expString)

  if (!exper %in% null_expers) {
    run_calc_scripts <- c(run_calc_scripts,
                          paste0("bash ", file.path(root_dir),
                                 "/mutual_calc.txt"))
  }

  
}

writeLines(run_calc_scripts,
           "sims-results/run-code/all_mutual_calcs.txt")
