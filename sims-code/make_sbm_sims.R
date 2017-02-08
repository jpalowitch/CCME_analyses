source("sims-code/sbm_funs3.R")

options(warn = 2)

total_expers <- c("1",
                  "2",
                  "3",
                  "4",
                  "5",
                  "6",
                  "7",
                  "8",
                  "9")

# The choices below correspond to the positions in the total_expers vector,
# NOT the actual experiment names
run_expers <- c(1)

# Specify whether or not to re-make sims (i.e. just write oslom scripts)
runSBM <- FALSE

# Specify whether or not to re-draw seeds
redrawSeeds <- FALSE

# Specify whether or not to write to an OSLOM folder (set to FALSE
# if experiment folders have not yet been created)
writeOSLOM <- FALSE

# Specify whether or not to write the final run scripts for OSLOM and SLPA
# (Set to false if you are not running through the entire loops all at once,
#  or if the OSLOM2 folder is not yet in the experiment folder)
writeScripts <- TRUE

# This should consistent throughout the experiments
nreps <- 20

for (exper in run_expers) {
  
  set.seed(12345)
  
  exper_string <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", exper_string)
  if (!dir.exists(root_dir)) {dir.create(root_dir)}
  
  # Loading the parameters (change them with sims/sbm_sims/make_par_lists.R)
  load(paste0("sims-results/sbm-par-lists/", exper_string, ".RData"))
  
  # Getting par settings
  par_divs <- ncol(par_settings)
  par_points <- 1:par_divs / (par_divs + 1)
  
  # Getting rep folder names
  rep_dirs <- as.character(1:nreps)
  
  # Initializing script lines
  oslom_run_lines <- character(0)
  slpa_run_lines <- character(0)
  fullNames <- character(0)
  datNames <- character(0)
  
  for (p in 1:par_divs) {
    
    cat("Making sim", exper_string, "par num", p, "- ")
    
    # Setting directory
    curr_dir_p <- file.path(root_dir, par_dirs[p])
    if (!dir.exists(curr_dir_p)) {dir.create(curr_dir_p)}
    
    # Getting par_list_p
    par_list_p <- par_list
    for (j in 1:length(pars)) {
      par <- pars[j]
      par_indx <- which(names(par_list) == par)
      par_list_p[[par_indx]] <- par_settings[j, p]
    }
    
    
    
    for (rep in 1:nreps) {
      
      if (rep == 1)
        cat("rep: ")
      cat(rep)
      if (rep == nreps)
        cat("\n")
      
      # Setting directory
      curr_dir_p_rep <- file.path(curr_dir_p, rep_dirs[rep])
      if (!dir.exists(curr_dir_p_rep)) {dir.create(curr_dir_p_rep)}
      sbm_fn <- file.path(curr_dir_p_rep, "sbm.RData")
      
      if (runSBM) {
        
        # Draw random seed and save
        if (redrawSeeds) {
          seed_draw <- sample(1e6, 1)
          writeLines(as.character(seed_draw), 
                     con = file.path(curr_dir_p_rep, "sbm_seed.txt"))
          set.seed(seed_draw)
        } else {
          set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                  "sbm_seed.txt")
                                        )))
        }
        
        sbm <- make_sbm(par_list_p)

        gc()
      
        save(sbm, file = sbm_fn)
        edge_list_to_write <- sbm$edge_list

        gc()
        
        # Making an ipairs file and saving
        write.table(sbm$edge_list, 
                    file = file.path(curr_dir_p_rep, "network.ipairs"), 
                    row.names = FALSE, 
                    col.names = FALSE)

        gc()

      } 

      dat_fn <- paste0(p, "_", rep, ".dat")      
      
      if (writeOSLOM) {
        
        load(sbm_fn)

        edge_list_to_write <- sbm$edge_list
        
        # Making OSLOM directory if it does not exist
        if (!dir.exists(file.path(root_dir, "OSLOM2")))
          dir.create(file.path(root_dir, "OSLOM2"))
        
        write.table(edge_list_to_write,
                    file = file.path(root_dir, "OSLOM2",
                                    dat_fn),
                    row.names = FALSE,
                    col.names = FALSE)
      }
      
      inside_addline <- paste0("./oslom_undir -f ",
                               dat_fn,
                               " -w -singlet -fast")
      oslom_run_lines <- c(oslom_run_lines,
                           inside_addline)
      
      fullNames <- c(fullNames, file.path(par_dirs[p], rep, "sbm.RData"))
      datNames <- c(datNames, dat_fn)
      
      
      slpa_addline <- paste0("java -jar ",
                             #file.path(getwd(), 
                             file.path(
                                       "methodFiles/GANXiS_v3.0.2/GANXiSw.jar"),
                             " -i ",
                             #file.path(getwd(), curr_dir_p_rep, "network.ipairs"),
                             file.path(curr_dir_p_rep, "network.ipairs"),
                             " -Sym 1 -r 0.1 -d ",
                             #file.path(getwd(), curr_dir_p_rep))
                             file.path(curr_dir_p_rep))
      slpa_run_lines <- c(slpa_run_lines,
                          slpa_addline)
      
    }
    
  }
  


  if (writeScripts) {
    writeLines(oslom_run_lines, file.path(root_dir, "OSLOM2/run_script.txt"))
    save(datNames, fullNames, 
         file = file.path(root_dir, "OSLOM2", paste0("sims_", exper_string, ".RData")))
    
    writeLines(slpa_run_lines, file.path(root_dir, "slpa_run_script.bat"))
  }

  
}

# Writing full OSLOM2 prep and run script
oslom_full_fn <- "sims-code/OSLOM2_sbm_sims.txt"
file.create(oslom_full_fn)
sink(oslom_full_fn)

for (exper in run_expers) {

  exper_string <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", exper_string)
  
  # Coping OSLOM files
  cat(paste0("cp -R methodFiles/OSLOM2/* ", 
             file.path(root_dir, "OSLOM2"), "\n"))
  
  # Set the directory
  cat(paste0("cd ", file.path(root_dir, "OSLOM2"), "\n"))
  
  # Compiling OSLOM
  cat("chmod 744 compile_all.sh\n./compile_all.sh\n")
  
  # Run the run_script
  cat('gnome-terminal -e "bash run_script.txt"\n')
  
  # Re-set directory
  cat(paste0("cd ../../../\n"))
  
}

sink()