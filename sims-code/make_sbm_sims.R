Args = commandArgs(trailingOnly=TRUE)

cat("making sbm sims\n")

source("sims-code/sbm_funs3.R")

total_expers <- readLines("sims-results/exper-names.txt")

if (length(Args) < 2) {
  batch_name <- "0"
  first_exper <- 1
  last_exper <- 9
  redrawSeeds <- TRUE
  writeOSLOM <- TRUE
  runSBM <- TRUE
  removeRoot <- TRUE
} else {
  batch_name <- Args[1]
  first_exper <- as.numeric(Args[2])
  last_exper <- as.numeric(Args[3])
  redrawSeeds <- TRUE
  writeOSLOM <- TRUE
  runSBM <- TRUE
  removeRoot <- TRUE
}

cat("args are", Args, "\n")

run_expers <- first_exper:last_exper

# Specify whether or not to write the final run scripts for OSLOM and SLPA
# (Set to false if you are not running through the entire loops all at once,
#  or if the OSLOM2 folder is not yet in the experiment folder)
writeScripts <- TRUE

# This should consistent throughout the experiments
nreps <- 20

for (exper in run_expers) {
  
  set.seed(12345)
  
  exper_string <- paste0("experiment", total_expers[exper])
  
  log_path <- file.path("sims-results/run-code/batch-scripts", exper_string)
  if (file.exists(paste0(log_path, "_progress.txt"))) {
    file.remove(paste0(log_path, "_progress.txt"))
  } else { 
    file.create(paste0(log_path, "_progress.txt"))
    fileConn <- file(paste0(log_path, "_progress.txt"), "a")
    writeLines(paste("Begun writing", exper_string, "\n"), fileConn)
    close(fileConn)
  }
  
  # Finding the folder
  root_dir <- file.path("sims-results", exper_string)
  if (removeRoot) {
    unlink(root_dir, recursive = TRUE)
  }
  if (!dir.exists(root_dir)) {
    dir.create(root_dir, recursive = TRUE)
  }
  
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
    
    fileConn <- file(paste0(log_path, "_progress.txt"), "a")
    writeLines(paste("--Begun writing ", p, "\n"), fileConn)
    close(fileConn)
    
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
        seed_fn <- file.path(curr_dir_p_rep, "sbm_seed.txt")
        if (!file.exists(seed_fn)) {
          seed_draw <- sample(1e6, 1)
          writeLines(as.character(seed_draw), 
                     con = file.path(curr_dir_p_rep, "sbm_seed.txt"))
          set.seed(seed_draw)
        }
        set.seed(as.numeric(readLines(file.path(curr_dir_p_rep, 
                                                  "sbm_seed.txt")
                                      )))
        
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
        
        # Writing in GML format
        G_igraph <- graph.edgelist(as.matrix(sbm$edge_list[ , 1:2]), 
                                   directed = FALSE)
        E(G_igraph)$weight <- sbm$edge_list$weight
        write_graph(G_igraph, file = file.path(curr_dir_p_rep, "network.gml"),
                    format = "gml")
        
        # Writing in csv format
        write.table(sbm$edge_list, sep=',',
                    file = file.path(curr_dir_p_rep, "network.csv"),
                    row.names = FALSE,
                    col.names = FALSE)
        
        # Saving membership in txt format
        membership <- unlist(sbm$truth$memberships)
        G_igraph_recast <- igraph_recast(sbm$edge_list, membership)
        write.table(G_igraph_recast
        writeLines(as.character(membership), 
                   con=file.path(curr_dir_p_rep, "membership.dat"))

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
    
    fileConn <- file(paste0(log_path, "_progress.txt"), "a")
    writeLines(paste("--Finished writing ", p, "\n"), fileConn)
    writeLines(paste("-------------------\n"), fileConn)
    close(fileConn)
    
  }
  

  oslomfn <- file.path(root_dir, "OSLOM2/run_script.txt")
  file.create(oslomfn, recursive = TRUE)
  fileConn <- file(oslomfn)
  writeLines(oslom_run_lines, con = fileConn)
  close(fileConn)
  save(datNames, fullNames, 
       file = file.path(root_dir, "OSLOM2", paste0("sims_", exper_string, ".RData")))
    
  slpafn <- file.path(file.path(root_dir, "slpa_run_script.bat"))
  file.create(slpafn)
  fileConn <- file(slpafn)
  writeLines(slpa_run_lines, con = fileConn)
  close(fileConn)
  
  fileConn <- file(paste0(log_path, "_progress.txt"), "a")
  writeLines(paste("Finished writing ", exper_string, "\n"), fileConn)
  close(fileConn)
  
}