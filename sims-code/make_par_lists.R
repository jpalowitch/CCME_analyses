source("sims-code/sbm_funs3.R")

# How many instances can you run at once? Default values are recommended for home computers
nrun_r <- 2
nrun_oslom <- 2
nrun_slpa <- 1

# Set the path to find & source the CCME code
CCMEpath <- "../CCME"

# For the  experiments, do you want to shove the dec vec up to one?
shove_dec <- TRUE

# Global settings
par_divs <- 9
par_seq_dec <- 1:par_divs / (par_divs + 1)
par_seq  <- round(100 * (1:par_divs / (par_divs + 1)))
par_dirs <- as.character(par_seq)

# Give the names of your experiments: must be manually entered.
total_expers <- c("1",
                  "4",
                  "5",
                  "9",
                  "7",
                  "8",
                  "6")

if (!dir.exists("sims-results/sbm-par-lists"))
  dir.create("sims-results/sbm-par-lists", recursive = TRUE)

# Experiment 1 -----------------------------------------------------------------

main_text <- "Increase N"
par_list <- make_param_list2()
pars <- c("N", "k", "max_k", "max_c", "min_c")
axis_par <- 1
par_settings <- matrix(0, 5, par_divs)
par_settings[1, ] <- round(2000 * (par_seq_dec + min(par_seq_dec) * 
                                     as.numeric(shove_dec)))
par_settings[2, ] <- sqrt(par_settings[1, ])
par_settings[3, ] <- 3 * par_settings[2, ]
par_settings[4, ] <- par_settings[1, ] * 3 / 10
par_settings[5, ] <- par_settings[4, ] * 2 / 3

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment1.RData")

# Experiment 2 -----------------------------------------------------------------

main_text <- "Overall community sizes"
par_list <- make_param_list2()
pars <- c("c_scaler")
axis_par_string <- expression(c[1])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- par_seq_dec + min(par_seq_dec) * as.numeric(shove_dec)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     axis_par_string,
     file = "sims-results/sbm-par-lists/experiment2.RData")

# Experiment 3 -----------------------------------------------------------------

main_text <- "Shrink degs"
par_list <- make_param_list2()
pars <- c("deg_scaler")
axis_par <- 1
axis_par_string <- expression(d[1])
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- par_seq_dec + min(par_seq_dec) * as.numeric(shove_dec)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment3.RData")

# Experiment 4 -----------------------------------------------------------------

main_text <- "Increase edge signal"
par_list <- make_param_list2()
pars <- c("s2n_e")
axis_par_string <- expression(s[e])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1

save(par_list, 
     main_text,
     axis_par,
     pars,
     axis_par_string,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment4.RData")

# Experiment 5 -----------------------------------------------------------------

main_text <- "Increase weight signal"
par_list <- make_param_list2()
pars <- c("s2n_w")
axis_par_string <- expression(s[w])
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment5.RData")

# Experiment 6 -----------------------------------------------------------------

main_text <- "Increase edge and weight signal"
par_list <- make_param_list2()
axis_par_string <- expression(s[e] ~ "and" ~ s[w])
pars <- c("s2n_e", "s2n_w")
axis_par <- 1
par_settings <- matrix(0, 2, par_divs)
par_settings[1, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1
par_settings[2, ] <- (min(par_seq_dec) * as.numeric(shove_dec) + 
                        par_seq_dec) * 2 + 1


save(par_list,
     main_text,
     axis_par,
     pars,
     axis_par_string,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment6.RData")


# Experiment 7 -----------------------------------------------------------------

main_text <- "Increase # on"
par_list <- make_param_list2(om = 2)
pars <- c("on")
axis_par_string <- "# Overlap nodes"
axis_par <- 1
par_settings <- matrix(0, 1, par_divs)
par_settings[1, ] <- round((1 - (min(par_seq_dec) * as.numeric(shove_dec) + 
                                   par_seq_dec)) * 4000)

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment7.RData")

# Experiment 9 -----------------------------------------------------------------

main_text <- "grow N, |BG|=1000, on=0.25N"
par_list <- make_param_list2(hv = 1000)
pars <- c("N", "k", "max_k", "max_c", "min_c", "on")
axis_par <- 1
par_settings <- matrix(0, 6, par_divs)
par_settings[1, ] <- round(5000 * (par_seq_dec + min(par_seq_dec) * 
                                     as.numeric(shove_dec)))
par_settings[2, ] <- sqrt(par_settings[1, ] + 1000)
par_settings[3, ] <- 3 * par_settings[2, ]
par_settings[4, ] <- par_settings[1, ] * 3 / 10
par_settings[5, ] <- par_settings[4, ] * 2 / 3
par_settings[6, ] <- 0.25 * par_settings[1, ]

save(par_list,
     main_text,
     axis_par,
     pars,
     par_settings,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment9.RData")

# Experiment 8 -----------------------------------------------------------------

main_text <- "Increase om"
par_list <- make_param_list2(on = 500)
pars <- c("om")
axis_par_string <- "# Memberships per overlap node"
axis_par <- 1
par_settings <- matrix(0, 1, 4)
par_settings[1, ] <- 4:1
par_seq <- 1:4
par_divs <- 4
par_dirs <- as.character(par_seq)

save(par_list, 
     main_text,
     axis_par,
     pars,
     par_settings,
     axis_par_string,
     par_seq,
     par_divs,
     par_dirs,
     file = "sims-results/sbm-par-lists/experiment8.RData")

# ------------------------------------------------------------------------------
# Writing run scripts
# ------------------------------------------------------------------------------
if (!dir.exists("sims-results/run-code"))
  dir.create("sims-results/run-code")

# Saving config files for future runs
writeLines(total_expers, "sims-results/exper-names.txt")

batch_script_dir <- "sims-results/run-code/batch-scripts"
if (!dir.exists(batch_script_dir))
  dir.create(batch_script_dir)


# ------------------------------------------------------------------------------
# Writing R batch files

# Writing batch run files
batches <- rep(1:nrun_r, each = ceiling(length(total_expers) / nrun_r))
batches <- batches[1:length(total_expers)]
batches <- lapply(1:nrun_r, function (i) which(batches == i))

# Initializing scripts to run batches, and logfiles
sbmfn0 <- paste0("sims-results/run-code/make-sbm-sims")
rmethfn0 <- paste0("sims-results/run-code/run-rmeth")
file.create(paste0(c(sbmfn0, rmethfn0), ".txt"))
file.create(paste0(c(sbmfn0, rmethfn0), "_log.txt"))

for (b in 1:nrun_r) {
  
  batch <- batches[[b]]
  batchname <- paste0("batch", b)
  
  # Initializing batch files
  sbmfn <- file.path(batch_script_dir, paste0("make-sbm-sims-", batchname))
  rmethfn <- file.path(batch_script_dir, paste0("run_rmeth-", batchname))
  file.create(paste0(c(sbmfn, rmethfn), ".txt"))
  file.create(paste0(c(sbmfn, rmethfn), "_log.txt"))
  
  # Writing on script to run sim batches
  fileConn <- file(paste0(sbmfn0, ".txt"), "a")
  writeLines(paste("Rscript --vanilla sims-code/make_sbm_sims.R", 
                   b, min(batch), max(batch),
                   "1> /dev/null",
                   "2>", paste0(sbmfn, "_log.txt"), "&"),
             fileConn)
  close(fileConn)
   
  # Writing on script to run R methods
  fileConn <- file(paste0(rmethfn0, ".txt"), "a")
  writeLines(paste("Rscript --vanilla sims-code/run_R_methods.R",
                   b, min(batch), max(batch),
                   "1> /dev/null",
                   "2>", paste0(rmethfn, "_log.txt"), "&"),
             fileConn)
  close(fileConn)
  
}

#-------------------------------------------------------------------------------
# Writing OSLOM batch files

# Writing batch run files
batches <- rep(1:nrun_oslom, each = ceiling(length(total_expers) / nrun_oslom))
batches <- batches[1:length(total_expers)]
batches <- lapply(1:nrun_oslom, function (i) which(batches == i))

# Initializing scripts to run batches, and logfiles
oslomfn0 <- paste0("sims-results/run-code/run-oslom")
file.create(paste0(c(oslomfn0), ".txt"))
file.create(paste0(c(oslomfn0), "_log.txt"))

for (b in 1:nrun_oslom) {
  
  batch <- batches[[b]]
  batchname <- paste0("batch", b)
  
  # Initializing batch files
  oslomfn <- file.path(batch_script_dir, paste0("run_oslom-", batchname))
  file.create(paste0(c(oslomfn), ".txt"))
  file.create(paste0(c(oslomfn), "_log.txt"))
  
  # Writing on script to run oslom batches
  fileConn <- file(paste0(oslomfn0, ".txt"), "a")
  writeLines(paste("bash", paste0(oslomfn, ".txt"),
                   "1> /dev/null",
                   "2>", paste0(oslomfn, "_log.txt"), "&"),
             fileConn)
  close(fileConn)
  
  # Writing batch scripts to run OSLOM
  fileConn <- file(paste0(oslomfn, ".txt"), "a")
  
  for (exper in batch) {
    
    exper_string <- paste0("experiment", total_expers[exper])
    
    # Finding the folder
    root_dir <- file.path("sims-results", exper_string)

    # Coping OSLOM files
    writeLines(paste0("cp -R methodFiles/OSLOM2/* ", 
                      file.path(root_dir, "OSLOM2")),
               fileConn)
    
    # Set the directory
    writeLines(paste0("cd ", file.path(root_dir, "OSLOM2")),
               fileConn)
    
    # Compiling OSLOM
    writeLines("chmod 744 compile_all.sh && ./compile_all.sh",
               fileConn)
    
    # create the log file
    writeLines('touch log.txt',
               fileConn)
    
    # Run the run_script
    writeLines('bash run_script.txt 1> /dev/null 2> log.txt &',
               fileConn)
    
    # Re-set directory
    writeLines(paste0("cd ../../../"),
               fileConn)
    
  }
  close(fileConn)
  
}

#-------------------------------------------------------------------------------
# Writing SLPA batch files


# Writing batch2 run files
batches2 <- rep(1:nrun_slpa, each = ceiling(length(total_expers) / nrun_slpa))
batches2 <- batches2[1:length(total_expers)]
batches2 <- lapply(1:nrun_slpa, function (i) which(batches2 == i))

# Initializing scripts to run non-R batches, and logfiles
slpawfn0 <- paste0("sims-results/run-code/run-slpa")
file.create(paste0(c(slpawfn0), ".txt"))
file.create(paste0(c(slpawfn0), "_log.txt"))

for (b in 1:nrun_slpa) {
  
  batch <- batches2[[b]]
  batchname <- paste0("batch", b)
  
  # Initializing batch files
  slpawfn <- file.path(batch_script_dir, paste0("run_slpaw-", batchname))
  file.create(paste0(c(slpawfn), ".txt"))
  file.create(paste0(c(slpawfn), "_log.txt"))
  
  # Writing on script to run slpa batches
  fileConn <- file(paste0(slpawfn0, ".txt"), "a")
  writeLines(paste("bash", paste0(slpawfn, ".txt"),
                   "1> /dev/null",
                   "2>", paste0(slpawfn, "_log.txt"), "&"),
             fileConn)
  close(fileConn)
  
  # Writing batch scripts to run SLPA
  fileConn <- file(paste0(slpawfn, ".txt"), "a")
  
  for (exper in batch) {
    
    exper_string <- paste0("experiment", total_expers[exper])
    
    # Finding the folder
    root_dir <- file.path("sims-results", exper_string)
    
    # Set log and run file destination
    runfile <- file.path(root_dir, 'slpa_run_script.bat')
    logfile <- file.path(root_dir, 'slpa_run_script_log.txt')
    
    # create the log file
    writeLines(paste('touch', logfile), fileConn)
    
    # Run the run_script
    writeLines(paste('bash', runfile, '1> /dev/null 2>', logfile, '&'),
               fileConn)
    
  }
  close(fileConn)
  
}

# Writing CCME path file
ccmepathfn <- "sims-results/run-code/ccme-path.txt"
fileConn <- file(ccmepathfn)
writeLines(paste0(CCMEpath), con = fileConn)
close(fileConn)

