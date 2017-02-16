library(RColorBrewer)
library(plotrix)
library(igraph)

total_expers <- readLines("sims-results/exper-names.txt")

source("sims-code/sbm_funs3.R")
source("mod_funs.R")

# Set method names:
methNames = c("ccme", "oslom", "slpa", "fast_greedy", "walktrap", "infomap", "ccme_fast")

# Set which methods to plot and their plot names
plot_meths <- c(4:6, 2, 3, 1, 7)
plot_names <- c("FastGreedy", "Walktrap", "Infomap", "OSLOM", "SLPAw", "CCME", "CCME-fast")

# Do mod?
doMod <- FALSE

# Re-get results?
getResults <- TRUE

# Plot main text?
main_text_plot <- FALSE

plot_expers <- c(1, 4:length(total_expers))

# This should consistent throughout the experiments
nreps <- 20

# Brewing colors
colPal <- brewer.pal(9, "Set1")


for (exper in plot_expers) {
  
  expString <- paste0("experiment", total_expers[exper])
  
  # Finding the folder
  root_dir <- file.path("sims-results", expString)
  
  # Getting par values
  if (exists("axis_par_string")) {rm("axis_par_string")}
  load(file.path("sims-results/sbm-par-lists", paste0(expString, ".RData")))
  
  if (getResults) {
    # Setting up score mats
    scores0 <- matrix(0, nreps, par_divs)
    mod_scores  <- rep(list(scores0), length(methNames))
    nmi_scores <- rep(list(scores0), length(methNames)) 
    typeI <- rep(list(scores0), length(methNames))
    typeII <- rep(list(scores0), length(methNames))
    on_typeI <- rep(list(scores0), length(methNames))
    on_typeII <-rep(list(scores0), length(methNames))
    
    # Figure out if this sim has homeless vertices or overlap
    hv_present <- grepl("hv", expString)
    on_present <- grepl("on", expString)
    
    
    for (p in 1:par_divs) {
      
      cat("########\n")
      cat("p=",p,"\n")
      cat("########\n")
      
      curr_dir_p <- file.path(root_dir, par_dirs[p])
      
      for (rep in 1:nreps) {
          
        cat("rep =",rep,"\n")
        curr_dir_p_rep <- file.path(curr_dir_p, rep)
        load(file.path(curr_dir_p_rep, "sbm.RData"))
                
        for (meth in methNames) {
          
          if (meth == "truth") {
            
            load(file.path(curr_dir_p_rep, paste0(methNames[2], ".RData")))
            
            if (sbm$param_list$N > 0) {
              
              if (doMod) {
                mod_score <- my_mod(sbm$edge_list, 
                                    sbm$truth$communities,
                                    sbm$param_list$N + sbm$param_list$hv)
              } else {
                mod_score <- 0
              }
              
            } else {
              mod_score <- 0
            }
            
            mod_scores[[match(meth, methNames)]][rep, p] <- mod_score
              
          } else {
            
            meth_fn <- file.path(curr_dir_p_rep, paste0(meth, ".RData"))
            if (file.exists(meth_fn)) {
              load(meth_fn)
                
              
              
              if (sbm$param_list$N > 0) {
                mod_score <- my_mod(sbm$edge_list, 
                                    results$communities,
                                    sbm$param_list$N + sbm$param_list$hv)
                nmi_score <- read_mutual(file.path(curr_dir_p_rep,
                                                   paste0(meth, "_mutual.txt")))
              } else {
                mod_score <- 0
                nmi_score <- 0
              }
              
              mod_scores[[match(meth, methNames)]][rep, p] <- mod_score
              
              
              nmi_scores[[match(meth, methNames)]][rep, p] <- nmi_score
              
              # Type I error calc
              
              if (sbm$param_list$N > 0) {
                hv_in_comms <- sum(sbm$truth$background %in% 
                                   unlist(results$communities))
                n_bg <- length(sbm$truth$background)
                if (n_bg == 0) {
                  typeI_err <- 0
                } else {
                  typeI_err <- hv_in_comms / length(sbm$truth$background)
                }
              } else {
                hv_in_comms <- length(unique(unlist(results$communities)))
                typeI_err <- hv_in_comms / sbm$param_list$hv
              }
              
              typeI[[match(meth, methNames)]][rep, p] <- typeI_err
              
              # Type II error calc
              if (sbm$param_list$N > 0) {
                commNodes_in_bg <- sum(unlist(sbm$truth$communities) %in%
                                       results$background)
                typeII_err <- commNodes_in_bg / length(unlist(sbm$truth$communities))
              } else {
                typeII_err <- 0
              }
              
              typeII[[match(meth, methNames)]][rep, p] <- typeII_err
              
            } else {
              
              mod_scores[[match(meth, methNames)]][rep, p] <- 0
              nmi_scores[[match(meth, methNames)]][rep, p] <- 0
              typeI[[match(meth, methNames)]][rep, p] <- 0
              typeII[[match(meth, methNames)]][rep, p] <- 0
              
            }
            
          }
          
          if (file.exists(meth_fn))
            rm(results)
          
        }
          
      }
      
      rm(sbm)
        
    }
    
    convertToMat <- function (resultsList, type = "mean") {
      
      if (type == "mean") {
        stats <- unlist(lapply(resultsList, function(Mat) apply(Mat, 2, mean)))
      } else {
        stats <- unlist(lapply(resultsList, function(Mat) apply(Mat, 2, sd)))
      }
      
      stats <-  matrix(stats, nrow = length(methNames), byrow = T)
      rownames(stats) <- methNames
      return(stats)
      
    }
    
    mod_means <- convertToMat(mod_scores)
    nmi_means <- convertToMat(nmi_scores)
    typeI_means <- convertToMat(typeI)
    typeII_means <- convertToMat(typeII)
    
    mod_sds <- convertToMat(mod_scores, type = "sd")
    nmi_sds <- convertToMat(nmi_scores, type = "sd")
    typeI_sds <- convertToMat(typeI, type = "sd")
    typeII_sds <- convertToMat(typeII, type = "sd")
    
    save(mod_means, mod_sds,
         nmi_means, nmi_sds,
         typeI_means, typeI_sds,
         typeII_means, typeII_sds,
         file = file.path(root_dir, "plot_results.RData"))
    
  } else {
    
    load(file.path(root_dir, "plot_results.RData"))
    
  }
  
  # Color palette
  colPal <- colPal[1:length(methNames)]
  
  source("makePerformancePlot.R")
  
  # Some plot defaults
  cex.main <- 1
  cex.lab <- 3.5
  cex.axis <- 3.5
  cex <- 3.5
  legCex <- 3
  lwd <- 2.5
  
  if (exists("axis_par_string")) {
    xlab_string <- axis_par_string
  } else {
    xlab_string <- pars[axis_par]
  }
  
  paramVec <- par_settings[axis_par, ]
  
  if (paste0(expString, "_mod.png") %in% 
      list.files(file.path("sims-results", expString))) {
    file.remove(paste0(expString, "_mod.png"))
  }

  if (main_text_plot) {
    main_str <- main_text
  } else {
    main_str <- ""
  }
  
  # mod
  
  mod_means <- mod_means[plot_meths, ]
  mod_sds <- mod_sds[plot_meths, ]
  rownames(mod_means) <- plot_names
  rownames(mod_sds) <- plot_names
  
  suppressWarnings(
    
    dummy <- makePerformancePlot(fn = file.path("sims-results",
                                                paste0(expString, "_mod.png")),
                                 meanMat = mod_means,
                                 sdMat = mod_sds,
                                 xRange = c(paramVec[1], paramVec[length(paramVec)]),
                                 main = main_str,
                                 xlab = xlab_string,
                                 ylab = "Overlapping Modularity",
                                 legPos = "topright",
                                 legCex = legCex,
                                 lwd = lwd,
                                 cex = cex,
                                 cex.main = cex.main,
                                 cex.lab = cex.lab,
                                 cex.axis = cex.axis)
  )
    
  # nmi
  
  nmi_means <- nmi_means[plot_meths, ]
  nmi_sds <- nmi_sds[plot_meths, ]
  rownames(nmi_means) <- plot_names
  rownames(nmi_sds) <- plot_names
  
  suppressWarnings(
    
    dummy <- makePerformancePlot(fn = file.path("sims-results",
                                                paste0(expString, "_nmi.png")),
                                 meanMat = nmi_means,
                                 sdMat = nmi_sds,
                                 xRange = c(paramVec[1], paramVec[length(paramVec)]),
                                 main = main_str,
                                 xlab = xlab_string,
                                 ylab = "Overlapping NMI",
                                 legPos = "bottomright",
                                 legCex = legCex,
                                 lwd = lwd,
                                 cex = cex,
                                 cex.main = cex.main,
                                 cex.lab = cex.lab,
                                 cex.axis = cex.axis)
    
  )
  
  # typeI
  
  typeI_means <- typeI_means[plot_meths, ]
  typeI_sds <- typeI_sds[plot_meths, ]
  rownames(typeI_means) <- plot_names
  rownames(typeI_sds) <- plot_names
  
  suppressWarnings(
    
    dummy <- makePerformancePlot(fn = file.path("sims-results",
                                                paste0(expString, "_typeI.png")),
                                 meanMat = typeI_means,
                                 sdMat = typeI_sds,
                                 xRange = c(paramVec[1], paramVec[length(paramVec)]),
                                 main = main_str,
                                 xlab = xlab_string,
                                 ylab = "% B.I.C.",
                                 legPos = "topright",
                                 legCex = legCex,
                                 lwd = lwd,
                                 cex = cex,
                                 cex.main = cex.main,
                                 cex.lab = cex.lab,
                                 cex.axis = cex.axis)
    
  )
  
  # typeII
  
  typeII_means <- typeII_means[plot_meths, ]
  typeII_sds <- typeII_sds[plot_meths, ]
  rownames(typeII_means) <- plot_names
  rownames(typeII_sds) <- plot_names
  
  suppressWarnings(
    
    dummy <- makePerformancePlot(fn = file.path("sims-results",
                                                paste0(expString, "_typeII.png")),
                                 meanMat = typeII_means,
                                 sdMat = typeII_sds,
                                 xRange = c(paramVec[1], paramVec[length(paramVec)]),
                                 main = main_str,
                                 xlab = xlab_string,
                                 ylab = "% C.I.B.",
                                 legPos = "topright",
                                 legCex = legCex,
                                 lwd = lwd,
                                 cex = cex,
                                 cex.main = cex.main,
                                 cex.lab = cex.lab,
                                 cex.axis = cex.axis)
    
  )
  
}