source("datFile.R")
library(igraph)
library(Matrix)

folder2dat <- function (rootDir,
                        batchTag,
                        weighted = TRUE,
                        fast = TRUE,
                        eraseDats = TRUE,
                        listFormat = 1) {
  
  fullNames <- list.files(rootDir, recursive = TRUE)
  
  fullNames <- fullNames[grepl("sbm.RData", fullNames)]
  datNames <- gsub(".RData", ".dat", fullNames)
  datNames <- gsub("/", "_", datNames)
  
  # Erase all .dats with the same names
  if(eraseDats){

    removed <- NULL
    
    cat("Attempting to remove redundant .dat files\n")
    
    removed <- suppressWarnings(
      
      sapply(datNames,
             function (datName) file.remove(file.path(rootDir, "OSLOM2", datName)))
      
    )

    cat(sum(removed),"removed\n\n")
    rm(removed)
    
  }
  
  for (i in 1:length(fullNames)) {
    
    fn <- fullNames[i]
    dat_fn <- datNames[i]
    load(file.path(rootDir, fn))
    
    cat("writing", dat_fn, "\n")
    
    N_use <- sbm$param_list$N + sbm$param_list$hv
    
    adjMat <- sparseMatrix(i = sbm$edge_list$node1,
                           j = sbm$edge_list$node2,
                           x = sbm$edge_list$weight,
                           dims = c(N_use, N_use),
                           symmetric = TRUE)
    
    G <- graph.adjacency(adjMat, mode = "undirected", weighted = TRUE)
    G = graph.adjacency(adjMat,mode = "undirected",weighted=TRUE)
    edges <- get.edgelist(G)
    edges <- cbind(edges,E (G)$weight)
    write.table(edges, file = file.path(rootDir, "OSLOM2", dat_fn), 
                row.names = FALSE, col.names = FALSE)
    
  }
      
  scriptLines <- NULL
  
  if (weighted) {
    weightTag <- "-w"
  } else {
    weightTag <- "-uw"
  }
  
  if (fast) {
    endTag <- "-singlet -fast"
  } else {
    endTag <- "singlet"
  }
  
  for(i in 1:length(fullNames)){
      
    datName <- datNames[i]

    scriptLines <- c(scriptLines,
                     paste("./oslom_undir",
                          weightTag,
                          "-f",
                          datName, weightTag, endTag)
                     )
    
  }
  
  scriptLines <- c(scriptLines,
                   paste0("./oslom_undir ",
                          "-uw",
                          " -f ",
                          "example.dat",
                          " -singlet")
                   )
  
  scriptConn <- file(file.path(rootDir, "OSLOM2", paste0(batchTag, ".txt")))
  writeLines(scriptLines, scriptConn)
  close(scriptConn)
  save(fullNames,
       file = file.path(rootDir, "OSLOM2", paste0(batchTag, ".RData"))
       )
  
  return(fullNames)
    
}



