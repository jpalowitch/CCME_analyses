saveDir <- "applications-results/enron/data"
if (!dir.exists(saveDir))
  dir.create(saveDir, recursive = TRUE)
load("mineAndSave_results.RData")

# Get edgelist0?
getEdgeList0 <- TRUE



if(getEdgeList0){
  
  # Makes an edgelist with:
  # column 1 - sender node index
  # column 2 - recipient node index
  # column 3 - a blank column to store number of messages, later
  # column 4 - an index to remember the filename
  
  # Find nodes
  fullData <- unlist(from_tosList)
  names(fullData) <- NULL
  gc()
  nodes0 <- unique(fullData)
  nodes0 <- sort(nodes0)
  
  
  
  # Make an integer matrix to record the edges
  edgeList0 <- matrix(0,toCount,4)
  
  # Make empty character vector to record filenames
  fns0 <- character(0)
  
  # Record the edges
  recordPos <- 1
  for(i in 1:length(folderList)){
    
    for(j in 1:length(from_tosList[[i]])){
      
      cat("folder",i,"entry",j,"position",recordPos,"\n")
      
      toCount_j <- length(from_tosList[[i]][[j]]$Tos)
      
      if(toCount_j>0){
        
        # Add to fns0
        fns0 <- c(fns0,names(from_tosList[[i]][j]))
        
        edgeList0[recordPos:(recordPos + toCount_j - 1),1] = 
          match(from_tosList[[i]][[j]]$Froms,nodes0)
        edgeList0[recordPos:(recordPos + toCount_j - 1),2] = 
          match(from_tosList[[i]][[j]]$Tos,nodes0)
        edgeList0[recordPos:(recordPos + toCount_j - 1),4] = 
          length(fns0)
        
        recordPos <- recordPos + toCount_j
        
      }
      
    }
    
  }
  
  save(fns0,edgeList0,nodes0,fullData,file = "edgeList0.RData")
  
}else{
  load("edgeList0.RData")
}

nodeList = rep(list(NULL),length(nodes0))
edgeListSave = edgeList0
fnsSave = fns0

senders <- sort(unique(edgeList0[,1]))
nSenders <- length(senders)
directedList <- rep(list(NULL),nSenders)

# Below loop makes the following list object:
# - one entry for every sender in column 1 of edgeList 0
# - each entry is a list with the following named components:
#   - recips: a vector giving the node indicies for the sender's recipients
#   - counts: a vector giving the number of messages to each node in recips
#   - fns: a list object with one entry per recipient, each entry being a vector giving the filename indices

for(i in 1:nSenders){
  
  if(i %% 10 == 0)
    cat(i,"\n")
  
  sender <- senders[i]
  msgIndx <- which(edgeList0[,1] == sender)
  recipsAndFns = edgeList0[msgIndx,c(2,4)]
  
  if(class(recipsAndFns) != "matrix")
    recipsAndFns = matrix(recipsAndFns,1,2)
  
  recipients = sort(unique(recipsAndFns[,1]))
  nRecips <- length(recipients)
  
  msgCounts <- integer(nRecips)
  fnList <- rep(list(character(0)),nRecips)
  
  for(j in 1:nRecips){
    
    recipl <- recipsAndFns[,1] == recipients[j]
    msgCounts[j] <- sum(recipl)
    fnList[[j]] <- recipsAndFns[recipl,2]
    
  }
  
  directedList[[i]] <- list("recips" = recipients,
                            "counts" = msgCounts,
                            "fns" = fnList)
  
  rm(recipients,msgCounts,fnList,recipsAndFns,msgIndx,nRecips,sender,recipl)
  
}

save(directedList,
     nodes0,
     senders,
     file = file.path(saveDir, "directedList.RData"))
