saveDir <- "applications-results/enron/data"

load(file.path(saveDir, "edgeList0.RData"))
rm(edgeList0,fullData,nodes0)
load(file.path(saveDir, "directedList.RData"))

# directedList is the following list object:
# - one entry for every sender in column 1 of edgeList0 (load edgeList0.RData again to see this object)
# - each entry is a list with the following named components:
#   - recips: a vector giving the node indicies for the sender's recipients
#   - counts: a vector giving the number of messages to each node in recips
#   - fns: a list object with one entry per recipient, each entry being a vector giving the filename indices

# Check a random file
# If you have the original email folder at hand, you can go into the folder
# below and verify that it does indeed contain an email from the sender to
# the recipient
toCheck <- sample(length(directedList),1)
directedList[[toCheck]]$recips[1]
directedList[[toCheck]]$counts[1]
directedList[[toCheck]]$fns[[1]]
fns0[directedList[[toCheck]]$fns[[1]][1]]
nodes0[directedList[[toCheck]]$recips[1]]
nodes0[senders[toCheck]]

nSenders <- length(senders)

# Counting total length of edgeList

totalLength <- 0
for (i in 1:nSenders) 
  totalLength <- totalLength + length(directedList[[i]]$recips)

# Making edgelist
# Result will be a matrix with columns:
# 1 - node index of sender
# 2 - node index of recipient
# 3 - message count

edgeList <- matrix(integer(0),totalLength,3)
recordPos <- 1

for(i in 1:nSenders){
  
  nRecips <- length(directedList[[i]]$recips)
  
  edgeList[recordPos:(recordPos + nRecips - 1),1] = senders[i]
  edgeList[recordPos:(recordPos + nRecips - 1),2] = directedList[[i]]$recips
  edgeList[recordPos:(recordPos + nRecips - 1),3] = directedList[[i]]$counts
  
  recordPos <- recordPos + nRecips
  
}

senderList_full <- edgeList
rm(edgeList)
gc()


# Removing bad addresses
# This task has messy indexing. Need to:
# 1 - remove all entries with bad address (i.e. no '@')
# 2 - shift node indices accordingly
# 3 - alter edgeList entries accordingly
badAdds <- which(!grepl("@",nodes0))
badRows1 <- which(senderList_full[,1] %in% badAdds)
badRows2 <- which(senderList_full[,2] %in% badAdds)
badRows <- union(badRows1, badRows2)
senderList_full <- senderList_full[-badRows,]
remainingNodeIndx <- sort(unique(as.vector(senderList_full[,1:2])))

# Finding remaining senders and filtering directedList
remainingSenders <- which(senders %in% remainingNodeIndx)
directedList <- directedList[remainingSenders]

# Matching nodes to remaining nodes
nodes0 <- nodes0[remainingNodeIndx]

# Matching senderList to remaining nodes
senderList_full[,1] <- match(senderList_full[,1],remainingNodeIndx)
senderList_full[,2] <- match(senderList_full[,2],remainingNodeIndx)

# Matching recipients to remaining nodes
directedList <- lapply(directedList,function(entry){entry$recips <- match(entry$recips,remainingNodeIndx);
                                                    return(entry);})
# Collecting new senders
senders <- senders[remainingSenders]
senders <- match(senders,remainingNodeIndx)

length(directedList)
length(senders)

length(nodes0)
length(unique(as.vector(senderList_full[,1:2])))
max(unique(as.vector(senderList_full[,1:2])))


# Check a random file
# If you have the original email folder at hand, you can go into the folder
# below and verify that it does indeed contain an email from the sender to
# the recipient
toCheck <- sample(length(directedList),1)
directedList[[toCheck]]$recips[1]
directedList[[toCheck]]$counts[1]
directedList[[toCheck]]$fns[[1]]
fns0[directedList[[toCheck]]$fns[[1]][1]]
nodes0[directedList[[toCheck]]$recips[1]]
nodes0[senders[toCheck]]



senderList_fullSave <- senderList_full

# The purpose of the following loop is to remove duplicate messages
# Some senders are also recipients, and hence those messages will be
# double-counted

violatingRows <- rep(FALSE,nrow(senderList_full))

for(i in 1:nSenders){
  
  if(i %% 100 == 0)
    cat(i,"\n")
  
  sender <- senders[i]
  
  sender_col1 <- which(senderList_full[,1] == sender)
  sender_col2 <- which(senderList_full[,2] == sender)
  
  if(length(sender_col2)>0){
    
    if(length(sender_col1)>1){
      col1mat <- senderList_full[sender_col1,]
    }else{
      col1mat <- matrix(senderList_full[sender_col1,],1,3)
    }
    
    if(length(sender_col2)>1){
      col2mat <- senderList_full[sender_col2,]
    }else{
      col2mat <- matrix(senderList_full[sender_col2,],1,3)
    }
  
    col1sums <- apply(col1mat,1,function(r)sum(r[1:2]))
    col2sums <- apply(col2mat,1,function(r)sum(r[1:2]))
    
    # Removing self-loop row (if it exists)
    col1sums <- setdiff(col1sums,2*sender)
    col2sums <- setdiff(col2sums,2*sender)
    
    if(sum(col2sums %in% col1sums) > 0){ # Then we need to remove some rows
    
      for(j in which(col2sums %in% col1sums)){
          
        rowMatch <- sender_col1[which(col1sums == col2sums[j])]
        rowj <- sender_col2[j]
        senderList_full[rowMatch,3] <- senderList_full[rowMatch,3] + senderList_full[rowj,3]
        violatingRows[rowj] <- TRUE
    
      }
      
    }
    
  }
  
}


edgeList <- senderList_full[!violatingRows,]
nodes <- nodes0
fns <- fns0
save(edgeList,nodes,fns,senderList_full,directedList,
     file = file.path(saveDir, "edgeList.RData"))
      
      
  
  