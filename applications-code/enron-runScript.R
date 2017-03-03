dataDir <- "applications-results/enron/data"
saveDir <- "applications-results/enron/results"
load(file.path(dataDir, "edgeList.RData"))
origwd <- setwd("../CCME")
source("CCME.R")
setwd(origwd)

# Testing whether all entries are unique
ordered_edgeList <- edgeList
unord_rows <- edgeList[ , 1] > edgeList[ , 2]
ordered_edgeList[unord_rows, ] <- edgeList[unord_rows, c(2:1, 3)]
unique_tags <- ordered_edgeList[ , 1] + ordered_edgeList[ , 2] / max(ordered_edgeList[ , 2])
if (sum(duplicated(unique_tags)) == 0) {
  edge_list <- ordered_edgeList
} else {
  cat('you have a problem\n')
}

edge_list <- as.data.frame(edge_list)
names(edge_list) <- c("node1", "node2", "weight")
edge_list <- edge_list[order(edge_list$node1), ]
set.seed(12345)
results <- CCME(edge_list = edge_list, updateOutput = TRUE)
save(results, file = file.path(saveDir, "results_ccme.RData"))

