library(igraph)
library(Matrix)

# Reading in the data
datadir <- 'sims-results/experiment6/90/1/'
fn <- file.path(datadir, 'network.csv')
edgelist <- read.table(fn, sep=',')
g <- graph.edgelist(as.matrix(edgelist[, 1:2]), directed=FALSE)
E(g)$weight <- edgelist[, 3]

# Louvain gets the correct membership perfectly
set.seed(12345)
cluster_result <- cluster_louvain(g)
mod_clustering <- cluster_result$membership

# Loading in sbm_clustering and comparing
sbm_clustering <- readLines("sbm_clustering.dat") %>% as.numeric
cat("NMI between mod and sbm:\n")
show(compare(sbm_clustering, mod_clustering, method = "nmi"))

# Averaging community-wise edge counts/edge weights/mod matrix
K <- max(mod_clustering)
twom <- sum(strength(g))
adjMat <- get.adjacency(g, attr="weight")
avgEdgeCount <- avgEdge <- sumEdge <- sumMod <- matrix(0, K, K)
for (i in 1:K) {
  commi <- which(mod_clustering == i)
  for (j in 1:K) {
    commj <- which(mod_clustering == j)
    avgEdgeCount[i, j] <- mean(adjMat[commi, commj] > 0)
    sumEdge[i, j] <- sum(adjMat[commi, commj])
    avgEdge[i, j] <- mean(adjMat[commi, commj])
  }
}

degs <- strength(g)
twom <- sum(degs)
dC <- tapply(degs, mod_clustering, sum)
modMat <- sumEdge - tcrossprod(dC) / twom

cat("Modularity matrix:\n")
show(modMat)
cat("Edge density matrix:\n")
show(avgEdgeCount)
cat("Mean weight matrix:\n")
show(avgEdge)
