    library(igraph)
    library(Matrix)
    
    doweights <- TRUE
    
    # Making toy graph
    set.seed(12345)
    g <- erdos.renyi.game(1000, p=0.5)
    if (doweights) {
      E(g)$weight <- rexp(nrow(get.edgelist(g)))
    }
    
    # Using louvain to get a clustering
    cluster_result <- cluster_louvain(g)
    membership <- cluster_result$membership
    mod <- cluster_result$modularity
    
    # Getting the adjacency matrix
    if (doweights) {
      attr_set <- "weight"
    } else {
      attr_set <- NULL
    }
    adjMat <- get.adjacency(g, attr=attr_set)
    
    # Getting modularity matrix
    if (doweights) {
      wt_set <- E(g)$weights
    } else {
      wt_set <- NULL
    }
    modMat <- modularity_matrix(g, weights=wt_set, membership=membership)
    
    # Calculating in/between-community edge counts and mod scores
    K <- max(membership)
    sumEdge <- modMatC <- matrix(0, K, K)
    for (i in 1:K) {
      commi <- which(membership == i)
      for (j in 1:K) {
        commj <- which(membership == j)
        sumEdge[i, j] <- sum(adjMat[commi, commj])
        modMatC[i, j] <- sum(modMat[commi, commj]) 
      }
    }
    
    # Computing community-wise null model
    degs <- strength(g)
    twom <- sum(degs)
    dC <- tapply(degs, membership, sum)
    
    # Subtracting null model matrix from edge count matrix
    my_modMatC <- sumEdge - tcrossprod(dC) / twom
    
    # Checking equality
    cat("Equality check is", mean((my_modMatC - modMatC)^2) < 1e-10, "\n")
    
    # Checking computed mod against modMat and my_modMat
    mod <- ifelse(doweights, 
                  cluster_result$modularity[2],
                  cluster_result$modularity)
    mod_from_mat <- sum(diag(modMatC)) / twom
    my_mod_from_mat <- sum(diag(my_modMatC)) / twom
    cat("Modularity from clustering func is", round(mod, 6), "\n")
    cat("Modularity from mod_from_mat is", round(mod_from_mat, 6), "\n")
    cat("Modularity from my_mod_from_mat is", round(my_mod_from_mat, 6), "\n")

