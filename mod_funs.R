library(Matrix)
library(igraph)

make_membership <- function (truth, reassign = 1:length(truth)) {
  N <- length(unique(unlist(truth)))
  mship <- rep(0, N)
  for (i in 1:length(truth)) {
    mship[truth[[i]]] <- reassign[i]
  }
  return(mship)
}

calc_mod <- function (test_net, reassign = 1:length(test_net$truth)) {
  G <- graph.adjacency(test_net$net)
  mship <- make_membership(test_net$truth, reassign)
  return(modularity(G, mship))
}

make_GO <- function(my_GO) {
  G <- graph.adjacency(my_GO$net, mode = "undirected")
  return(G)
}


my_mod0 <- function (G, mship, weights = NULL, return_mod_mat = FALSE) {

  edge_list <- get.edgelist(G)
  n <- max(as.vector(edge_list))

  if (!is.null(weights)) {
    wG <- G
    E(wG)$weight <- weights
    strengths <- strength(wG)
    m <- sum(strengths) / 2
    mean_mat <- tcrossprod(strengths, strengths) / (2 * m)
    adj_mat <- sparseMatrix(edge_list[ , 1], 
                            edge_list[ , 2],
                            x = weights,
                            dims = c(n, n),
                            symmetric = TRUE)
  } else {
    degrees <- degree(G)
    m <- sum(degrees) / 2
    mean_mat <- tcrossprod(degrees, degrees) / (2 * m)
    adj_mat <- sparseMatrix(edge_list[ , 1], 
                            edge_list[ , 2],
                            dims = c(n, n),
                            symmetric = TRUE)
  }

  mod_mat <- as.matrix(adj_mat - mean_mat)

  if (return_mod_mat) {
    return(mod_mat)
  } else {
      mod <- 0
    for (k in unique(mship))
      mod <- sum(mod_mat[mship == k, mship == k]) + mod
    mod <- mod / (2 * m)
    return(mod)
  }
}

my_mod <- function (edge_list, comm_set, N) {
  
  n_c <- length(comm_set)
  if (n_c == 0) {
    return (-1)
  }
  
  # Removing lower diagonal
  edge_list <- edge_list[edge_list[ , 1] < edge_list[ , 2], ]
  
  adjMat <- sparseMatrix(i = edge_list$node1,
                         j = edge_list$node2,
                         x = edge_list$weight,
                         dims = c(N, N),
                         symmetric = TRUE)
  strengths <- colSums(adjMat)
  s_T <- sum(strengths)
  
  # Finding membership mat and om
  mem_mat <- matrix(0, N, length(comm_set))
  for (c in 1:n_c) 
    mem_mat[comm_set[[c]], c] <- 1
  om <- rowSums(mem_mat)
  
  mod <- 0
  for (c in 1:n_c) {
    nodes1 <- rep(comm_set[[c]], length(comm_set[[c]]))
    nodes2 <- rep(comm_set[[c]], each = length(comm_set[[c]]))
    weights_indx <- edge_list[ , 1] %in% nodes1 & edge_list[ , 2] %in% nodes2
    weights_c <- edge_list[weights_indx , 3]
    nodes1_vec <- edge_list[weights_indx, 1]
    nodes2_vec <- edge_list[weights_indx, 2]
    mean_list <- strengths[nodes1_vec] * strengths[nodes2_vec] / s_T
    scaled_w <- (weights_c - mean_list) / (om[nodes1_vec] * om[nodes2_vec])
    mod <- mod + sum(scaled_w)
  }
  
  mod <- 2 * mod / s_T
  
  return(mod)
  
}