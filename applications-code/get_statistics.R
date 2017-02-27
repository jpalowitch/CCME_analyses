library(gdata)

get_statistics <- function (results) {
  
  jaccard <- function (X, Y) {length(intersect(X, Y)) / length(union(X, Y))}
  
  comms <- results$communities
  K <- length(comms)
  
  # Calculate pair-wise overlap
  cat("computing overlap...\n")
  overlap_mat <- matrix(0, K, K)
  
  for (i in 1:K) {
    
    for (j in 1:K) {
      
      overlap_mat[i, j] <- jaccard(comms[[i]], comms[[j]])
      
    }
    
  }
  
  # Creating memberships and comm_nodes
  cat("getting memberships...\n")
  full_node_vec <- unlist(comms)
  full_comm_vec <- unlist(lapply(1:K, function (i) rep(i, length(comms[[i]]))))
  mships <- split(full_comm_vec, full_node_vec)
  comm_nodes <- split(full_node_vec, full_node_vec)
  comm_nodes <- unlist(lapply(comm_nodes, function (L) L[1]))
  mships <- mships[order(comm_nodes)]
  comm_nodes <- sort(comm_nodes)
  comm_nodes <- unname(comm_nodes)
  
  
  # Assessing overlap
  overlap_nodes <- which(unlist(lapply(mships, length)) > 1)
  on <- length(overlap_nodes)
  om <- unlist(lapply(mships[overlap_nodes], length))
  
  return(list("K" = K,
              "node_count" = length(comm_nodes),
              "comm_sizes" = unlist(lapply(comms, length)),
              "comm_nodes" = comm_nodes,
              "mships" = mships,
              "overlap_nodes" = overlap_nodes,
              "jaccards" = upperTriangle(overlap_mat),
              "on" = on,
              "om" = om))
  
}