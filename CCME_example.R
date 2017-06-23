# Before running this script you must run 'get_method_codes.txt' from terminal

library(igraph)
library(Matrix)
source("sims-code/sbm_funs3.R")
CCMEpath <- "methodFiles/ccme"
oldwd <- setwd(CCMEpath)
source("CCME.R")
setwd(oldwd)

# Setting graph parameters
toy_par_list <- make_param_list2(N = 500)

# Making toy graph
set.seed(12345)
toy_wnet <- make_sbm(toy_par_list)

# Visualizing communities
G <- graph.edgelist(as.matrix(toy_wnet$edge_list[ , 1:2]), directed = FALSE)
E(G)$weight <- toy_wnet$edge_list$weight
Gadj <- as.matrix(get.adjacency(G, attr = "weight"))
comms <- toy_wnet$truth$communities; comm_order <- unlist(comms)
image(Gadj[comm_order, comm_order])

# Running CCME. Note that the edge list must not repeat edges.
# So if you have --1--3--2.5-- in a line then you should not have
# --3--1--2.5-- elsewhere.
set.seed(12345)
results <- CCME(toy_wnet$edge_list, updateOutput = TRUE)

# The default network from 'make_sbm' is actually pretty tough.
# The communities contain almost disconnected components, as shown below.
# This code labels the community nodes found by CCME by their true community:
lapply(results$communities, function (L) unlist(toy_wnet$truth$memberships)[L])
