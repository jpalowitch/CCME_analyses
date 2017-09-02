library(Rcpp)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
sourceCpp("methodFiles/CCME/new_funs.cpp")
source("methodFiles/CCME/CCME.R")
source("sims-code/sbm_funs3.R")

# Make a weighted SBM network
message("making weighted SBM\n")
set.seed(12345)
par_list <- make_param_list2(N = 1000, k = 200, max_k = 250)
sbm <- make_sbm(par_list)

# Running CCME
message("running CCME\n")
set.seed(123456)
res <- CCME(sbm$edge_list)

# Creating igraph object for plotting
message("preparing graph object\n")
G <- graph.edgelist(as.matrix(sbm$edge_list[ , 1:2]), directed = FALSE)
E(G)$weight <- sbm$edge_list$weight
nodeorder <- unlist(res$communities)
adj <- as.matrix(get.adjacency(G, attr = "weight"))

# Plotting
message("plotting png (be patient)...\n")
adj1_melt <- melt(adj[nrow(adj):1, ])
adj2_melt <- melt(adj[nodeorder, nodeorder][length(nodeorder):1, ])
colnames(adj1_melt) <- colnames(adj2_melt) <- c("node1", "node2", "weight")
p1 <- ggplot(adj1_melt, aes(x = node1, y = node2, fill = weight, colour = weight)) + 
  geom_tile() + 
  scale_fill_gradient(low = "#000000", high = "#FF0000") + 
  scale_colour_gradient(low = "#000000", high = "#FF0000") + 
  ggtitle("Unordered Adjacency") + theme_minimal()
p2 <- ggplot(adj2_melt, aes(x = node1, y = node2, fill = weight, colour = weight)) + 
  geom_tile() + 
  scale_fill_gradient(low = "#000000", high = "#FF0000") + 
  scale_colour_gradient(low = "#000000", high = "#FF0000") + 
  ggtitle("CCME-ordered Adjacency") + theme_minimal()
ggsave("demo_plot.png",
       grid.arrange(p1, p2, ncol = 2),
       width = 12, height = 5)
message("done\n")
