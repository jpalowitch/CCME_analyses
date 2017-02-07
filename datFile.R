## Function to convert adjacency into dat file



library(igraph)

makeDat = function(adjMat,fn,uw = FALSE){

rownames(adjMat) = NULL
colnames(adjMat) = NULL

if(!uw){

	G = graph.adjacency(adjMat,mode = "undirected",weighted=TRUE)
	edges = get.edgelist(G)
	edges = cbind(edges,E(G)$weight)

}

if(uw){

	G = graph.adjacency(adjMat,mode = "undirected")
	edges = get.edgelist(G)

}
write.table(edges, file=fn, row.names=FALSE, col.names=FALSE)
}