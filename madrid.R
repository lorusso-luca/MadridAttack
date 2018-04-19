library(igraph)
library(lsa)

# read edges
edges = matrix(scan("madrid-edges.dat", 0), ncol=3, byrow=TRUE)
# read names
names = scan("madrid-names.dat", "a")
# build graph
g = graph_from_edgelist(edges[,1:2], directed=FALSE)
# add weights
E(g)$weight = edges[,3]
# remove multi-edges
g = simplify(g)
E(g)$weight = E(g)$weight / 2
V(g)$names = names
# delete isolated nodes
g=delete_vertices(g,which(degree(g)==0)) names=V(g)$names
