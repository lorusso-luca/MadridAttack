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
g=delete_vertices(g,which(degree(g)==0))
names=V(g)$names

coords = layout_with_fr(g)
oc = cluster_optimal(g)

c2 = cluster_leading_eigen(g) 

# visualizing clusters
plot(g, layout=coords, vertex.label=NA, mark.groups=as.list(oc),vertex.size=10)

plot(g, layout=coords, vertex.label=NA, mark.groups=as.list(c2),vertex.size=10)

plot(g)
