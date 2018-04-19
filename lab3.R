library(igraph)
library(lsa)
g = make_graph("Zachary")
coords = layout_with_fr(g)
# plot the graph
plot(g, layout=coords, vertex.label=NA,vertex.size=10)

#find dense subgraph based on modularity
c1 = cluster_fast_greedy(g)
# modularity measure
modularity(c1)

plot(g, layout=coords, vertex.label=NA, vertex.color=membership(c1),vertex.size=10)

sizes(c1)
membership(c1)

# greedy method (hiearchical, fast method)
c2 = cluster_leading_eigen(g)
# modularity measure
modularity(c2) 

plot(g, layout=coords, vertex.label=NA, vertex.color=membership(c2),vertex.size=10)

oc = cluster_optimal(g)
plot(g, layout=coords, vertex.label=NA, vertex.color=membership(oc),vertex.size=10)
