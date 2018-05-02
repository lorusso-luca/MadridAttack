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
modularity(oc)
sizes(oc)
membership(oc)

sizes(oc)
membership(oc)
#CAPO CLUSTER
for (i in 1:5){
  print(paste("COMMUNITY ",i))
  temp <- induced_subgraph(g, which(membership(oc) %in% i), impl = c("auto", "copy_and_delete",
                                                                     "create_from_scratch"))
  plot(induced_subgraph(g, which(membership(oc) %in% i), impl = c("auto", "copy_and_delete",
                                                                  "create_from_scratch")), vertex.size = betweenness(temp))
  
  print(betweenness(temp))
}






table(get.edge.attribute(g))

c2 = cluster_leading_eigen(g) 
modularity(c2)
# visualizing clusters
plot(g, layout=coords, vertex.label=NA, vertex.color=membership(oc),vertex.size=15)

##individuare il terrorista con più connessioni
plot(degree(g))
vertex.attributes(g,'1')
vertex.attributes(g,'3')
vertex.attributes(g,'7')


## i tre terroristi con più connessione del grafo
plot(betweenness(g))
vertex.attributes(g,'57')
vertex.attributes(g,'3')
vertex.attributes(g,'37')


##studio sottografo con tutte le connessione dal nodo 1 (nodo con più connessioni)

sub1 = make_ego_graph(g, nodes = "1", order=1)[[1]]
lo = layout_as_star(sub1)
plot(sub1,layout=lo, edge.width = E(g)$weight)


#pesodi tutti gli archi del g
table(edge_attr(g))
ends(g,7)

##- individuare terrorista più pericoloso = con più connessioni tra i vari cluster [meno pericoloso]
##- individuare il “capo di ogni cluster” = più connesso all interno del cluster (indipendentemente dal peso) [meno connessoo]
##- il gruppo più coeso [meno coeso]
##- somma totale peso archi entranti e uscenti maggiore [e minore]




edge_betweenness(g)



print(b)
s = sapply(blocks(b), FUN =length)
plot_hierarchy(b, vertex.label=cohesion(b), vertex.size=(s / max(s)) * 20 + 10) 

vertex.attributes(g,'1')


