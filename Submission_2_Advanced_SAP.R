# Ali Tafti
# Optional Advanced Lab 2: SAP Community Network
#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
#dir_path <-"~/YourWorkingDirectoryFilePath"
#setwd(dir_path)
setwd("C:/E!!/Software/RStudio/SocialMediaAndNetworks")
# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"C:/E!!/UIC/IDS564/AdvancedLab2/SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub)
## Vertices
vcount(g_SAPSub)


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

# Initializing layout and plotting
minC <- rep(-Inf, vcount(g_SAPSub_simpl))
maxC <- rep(Inf, vcount(g_SAPSub_simpl))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(g_SAPSub_simpl, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
co[1,]
plot(g_SAPSub_simpl, layout=co, vertex.size=15, edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA)
plot(g_SAPSub_simpl, layout=layout.kamada.kawai(g_SAPSub_simpl), vertex.size=15, edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA)
plot(g_SAPSub_simpl.GC, layout=layout.drl(g_SAPSub_simpl.GC), vertex.size=ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>10,10,3), edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>10,"red","dodgerblue"))

par(mfrow=c(1, 1))
sel_deg_graph <- induced.subgraph(g_SAPSub_simpl.GC, which(degree(g_SAPSub_simpl.GC, mode = 'out')>20))
plot(sel_deg_graph,  layout=layout.drl(g_SAPSub_simpl.GC), vertex.size=ifelse(degree(g_SAPSub_simpl.GC, mode = 'out')>10,10,3), edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(g_SAPSub_simpl.GC, mode = 'out')>10,"red","dodgerblue"), edge.width = inv_weight)

par(mfrow=c(1, 1))
sel_deg_graph <- induced.subgraph(g_SAPSub_simpl.GC, which(sel_deg_graph <- induced.subgraph(g_SAPSub_simpl.GC, which(neighbors(g_SAPSub_simpl, v=c('592540'))))))
plot(sel_deg_graph,  layout=layout.drl(sel_deg_graph), vertex.size=ifelse(degree(sel_deg_graph, mode = 'out')>2,10,3), edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(sel_deg_graph, mode = 'out')>2,"red","dodgerblue"), edge.width = inv_weight)

plot(g_SAPSub_simpl.GC, layout=layout.drl(g_SAPSub_simpl.GC), vertex.size=ifelse(), edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>10,"red","dodgerblue"))

par(mfrow=c(1, 2))
plot(g_SAPSub_simpl.GC, layout=layout.drl(g_SAPSub_simpl.GC), main="Hubs", vertex.color = "dodgerblue" , vertex.label="", vertex.size=20 * sqrt(hub.score(g_SAPSub_simpl.GC)$vector), rescale=TRUE, edge.arrow.size=0.05)
plot(g_SAPSub_simpl.GC, layout=layout.drl(g_SAPSub_simpl.GC), main="Authorities", vertex.color = "dodgerblue", vertex.label="", vertex.size=20 *sqrt(authority.score(g_SAPSub_simpl.GC)$vector), rescale=TRUE, edge.arrow.size=0.05)

# Fetching first 3 elemnts sorted by betweenness:
# eb <- edge.betweenness(karate)
# E(karate)[order(eb, decreasing=T)[1:3]]

cores123 <- graph.coreness(g_SAPSub_simpl.GC)
sna::gplot.target(g_SAPSub_simpl.GC, cores123, circ.lab = FALSE, circ.col="skyblue", usearrows = FALSE, vertex.col=cores123, edge.col="darkgray")
detach("package:network")
detach("package:sna")

aidsblog <- simplify(g_SAPSub_simpl.GC)
dyad.census(aidsblog)
triad.census(aidsblog)

transitivity(g_SAPSub_simpl.GC)
transitivity(g_SAPSub_simpl.GC, "local", vids=c("592540","1195854"))

#ego.instr <- induced.subgraph(karate, neighborhood(karate, 1, 1)[[1]])
# ego.admin <- induced.subgraph(karate, neighborhood(karate, 1, 34)[[1]])
# graph.density(karate)
# [1] 0.1390374
# graph.density(ego.instr)
# [1] 0.25
#graph.density(ego.admin)
# [1] 0.2091503
ego.SAP1 <- induced.subgraph(g_SAPSub_simpl.GC, neighbors(g_SAPSub_simpl.GC, v = c('592540')))
plot(ego.SAP1, layout=layout.fruchterman.reingold(ego.SAP1), main="Neighborhood n/w of 592540", vertex.color = ifelse(V(ego.SAP1)$label=="1195854","red","dodgerblue"), vertex.label="", vertex.size= ifelse(V(ego.SAP1)$label=="1195854",10,3), rescale=TRUE, edge.arrow.size=0.2)

ego.SAP2 <- induced.subgraph(g_SAPSub_simpl.GC, neighbors(g_SAPSub_simpl.GC, v = c('1195854')))
graph.density(g_SAPSub_simpl.GC)
graph.density(ego.SAP1)
graph.density(ego.SAP2)

assortativity.nominal(g_SAPSub_simpl.GC, (V(g_SAPSub_simpl.GC)$label="1195854")+1, directed=TRUE)
assortativity.nominal(g_SAPSub_simpl.GC, (V(g_SAPSub_simpl.GC)$label="1195854"), directed=FALSE)
assortativity.degree(g_SAPSub_simpl.GC)

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

inv_weight_GC<-1/log(E(g_SAPSub_simpl.GC)$weight  + 1)
num_weight_GC<-E(g_SAPSub_simpl.GC)$weight 
length(inv_weight_GC)
E(g_SAPSub_simpl.GC)$weight <-inv_weight_GC

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'))
neighbors(g_SAPSub_simpl, v=c('592540'))

is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")

# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)
transitivity(g_SAPSub_simpl.GC, weights = inv_weight_GC)

# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)
diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
diameter(g_SAPSub_simpl, weights= inv_weight)

average.path.length(g_SAPSub_simpl.GC, directed=TRUE)
diameter(g_SAPSub_simpl.GC)
diameter(g_SAPSub_simpl.GC, weights= num_weight_GC)
diameter(g_SAPSub_simpl.GC, weights= inv_weight_GC)

# Summarize the graph structure
summary(g_SAPSub_simpl)
summary(g_SAPSub_simpl.GC)

table(sapply(maximal.cliques(g_SAPSub_simpl), length))
table(sapply(maximal.cliques(g_SAPSub_simpl.GC), length))

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Check for number of components and select a giant component out of it
comps <- decompose.graph(g_SAPSub_simpl)
table(sapply(comps, vcount))
g_SAPSub_simpl.GC <- decompose.graph(g_SAPSub_simpl)[[1]]
average.path.length(g_SAPSub_simpl.GC)
diameter(g_SAPSub_simpl.GC)

# works only for undirected graphs
# kc <- fastgreedy.community(g_SAPSub_simpl.GC)
# length(kc)
# sizes(kc)

# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
table(sapply(maximal.cliques(g_SAPSub_simpl.GC), length))
#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Embeddedness/ inverse of structural hole access (see Burt 2004)
#constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
constraints_SAP <- round(constraint(g_SAPSub_simpl.GC, nodes=V(g_SAPSub_simpl.GC)), digits=4)
# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)
degree_sap <- degree(g_SAPSub_simpl.GC)
# Node betweenness
#betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
betweens_SAP <- round(betweenness(g_SAPSub_simpl.GC, v=V(g_SAPSub_simpl.GC), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
#edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl.GC, e=E(g_SAPSub_simpl.GC), directed = TRUE)
# Local clustering coefficients
#clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 
clustering_SAP <- transitivity(g_SAPSub_simpl.GC, type="local", vids=V(g_SAPSub_simpl.GC)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight_GC, inv_weight_GC)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight_GC, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(betweens_SAP, constraints_SAP, clustering_SAP, degree_sap)
a_node<-aggregate(betweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")


# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

# Log-log degree distributino
par(mfrow=c(1, 2))
d.net <-degree(g_SAPSub_simpl.GC)
dd.net <- degree.distribution(g_SAPSub_simpl.GC)
d <- 1:max(d.net)-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(g_SAPSub_simpl.GC,V(g_SAPSub_simpl.GC))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

plot(g_SAPSub_simpl.GC, layout=layout.drl(g_SAPSub_simpl.GC), vertex.size=ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>20,10,3), edge.arrow.size=0.2, rescale=TRUE, vertex.label = ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>20,V(g_SAPSub_simpl.GC)$name,NA), vertex.color = ifelse(degree(g_SAPSub_simpl.GC, mode = 'in')>20,"red","dodgerblue"), vertex.label.color="black", vertex.label.cex = 2, vertex.label.dist = 10)

reciprocity(g_SAPSub_simpl.GC, mode="ratio")
graph.strength(g_SAPSub_simpl.GC)
