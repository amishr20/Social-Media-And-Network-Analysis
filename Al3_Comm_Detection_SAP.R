getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
# dir_path <- "C:/E!!/UIC/IDS564/AdvancedLab3/" 
# setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

library(igraph)
edge_frame=read.csv("C:/E!!/UIC/IDS564/AdvancedLab3/CollabNetEdgeListFilteredDec7_2012.csv", header = TRUE, sep = ",")
node_frame=read.csv("C:/E!!/UIC/IDS564/AdvancedLab3/NodesNetList_corrected_Feb19_2016.csv", header = TRUE, sep = ",")

g_SAP_Full=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

SAP_components <- decompose.graph(g_SAP_Full, min.vertices = 100)
table(sapply(SAP_components, vcount))

# Had to do this because, SAP_components is a list of subgraphs. [] -> returns a list, [[]] returns the element of a list
g_SAP <- SAP_components[[1]]

E(g_SAP)$weight <- 1

g_SAP_Simple <- simplify(g_SAP, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight="sum"))
is_simple(g_SAP_Simple)


#Get a particular attribute of the Vertices
#V(g_SAP_Simple)$country

# Trial of fast greedy
fc <- cluster_fast_greedy(g_SAP_Simple)
membership(g_SAP_Simple)
sizes(g_SAP_Simple)

# Plot the graph
minC <- rep(-Inf, vcount(g_SAP_Simple))
maxC <- rep(Inf, vcount(g_SAP_Simple))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(g_SAP_Simple, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
co[1,]
plot(g_SAP_Simple, layout=co, vertex.size=15, edge.arrow.size=0.2,
     rescale=TRUE)

member.country <- get.vertex.attribute(g_SAP_Simple, "country")
stud.gender<- get.vertex.attribute(g_primschool, "gender")

# 1. Community detection using the Fast Greedy Algorithm
SAP_comm_fast <- fastgreedy.community(g_SAP_Simple, weights=E(g_SAP_Simple)$weight)
length(SAP_comm_fast)
sizes(SAP_comm_fast)
c.m <- membership(SAP_comm_fast)
options(max.print=1000000)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, member.country, useNA = c("no"))
# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(SAP_comm_fast,g_SAP_Simple, vertex.label= NA, vertex.size=2)

# 2. Walktrap 
SAP_comm_walk <- walktrap.community(g_SAP_Simple, weights=E(g_SAP_Simple)$weight)
length(SAP_comm_walk)
sizes(SAP_comm_walk)
scw.m <- membership(SAP_comm_walk)
table(scw.m, member.country, useNA = c("no"))
plot(SAP_comm_walk, g_SAP_Simple, vertex.label= NA, vertex.size=2)
