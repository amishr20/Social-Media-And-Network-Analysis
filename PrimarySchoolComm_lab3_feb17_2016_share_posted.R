getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "C:/E!!/UIC/IDS564/Lab3/"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

# Edges
ecount(g_primschool)
## Vertices
vcount(g_primschool)
is.weighted(g_primschool)

V(g_primschool)$name
E(g_primschool)$weight
V(g_primschool)$gender
V(g_primschool)[V(g_primschool)$classname=="1B"]

is.simple(g_primschool)
is.connected(g_primschool)


# Trial to check use of cluster_fast_greedy
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
fc <- cluster_fast_greedy(g)
membership(fc)
sizes(fc)

# Plot the graph
minC <- rep(-Inf, vcount(g_primschool))
maxC <- rep(Inf, vcount(g_primschool))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(g_primschool, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
co[1,]
plot(g_primschool, layout=co, vertex.size=15, edge.arrow.size=0.2,
     rescale=TRUE)


# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")
# Does edge weight make any difference here?


# 1. Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_fast)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))
# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=2)

# 2. Walktrap 
school_comm_walk <- walktrap.community(g_primschool, weights=E(g_primschool)$weight)
scw.m <- membership(school_comm_walk)
table(scw.m, stud.class, useNA = c("no"))
plot(school_comm_walk,g_primschool, vertex.label= NA, vertex.size=2)

# 3. Label Propogation
school_comm_labelprop <- label.propagation.community(g_primschool, weights = E(g_primschool)$weight)
scl.m <- membership(school_comm_labelprop)
table(scl.m, stud.class, useNA = c("no"))
plot(school_comm_labelprop,g_primschool, vertex.label= NA, vertex.size=2)

# 4. SpinGlass
school_comm_spinglass <- spinglass.community(g_primschool, weights = E(g_primschool)$weight)
scs.m <- membership(school_comm_spinglass)
table(scs.m, stud.class, useNA = c("no"))
plot(school_comm_spinglass,g_primschool, vertex.label= NA, vertex.size=2)

# 5 Girvan-Newman
school_comm_GN <- edge.betweenness.community(g_primschool, vertex.label = NA, vertex.size = 2)
sgn.m <- membership(school_comm_GN)
table(school_comm_GN, stud.class, useNA = c("no"))
plot(school_comm_GN,g_primschool, vertex.label= NA, vertex.size=2)

# Here, we are testing community significance for just two of the communities. Students will complete tests for the remainder of communities for each algorithm. 
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
community.significance.test(g_primschool, v_comp1) #significant
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3) #significant
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5) #significant
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)

v_comp11 <- V(g_primschool)[scw.m==1]
v_comp12 <- V(g_primschool)[scw.m==2]
v_comp13 <- V(g_primschool)[scw.m==3]
v_comp14 <- V(g_primschool)[scw.m==4]
v_comp15 <- V(g_primschool)[scw.m==5]
v_comp16 <- V(g_primschool)[scw.m==6]
v_comp17 <- V(g_primschool)[scw.m==7]
v_comp18 <- V(g_primschool)[scw.m==8]
v_comp19 <- V(g_primschool)[scw.m==9]
community.significance.test(g_primschool, v_comp11) #sig
community.significance.test(g_primschool, v_comp12) #sig
community.significance.test(g_primschool, v_comp13)
community.significance.test(g_primschool, v_comp14)
community.significance.test(g_primschool, v_comp15)
community.significance.test(g_primschool, v_comp16)
community.significance.test(g_primschool, v_comp17)
community.significance.test(g_primschool, v_comp18)
community.significance.test(g_primschool, v_comp19)




# In the igraph help, online documentation or KC book, students will find the function calls for the walktrap, spinglass, and label propagation algorithms 
# Why are the benefits and drawbacks of the Girvan-Newman algorithm for community detection? Hint: try it in igraph

# Consider students in first grade and 5th grade. To what extent does community structure indicate that students segregate by gender in these two grades?
# Use the Fast Greedy algorithm for analysis.
v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)

#segregation in Grade 1
g1_community <- fastgreedy.community(subgraph_grade1, weights = E(subgraph_grade1)$weight)
g1.m <- membership(g1_community)
g1.gender<- get.vertex.attribute(subgraph_grade1, "gender")
table(g1.m,g1.gender, useNA = "no")
#segregation in Grade 5
g5_community <- fastgreedy.community(subgraph_grade5, weights = E(subgraph_grade5)$weight)
g5.m <- membership(g5_community)
g5.gender <- get.vertex.attribute(subgraph_grade5, "gender")
table(g5.m,g5.gender, useNA = "no")

#segregation in Grade 1A
subgraph_grade1A<-induced_subgraph(g_primschool, V(g_primschool)[V(g_primschool)$classname=="1A"])
g1A_community <- fastgreedy.community(subgraph_grade1A, weights = E(subgraph_grade1A)$weight)
g1A.m <- membership(g1A_community)
g1A.gender<- get.vertex.attribute(subgraph_grade1A, "gender")
table(g1A.m,g1A.gender, useNA = "no")
#segregation in Grade 1B
subgraph_grade1B<-induced_subgraph(g_primschool, V(g_primschool)[V(g_primschool)$classname=="1B"])
g1B_community <- fastgreedy.community(subgraph_grade1B, weights = E(subgraph_grade1B)$weight)
g1B.m <- membership(g1B_community)
g1B.gender<- get.vertex.attribute(subgraph_grade1B, "gender")
table(g1B.m,g1B.gender, useNA = "no")
#segregation in Grade 5A
subgraph_grade5A<-induced_subgraph(g_primschool, V(g_primschool)[V(g_primschool)$classname=="5A"])
g5A_community <- fastgreedy.community(subgraph_grade5A, weights = E(subgraph_grade5A)$weight)
g5A.m <- membership(g5A_community)
g5A.gender<- get.vertex.attribute(subgraph_grade5A, "gender")
table(g5A.m,g5A.gender, useNA = "no")
#segregation in Grade 5B
subgraph_grade5B<-induced_subgraph(g_primschool, V(g_primschool)[V(g_primschool)$classname=="5B"])
g5B_community <- fastgreedy.community(subgraph_grade5B, weights = E(subgraph_grade5B)$weight)
g5B.m <- membership(g5B_community)
g5B.gender<- get.vertex.attribute(subgraph_grade5B, "gender")
table(g5B.m,g5B.gender, useNA = "no")
