#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
#dir_path <-“~/YourWorkingDirectoryFilePath”
#setwd(dir_path)
# clear everything out of memory
rm(list=ls())  
infile<-"C:/E!!/UIC/IDS564/Lab2/MergerNet_Jan21_2016_forR.csv"
## Load package
library(igraph)
el=read.csv(infile, header = TRUE, sep = ",")
g_acq=graph.data.frame(el, directed = TRUE, vertices= NULL)

### List of all the years represented in the set
el[,"year"]
df <-data.frame(el)
class(df$weight)
# ---   
#[1] "integer"
# ---

class(df$source)
# ---
# [1] "factor"
# ---

class(el)
# ---
# [1] "data.frame"
# ---

# Edges
ecount(g_acq)
## Vertices
vcount(g_acq)

#Is it a simple graph? No!
## Check whether Self_loops exist, as do multiple edges
is.simple(g_acq)
# ---
#[1] FALSE
# ---

g_acq_simpl<-simplify(g_acq)
### The above should default to the option below, to sum the existing edge weights ### when combining them
##g_acq_simpl<-simplify(g_acq,edge.attr.comb="sum" )


is.connected(g_acq, mode=c("weak"))
is.connected(g_acq, mode=c("strong"))

closeness(g_acq_simpl, v = c("511","541","518","519"), mode = "out")
closeness(g_acq_simpl, v = c("511","541","518","519"), mode = "in")



# Will use the inverse of log weight for shortest path calculations
num_weight<-E(g_acq_simpl)$weight 
inv_weight<-1/log(E(g_acq_simpl)$weight  + 1)
length(inv_weight)
E(g_acq_simpl)$weight <- inv_weight

shortest.paths(g_acq_simpl, v = c("511", "541", "518", "519"), to ='814', weights = E(g_acq_simpl)$weight)
shortest.paths(g_acq_simpl, v = '711', to = c("511", "541", "518", "519"), weights = E(g_acq_simpl)$weight)


diameter(g_acq_simpl)
betweenness(g_acq_simpl, v = c("511","541","518","519"), directed = TRUE, nobigint= TRUE, normalized = TRUE, weights = inv_weight)

# Restore value to the original
#E(g_acq_simpl)$weight <- num_weight
# Select your subgraph
sub_net<-induced.subgraph(g_acq_simpl, v=c('511', '541',
'518', '519', '517', '325', '423', '446', '512', '523',
'561', '621', '115', '482', '485', '487', '491', '492',
'521', '712' ))
plot(sub_net)

# Change edge weights
# Will use the inverse of log weight for shortest path calculations
inv_weight<-1/log(E(sub_net)$weight  + 1)
E(sub_net)$weight <- inv_weight
length(inv_weight)

# Modify graph
library(igraphdata)
V(sub_net)$color <- "yellow"
V(sub_net)[c('511','541','518','519')]$color <- "red"
V(sub_net)$shape <- "rectangle"
V(sub_net)[c('511','541','518','519')]$shape <- "circle"
plot(sub_net)
# Change Vertex size
#normalized_betweeness <- 100*( betweenness(sub_net) - min(betweenness(sub_net)) )/ (max(betweenness(sub_net)) - min(betweenness(sub_net)))


# Initializing layout
minC <- rep(-Inf, vcount(sub_net))
maxC <- rep(Inf, vcount(sub_net))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(sub_net, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)
co[1,]
plot(sub_net, layout=co, vertex.size=betweenness(sub_net), edge.arrow.size=0.2,
     rescale=TRUE)

#plot(sub_net, layout=co, vertex.size=30, edge.arrow.size=0.2,
#     rescale=TRUE,
#     xlim=range(co[,1]), ylim=range(co[,2]), vertex.label.dist=0,
#     vertex.label.color="red")
#axis(1)
#axis(2)










