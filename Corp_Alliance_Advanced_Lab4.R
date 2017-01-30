library(igraph)

# Step 1: Read file
corp_allaince_EL_2007 <- read.csv("C:/E!!/UIC/IDS564/AdvancedLab4/Share_corp_alliance_EdgeList_2007.csv",sep = ',', header = TRUE)
corp_allaince_EL_2014 <- read.csv("C:/E!!/UIC/IDS564/AdvancedLab4/Share_corp_alliance_EdgeList_2014.csv",sep = ',', header = TRUE)

# Step 2: Convert input file to graph (Caution: The excel file has 3 columns. 
#         By default edges were made from Col1 to Col2)
corp_all_EL_2007_ig <- graph.data.frame(corp_allaince_EL_2007[,c(2,3)])
corp_all_EL_2014_ig <- graph.data.frame(corp_allaince_EL_2014[,c(2,3)])
# Verify step 2
E(corp_all_EL_2007_ig)
V(corp_all_EL_2007_ig)

# Step 3: Get the degree of each node and convert it to a frequency table  
freq_of_degree_EL_2007 <- table(degree(corp_all_EL_2007_ig))
freq_of_degree_EL_2014 <- table(degree(corp_all_EL_2014_ig))

# Step 4: Construct a data frame 
df_EL_2007 <- data.frame(freq_of_degree_EL_2007)
df_EL_2014 <- data.frame(freq_of_degree_EL_2014)
summary(freq_of_degree_EL_2007)


# Step 5: Calculate Cumulative SUM to get frequency distribution
df_EL_2007 <- cbind(df_EL_2007,cumsum(df_EL_2007$Freq))
df_EL_2014 <- cbind(df_EL_2014,cumsum(df_EL_2014$Freq))

# Step 6: Calculate total degree & total corporates
Total_Deg_2007 <- sum(as.numeric(df_EL_2007$Var1)*as.numeric(df_EL_2007$Freq))
Total_Deg_2014 <- sum(as.numeric(df_EL_2014$Var1)*as.numeric(df_EL_2014$Freq))
Total_corp_2007 <- sum(df_EL_2007$Freq)
Total_corp_2014 <- sum(df_EL_2014$Freq)

# Step 7: CDF need values from 0 to 1
Prob_dist_2007 <- df_EL_2007$`cumsum(df_EL_2007$Freq)`/Total_corp_2007
Prob_dist_2014 <- df_EL_2014$`cumsum(df_EL_2014$Freq)`/Total_corp_2014
df_EL_2007 <- data.frame(df_EL_2007,Prob_dist_2007)
df_EL_2014 <- data.frame(df_EL_2014,Prob_dist_2014)

# step 8: Calculate average degree & 'm'. Note: Total degree = 2*t*m
avg_degree_2007 <- Total_Deg_2007/Total_corp_2007
m_07 <- 0.5*avg_degree_2007
avg_degree_2014 <- Total_Deg_2014/Total_corp_2014
m_14 <- 0.5*avg_degree_2014

colnames(df_EL_2007)[colnames(df_EL_2007)=="Var1"] <- "Degree"
colnames(df_EL_2014)[colnames(df_EL_2014)=="Var1"] <- "Degree"
colnames(df_EL_2007)[colnames(df_EL_2007)=="cumsum.df_EL_2007.Freq."] <- "Cumsum"
colnames(df_EL_2014)[colnames(df_EL_2014)=="cumsum.df_EL_2014.Freq."] <- "Cumsum"
colnames(df_EL_2007)[colnames(df_EL_2007)=="Prob_dist_2007"] <- "Prob_Dist"
colnames(df_EL_2014)[colnames(df_EL_2014)=="Prob_dist_2014"] <- "Prob_Dist"

# step 9: Get Y column and X column: Note Y = log(1-F(d)) & X = log(d+2*alpha*m/(1-alpha))
deg_2007 <- as.numeric(levels(df_EL_2007$Degree))[df_EL_2007$Degree]
deg_2014 <- as.numeric(levels(df_EL_2014$Degree))[df_EL_2014$Degree]

df_EL_2007$Degree <- deg_2007
df_EL_2014$Degree <- deg_2014

str(df_EL_2007)
str(df_EL_2014)

alpha_07 <- get_Alpha_func(df_EL_2007,m_07) # 0.20525  # 0.1099045 for Giant Component # is close to Zero. So ____ ?
alpha_14 <- get_Alpha_func(df_EL_2014,m_14) # 0.29999  # 0.0583335 for Giant Component


# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
get_Alpha_func <- function(data_frame, m)
{
  array_Beta_1 <- array(0,9)
  array_Alpha_1 <- array(0,9)
  Y <- log(1-data_frame[,"Prob_Dist"][-length(data_frame[,"Prob_Dist"])]) # Length = 31. Leave last element as it is 1 and log(0) is not defined
  for (i in 1:9 ) {
    alpha_0 <- i/10
    X <- log(data_frame[,"Degree"][-length(data_frame[,"Degree"])] + 2*alpha_0*m/(1-alpha_0))
    l_reg <- lm(Y~X)
    array_Beta_1[i] <- l_reg$coefficients[2]
    array_Alpha_1[i] <- 1 + 2/array_Beta_1[i] 
    print(array_Alpha_1[i])
  }
 
  print(array_Alpha_1 - seq(0.1,0.9,0.1))
  plot(seq(0.1,0.9,0.1),array_Alpha_1, xlab = 'array_Alpha_0', main = 'Alpha for 2014')
  #lines(seq(0.0,1,0.1),seq(0.0,1,0.1))
  closest_alpha <- which.min(abs(array_Alpha_1 - seq(0.1,0.9,0.1)))
  return(array_Alpha_1[closest_alpha])
}


# -------------------------
# Which Corporate has formed most alliances
# Apps in advertising, info retrieval, national security, urban planning, recommendation system, n/w analysis

# Step 10: Get giant components
  # Check for number of components and select a giant component out of it
  comps <- decompose.graph(corp_all_EL_2007_ig)
   table(sapply(comps, vcount))
   V(corp_all_EL_2007_ig.GC)
cl <- clusters(corp_all_EL_2007_ig) 
corp_all_EL_2007_ig.GC <- induced.subgraph(corp_all_EL_2007_ig, which(cl$membership == which.max(cl$csize)))

cl_14 <- clusters(corp_all_EL_2014_ig) 
corp_all_EL_2014_ig.GC <- induced.subgraph(corp_all_EL_2014_ig, which(cl_14$membership == which.max(cl_14$csize)))
# rm(acorp_all_el_2007_ig,acorp_all_el_2014_ig)

# plot(corp_all_EL_2007_ig.GC,  layout=layout.drl(corp_all_EL_2007_ig.GC), vertex.size=ifelse(degree(corp_all_EL_2007_ig.GC)>2,10,3), edge.arrow.size=0.2,
#     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(corp_all_EL_2007_ig.GC, mode = 'out')>2,"red","dodgerblue"))

# Step 11: Get Structural Network attributes
plot(corp_all_EL_2007_ig.GC, vertex.size=0.1*(degree(corp_all_EL_2007_ig.GC)), edge.arrow.size=0.2,
     rescale=TRUE, vertex.label = NA, vertex.color = ifelse(degree(corp_all_EL_2007_ig.GC)>2,"red","dodgerblue"))
table(degree(corp_all_EL_2007_ig.GC))
hist(table(degree(corp_all_EL_2007_ig.GC)), main = "2007 degree distribution", xlab = "Degree", breaks = 10, col = "dodgerblue")

corp_all_EL_2007_ig.GC_un <-  as.undirected(corp_all_EL_2007_ig.GC)
corp_all_EL_2007_ig.GC_un <- simplify(corp_all_EL_2007_ig.GC_un, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight="sum"))
fc <- cluster_fast_greedy(corp_all_EL_2007_ig.GC_un)
membership(corp_all_EL_2007_ig.GC_un)
sizes(corp_all_EL_2007_ig.GC_un)

# 1. Community detection using the Fast Greedy Algorithm
com_detection <- fastgreedy.community(corp_all_EL_2007_ig.GC_un)
length(com_detection)
sizes(com_detection)
c.m <- membership(com_detection)
options(max.print=1000000)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, member.country, useNA = c("no"))
# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(corp_all_EL_2007_ig.GC_un,com_detection, vertex.label= NA, vertex.size=2)

