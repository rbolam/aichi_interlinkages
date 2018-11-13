

# ------------------------------ Aichi Interlinkages ----------------------------####

# a script which we can hopefully share

library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)

## Links
linkdf <- read.csv("Marques_interactions.csv")
linkmat <- as.matrix(linkdf[,-1])
rownames(linkmat) <- names(linkdf)[-1]
colnames(linkmat) <- names(linkdf)[-1]
linkmat

linkmat[linkmat < 2] <- 0 



## Make into network graph
g <- graph_from_adjacency_matrix(linkmat, mode="undirected", weighted=TRUE, diag=FALSE)

## Check that weight is listed in graph
E(g)$weight 

## Quick plot (very messy! - just copied script from Mark)
ggraph(g, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()



## Make plot with Target 12 links

## Make df with t12 links only:
t12 <- linkdf %>%
  select(Target, T12) %>%
  filter(!is.na(T12)) %>%
  rename(width = T12)

t12$Target12 <- c("T12")

## Reorder variables so the nodes are in first two columns:
t12 <- select(t12, Target12, Target, width) 

g12<-graph_from_data_frame(d = t12) 

ggraph(g12, layout="igraph", algorithm = "star") +
  geom_edge_arc(aes(width = t12$width), curvature = 0) +
  scale_edge_width(range = c(0.5, 2), breaks = c(1, 2, 3)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()

## Some code I got from Matt for looking at networks:

#Here are the centrality measures, I don't think you'll have edge weights but might be useful in the future:
###node level
#Centrality
# Compute degree
degree(g)
degree(g,mode="in")
degree(g,mode="out")

#A slightly more nuanced metric is "strength centrality", which is defined as the sum of the weights of all the connections for a given node. This is also sometimes called "weighted degree centrality".
# Compute strength
strength(g)
#Closeness centrality is a measure of how far other nodes are from the node in question. Nodes with high closeness centrality are likely to be relatively efficient in receiving or transmitting information to/from distant parts of the social network.
# Weighted closeness
closeness(g)
#Betweenness measures the number of shortest paths between nodes in the network that go through the node in question. Nodes with relatively high betweenness are likely to be key conduits of information flow across a network, and their removal may have a large impact on spreading phenomena.
# Weighted betweenness
betweenness(g)
#Eigenvector centrality is defined as the values of the principal eigenvector for the network when represented as a matrix. Under this metric, a node's centrality score is proportional to the centrality scores of it's connections.
# Eigenvector centrality
eigen_centrality(g)
