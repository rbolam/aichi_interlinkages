

# ------------------------------ Aichi Interlinkages ----------------------------####

# a script which we can hopefully share

library(igraph)
library(ggplot2)
library(ggraph)

## Links
linkdf <- read.csv("Marques_interactions.csv")
linkmat <- as.matrix(linkdf[,-1])
rownames(linkmat) <- names(linkdf)[-1]
colnames(linkmat) <- names(linkdf)[-1]
linkmat

## Make into network graph
g <- graph_from_adjacency_matrix(linkmat, mode="directed", weighted=TRUE, diag=FALSE)

## Quick plot (very messy! - just copied script from Mark)
ggraph(g, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width=weight, alpha=weight)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()



