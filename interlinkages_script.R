

# ------------------------------ Aichi Interlinkages ----------------------------####

# a script which we can hopefully share
library(plyr)
library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)
library(gridExtra)


## Dataframe with Strategic Goals
stratgoals <- data.frame(Target=paste("T", seq(1,20), sep=""), StrategicGoal=rep(c("A","B","C","D","E"),times=c(4,6,3,3,4)),
                         SGcol=rep(c("dodgerblue3","firebrick2","seagreen3","darkorchid3","orange"),times=c(4,6,3,3,4)))


####################################
## Network graph for interactions ##
####################################

## Links dataframe
linkdf <- read.csv("Marques_interactions.csv")
linkmat <- as.matrix(linkdf[,-1])
rownames(linkmat) <- names(linkdf)[-1]
colnames(linkmat) <- names(linkdf)[-1]
linkmat

## Make interactions <2 into zeros    ** this will affect network measures **
linkmatall <- linkmat
linkmat[linkmat < 2] <- 0 

## Make into network graph -> I have made this directed for the measures below
g <- graph_from_adjacency_matrix(linkmat, mode="directed", weighted=TRUE, diag=FALSE)
gall <- graph_from_adjacency_matrix(linkmatall, mode="directed", weighted=TRUE, diag=FALSE)

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




##########################################
## Networkgraph for certainty/agreement ##
##########################################

## Agreement dataframe
agreedf <- read.csv("Marques_agreement.csv")
agreemat <- as.matrix(agreedf[,-1])
rownames(agreemat) <- names(agreedf)[-1]
colnames(agreemat) <- names(agreedf)[-1]
agreemat

## Make into network graph -> I have made this directed for the measures below
ag <- graph_from_adjacency_matrix(agreemat, mode="directed", weighted=TRUE, diag=FALSE)





#################################
## Make plot with Target 12 links

## Make df with t12 links only:
t12 <- linkdf %>%
  select(Target, T12) %>%
  filter(!is.na(T12)) %>%
  rename(width = T12)

t12$Target12 <- c("T12")

## Reorder variables so the nodes are in first two columns:
t12 <- select(t12, Target12, Target, width) 

## Create graph -> I have made it directed 
g12<-graph_from_data_frame(d = t12, directed=T) 

## Changing node colours to Strategic Goals
V(g12)$SG <- as.character(stratgoals$StrategicGoal[match( V(g12)$name, stratgoals$Target)])

## Draw
ggraph(g12, layout="igraph", algorithm = "star") +
  geom_edge_arc(aes(width = t12$width), curvature = 0) +
  scale_edge_width(range = c(0.5, 2), breaks = c(1, 2, 3)) +
  geom_node_point(aes(colour=factor(SG)), size=15) +
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
par(mfrow=c(1,3))
barplot(degree(g), col=as.character(stratgoals$SGcol), main="All", ylab="Degree")
barplot(degree(g,mode="in"), col=as.character(stratgoals$SGcol), main="In", ylab="Degree")
barplot(degree(g,mode="out"), col=as.character(stratgoals$SGcol), main="Out", ylab="Degree")

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


####################################################
## Drawing downstream impacts of each Strategic Goal,
## one at a time

for(i in 1:5){

  # Select targets within strategic goal
  SGtargets <- stratgoals %>% filter(StrategicGoal==c("A","B","C","D","E")[i]) %>% select(Target)
  
  ## Make df with SG links only (sorry this is base R!)
  SGdf <- linkdf
  SGdf[,which(!is.element(names(SGdf), SGtargets$Target))] <- NA
  SGmat <- as.matrix(SGdf[,-1])
  rownames(SGmat) <- SGdf$Target
  
  # Make graph
  SGgraph <- graph_from_adjacency_matrix(SGmat, weighted=TRUE, diag=FALSE)
  
  ## Change node colours to Strategic Goals
  V(SGgraph)$SG <- as.character(stratgoals$StrategicGoal[match(V(SGgraph)$name, stratgoals$Target)])
  
  ## Draw
  print(ggraph(SGgraph, layout="linear", circular=TRUE) +
    geom_edge_arc(aes(width = weight)) +
    scale_edge_width(range = c(0.5, 2), breaks = c(1, 2, 3)) +
    geom_node_point(aes(colour=factor(SG)), size=15) +
    geom_node_label(aes(label=name), size=3, repel=TRUE) +
    guides(colour=FALSE) +
    coord_fixed() +
    theme_void() +
    labs(title=paste("Strategic Goal", c("A","B","C","D","E")[i]))
  )
  # -> possible to colour links based on downstream target? 
  
  
}



# Compare network measure between goals, and between targets? 


##################################################
## Turning hypotheses into tests - interactions ##
##################################################

## Targets within the same strategic goal are more similar
## to each other than to targets within different goals 

#####
## 1. Degree = the number of adjacent edges
## (this is basically the same as Fig 2 from Marques et al.)

netdegree1 <- data.frame(TargetN=names(degree(g)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                         degree.all=as.numeric(degree(g)),
                         degree.in=as.numeric(degree(g,mode="in")),
                         degree.out=as.numeric(degree(g,mode="out")) )  
netdegree1

## Barplots
b1 <- ggplot(netdegree1, aes(x=Target, y=degree.all, fill=SG)) + geom_bar(stat="identity")
b2 <- ggplot(netdegree1, aes(x=Target, y=degree.in, fill=SG)) + geom_bar(stat="identity")
b3 <- ggplot(netdegree1, aes(x=Target, y=degree.out, fill=SG)) + geom_bar(stat="identity")
grid.arrange(b1, b2, b3, nrow = 1)

## Points
p1 <- ggplot(netdegree1, aes(x=SG, y=degree.all, color=SG)) + geom_point(size=2) + geom_text(label=netdegree1$Target, hjust=-1) + theme(legend.position="none") +
  labs(title="Interactions - All", x="Strategic Goal", y = "Degree")
p2 <- ggplot(netdegree1, aes(x=SG, y=degree.in, color=SG)) + geom_point(size=2) + geom_text(label=netdegree1$Target, hjust=-1) + theme(legend.position="none") +
  labs(title="Interactions - In", x="Strategic Goal", y = "Degree")
p3 <- ggplot(netdegree1, aes(x=SG, y=degree.out, color=SG)) + geom_point(size=2) + geom_text(label=netdegree1$Target, hjust=-1) + theme(legend.position="none") +
  labs(title="Interactions - Out", x="Strategic Goal", y = "Degree")
grid.arrange(p1, p2, p3, nrow = 1)

## Stats - all SGs
kruskal.test(degree.all ~ SG, data=netdegree1) # non-sig
kruskal.test(degree.in ~ SG, data=netdegree1)  # sig
kruskal.test(degree.out ~ SG, data=netdegree1) # sig

## post-hoc testing
degreeposthoc <- data.frame()
for(i in 1:4){
  for(j in c(i+1):5){
  out1 <- wilcox.test(degree.in ~ SG, data=netdegree1[which(netdegree1$SG==LETTERS[i]|netdegree1$SG==LETTERS[j]),])
  out2 <- wilcox.test(degree.out ~ SG, data=netdegree1[which(netdegree1$SG==LETTERS[i]|netdegree1$SG==LETTERS[j]),])
  outdf <- data.frame(SG1=LETTERS[i], SG2=LETTERS[j], in.p=out1$p.value, out.p=out2$p.value)
  degreeposthoc <- rbind(degreeposthoc, outdf)
  }}
degreeposthoc


#####
## 2. Strength = summing up the edge weights of the adjacent edges for each vertex

netstrength1 <- data.frame(TargetN=names(strength(g)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                         strength.all=as.numeric(strength(g)),
                         strength.in=as.numeric(strength(g,mode="in")),
                         strength.out=as.numeric(strength(g,mode="out")) )  
netstrength1

## Barplots
bs1 <- ggplot(netstrength1, aes(x=Target, y=strength.all, fill=SG)) + geom_bar(stat="identity")
bs2 <- ggplot(netstrength1, aes(x=Target, y=strength.in, fill=SG)) + geom_bar(stat="identity")
bs3 <- ggplot(netstrength1, aes(x=Target, y=strength.out, fill=SG)) + geom_bar(stat="identity")
grid.arrange(bs1, bs2, bs3, nrow = 1)

## Points
ps1 <- ggplot(netstrength1, aes(x=SG, y=strength.all, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Interactions - All", x="Strategic Goal", y = "Strength")
ps2 <- ggplot(netstrength1, aes(x=SG, y=strength.in, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Interactions - In", x="Strategic Goal", y = "Strength")
ps3 <- ggplot(netstrength1, aes(x=SG, y=strength.out, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Interactions - Out", x="Strategic Goal", y = "Strength")
grid.arrange(ps1, ps2, ps3, nrow = 1)

grid.arrange(p1, p2, p3, ps1, ps2, ps3, nrow = 2) # extremely similary patterns to degree


#################
## 3. Betweenness - roughly defined by the number of shortest paths going threough a vertex or edge
## (not sure what this means exactly for our understanding)

netbetween1 <- data.frame(TargetN=names(betweenness(g)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                           between1=as.numeric(betweenness(g)), # directed
                           between2=as.numeric(betweenness(g,directed=F)) )  # undirected
netbetween1

## Points
pb1 <- ggplot(netbetween1, aes(x=SG, y=between1, color=SG)) + geom_point(size=2) + geom_text(label=netbetween1$Target, hjust=-1) + theme(legend.position="none")
pb2 <- ggplot(netbetween1, aes(x=SG, y=between2, color=SG)) + geom_point(size=2) + geom_text(label=netbetween1$Target, hjust=-1) + theme(legend.position="none")

grid.arrange(pb1, pb2, nrow = 1)




## Goal A has the strongest downstream 
## impacts on Goal B

# Can manipulate input matrix to test this 

linkmatall
goalB <- as.data.frame(linkmatall[which(stratgoals$StrategicGoal=="B"),])
goalB$Target <- row.names(goalB)





###############################################
## Turning hypotheses into tests - agreement ##
###############################################

# Is there variation in strength of agreement (i.e. understanding) among goals?

#####
## Strength = summing up the edge weights of the adjacent edges for each vertex


agstrength1 <- data.frame(TargetN=names(strength(ag)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                           strength.all=as.numeric(strength(ag)),
                           strength.in=as.numeric(strength(ag,mode="in")),
                           strength.out=as.numeric(strength(ag,mode="out")) )  
agstrength1

## Points
psag1 <- ggplot(agstrength1, aes(x=SG, y=strength.all, color=SG)) + geom_point(size=2) + geom_text(label=agstrength1$Target, hjust=-1) + 
  theme(legend.position="none") + labs(title="Agreement - all", x="Strategic Goal", y = "Strength")
psag2 <- ggplot(agstrength1, aes(x=SG, y=strength.in, color=SG)) + geom_point(size=2) + geom_text(label=agstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Agreement -in", x="Strategic Goal", y = "Strength")
psag3 <- ggplot(agstrength1, aes(x=SG, y=strength.out, color=SG)) + geom_point(size=2) + geom_text(label=agstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Agreement - out", x="Strategic Goal", y = "Strength")

grid.arrange(psag1, psag2, psag3, nrow = 1)

grid.arrange(ps1, ps2, ps3, psag1, psag2, psag3, nrow = 2) # compare to interactions


## Stats
kruskal.test(strength.all ~ SG, agstrength1) # non sig
kruskal.test(strength.in ~ SG, agstrength1)  # non sig
kruskal.test(strength.out ~ SG, agstrength1) # sig


## Interactions against agreement

agintstrength <- left_join(agstrength1, netstrength1, by=c("TargetN","Target","SG"))
agintstrength  

inag1 <- ggplot(agintstrength, aes(x=strength.all.x, y=strength.all.y, color=SG)) + geom_point(size=2) + #geom_text(label=agintstrength$Target, hjust=-1) +  
  theme(legend.position="none") + labs(title="Strength - all", x="Agreement", y = "Interaction")
inag2 <- ggplot(agintstrength, aes(x=strength.in.x, y=strength.in.y, color=SG)) + geom_point(size=2) + #geom_text(label=agintstrength$Target, hjust=-1) +
  theme(legend.position="none") + labs(title="Strength - in", x="Agreement", y = "Interaction")
inag3 <- ggplot(agintstrength, aes(x=strength.out.x, y=strength.out.y, color=SG)) + geom_point(size=2) + #geom_text(label=agintstrength$Target, hjust=-1) +  
  theme(legend.position="none") + labs(title="Strength - out", x="Agreement", y = "Interaction")

grid.arrange(inag1, inag2, inag3, nrow = 1)




######################################
## Cluster analyses on interactions ##
######################################

# If targets within Goals are closely linked to each other,
# then a clustering algorithm should place targets into clusters based on the SGs

## 1. Walktrap

intclus1 <- walktrap.community(g, weights=E(g)$weight, steps=2)
intclus1 
names(intclus1)
intclus1$membership

g$wt1 <- intclus1$membership
ggraph(g, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=g$wt1), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()

# Try different number of steps
intclus2 <- walktrap.community(g, weights=E(g)$weight, steps=3)
intclus3 <- walktrap.community(g, weights=E(g)$weight, steps=4)
intclus3
intclus2
intclus1
# all the same


## 2. Optimal clustering

intclus4 <- cluster_optimal(g, weights=E(g)$weight)
intclus4
# same as above

## Compare to graph without interactions <2 removed

intallclus1 <- walktrap.community(gall, weights=E(gall)$weight, steps=2)
intallclus1
intallclus2 <- walktrap.community(gall, weights=E(gall)$weight, steps=3)
intallclus2
intallclus3 <- cluster_optimal(gall, weights=E(gall)$weight)
intallclus3

gall$wt1 <- intallclus1$membership
ggraph(gall, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=gall$wt1), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()




#############################################
## Collapsing network into strategic goals ##
#############################################

## Change Targets to Strategic Goals 
linkdf2 <- as.data.frame(linkdf) %>% gather(key="DownstreamTarget", value=Value, -Target)
linkdf2 <- left_join(linkdf2, stratgoals[,-3])
names(linkdf2)[4] <- "UpstreamSG"
linkdf2 <- left_join(linkdf2, stratgoals[,-3], by=c("DownstreamTarget"="Target"))
names(linkdf2)[5] <- "DownstreamSG"
linkdf2 <- linkdf2[-which(is.na(linkdf2$Value)),]
linkdf2 <- aggregate(linkdf2$Value, by=list(linkdf2$UpstreamSG, linkdf2$DownstreamSG), sum)
names(linkdf2) <- c("UpstreamSG","DownstreamSG","Weight")
linkdf2 <- as.data.frame(linkdf2) %>% spread(key="DownstreamSG", value="Weight")

## Make into matrix
linkmat2 <- as.matrix(linkdf2[,-1])
rownames(linkmat2) <- names(linkdf2)[-1]
linkmat2

## Make into network graph 
SGgraph <- graph_from_adjacency_matrix(linkmat2, mode="directed", weighted=TRUE, diag=FALSE)

## Quick plot 
ggraph(SGgraph, layout="linear", circular=TRUE) +
  geom_edge_arc(arrow = arrow(length = unit(5, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm') , 
                aes(width = weight)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()










