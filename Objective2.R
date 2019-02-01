

#### ------------------------------ Aichi Interlinkages ----------------------------####

## Objective 2: Test whether targets fit within their strategic goals 

# Not just about overall network measures, but also about *which* downstream targets/SGs are linked to 


rm(list=ls())
library(plyr)
library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)
library(gridExtra)

load("Prepared_data.RData")
ls()


###############################
## Target interactions networks

## All interactions
ggraph(Tgraphall, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=SG), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()

## Only interactions >=2
ggraph(Tgraphstrong, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=SG), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()


###########################
## Target agreement network 

ggraph(Tgraphagree, layout="linear", circular=TRUE) +
  geom_edge_arc(aes(width = weight)) +
  geom_node_point(aes(colour=SG), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) +
  coord_fixed() +
  theme_void()


###########################
## Target indicator network

ggraph(indicatorgraph, layout="linear", circular=TRUE) +
  geom_edge_fan(aes(width = weight)) +
  geom_node_point(aes(colour=SG), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void()  



##########################
## Network measurements ##
##########################

## Strength = summing up the edge weights of the adjacent edges for each vertex

# Can compare networks with and without interactions=1


netstrength1 <- data.frame(TargetN=names(strength(Tgraphall)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                           ALLstrength.all=as.numeric(strength(Tgraphall)),
                           ALLstrength.in=as.numeric(strength(Tgraphall,mode="in")),
                           ALLstrength.out=as.numeric(strength(Tgraphall,mode="out")),
                           STRONGstrength.all=as.numeric(strength(Tgraphstrong)),
                           STRONGstrength.in=as.numeric(strength(Tgraphstrong,mode="in")),
                           STRONGstrength.out=as.numeric(strength(Tgraphstrong,mode="out"))                          
                           
                           )  
netstrength1


## Points
psa1 <- ggplot(netstrength1, aes(x=SG, y=ALLstrength.all, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="All interactions - total", x="Strategic Goal", y = "Strength")
psa2 <- ggplot(netstrength1, aes(x=SG, y=ALLstrength.in, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="All interactions - In", x="Strategic Goal", y = "Strength")
psa3 <- ggplot(netstrength1, aes(x=SG, y=ALLstrength.out, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="All interactions - Out", x="Strategic Goal", y = "Strength")

pss1 <- ggplot(netstrength1, aes(x=SG, y=STRONGstrength.all, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Strong interactions - total", x="Strategic Goal", y = "Strength")
pss2 <- ggplot(netstrength1, aes(x=SG, y=STRONGstrength.in, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Strong interactions - In", x="Strategic Goal", y = "Strength")
pss3 <- ggplot(netstrength1, aes(x=SG, y=STRONGstrength.out, color=SG)) + geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Strong interactions - Out", x="Strategic Goal", y = "Strength")

grid.arrange(psa1, psa2, psa3, pss1, pss2, pss3, nrow = 2) 



















