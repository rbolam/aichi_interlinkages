

#### ------------------------------ Aichi Interlinkages ----------------------------####

## Data preparation 

rm(list=ls())
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

## 1. Maintain all interactions

linkmatall <- linkmat
# Make into directed network graph
gall <- graph_from_adjacency_matrix(linkmatall, mode="directed", weighted=TRUE, diag=FALSE)

## 2. Make interactions <2 into zeros  ** this will affect network measures **

linkmat[linkmat < 2] <- 0 
# Make into directed network graph
gall <- graph_from_adjacency_matrix(linkmatall, mode="directed", weighted=TRUE, diag=FALSE)



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



##########################################################
## Collapsing interactions network into strategic goals ##
##########################################################

# The networks below are based on *ALL* interactions (i.e. those weighted 1-3)

##########
## 1. Mean

## Change Targets to Strategic Goals 
linkdf2 <- as.data.frame(linkdf) %>% gather(key="DownstreamTarget", value=Value, -Target)
linkdf2 <- left_join(linkdf2, stratgoals[,-3])
names(linkdf2)[4] <- "UpstreamSG"
linkdf2 <- left_join(linkdf2, stratgoals[,-3], by=c("DownstreamTarget"="Target"))
names(linkdf2)[5] <- "DownstreamSG"
linkdf2 <- linkdf2[-which(is.na(linkdf2$Value)),]
linkdf2 <- aggregate(linkdf2$Value, by=list(linkdf2$UpstreamSG, linkdf2$DownstreamSG), mean)
names(linkdf2) <- c("UpstreamSG","DownstreamSG","Weight")
linkdf2 <- as.data.frame(linkdf2) %>% spread(key="DownstreamSG", value="Weight")

## Make into matrix
linkmatSG <- as.matrix(linkdf2[,-1])
rownames(linkmatSG) <- names(linkdf2)[-1]
linkmatSG

## Make into network graph 
SGgraph <- graph_from_adjacency_matrix(linkmatSG, mode="directed", weighted=TRUE, diag=FALSE)
E(SGgraph)$upstream <- rep(c("A","B","C","D","E"), each=4)

############
## 2. Summed

## Change Targets to Strategic Goals 
linkdf2b <- as.data.frame(linkdf) %>% gather(key="DownstreamTarget", value=Value, -Target)
linkdf2b <- left_join(linkdf2b, stratgoals[,-3])
names(linkdf2b)[4] <- "UpstreamSG"
linkdf2b <- left_join(linkdf2b, stratgoals[,-3], by=c("DownstreamTarget"="Target"))
names(linkdf2b)[5] <- "DownstreamSG"
linkdf2b <- linkdf2b[-which(is.na(linkdf2b$Value)),]
linkdf2b <- aggregate(linkdf2b$Value, by=list(linkdf2b$UpstreamSG, linkdf2b$DownstreamSG), sum)
names(linkdf2b) <- c("UpstreamSG","DownstreamSG","Weight")
linkdf2b <- as.data.frame(linkdf2b) %>% spread(key="DownstreamSG", value="Weight")

## Make into matrix
linkmatSGb <- as.matrix(linkdf2b[,-1])
rownames(linkmatSGb) <- names(linkdf2b)[-1]
linkmatSGb

## Make into network graph 
SGgraphb <- graph_from_adjacency_matrix(linkmatSGb, mode="directed", weighted=TRUE, diag=FALSE)
E(SGgraphb)$upstream <- rep(c("A","B","C","D","E"), each=4)



#######################################################
## Collapsing agreement network into strategic goals ##
#######################################################

## Mean agreement

## Change Targets to Strategic Goals 
agreedf2 <- as.data.frame(agreedf) %>% gather(key="DownstreamTarget", value=Value, -Target)
agreedf2 <- left_join(agreedf2, stratgoals[,-3])
names(agreedf2)[4] <- "UpstreamSG"
agreedf2 <- left_join(agreedf2, stratgoals[,-3], by=c("DownstreamTarget"="Target"))
names(agreedf2)[5] <- "DownstreamSG"
agreedf2 <- agreedf2[-which(is.na(agreedf2$Value)),]
agreedf2 <- aggregate(agreedf2$Value, by=list(agreedf2$UpstreamSG, agreedf2$DownstreamSG), mean)
names(agreedf2) <- c("UpstreamSG","DownstreamSG","Weight")
agreedf2 <- as.data.frame(agreedf2) %>% spread(key="DownstreamSG", value="Weight")

## Make into matrix
agreematSG <- as.matrix(agreedf2[,-1])
rownames(agreematSG) <- names(agreedf2)[-1]
agreematSG

## Make into network graph 
SGagreegraph <- graph_from_adjacency_matrix(agreematSG, mode="directed", weighted=TRUE, diag=FALSE)
E(SGagreegraph)$upstream <- rep(c("A","B","C","D","E"), each=4)





####################
## Save workspace ##
####################

ls()
save.image("Prepared_data.RData")


