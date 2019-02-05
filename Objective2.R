

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

##############
## Strength ## = summing up the edge weights of the adjacent edges for each vertex
##############

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

#########
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

## Strength in vs strength out
ggplot(netstrength1, aes(x=ALLstrength.in, y=ALLstrength.out, color=SG)) + 
  geom_point(size=2) + geom_text(label=netstrength1$Target, hjust=-0.5) + theme(legend.position="none") +
  theme(legend.position="none") + labs(x="Strength in", y = "Strength out")

summary(lm(ALLstrength.out~ALLstrength.in, netstrength1)) # non-sig


#############
## Histograms - distribution of strength measures

bw <- 5
shist1 <- ggplot(netstrength1, aes(x=ALLstrength.all)) + 
  geom_histogram(binwidth=bw, color="black", fill="white") + 
  labs(title="All interactions - total", x="Strength", y = "Frequency")
shist2 <- ggplot(netstrength1, aes(x=ALLstrength.in)) + 
  geom_histogram(binwidth=bw, color="black", fill="white") + 
  labs(title="All interactions - in", x="Strength", y = "Frequency")
shist3 <- ggplot(netstrength1, aes(x=ALLstrength.out)) + 
  geom_histogram(binwidth=bw, color="black", fill="white") + 
  labs(title="All interactions - out", x="Strength", y = "Frequency")

grid.arrange(shist1, shist2, shist3, nrow = 1) 


######################
## Where do links go ?

# compare strength of links to SGs among targets? 

linkmatall
SGs <- rep(c("A","B","C","D","E"), times=c(4,6,3,3,4))

## Rearrange data
tt <- as.data.frame(linkmatall)
tt <- cbind(UpstreamTarget=rownames(tt), tt)
tt <-  gather(tt, key="DownStreamTarget", value=Weight, -UpstreamTarget)
tt$DownstreamSG <- rep(SGs, each=20)
ttSG <- aggregate(tt$Weight, by=list(tt$UpstreamTarget,tt$DownstreamSG), function(x) mean(x, na.rm=T))
names(ttSG) <- c("UpstreamTarget","DownstreamSG","MeanWeight")
ttSG$UpstreamTN <- as.numeric(gsub("T", "", ttSG$UpstreamTarget))

## Plot mean downstream weight of interactions by SG
ggplot(ttSG, aes(x=UpstreamTN, y=MeanWeight, color=DownstreamSG)) +
  geom_point(position=position_jitter(h=0, w=0.2),alpha = 0.5, size = 3) +
  labs(title="Mean strength of downstream interaction", x="Upstream Target", y="Mean weight") +
  scale_x_continuous(breaks=seq(1,20,1))

## Break up into Goals
dslg <- list()
for(i in 1:5){
  ttplot <- ttSG[which(is.element(ttSG$UpstreamTN, list(c(1,2,3,4),c(5,6,7,8,9,10),c(11,12,13),c(14,15,16),c(17,18,19,20))[[i]])),]
  ttplot$MeanWeight[which(ttplot$DownstreamSG==c("A","B","C","D","E")[i])] <- NA
  dslg$SG <- ggplot(ttplot, aes(x=UpstreamTN, y=MeanWeight, color=DownstreamSG)) +
    geom_point(position=position_jitter(h=0, w=0.2),alpha = 0.5, size = 3) +
    labs(title=paste("Strategic Goal", c("A","B","C","D","E")[i]), x="Upstream Target", y="Mean weight")+
    scale_x_continuous(breaks=seq(min(ttplot$UpstreamTN),max(ttplot$UpstreamTN),1)) 
  names(dslg)[length(dslg)] <- paste("SG",c("A","B","C","D","E")[i], sep="")  }

grid.arrange(dslg[[1]], dslg[[2]], dslg[[3]], dslg[[4]], dslg[[5]], nrow = 2) 




## Same for up stream interactions? 





######################
## Eigen centrality ## = measure of the influence of a node in a network
######################   (A high eigenvector score means that a node is connected to many nodes who themselves have high scores)

## Save eigen centrality in df
eigenints <- data.frame(TargetN=names(strength(Tgraphall)), Target=seq(1,20), SG=as.character(stratgoals$StrategicGoal), 
                        eigenundirected=eigen_centrality(Tgraphall)$vector,
                        eigendirected=eigen_centrality(Tgraphall, directed=T)$vector,
                        eigenindicators=eigen_centrality(indicatorgraph)$vector )
eigenints

## Plots
pe1 <- ggplot(eigenints, aes(x=SG, y=eigenundirected, color=SG)) + geom_point(size=2) + geom_text(label=eigenints$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Undirected interactions", x="Strategic Goal", y = "Eigen centrality")
pe2 <- ggplot(eigenints, aes(x=SG, y=eigendirected, color=SG)) + geom_point(size=2) + geom_text(label=eigenints$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Directed interactions", x="Strategic Goal", y = "Eigen centrality")
pe3 <- ggplot(eigenints, aes(x=SG, y=eigenindicators, color=SG)) + geom_point(size=2) + geom_text(label=eigenints$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Indicator network", x="Strategic Goal", y = "Eigen centrality")
pe4 <- ggplot(eigenints, aes(x=eigenundirected, y=eigenindicators, color=SG)) + geom_point(size=2) + geom_text(label=eigenints$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Indicator vs interactions", x="Eigen centrality interactions (undirected)", y = "Eigen centrality indicators")
pe5 <- ggplot(eigenints, aes(x=eigendirected, y=eigenindicators, color=SG)) + geom_point(size=2) + geom_text(label=eigenints$Target, hjust=-1) + theme(legend.position="none") +
  theme(legend.position="none") + labs(title="Indicator vs interactions", x="Eigen centrality interactions (directed)", y = "Eigen centrality indicators")


grid.arrange(pe1, pe2, nrow = 1) 
grid.arrange(pe3, pe4, pe5, nrow = 1) 

# order high to low? 




















