

#### ------------------------------ Aichi Interlinkages ----------------------------####

## Objective 1: Test how well the strategic goals fit the DPSIR framework 

rm(list=ls())
library(plyr)
library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)
library(gridExtra)
library(reshape2)

load("Prepared_data.RData")
ls()


#############################################
## Summed vs mean strength of interactions ##
#############################################

# *NOTES* 
# There are a different number of targets within each goal, and so summing interactions 
# is expected to create a bias towards goals with more targets.
# The difference between summed and mean interaction strength is explored below. 

# The networks below are based on *ALL* interactions (i.e. those weighted 1-3)

## Mean strength
linkmatSG

## Summed strength
linkmatSGb

##################
## Plot comparison 

ggSGmean <- ggraph(SGgraph, layout="linear", circular=TRUE) +
  geom_edge_fan(arrow = arrow(length = unit(5, 'mm')), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm') ,  
                aes(width = weight, colour=upstream)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void()  + ggtitle("Mean") 

ggSGsum <- ggraph(SGgraphb, layout="linear", circular=TRUE) +
  geom_edge_fan(arrow = arrow(length = unit(5, 'mm')),  start_cap = circle(3, 'mm'),end_cap = circle(3, 'mm') , 
                aes(width = weight, colour=upstream)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void() + ggtitle("Sum")

grid.arrange(ggSGmean, ggSGsum, nrow = 1)

ggSGmean



##########################
## Network of agreement ##
##########################

agreematSG

ggraph(SGagreegraph, layout="linear", circular=TRUE) +
  geom_edge_fan(arrow = arrow(length = unit(5, 'mm')), start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm') ,  
                aes(width = weight, colour=upstream)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void()  + ggtitle("Strength of agreement among experts") 




################################
## Agreement versus interactions

## Prepare dataframe (check oot ma piping!)
tocompare <- as.data.frame(agreedf2) %>% gather(key="DownstreamSG", value=Agreement, -UpstreamSG) %>% 
  left_join(gather(linkdf2, key="DownstreamSG", value=Interaction, -UpstreamSG)) %>%
  filter(DownstreamSG!=UpstreamSG) 
tocompare

## Plotting
intvagree <- ggplot(tocompare, aes(x=Agreement, y=Interaction, colour=UpstreamSG)) + geom_point(size=3) 
agreevsSG <- ggplot(tocompare, aes(x=UpstreamSG, y=Agreement, colour=DownstreamSG)) + geom_point(size=3) 

grid.arrange(intvagree, agreevsSG, nrow = 1)

## Stats?
summary(lm(Interaction ~ Agreement, tocompare))
summary(lm(Agreement ~ UpstreamSG, tocompare))
summary(lm(Interaction ~ UpstreamSG, tocompare))



###########################
## Network of indicators ##
###########################

indicatorsSGmat

ggraph(indSGgraph, layout="linear", circular=TRUE) +
  geom_edge_fan(aes(width = weight)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void()  


#################################
## Theoretical DPSIR framework ##
#################################

ggraph(dpsirgraph, layout="linear", circular=TRUE) +
  geom_edge_fan(arrow = arrow(length = unit(5, 'mm')),  start_cap = circle(3, 'mm'),end_cap = circle(3, 'mm') , 
                aes(width = weight, colour=upstream)) +
  geom_node_point(aes(colour=name), size=15) +
  geom_node_label(aes(label=name), size=3, repel=TRUE) +
  guides(colour=FALSE) + coord_fixed() + theme_void() + ggtitle("DPSIR network")




######################
## Network measures ##
######################

strength(g)
closeness(g)
betweenness(g)
eigen_centrality(g)

#
# ** Need to think about whether these should be directional ! 
#

## Strength dataframes 
strengthall <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=strength(SGgraph), Indicators=strength(indSGgraph), DPSIR=strength(dpsirgraph))
strengthin <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=strength(SGgraph, mode="in"), Indicators=NA, DPSIR=strength(dpsirgraph, mode="in"))
strengthout <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=strength(SGgraph, mode="out"), Indicators=NA, DPSIR=strength(dpsirgraph, mode="out"))

## Closeness dataframes
closenessall <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=closeness(SGgraph), Indicators=closeness(indSGgraph), DPSIR=closeness(dpsirgraph))
closenessin <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=closeness(SGgraph, mode="in"), Indicators=NA, DPSIR=closeness(dpsirgraph, mode="in"))
closenessout <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=closeness(SGgraph, mode="out"), Indicators=NA, DPSIR=closeness(dpsirgraph, mode="out"))

closenessallI <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=closeness(SGgraph, weights=1/E(SGgraph)$weight), Indicators=closeness(indSGgraph, weights=1/E(indSGgraph)$weight), DPSIR=closeness(dpsirgraph, weights=1/E(dpsirgraph)$weight))
closenessinI <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=closeness(SGgraph, mode="in", weights=1/E(SGgraph)$weight), Indicators=NA, DPSIR=closeness(dpsirgraph, mode="in", weights=1/E(dpsirgraph)$weight))
closenessoutI <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=closeness(SGgraph, mode="out", weights=1/E(SGgraph)$weight), Indicators=NA, DPSIR=closeness(dpsirgraph, mode="out", weights=1/E(dpsirgraph)$weight))






## Betweenness and eigen centrality 
betweennessall <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=betweenness(SGgraph), Indicators=betweenness(indSGgraph), DPSIR=betweenness(dpsirgraph))
eigen_centralityall <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                             Interaction=eigen_centrality(SGgraph)$vector, Indicators=eigen_centrality(indSGgraph)$vector, DPSIR=eigen_centrality(dpsirgraph)$vector)

## Melt dataframes 
strengthall2 <- melt(strengthall, id="StrategicGoal")
strengthin2 <- melt(strengthin, id="StrategicGoal")
strengthout2 <- melt(strengthout, id="StrategicGoal")
closenessall2 <- melt(closenessall, id="StrategicGoal")
closenessin2 <- melt(closenessin, id="StrategicGoal")
closenessout2 <- melt(closenessout, id="StrategicGoal")

closenessallI2 <- melt(closenessallI, id="StrategicGoal")
closenessinI2 <- melt(closenessinI, id="StrategicGoal")
closenessoutI2 <- melt(closenessoutI, id="StrategicGoal")


betweennessall2 <- melt(betweennessall, id="StrategicGoal")
eigen_centralityall2 <- melt(eigen_centralityall, id="StrategicGoal")

## Plot set up 
allm1 <- ggplot(data=strengthall2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Strength ALL network measure", x="Strategic Goal", y="Strength")
allm1b <- ggplot(data=strengthin2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Strength IN network measure", x="Strategic Goal", y="Strength")
allm1c <- ggplot(data=strengthout2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Strength OUT network measure", x="Strategic Goal", y="Strength")

allm2 <- ggplot(data=closenessall2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness ALL network measure", x="Strategic Goal", y="Closeness")
allm2b <- ggplot(data=closenessin2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness IN network measure", x="Strategic Goal", y="Closeness") 
allm2c <- ggplot(data=closenessout2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness OUT network measure", x="Strategic Goal", y="Closeness") 

allm2I <- ggplot(data=closenessallI2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness ALL - inverse weights", x="Strategic Goal", y="Closeness")
allm2Ib <- ggplot(data=closenessinI2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness IN - inverse weights", x="Strategic Goal", y="Closeness") 
allm2Ic <- ggplot(data=closenessoutI2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(size=3) + labs(title="Closeness OUT - inverse weights", x="Strategic Goal", y="Closeness") 
                          
allm3 <- ggplot(data=betweennessall2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(position=position_jitter(h=0, w=0.2),alpha = 0.5, size=3) + labs(title="Betweenness network measure", x="Strategic Goal", y="Betweenness")
allm4 <- ggplot(data=eigen_centralityall2, aes(x=StrategicGoal, y=value, colour=variable)) +
  geom_point(position=position_jitter(h=0, w=0.2),alpha = 0.5, size=3) + labs(title="Eigen centrality network measure", x="Strategic Goal", y="Eigen centrality")

## Strength 
grid.arrange(allm1,  allm1b, allm1c, nrow = 1)

## Closeness 
grid.arrange(allm2, allm2b, allm2c, allm2I, allm2Ib, allm2Ic, nrow = 2)

# Betweenness and Eigen centrality
grid.arrange(allm3, allm4, nrow = 1)


## Subset for summary 

strengthin <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                         Interaction=strength(SGgraph, mode="in"), DPSIR=strength(dpsirgraph, mode="in"))
strengthout <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                          Interaction=strength(SGgraph, mode="out"), DPSIR=strength(dpsirgraph, mode="out"))
closenessinI <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                           Interaction=closeness(SGgraph, mode="in", weights=1/E(SGgraph)$weight), DPSIR=closeness(dpsirgraph, mode="in", weights=1/E(dpsirgraph)$weight))
closenessoutI <- data.frame(StrategicGoal=c("A","B","C","D","E"),
                            Interaction=closeness(SGgraph, mode="out", weights=1/E(SGgraph)$weight), DPSIR=closeness(dpsirgraph, mode="out", weights=1/E(dpsirgraph)$weight))


grid.arrange(allm1b, allm1c, nrow = 1)

grid.arrange(allm2Ib, allm2Ic, nrow = 1)














