

#### ------------------------------ Aichi Interlinkages ----------------------------####

## Objective 1

rm(list=ls())
library(plyr)
library(igraph)
library(ggplot2)
library(tidyverse)
library(ggraph)
library(gridExtra)

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

















