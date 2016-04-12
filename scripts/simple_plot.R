################################################################################
###############Plot the Network################################################
################################################################################
setwd("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")

library(tm)
library(network)

data=read.csv("network.csv")
graph2=graph_from_edgelist(data,directed=F)
#############################################################################
#adj is now the adjacentcy matrix for the first ten documents...
adj_net=network(graph2,directed=FALSE)


pdf("simple_plot_small.pdf",height=10,width=10, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
# Simple plot
## Set random number seed so the plot is replicable
set.seed(5)

## Plot the network with labels
plot(adj_net,displaylabels=T,vertex.cex=1,label.cex=1,
     edge.col=rgb(150,150,150,100,maxColorValue=255),
     label.pos=5,vertex.col="lightblue")
# check out all the options with ?plot.network
dev.off()


pdf("simple_plot2_small.pdf",height=10,width=10, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
# Simple plot
## Set random number seed so the plot is replicable
set.seed(5)

## Plot the network with labels
plot(adj_net,vertex.cex=1,
     edge.col=rgb(150,150,150,100,maxColorValue=255),
     label.pos=5,vertex.col="lightblue")
# check out all the options with ?plot.network
dev.off()


