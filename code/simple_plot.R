################################################################################
###############Plot the Network################################################
################################################################################
setwd("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/WoS")

library(tm)
library(network)
D=3 #number of documents to plot the network for
#Note that using all 40530 documents with 124385 verticies and even more edges
  #will not work in R...in fact I was not able to get the first 50 to work
#I will focus on the first 3 documents and also the first 10
data=read.csv("data4corpus.csv",header=F)[1:D,]
dim(data)
data[,2]=seq(1:dim(data)[1])
dim(data)
head(data)

#Read in the cleaned journal corpus
df=data.frame(id=data[,2],text=data[,1])
custom.reader=readTabular(mapping=list(content="text", id="id"))
sci=VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))

tfm=TermDocumentMatrix(sci)
dim(tfm)
max(tfm)
min(tfm)

TERMS=as.matrix(tfm)
TERMS[TERMS > 1] <- 1
min(TERMS)
max(TERMS)


adj=matrix(0,nrow=dim(TERMS)[1],ncol=dim(TERMS)[1])
colnames(adj)=rownames(tfm)
rownames(adj)=rownames(tfm)

for(i in 1:dim(TERMS)[1]){
  for(j in 1:D){
    if(TERMS[i,j]>0) adj[i,]=adj[i,]+TERMS[,j] 
  }
}
diag(adj)=0
adj[adj > 1] <- 1
min(adj)
max(adj)

sum(apply(TERMS,2,sum))
sum(apply(adj,2,sum))
sum(diag(adj))


#############################################################################
#adj is now the adjacentcy matrix for the first ten documents...
adj_net=network(adj,directed=FALSE)


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


