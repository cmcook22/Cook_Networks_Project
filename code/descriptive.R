################################################################################
###############Descriptive Stats################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")

library(tm)
library(topicmodels)
library(stringr)
library(igraph)
library(ggplot2)
library(bipartite)
library(car)

setwd(wd1)
doc=read.table("Data/WoS/WoSmall.txt",header=F)
doc=cbind(seq(1,length(doc)),doc)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  

Ld=apply(tfm,1,sum)
LC=sum(apply(tfm,1,sum))
s=apply(tfm,2,sum)
p=0.05
  
#Word Edge Weights, Edge Dot Similarity, Edge Significance
one_mode=as.one.mode(as.matrix(tfm),fill=0,project="higher")
g=graph.adjacency(as.matrix(one_mode),mode="undirected",weighted=NULL) 
weight=as_edgelist(g,names=FALSE)
weight=cbind(as.numeric(weight[,1]),as.numeric(weight[,2]),
               matrix(nrow=length(weight[,1]),ncol=3))
  
for(i in 1:(length(weight[,1]))){
      
      x=as.matrix(tfm)[,weight[i,1]]
      y=as.matrix(tfm)[,weight[i,2]]
      weight[i,3]=(x %*% y) 
      
      x=s[weight[i,1]]
      y=s[weight[i,2]]
      weight[i,4]=(x * y * sum(Ld^2)) / (LC^2)
      weight[i,5]=qpois(p=p,lambda=weight[i,4])
}
#Edge List for Word Network  
  weight=weight[which((weight[,3]-weight[,5])>=0),c(1,2,5)]
#Turn into a network object
  graph=graph_from_edgelist(weight[,c(1,2)],directed=F)

###Betweenness

between=igraph::betweenness(graph)
mean(between)
max(between)


colnames(tfm[,head(order(between,decreasing=TRUE))]) 
#Plotting boxplots for betweenness 
boxplot(between,main="Betweenness Centrality",id.method="identify")
  
###Transitivity
  #find the transitivity for each of the simulated newtorks
  trans=transitivity(graph)
  
  library(sna)
  graph2=get.adjacency(graph,type="both")
  graph3=as.sociomatrix.sna(as.matrix(graph2))
  sna::triad.census(graph3)
###Degree Assoc.
  #find the degree association for each of the simulated newtorks
  deg_assoc=assortativity_degree(graph)
  