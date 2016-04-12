library(tm)
library(topicmodels)
library(stringr)
library(igraph)
library(ggplot2)
library(bipartite)
library(car)

################################################################################
###############Descriptive Stats################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
setwd(wd1)
source("Scripts/functions.R")

doc=read.csv("Data/Indeed/All2.csv",header=F)
doc=cbind(seq(1,length(doc)),doc)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
words_C<-clean.corpus(words_C)
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  

#Dist of words in common between journals...
dummy=((as.matrix(tfm)>0)+0)
sums=matrix(nrow=1281,ncol=5)
adj=c(10,5,10,10,10)
for (i in 0:4){
  sums[,(i+1)]=apply(dummy[(1+i*adj[i+1]):(10+i*adj[i+1]),],2,sum)
}
dummy_sums=(as.matrix(sums)>0)+0
for(i in 1:5){
  print(length(which(apply(dummy_sums,1,sum)>i))/1281)
}

#Word Edge Weights, Edge Dot Similarity, Edge Significance
one_mode=as.one.mode(as.matrix(tfm),fill=0,project="higher",weight=T)
isSymmetric(one_mode)
g=graph.adjacency(as.matrix(one_mode),mode="undirected",weighted=T) 

# write boxplots to a pdf file
pdf("IndeedERGM/Indeed_box1.pdf",height=2,width=5, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
boxplot(one_mode[upper.tri(one_mode)])
dev.off()

# write boxplots to a pdf file
pdf("IndeedERGM/Indeed_hist1.pdf",height=2,width=5, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
hist(one_mode[upper.tri(one_mode)])
dev.off()

max=matrix(NA,nrow=1092,ncol=3)
max[,2]=rownames(one_mode)

for(i in 1:1092){
  max[i,1]=max(one_mode[i,])
  wh.m=which.max(one_mode[i,])
  max[i,3]=rownames(one_mode)[wh.m]
}
max[which(max[,1]=="20"),]
max[which(max[,1]=="15"),]
max[which(max[,1]=="13"),]
max[which(max[,1]=="12"),]
max[which(max[,1]=="11"),]
max[which(max[,1]=="10"),]

Ld=apply(tfm,1,sum)
LC=sum(apply(tfm,1,sum))
s=apply(tfm,2,sum)
p=0.05

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
#weight2=weight[which((weight[,3]-weight[,5])>0),c(1,2,3)]
#write.csv(weight2,"IndeedERGM/network2Indeed.csv")
weight2=read.csv("IndeedERGM/network2Indeed.csv")[,-1]
weight2=as.matrix(weight2)
#Turn into a network object
graph=graph_from_edgelist(as.matrix(weight2[,c(1,2)]),directed=F)

###Betweenness

between=igraph::betweenness(graph)
mean(between)
max(between)

colnames(tfm[,head(order(between,decreasing=TRUE))]) 

#Plotting boxplots for betweenness 
# write boxplots to a pdf file
pdf("IndeedERGM/Indeed_between.pdf",height=2,width=5, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
boxplot(between,main="Betweenness Centrality",id.method="identify")
dev.off()

###Transitivity
#find the transitivity for each of the simulated newtorks
trans=transitivity(graph)

library(sna)
graph2=get.adjacency(graph,type="both")
graph3=as.sociomatrix.sna(as.matrix(graph2))
sna::triad.census(graph3)
detach(sna)
###Degree Assoc.
#find the degree association for each of the simulated newtorks
deg_assoc=assortativity_degree(graph)

#degree dist
max(degree(graph))
min(degree(graph))
hist(degree(graph))
plot(sort(degree(graph)))

###Communitys
cluster1=infomap.community(graph)
length(cluster1)
#cluster2=cluster_edge_betweenness(graph3,directed=FALSE)
#length(cluster2)
cluster3=cluster_fast_greedy(graph)
length(cluster3)
cluster4=cluster_label_prop(graph)
length(cluster4)
cluster5=cluster_leading_eigen(graph)
length(cluster5)
cluster6=cluster_louvain(graph)
length(cluster6)
#cluster7=cluster_optimal(graph)
#length(cluster7)
cluster8=cluster_walktrap(graph)
length(cluster8)
#cluster9=cluster_spinglass(graph)
#length(cluster9)

#testing=kmeans(as.matrix(t(tfm)),6)
#testing$cluster 
#testing$cluster[testing$cluster==5]=1
#testing$cluster[testing$cluster==6]=5

clust=as.vector(membership(cluster1))
clust1=apply(dummy_sums,1,which.max)
testing=kmeans(as.matrix(t(tfm)),5)
clust2=testing$cluster 

clust3=as.vector(membership(cluster3))
clust4=as.vector(membership(cluster5))
clust5=as.vector(membership(cluster6))
clust6=as.vector(membership(cluster8))
###########################################################################
###############ERGM################################################
############################################################################

#Create a network object using the sociomatrix and its attributes
detach("package:bipartite", unload=TRUE)
detach("package:igraph", unload=TRUE)

library(network)
library(ergm)
ga.net<-network(as.matrix(weight2[,c(1,2)]),directed=F, hyper=F, loops=F, 
                multiple=F, bipartite=F,matrix.type="edgelist")

#Set attributes
attributes=data.frame(id=seq(1:dim(tfm)[2]),word=colnames(tfm),clust,
                      clust1,clust2,clust3,clust4,clust5,clust6)
set.vertex.attribute(ga.net,names(attributes),attributes)
set.edge.attribute(ga.net,"weight",weight2[,3])
ga.net

#Arguments for the ergm function:
ga.fit<-ergm(ga.net~edges,estimate="MPLE") 
summary(ga.fit) 

ga.fit.gof<-gof(ga.fit)
summary(ga.fit.gof)

# write boxplots to a pdf file
pdf("IndeedERGM/Indeed_fit.pdf",height=2,width=100, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
plot(ga.fit.gof) 
dev.off()

ga.fit1<-ergm(ga.net~edges+isolates,estimate="MPLE") 
summary(ga.fit1) 

ga.fit1.gof<-gof(ga.fit1)
summary(ga.fit1.gof)

#write boxplots to a pdf file
pdf("IndeedERGM/Indeed_fit1.pdf",height=2,width=100, pointsize=8)
#set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
plot(ga.fit1.gof) 
dev.off()

ga.fit2<-ergm(ga.net~edges+isolates+nodematch("clust"),estimate="MPLE")
summary(ga.fit2) 

ga.fit2.gof<-gof(ga.fit2)
summary(ga.fit2.gof) 

# write boxplots to a pdf file
pdf("IndeedERGM/Indeed_fit2.pdf",height=2,width=100, pointsize=8)
# set some graphical parameters (see ?par)
par(las=1,mar=c(3.25,4,1,1))
plot(ga.fit2.gof) 
dev.off()

ga.fit3<-ergm(ga.net~edges+nodematch("clust"),estimate="MPLE")
summary(ga.fit3) 

ga.fit4<-ergm(ga.net~edges+isolates+nodefactor("clust"),estimate="MPLE")
summary(ga.fit4)

ga.fit5<-ergm(ga.net~edges+isolates+nodematch("clust1"),estimate="MPLE")
summary(ga.fit5)

ga.fit6<-ergm(ga.net~edges+isolates+nodefactor("clust1"),estimate="MPLE")
summary(ga.fit6)

ga.fit7<-ergm(ga.net~edges+isolates+nodematch("clust2"),estimate="MPLE")
summary(ga.fit7)

library(ergm.count)
ga.fit8<-ergm(ga.net~edges,response="weight",estimate="MPLE")
summary(ga.fit8)

###########################################################################
###############QAP################################################
############################################################################
library(sna)
library(bipartite)
library(igraph)

#Word Edge Weights, Edge Dot Similarity, Edge Significance
one_mode=as.one.mode(as.matrix(tfm),fill=0,project="higher",weight=T)
isSymmetric(one_mode)

x=matrix(NA,nrow=1092,ncol=1092)
for(i in 1:1092){
  x[i,]=rep(clust[i],1092)
}
x=x-t(x)
x=x-t(x)
x[x>0]=1
x[x<0]=1

qap_ind=netlm(one_mode,x,intercept=T,mode="graph",diag=F)

x1=matrix(NA,nrow=1092,ncol=1092)
for(i in 1:1092){
  x1[i,]=rep(clust1[i],1092)
}
x1=x1-t(x1)
x1[x1>0]=1
x1[x1<0]=1

qap_ind=netlm(one_mode,x1,intercept=T,mode="graph",diag=F)