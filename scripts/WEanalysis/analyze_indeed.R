library(gtools)
library(tm)
library(stringr)
library(igraph)
library(bipartite)

#############################################################################
##############################################################################
##############################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
wd2=paste("C:/Users/admin-ccook/Desktop/Code/Testing/comparing/Sources/")
setwd(wd1)
source("Scripts/functions.R")

doc=read.csv("Data/Indeed/All.csv",header=F)
doc=cbind(seq(1,length(doc)),doc)
doc=doc[-c(47:51),]
dim(doc)
alpha=read.table("Data/Indeed/alpha.txt",header=F)
alpha=alpha[-c(47:51),]
dim(alpha)

setwd(wd2)
write.table(alpha,"alpha.txt",row.names=F,col.names=F)
write.table(doc[,-1],"corpus.txt",row.names=F,col.names=F,quote=F)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
words_C<-clean.corpus(words_C)
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  

#Word Edge Weights, Edge Dot Similarity, Edge Significance
one_mode=as.one.mode(as.matrix(tfm),fill=0,project="lower",weight=T)
isSymmetric(one_mode)
dim(one_mode)
is.matrix(one_mode)

graph=graph.adjacency(as.matrix(one_mode),mode="undirected",weighted=TRUE) 

###Communitys
cluster5=cluster_leading_eigen(graph)
length(cluster5)

setwd(wd1)
gamma=list()
runs=100

K=3
left=c(1,1,1,2,2,3)
right=c(1,2,3,2,3,3)
start=matrix(0,nrow=dim(doc)[1],ncol=K)
m=membership(cluster5)
m[m==4]=sample(1:3,1)
for(i in 1:length(unique(m))){
  start[which(m==i),i]=1
}

K=5
left=c(1,1,1,1,1,2,2,2,2,3,3,3,4,4,5)
right=c(1,2,3,4,5,2,3,4,5,3,4,5,4,5,5)
start=matrix(0,nrow=dim(doc)[1],ncol=K)
m=membership(cluster5)
m[21:25]=5
for(i in 1:length(unique(m))){
  start[which(m==i),i]=1
}

K=10
left=c(rep(1,10),rep(2,9),rep(3,8),rep(4,7),rep(5,6),rep(6,5),7,7,7,7,8,8,8,
9,9,10)
right=c(seq(1:10),seq(2:10),seq(3:10),seq(4:10),seq(5:10),seq(6:10),7,8,9,10,
8,9,10,9,10,10)
start=matrix(0,nrow=dim(doc)[1],ncol=K)
m=membership(cluster5)
m[21:25]=5
for(i in 1:length(unique(m))){
  start[which(m==i),i]=1
}

for(i in 1:runs){
  set.seed(12345)
  fit=weightedEdge(edges=one_mode,topics=K,iter=1000,l=left,r=right,start=TRUE,
                   s=start)
  gamma[[i]]=fit$Theta
  print(i)
}

print(c("got through WEdge"))

setwd(wd2)
for(i in 1:length(gamma)){
  write.table(round(gamma[[i]],4),str_c("Compare/gamma",i,".txt"),row.names=F,col.names=F)
}

#Compare using the cpp file and the pipe...

accur=matrix(nrow=runs,ncol=2)
for(i in 1:runs){
  call=paste("compare_models Compare/gamma",i,".txt alpha.txt corpus.txt > Results/results",i,".txt",sep="")
  p1=pipe(call,"r")
  close(p1)
  call=paste("Results/results",i,".txt",sep="")
  task=readLines(call)
  accur[i,2]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
}

print(c("saved accur"))

repro=matrix(nrow=(runs-1),ncol=2)
for(i in 1:(runs-1)){
  call=paste("compare_models Compare/gamma",i,".txt Compare/gamma",(i+1),".txt corpus.txt > Results/results",i,".txt",sep="")
  p1=pipe(call,"r")
  close(p1)
  call=paste("Results/results",i,".txt",sep="")
  task=readLines(call)
  repro[i,2]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
}
print(c("saved repro"))

setwd(wd1)
write.csv(accur,"Results_we/accur_indeed2.csv")
write.csv(repro,"Results_we/repro_indeed2.csv")



