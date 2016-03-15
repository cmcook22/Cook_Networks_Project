################################################################################
###############Plot the Network################################################
################################################################################
setwd("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/Data/WoS")

library(tm)
library(network)
library(dplyr)
library(slam)

data1=read.csv("data4corpus1.csv",header=F)
data2=read.csv("data4corpus2.csv",header=F)
data3=read.csv("data4corpus3.csv",header=F)
data4=read.csv("data4corpus4.csv",header=F)
dim(data1)
dim(data2)
dim(data3)
dim(data4)

data=rbind(data1,data2,data3,data4)
dim(data)
data=data.frame(data,seq(1:dim(data)[1]))
dim(data)
head(data)

journal=read.table("journal_file.txt",header=F)
journal=journal[-c(16534,16535)]
table(journal)
D=10
econ=sample(which(journal=="AMERICAN_ECONOMIC_REVIEW.txt"),D)
astro=sample(which(journal=="ASTRONOMICAL_JOURNAL.txt"),D)
bio=sample(which(journal=="CELL.txt"),D)
geo=sample(which(journal=="GEOLOGY.txt"),D)
math=sample(which(journal=="JOURNAL_OF_DIFFERENTIAL_EQUATIONS.txt"),D)
psych=sample(which(journal=="SCHIZOPHRENIA_BULLETIN.txt"),D)
data_new=data[c(econ,astro,geo,bio,math,psych),-2]
length(data_new)
write.table(data_new,"WoSmall.txt",row.names=F,col.names=F)

econ=matrix(rep(c(1,0,0,0,0,0),D),nrow=D,ncol=6,byrow=T)
astro=matrix(rep(c(0,1,0,0,0,0),D),nrow=D,ncol=6,byrow=T)
bio=matrix(rep(c(0,0,1,0,0,0),D),nrow=D,ncol=6,byrow=T)
geo=matrix(rep(c(0,0,0,1,0,0),D),nrow=D,ncol=6,byrow=T)
math=matrix(rep(c(0,0,0,0,1,0),D),nrow=D,ncol=6,byrow=T)
psych=matrix(rep(c(0,0,0,0,0,1),D),nrow=D,ncol=6,byrow=T)
alpha=rbind(econ,astro,bio,geo,math,psych)
write.table(alpha,"alpha.txt",row.names=F,col.names=F)






