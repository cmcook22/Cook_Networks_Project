################################################################################
###############Plot the Network################################################
################################################################################
setwd("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/WoS")

library(tm)
library(network)
library(dplyr)
#journal=read.table("journal_file.txt")
#D=3
data1=read.csv("data4corpus1.csv",header=F)
#[1:D,]
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

#Read in the cleaned journal corpus
df=data.frame(id=data[,2],text=data[,1])
custom.reader=readTabular(mapping=list(content="text", id="id"))
sci=VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))

tfm=TermDocumentMatrix(sci)
dim(tfm)
max(tfm)
min(tfm)

edgL=data.frame(first=NA,second=NA)

for(i in 1:dim(tfm)[1]){
  for(j in 1:D){
    if(as.numeric(inspect(tfm[i,j]))>0){
      hold=which(as.numeric(inspect(tfm[-c(1:i),j]))>0)
      if(sum(hold>i)>0){
        second=hold
        first=rep(i,length(second))
        edgL=rbind(edgL,cbind(first,second)) 
      }
    }
    edgL %>% distinct
  }
  print(i)
}

edgL=edgL[-which(edgL[,1]==edgL[,2]),]
edgL=edgL[-1,]  
dim(edgL)
write.csv(edgL,
    "C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/WoS/edgeList.csv")


#############################################################################
#adj is now the adjacentcy matrix for the first ten documents...
#edgL=read.csv()
edg_net=network(edgL,directed=FALSE)


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


