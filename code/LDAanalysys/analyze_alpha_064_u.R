################################################################################
###############Language Results################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
wd2=paste("C:/Users/admin-ccook/Desktop/Code/Testing/comparing/Sources/")
set.seed(12345)
library(tm)
library(topicmodels)
library(stringr)

id=c(02,05,08)
runs=100
K=20

for(j in 1:length(id)){
  setwd(wd1)
  doc=read.csv(paste("Data/Alpha064u/corpus",id[j],".csv",sep=""),header=FALSE)
  doc=cbind(seq(1,length(doc)),doc)
  
  df<-data.frame(id=doc[,1],text=doc[,2])
  custom.reader <- readTabular(mapping=list(content="text", id="id"))
  words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
  tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
  dim(tfm)  
  
  alpha=read.csv(paste("Data/Alpha064u/thetas",id[j],".csv",sep=""),header=FALSE)
  D=dim(tfm)[1]
  gamma=list()
  
  for(i in 1:runs){
    set.seed(12345)
    fit=LDA(tfm,k=K,method="VEM",seed=12345,best=TRUE)
    gamma[[i]]=fit@gamma
  }
  
  print(c("got through lda",j))
  
  setwd(wd2)
  for(i in 1:length(gamma)){
    write.table(gamma[[i]],str_c("Compare/gamma",i,".txt"),row.names=F,col.names=F)
  }
  
  write.table(alpha,"alpha.txt",row.names=F,col.names=F)
  write.table(doc[,-1],"corpus.txt",row.names=F,col.names=F,quote=F)
  
  #Compare using the cpp file and the pipe...
  
  accur=matrix(nrow=runs,ncol=length(id))
  for(i in 1:runs){
    call=paste("compare_models Compare/gamma",i,".txt alpha.txt corpus.txt > Results/results",i,".txt",sep="")
    p1=pipe(call,"r")
    close(p1)
    call=paste("Results/results",i,".txt",sep="")
    task=readLines(call)
    accur[i,j]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  
  print(c("saved accur",j))
  
  repro=matrix(nrow=(runs-1),ncol=length(id))
  for(i in 1:(runs-1)){
    call=paste("compare_models Compare/gamma",i,".txt Compare/gamma",(i+1),".txt corpus.txt > Results/results",i,".txt",sep="")
    p1=pipe(call,"r")
    close(p1)
    call=paste("Results/results",i,".txt",sep="")
    task=readLines(call)
    repro[i,j]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  print(c("saved repro",j))
  
}

setwd(wd1)
write.csv(accur,"Results/accur_alpha3.csv")
write.csv(repro,"Results/repro_alpha3.csv")
