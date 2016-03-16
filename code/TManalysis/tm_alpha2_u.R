################################################################################
###############Language Results################################################
################################################################################
wd1=paste("/Users/cynthiacook/Desktop/Project/")
wd2=paste("/Users/cynthiacook/Desktop/Code/")
set.seed(12345)
library(tm)
library(topicmodels)
library(stringr)

id=c(02,05,08)
t=c(30,30,30)
runs=100
K=20

for(j in 1:length(id)){
  setwd(wd1)
  doc=read.csv(paste("Data/Alpha064u/corpus",id[j],".csv",sep=""),header=FALSE)
  
  alpha=read.csv(paste("Data/Alpha064u/thetas",id[j],".csv",sep=""),header=FALSE)
  setwd(wd2)
  write.table(doc,"corpus.txt",row.names=F,col.names=F,quote=F)
  write.table(alpha,"alpha.txt",row.names=F,col.names=F,quote=F)
  for(i in 1:runs){
    call=paste("bin/topicmap -f corpus.txt -t 10 -o Testing/comparing/Sources/Results/test_results",i,sep="")
    p1=pipe(call,"r")
    Sys.sleep(t[j])
    close(p1)
    call=paste("mv Testing/comparing/Sources/Results/test_results",i,"/lda_gammas_final.txt Testing/comparing/Sources/Compare/lda_gammas_final", i, ".txt",sep="")
    p2=pipe(call,"r")
    close(p2)
  }
  
  print(c("got through tm",j))
  
  #Compare using the cpp file and the pipe...
  
  accur=matrix(nrow=runs,ncol=length(id))
  for(i in 1:runs){
    call=paste("Testing/comparing/Sources/compare_models Testing/comparing/Sources/Compare/lda_gammas_final", i,".txt alpha.txt corpus.txt > Testing/comparing/Sources/Res/results",i,".txt",sep="")
    p3=pipe(call,"r")
    close(p3)
    call=paste("Testing/comparing/Sources/Res/results",i,".txt",sep="")
    task=readLines(call)
    accur[i,j]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  
  print(c("saved accur",j))
  
  repro=matrix(nrow=(runs-1),ncol=length(id))
  for(i in 1:(runs-1)){
    call=paste("Testing/comparing/Sources/compare_models Testing/comparing/Sources/Compare/lda_gammas_final", i,".txt Testing/comparing/Sources/Compare/lda_gammas_final", (i+1),".txt corpus.txt > Testing/comparing/Sources/Res/results",i,".txt",sep="")
    p4=pipe(call,"r")
    close(p4)
    call=paste("Testing/comparing/Sources/Res/results",i,".txt",sep="")
    task=readLines(call)
    repro[i,j]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  print(c("saved repro",j))
  
}
setwd(wd2)

write.csv(accur,"results1_accur.csv")
write.csv(repro,"results1_repro.csv")
