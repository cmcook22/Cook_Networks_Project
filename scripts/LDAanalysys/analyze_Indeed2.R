library(tm)
library(topicmodels)
library(stringr)
#Functions to clean the code, these are just a first pass and can be better
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, stemDocument, language="english")
  return(corpus)
}
Swords <- c(stopwords('english'),"c")
custom.stopwords<-Swords[-which(Swords=="own")]

################################################################################
###############Indeed Results################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
wd2=paste("C:/Users/admin-ccook/Desktop/Code/Testing/comparing/Sources/")
set.seed(12345)

runs=100
K=10

setwd(wd1)
doc=read.csv("Data/Indeed/All2.csv",header=F)
doc=cbind(seq(1,length(doc)),doc)

sys=matrix(rep(c(1,0,0,0,0),10),nrow=10,ncol=5,byrow=T)
mec=matrix(rep(c(0,1,0,0,0),5),nrow=5,ncol=5,byrow=T)
ele=matrix(rep(c(0,0,1,0,0),10),nrow=10,ncol=5,byrow=T)
sof=matrix(rep(c(0,0,0,1,0),10),nrow=10,ncol=5,byrow=T)
sch=matrix(rep(c(0,0,0,0,1),10),nrow=10,ncol=5,byrow=T)
alpha=rbind(sys,mec,ele,sof,sch,c(1,0,0,0,0),c(0,1,0,0,0),
            c(0,0,1,0,0),c(0,0,0,1,0),c(0,0,0,0,1))
write.table(alpha,"Data/Indeed/alpha2.txt",row.names=F,col.names=F)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
words_C<-clean.corpus(words_C)
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  
D=dim(tfm)[1]
gamma=list()

for(i in 1:runs){
  set.seed(12345)
  fit=LDA(tfm,k=K,method="VEM",seed=12345,best=TRUE)
  gamma[[i]]=fit@gamma
}

print(c("got through lda"))

setwd(wd2)
for(i in 1:length(gamma)){
  write.table(gamma[[i]],str_c("Compare/gamma",i,".txt"),row.names=F,col.names=F)
}

write.table(alpha,"alpha.txt",row.names=F,col.names=F)
write.table(doc[,-1],"corpus.txt",row.names=F,col.names=F,quote=F)

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
write.csv(accur,"Results_lda/accur_indeed2k3.csv")
write.csv(repro,"Results_lda/repro_indeed2k3.csv")


