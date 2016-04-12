################################################################################
###############Language Results################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
wd2=paste("C:/Users/admin-ccook/Desktop/Code/Testing/comparing/Sources/")
set.seed(12345)
library(tm)
library(topicmodels)
library(stringr)
library(lda)

runs=100
K=24

setwd(wd1)
doc=read.table("Data/WoS/WoSmall.txt",header=F)
doc=cbind(seq(1,length(doc)),doc)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  
D=dim(tfm)[1]
gamma=list()

alpha=read.table("Data/WoS/alpha.txt",header=F)  

#turn doc term matrix into the correct form for lda pkg
####Preprocess Data
documents=dtm2ldaformat(tfm, omit_empty = TRUE)
vocab=Terms(tfm)

for(i in 1:runs){
  set.seed(12345)
  fit2=lda.collapsed.gibbs.sampler(documents = documents[[1]], K = K, 
        vocab = vocab, num.iterations = 100, alpha = 0.4,
        eta = 0.4, initial = NULL, burnin = 5000, 
        compute.log.likelihood = FALSE)
  
  gamma[[i]]=t(fit2$document_sums)/(apply(fit2$document_sums,2,sum))
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
  accur[i,1]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
}

print(c("saved accur"))

repro=matrix(nrow=(runs-1),ncol=2)
for(i in 1:(runs-1)){
  call=paste("compare_models Compare/gamma",i,".txt Compare/gamma",(i+1),".txt corpus.txt > Results/results",i,".txt",sep="")
  p1=pipe(call,"r")
  close(p1)
  call=paste("Results/results",i,".txt",sep="")
  task=readLines(call)
  repro[i,1]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
}
print(c("saved repro"))

setwd(wd1)
write.csv(accur,"Results_lda/accur_sci_CGS3.csv")
write.csv(repro,"Results_lda/repro_sci_CGS3.csv")


