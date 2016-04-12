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
###############Language Results################################################
################################################################################
wd1=paste("//Users//cynthiacook//Desktop//Project//")
wd2=paste("//Users//cynthiacook//Desktop//Code//")
set.seed(12345)
library(tm)
library(topicmodels)
library(stringr)

runs=100
K=24

setwd(wd1)
doc=read.csv("corpus.csv",header=F)
doc=cbind(seq(1,length(doc)),doc)

#Read in the cleaned journal corpus
df<-data.frame(id=doc[,1],text=doc[,2])
custom.reader <- readTabular(mapping=list(content="text", id="id"))
words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
words_C<-clean.corpus(words_C)
tfm=DocumentTermMatrix(words_C,control=list(wordLengths=c(1,Inf)))
dim(tfm)  
D=dim(tfm)[1]
gamma=list()

alpha=read.table("alpha.txt",header=F)  

setwd(wd2)
  write.table(doc,"corpus.txt",row.names=F,col.names=F,quote=F)
  write.table(alpha,"alpha.txt",row.names=F,col.names=F,quote=F)
  
for(i in 1:runs){
    call=paste("bin//topicmap -f corpus.txt -t 10 -o Testing//comparing//Sources/Results//test_results",i,sep="")
    p1=pipe(call,"r")
    Sys.sleep(60)
    close(p1)
    call=paste("mv Testing/comparing/Sources/Results/test_results",i,"/lda_gammas_final.txt Testing/comparing/Sources/Compare/lda_gammas_final", i, ".txt",sep="")
    p2=pipe(call,"r")
    close(p2)
  }
  
  print(c("got through tm"))

#Compare using the cpp file and the pipe...
  
  accur=matrix(nrow=runs,ncol=2)
  for(i in 1:runs){
    call=paste("Testing/comparing/Sources/compare_models Testing/comparing/Sources/Compare/lda_gammas_final", i,".txt alpha.txt corpus.txt > Testing/comparing/Sources/Res/results",i,".txt",sep="")
    p3=pipe(call,"r")
    close(p3)
    call=paste("Testing/comparing/Sources/Res/results",i,".txt",sep="")
    task=readLines(call)
    accur[i,2]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  
  print(c("saved accur"))
  
  repro=matrix(nrow=(runs-1),ncol=2)
  for(i in 1:(runs-1)){
    call=paste("Testing/comparing/Sources/compare_models Testing/comparing/Sources/Compare/lda_gammas_final", i,".txt Testing/comparing/Sources/Compare/lda_gammas_final", (i+1),".txt corpus.txt > Testing/comparing/Sources/Res/results",i,".txt",sep="")
    p4=pipe(call,"r")
    close(p4)
    call=paste("Testing/comparing/Sources/Res/results",i,".txt",sep="")
    task=readLines(call)
    repro[i,1]=as.numeric(gsub(pattern="bm similarity: ",replacement="",task[5]))
  }
  print(c("saved repro"))

setwd(wd2)

write.csv(accur,"indeed_accur2.csv")
write.csv(repro,"indeed_repro2.csv")


