################################################################################
###############Language Results################################################
################################################################################
#Functions needed later
cosine=function(x,y){
  return((x %*% y) / sqrt(x%*%x * y%*%y))
}

setwd("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")

#3 small and unique languages
words=read.csv("Data/Language_Small/language.csv",header=T)
D=1000
K=3
#percent=c(0.5,0.56,0.6,0.66,0.7,0.76,0.8,0.86,0.9,0.96)
percent=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.98)
doc=matrix("NA",nrow=D,ncol=length(percent))
set.seed(12345)
alpha=list()

#Create data
for(j in 1:length(percent)){
  alpha[[j]]=matrix(NA,nrow=D,ncol=K)
  for(i in 1:(D*percent[j])){
    doc[i,j]=paste0(as.character(sample(words[,1],10,replace=TRUE)),collapse=" ")
    alpha[[j]][i,]=c(1,0,0)
  }
  for(i in 1:((D-D*percent[j])/2)){
    doc[i+(D*percent[j]),j]=paste0(as.character(sample(words[,2],10,replace=TRUE))
                                   ,collapse=" ")
    alpha[[j]][i+(D*percent[j]),]=c(0,1,0)
  }
  for(i in 1:((D-D*percent[j])/2)){
    doc[i+(D*percent[j])+((D-D*percent[j])/2),j]=paste0(as.character(sample(words[,3],
                                                    10,replace=TRUE)),collapse=" ")
    alpha[[j]][i+(D*percent[j])+((D-D*percent[j])/2),]=c(0,0,1)
  }
}
doc=cbind(seq(1:D),doc)

#Run symmetric lda and save likelihood 
library(tm)
library(topicmodels)

runs=100
logLik=matrix(nrow=runs,ncol=length(percent))
gamma=list()

comparisons=matrix(nrow=runs,ncol=length(percent))
comparisons2=matrix(nrow=runs,ncol=length(percent))
hold=rep(NA,D)
hold2=rep(NA,D)

for(i in 1:runs){
  for(j in 1:length(percent)){
    alpha2=alpha
    e=which(alpha[[j]][,1]==1)
    s=which(alpha[[j]][,2]==1)
    f=which(alpha[[j]][,3]==1)
    set.seed(12345)
    
    df<-data.frame(id=doc[,1],text=doc[,j+1])
    custom.reader <- readTabular(mapping=list(content="text", id="id"))
    words_C<- VCorpus(DataframeSource(df), readerControl=list(reader=custom.reader))
    tfm=DocumentTermMatrix(words_C)
  
    fit=LDA(tfm,k=K,method="VEM",seed=12345,best=TRUE)
    gamma[[j]]=fit@gamma
    logLik[i,j]=logLik(fit)
    
    max=as.numeric(names(which.max(table(apply(fit@gamma[1:length(e),],1,which.max)))))
    if(max==1){
      max2=as.numeric(names(which.max(table(apply(fit@gamma[length(e):(length(e)+length(s)),],1,
                                                  which.max)))))
      if(max2==3){
        alpha2[[j]][s,]=c(rep(0,length(s)),rep(0,length(s)),rep(1,length(s)))
        alpha2[[j]][f,]=c(rep(0,length(f)),rep(1,length(f)),rep(0,length(f)))
      }
    }
    if(max==2){
      alpha2[[j]][e,]=c(rep(0,length(e)),rep(1,length(e)),rep(0,length(e)))
      max2=as.numeric(names(which.max(table(apply(fit@gamma[length(e):(length(e)+length(s)),],1,
                                 which.max)))))
      if(max2==1){
        alpha2[[j]][s,]=c(rep(1,length(s)),rep(0,length(s)),rep(0,length(s)))
        alpha2[[j]][f,]=c(rep(0,length(f)),rep(0,length(f)),rep(1,length(f)))
      }
      if(max2==3){
        alpha2[[j]][s,]=c(rep(0,length(s)),rep(0,length(s)),rep(1,length(s)))
        alpha2[[j]][f,]=c(rep(1,length(f)),rep(0,length(f)),rep(0,length(f)))
      }
    }
    if(max==3){
      alpha2[[j]][e,]=c(rep(0,length(e)),rep(0,length(e)),rep(1,length(e)))
      max2=as.numeric(names(which.max(table(apply(fit@gamma[length(e):(length(e)+length(s)),],1,
                                 which.max)))))
      if(max2==1){
        alpha2[[j]][s,]=c(rep(1,length(s)),rep(0,length(s)),rep(0,length(s)))
        alpha2[[j]][f,]=c(rep(0,length(f)),rep(1,length(f)),rep(0,length(f)))
      }
      if(max2==2){
        alpha2[[j]][s,]=c(rep(0,length(s)),rep(1,length(s)),rep(0,length(s)))
        alpha2[[j]][f,]=c(rep(1,length(f)),rep(0,length(f)),rep(0,length(f)))
      }
    }
    
    for(k in 1:D){
      hold[k]=cosine(alpha2[[j]][k,],gamma[[j]][k,])
      hold2[k]=sum(abs(alpha2[[j]][k,]-gamma[[j]][k,]))
    }
    comparisons[i,j]=mean(hold)
    comparisons2[i,j]=mean(hold2)
  }
}


apply(comparisons,2,summary)
apply(logLik,2,mean)
plot(apply(comparisons,2,summary)[4,]~percent,type="l")
lines(apply(comparisons,2,summary)[2,]~percent,type="l")
lines(apply(comparisons,2,summary)[5,]~percent,type="l")
par(mfrow=c(1,6))
for (i in 1:6){
  boxplot(comparisons[i,])
}

#####plot for WoS
y=rep(NA,length(percent))
for(i in 1:length(percent)){
  y[i]=length(which(comparisons[,i]>0.8))/runs
} 

plot(percent,y,xlab="percent",
     ylab="Success Rate",type="l",col=1,ylim=c(0,1))


