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
Swords <- c(stopwords('english'),"c", "i")
custom.stopwords<-Swords[-which(Swords=="own")]

weightedEdge=function(edges,topics,a=1,b=0.5,g=rep(1/topics,topics),
                      iter=10000,toler=0.0001,l,r,start=FALSE,s=NA){
  data=edges
  K=topics
  D=dim(data)[1] # num of docs
  E=choose(D,K-1) #num of potential edges in network
  weight=data[lower.tri(data)] #lower triange of data
  
  ###MCMC parameters
  T=iter # length of chain
  tol=toler
  
  ###hyper-parameters
  alpha_start=rep(a,length(r))
  beta_start=rep(b,length(r))
  gamma_start=matrix(rep(g,D),nrow=D,ncol=K) 
  gamma=matrix(NA,nrow=D,ncol=K)
  alpha=rep(NA,length(r))
  beta=rep(NA,length(r))
  
  ###storage for iterations of MCMC
  Lambda=list()
  Z=list()
  Theta=list()
  z=matrix(NA,nrow=D,ncol=K)
  theta=matrix(NA,nrow=D,ncol=K)
  lambda=rep(NA,(choose(K,K-1)+K))
  
  #Starting Values
  for (d in 1:D){  
    theta[d,]=rdirichlet(n=1,alpha=gamma_start[d,])
    z[d,]=rmultinom(n=1,size=1,prob=theta[d,])
  }
  Z[[1]]=z
  Theta[[1]]=theta
  if(start==TRUE){
    Z[[1]]=s
    Theta[[1]]=s
  } 
  
  id_k=matrix(nrow=T,ncol=D)
  id_k[1,]=apply(Theta[[1]],1,which.max)
  
  for (k in 1:length(r)){
    id1=which(id_k[1,]==l[k])
    id2=which(id_k[1,]==r[k])
    
    alpha[k]=alpha_start[k]+sum(data[id1,id2])/2
    beta[k]=beta_start[k]+length(id1)
    
    lambda[k]=rgamma(n=1,shape=alpha[k],rate=beta[k])
  }
  Lambda[[1]]=lambda
  
  #MCMC chain
  for(t in 1:(T-1)){
    for(d in 1:d){
      gamma[d,]=gamma_start[d,]+table(factor(id_k[,d],levels=c(1:K)))
      theta[d,]=rdirichlet(n=1,alpha=gamma[d,])
      z[d,]=rmultinom(n=1,size=1,prob=Theta[[t]][d,])
    }
    Z[[t+1]]=z
    Theta[[t+1]]=theta
    id_k[(t+1),]=apply(Theta[[t+1]],1,which.max)
    
    for (k in 1:length(r)){
      id1=which(id_k[(t+1),]==l[k])
      id2=which(id_k[(t+1),]==r[k])
      
      alpha[k]=alpha[k]+sum(data[id1,id2])
      beta[k]=beta[k]+length(id1)
      
      lambda[k]=rgamma(n=1,shape=alpha[k],rate=beta[k])
    }
    Lambda[[t+1]]=lambda
  }
  hold=sum(abs(Theta[[T-1]]-Theta[[T]]))<(tol*K*D)
  results=list()
  results$Z=Z[[T]]
  results$Theta=Theta[[T]]
  results$converge=hold
  return(results)
}