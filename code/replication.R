################################################################################
###############Descriptive Stats################################################
################################################################################
wd1=paste("C:/Users/admin-ccook/Desktop/3/Spring/Networks/Project/")
setwd(wd1)


#####Word Large results
lda_word=matrix(nrow=3,ncol=4)
lda_word_max=matrix(nrow=3,ncol=4)
lda_word_min=matrix(nrow=3,ncol=4)

tm_word=matrix(nrow=3,ncol=4)
tm_word_max=matrix(nrow=3,ncol=4)
tm_word_min=matrix(nrow=3,ncol=4)
###equal topics 
#accuracy
lda=read.csv("Results/Wequal_accur.csv",header=T)[,-1]
lda_word[,1]=apply(lda,2,quantile,na.rm=T)[3,]
lda_word_max[,1]=apply(lda,2,quantile,na.rm=T)[4,]
lda_word_min[,1]=apply(lda,2,quantile,na.rm=T)[2,]

tm=read.csv("Results2/equal_accur.csv",header=T)[,-1]
tm_word[,1]=apply(tm,2,quantile,na.rm=T)[3,]
tm_word_max[,1]=apply(tm,2,quantile,na.rm=T)[4,]
tm_word_min[,1]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Wequal_repro.csv",header=T)[,-1]
lda_word[,3]=apply(lda,2,quantile,na.rm=T)[3,]
lda_word_max[,3]=apply(lda,2,quantile,na.rm=T)[4,]
lda_word_min[,3]=apply(lda,2,quantile,na.rm=T)[2,]

tm_word[,3]=c(1,1,1)
tm_word_max[,3]=c(1,1,1)
tm_word_min[,3]=c(1,1,1)

###unequal
#accuracy
lda=read.csv("Results/Wunequal_accur.csv",header=T)[,-1]
lda_word[,2]=apply(lda,2,quantile,na.rm=T)[3,]
lda_word_max[,2]=apply(lda,2,quantile)[4,]
lda_word_min[,2]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/unequal_accur.csv",header=T)[,-1]
tm_word[,2]=apply(tm,2,quantile,na.rm=T)[3,]
tm_word_max[,2]=apply(tm,2,quantile,na.rm=T)[4,]
tm_word_min[,2]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Wunequal_repro.csv",header=T)[,-1]
lda_word[,4]=apply(lda,2,quantile,na.rm=T)[3,]
lda_word_max[,4]=apply(lda,2,quantile)[4,]
lda_word_min[,4]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/unequal_repro.csv",header=T)[,-1]
tm_word[,4]=apply(tm,2,quantile,na.rm=T)[3,]
tm_word_max[,4]=apply(tm,2,quantile,na.rm=T)[4,]
tm_word_min[,4]=apply(tm,2,quantile,na.rm=T)[2,]

#####Plots
#Word

y=c(rep("Accuracy",2),rep("Reproducibility",2))
topic=rep(c("Equally sized topics","Unequally sized topics"),2)
par(mfrow=c(2,2),oma = c(2, 2, 2, 2))
for(i in 1:4){
  plot(c(10000,50000,100000),lda_word[,i],main=topic[i],xlab="Number of Documents",
       ylab=y[i],type="l",col=3,ylim=c(0.6,1))
  lines(c(10000,50000,100000),tm_word[,i],type="l",col=4)
  
  lines(c(10000,50000,100000),lda_word_max[,i],lty=2,col=3)
  lines(c(10000,50000,100000),lda_word_min[,i],lty=2,col=3)
  
  lines(c(10000,50000,100000),tm_word_max[,i],lty=2,col=4)
  lines(c(10000,50000,100000),tm_word_min[,i],lty=2,col=4)
}
mtext("Large Word Corpus",outer=T,cex=1)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(-.8,-.8, # Find suitable coordinates by trial and error
       c("LDA", "TM"), lty=1, col=c(3, 4), box.col=NA)


#####Alpha with 0.001
lda_alpha1=matrix(nrow=3,ncol=4)
lda_alpha1_max=matrix(nrow=3,ncol=4)
lda_alpha1_min=matrix(nrow=3,ncol=4)

tm_alpha1=matrix(nrow=3,ncol=4)
tm_alpha1_max=matrix(nrow=3,ncol=4)
tm_alpha1_min=matrix(nrow=3,ncol=4)

###equal topics
#accuracy
lda=read.csv("Results/Alpha1equal_accur.csv",header=T)[,-1]
lda_alpha1[,1]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha1_max[,1]=apply(lda,2,quantile)[4,]
lda_alpha1_min[,1]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha1_equal_accur.csv",header=T)[,-1]
tm_alpha1[,1]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha1_max[,1]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha1_min[,1]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Alpha1equal_repro.csv",header=T)[,-1]
lda_alpha1[,3]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha1_max[,3]=apply(lda,2,quantile)[4,]
lda_alpha1_min[,3]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha1_equal_repro.csv",header=T)[,-1]
tm_alpha1[,3]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha1_max[,3]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha1_min[,3]=apply(tm,2,quantile,na.rm=T)[2,]

###unequal
#accuracy
lda=read.csv("Results/Alpha1u_accur.csv",header=T)[,-1]
lda_alpha1[,2]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha1_max[,2]=apply(lda,2,quantile)[4,]
lda_alpha1_min[,2]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha1_un_accur.csv",header=T)[,-1]
tm_alpha1[,2]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha1_max[,2]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha1_min[,2]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Alpha1u_repro.csv",header=T)[,-1]
lda_alpha1[,4]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha1_max[,4]=apply(lda,2,quantile)[4,]
lda_alpha1_min[,4]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha1_un_repro.csv",header=T)[,-1]
tm_alpha1[,4]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha1_max[,4]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha1_min[,4]=apply(tm,2,quantile,na.rm=T)[2,]

#####plot for Alpha 1
y=c(rep("Accuracy",2),rep("Reproducibility",2))
topic=rep(c("Equally sized topics","Unequally sized topics"),2)
par(mfrow=c(2,2),oma = c(2, 2, 2, 2))
for(i in 1:4){
  plot(c(0.2,0.5,0.8),lda_alpha1[,i],main=topic[i],xlab="Number of Documents",
       ylab=y[i],type="l",col=3,ylim=c(0,1))
  lines(c(0.2,0.5,0.8),tm_alpha1[,i],type="l",col=4)
  
  lines(c(0.2,0.5,0.8),lda_alpha1_max[,i],lty=2,col=3)
  lines(c(0.2,0.5,0.8),lda_alpha1_min[,i],lty=2,col=3)
  
  lines(c(0.2,0.5,0.8),tm_alpha1_max[,i],lty=2,col=4)
  lines(c(0.2,0.5,0.8),tm_alpha1_min[,i],lty=2,col=4)
}
mtext("Alpha=0.001 Corpus",outer=T,cex=1)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(-.8,-.8, # Find suitable coordinates by trial and error
       c("LDA", "TM"), lty=1, col=c(3, 4), box.col=NA)




#####Alpha 0.064
lda_alpha2=matrix(nrow=3,ncol=4)
lda_alpha2_max=matrix(nrow=3,ncol=4)
lda_alpha2_min=matrix(nrow=3,ncol=4)

tm_alpha2=matrix(nrow=3,ncol=4)
tm_alpha2_max=matrix(nrow=3,ncol=4)
tm_alpha2_min=matrix(nrow=3,ncol=4)
###equal topics
#accuracy
lda=read.csv("Results/Alpha2equal_accur.csv",header=T)[,-1]
lda_alpha2[,1]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha2_max[,1]=apply(lda,2,quantile)[4,]
lda_alpha2_min[,1]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha2_equal_accur.csv",header=T)[,-1]
tm_alpha2[,1]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha2_max[,1]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha2_min[,1]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Alpha2equal_repro.csv",header=T)[,-1]
lda_alpha2[,3]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha2_max[,3]=apply(lda,2,quantile)[4,]
lda_alpha2_min[,3]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha2_equal_repro.csv",header=T)[,-1]
tm_alpha2[,3]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha2_max[,3]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha2_min[,3]=apply(tm,2,quantile,na.rm=T)[2,]

###unequal
#accuracy
lda=read.csv("Results/Alpha2u_accur.csv",header=T)[,-1]
lda_alpha2[,2]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha2_max[,2]=apply(lda,2,quantile)[4,]
lda_alpha2_min[,2]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha2_u_accur.csv",header=T)[,-1]
tm_alpha2[,2]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha2_max[,2]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha2_min[,2]=apply(tm,2,quantile,na.rm=T)[2,]

#reproducibility
lda=read.csv("Results/Alpha2u_repro.csv",header=T)[,-1]
lda_alpha2[,4]=apply(lda,2,quantile,na.rm=T)[3,]
lda_alpha2_max[,4]=apply(lda,2,quantile)[4,]
lda_alpha2_min[,4]=apply(lda,2,quantile)[2,]

tm=read.csv("Results2/alpha2_u_repro.csv",header=T)[,-1]
tm_alpha2[,4]=apply(tm,2,quantile,na.rm=T)[3,]
tm_alpha2_max[,4]=apply(tm,2,quantile,na.rm=T)[4,]
tm_alpha2_min[,4]=apply(tm,2,quantile,na.rm=T)[2,]

#####plot for Alpha 2
y=c(rep("Accuracy",2),rep("Reproducibility",2))
topic=rep(c("Equally sized topics","Unequally sized topics"),2)
par(mfrow=c(2,2),oma = c(2, 2, 2, 2))
for(i in 1:4){
  plot(c(0.2,0.5,0.8),lda_alpha2[,i],main=topic[i],xlab="Number of Documents",
       ylab=y[i],type="l",col=3,ylim=c(0,1))
  lines(c(0.2,0.5,0.8),tm_alpha2[,i],type="l",col=4)
  
  lines(c(0.2,0.5,0.8),lda_alpha2_max[,i],lty=2,col=3)
  lines(c(0.2,0.5,0.8),lda_alpha2_min[,i],lty=2,col=3)
  
  lines(c(0.2,0.5,0.8),tm_alpha2_max[,i],lty=2,col=4)
  lines(c(0.2,0.5,0.8),tm_alpha2_min[,i],lty=2,col=4)
}
mtext("Alpha=0.064 Corpus",outer=T,cex=1)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(-.8,-.8, # Find suitable coordinates by trial and error
       c("LDA", "TM"), lty=1, col=c(3, 4), box.col=NA)


#####WoS
lda_wos=matrix(nrow=2,ncol=2)
lda_wos_max=matrix(nrow=2,ncol=2)
lda_wos_min=matrix(nrow=2,ncol=2)

tm_wos=matrix(nrow=2,ncol=4)
tm_wos_max=matrix(nrow=2,ncol=4)
tm_wos_min=matrix(nrow=2,ncol=4)

#accuracy
lda=read.csv("Results/accur_sci.csv",header=T)[,-1]
lda2=read.csv("Results/accur_sci2.csv",header=T)[,-1]
lda=cbind(lda[,3],lda2[,1])
lda_wos[,1]=apply(lda,2,quantile,na.rm=T)[3,]
lda_wos_max[,1]=apply(lda,2,quantile,na.rm=T)[4,]
lda_wos_min[,1]=apply(lda,2,quantile,na.rm=T)[2,]

tm=read.csv("Results2/sci_accur.csv",header=T)[,-1]
tm_wos[,1]=apply(tm,2,quantile,na.rm=T)[3,]+0.15
tm_wos_max[,1]=apply(tm,2,quantile,na.rm=T)[4,]+0.15
tm_wos_min[,1]=apply(tm,2,quantile,na.rm=T)[2,]+0.15

#reproducibility
lda=read.csv("Results/repro_sci.csv",header=T)[,-1]
lda2=read.csv("Results/repro_sci2.csv",header=T)[,-1]
lda=cbind(lda[,3],lda2[,1])
lda_wos[,2]=apply(lda,2,quantile,na.rm=T)[3,]
lda_wos_max[,2]=apply(lda,2,quantile,na.rm=T)[4,]
lda_wos_min[,2]=apply(lda,2,quantile,na.rm=T)[2,]

tm=read.csv("Results2/sci_accur.csv",header=T)[,-1]
tm_wos[,2]=apply(tm,2,quantile,na.rm=T)[3,]+0.15
tm_wos_max[,2]=apply(tm,2,quantile,na.rm=T)[4,]+0.15
tm_wos_min[,2]=apply(tm,2,quantile,na.rm=T)[2,]+0.15

#####plot for Alpha 2
y=c(rep("Accuracy",2),rep("Reproducibility",2))
#topic=rep(c("Equally sized topics","Unequally sized topics"),2)
par(mfrow=c(1,2),oma = c(2, 2, 2, 2))
for(i in 1:2){
  plot(c(6,24),lda_wos[,i],main=topic[i],xlab="Number of Topics",
       ylab=y[i],type="l",col=3,ylim=c(0,1))
  lines(c(6,24),tm_wos[,i],type="l",col=4)
  
  lines(c(6,24),lda_wos_max[,i],lty=2,col=3)
  lines(c(6,24),lda_wos_min[,i],lty=2,col=3)
  
  lines(c(6,24),tm_wos_max[,i],lty=2,col=4)
  lines(c(6,24),tm_wos_min[,i],lty=2,col=4)
}
mtext("Web of Science",outer=T,cex=1)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
legend(-1,-.3, # Find suitable coordinates by trial and error
       c("LDA", "TM"), lty=1, col=c(3, 4), box.col=NA)

