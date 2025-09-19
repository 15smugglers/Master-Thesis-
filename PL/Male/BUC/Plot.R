## Get index until which sampeled
tail(which(sigmamcmc>0)) #get the last value the model still fits
index <- 1810 #find a smarter whay to get that 


### loading the mcmc outputs
lambdamcmc <- lambdamcmc[,,1:index]
betamcmc <- betamcmc[,1:index]
sigmamcmc <- sigmamcmc[1:index]
thetamcmc <- thetamcmc[1:index]

M <- index/SS

############### the bayes estimates (posterior mean)#################3
n<- dim(lambdamcmc)[1]
tt <- dim(lambdamcmc)[2]
lambdahat <- matrix(0,n,tt)
for(i in 1:n){
  lambdahat[i,]<- rowSums(lambdamcmc[i,,])/(M/SS)
}
betahat <- rowSums(betamcmc)/(M/SS)
thetahat <- mean(thetamcmc)
sigmahat <- mean(sigmamcmc)
##########################################################

################## check for convergence
###### log lik
l <- numeric(M/SS)
for(i in 1:(M/SS)){
  l[i]<- PL_log_lik(D=D,lambda=lambdamcmc[,,i],beta=betamcmc[,i],B=B)
}
plot(l,type='l')
plot(l[-(1:1000)],type='l',ylab="log-lik")

#######sigma
plot(sigmamcmc,type='l')
acf(sigmamcmc,lag.max=5000)
effectiveSize(sigmamcmc)


############beta
plot(betamcmc[24,-c(1:1000)],type='l',ylim=c(-3,3)) #5 b.c. we just pick a random one? 
#All levels seems to show convergence. But it doesnt really fit what we have for male 
apply(betamcmc,1,lines)
acf(betamcmc[5,-c(1:1000)],lag.max = 5000)
effectiveSize(betamcmc[5,])
###########theta
plot(thetamcmc,type='l')
acf(thetamcmc,lag.max = 5000)
effectiveSize(thetamcmc)

## lambda
plot(lambdamcmc[10,10,-c(1:1000)],type='l')
acf(lambdamcmc[10,10,-c(1:1000)],lag.max = 8000)
effectiveSize(lambdamcmc[10,10,])

M <- index*SS

########## checking the monotonicity of the betas
C <- matrix(0,(M/SS)*nd,2)
for(i in 1:nd){
  C[((i-1)*(M/SS)+1):(i*(M/SS)),1]<- betamcmc[i,]
  C[((i-1)*(M/SS)+1):(i*(M/SS)),2]<- i
}

boxplot(C[,1]~C[,2],ylab="values",xlab="ranks")
abline(h=0,col='red',lty="dashed")
###################################################
