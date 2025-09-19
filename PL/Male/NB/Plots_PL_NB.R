#Setup do run and store pictures as pdf 
library(dplyr)
index <- first(which(sigmamcmc==0))-1
## Get M 
M=SS*index

## Chop down data 
lambdamcmc <-lambdamcmc[,,1:index]
betamcmc <- betamcmc[,1:index]
sigmamcmc <- sigmamcmc[1:index]
thetamcmc <- thetamcmc[1:index]


PLOTDIR <- getwd()

save_last_plot_pdf <- function(name,
                               dir = PLOTDIR,
                               width_in = 7, height_in = 5) {
  pdf_file <- file.path(dir, paste0(name, ".pdf"))
  grDevices::dev.copy2pdf(file = pdf_file, width = width_in, height = height_in)
  message("Gespeichert: ", pdf_file)
}


## Likelihood function
logpp <- function(dp,lambda,beta,B){ 
  o <- dp$o
  n <- length(o)
  t <- dp$time
  r <- dp$rank
  f <- lambda[o,t-B+1] +beta[r]
  ff <- numeric(n)
  for(j in 1:n){
    ff[j]<- f[j]- log(sum(exp(f[j:n]))) 
  }
  return(sum(ff))
}

PL_log_lik <- function(D,lambda,beta,B){
  
  logp <- sapply(D,function(x) logpp(dp=x,lambda=lambda,beta=beta,B=B))
  return(sum(logp)) #take the sum b.c. we look at the log likelihood!
}



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
  l[i]<- PL_log_lik(D=D,lambda=lambdamcmc[,,i],beta=rep(0,nd),B=B)
}
plot(l,type='l',xlab="MCMC-sample",ylab = "log-lkd")
save_last_plot_pdf("m_log-likelihood")
#plot(l[-(1:1000)],type='l',ylab="log-lik")

#######sigma
plot(sigmamcmc,type='l',xlab = "MCMC-sample",ylab = expression(sigma) )
save_last_plot_pdf("m_sigma_traceplot")
#acf(sigmamcmc,lag.max=5000)
#effectiveSize(sigmamcmc)

plot(sigmamcmc,type='l',xlab = "MCMC-sample",ylab = expression(sigma) )
acf(sigmamcmc,lag.max=5000, main="ACF of sigma")
save_last_plot_pdf("m_sigma_acf_traceplot")


############beta
############beta
plot(betamcmc[5,-c(1:1000)],type='l', xlab = "MCMC-sample",ylab = expression(beta),ylim=c(-3,3)) #5 b.c. we just pick a random one? 
save_last_plot_pdf("m_beta_traceplot")
#All levels seems to show convergence. But it  doesnt really fit what we have for male 
apply(betamcmc,1,lines)
#acf(betamcmc[5,-c(1:1000)],lag.max = 5000)
#effectiveSize(betamcmc[5,])


plot(betamcmc[5,-c(1:1000)],type='l', xlab = "MCMC-sample",ylab = expression(beta),ylim=c(-3,3)) #5 b.c. we just pick a random one? 
acf(betamcmc[5,-c(1:1000)],lag.max = 5000,main="ACF of beta")
save_last_plot_pdf("m_beta_acf_traceplot")




###########theta
plot(thetamcmc,type='l',xlab="MCMC-sample",ylab = expression(theta))
save_last_plot_pdf("m_theta_traceplot")
#acf(thetamcmc,lag.max = 5000)

plot(thetamcmc,type='l',xlab="MCMC-sample",ylab = expression(theta))
acf(thetamcmc,lag.max = 5000,main="ACF of theta")
save_last_plot_pdf("m_theta_acf_traceplot")


## lambda
plot(lambdamcmc[10,10,-c(1:1000)],type='l')
save_last_plot_pdf("m_lambda_traceplot")
#acf(lambdamcmc[10,10,-c(1:1000)],lag.max = 8000)
#effectiveSize(lambdamcmc[10,10,])

plot(lambdamcmc[10,10,-c(1:1000)],type='l')
acf(lambdamcmc[10,10,-c(1:1000)],lag.max = 8000,main="ACF of lambda")
save_last_plot_pdf("m_lambda_acf_traceplot")



########## checking the monotonicity of the betas
C <- matrix(0,(M/SS)*nd,2)
for(i in 1:nd){
  C[((i-1)*(M/SS)+1):(i*(M/SS)),1]<- betamcmc[i,]
  C[((i-1)*(M/SS)+1):(i*(M/SS)),2]<- i
}

boxplot(C[,1]~C[,2],ylab="values",xlab="ranks")
abline(h=0,col='red',lty="dashed")
save_last_plot_pdf("m_beta_boxplot")
###################################################



