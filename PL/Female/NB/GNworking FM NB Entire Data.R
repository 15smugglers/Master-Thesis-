#######################################################################################
library(coda);library(mvtnorm); library(data.table)
##############get data list###################################
# Setup the note 
set.seed(123)
note=list()

#data setup
note$B=1970                  
note$E=2010                  
if (note$B>note$E) stop('B before E')
note$doi="Gombe"
note$gender='female'            #'male', 'female
note$selectlists='any'  
note$RUNDIR="C:/Users/fimaass/Downloads/PL/Entire Data/NB/Female"
note$DOWRITE=TRUE            #set to false if you dont want to save along the way - sensible for short runs only.
note$WRITE.SUBSAMPLE=100     #write the state to a file less frequently as this takes time for long runs
note$NEWRUN=TRUE             #set to FALSE if resarting old run
note$savefile='example-FT.RData'  #write MCMC samples to savefile
note$DOBETA = FALSE #turn into TRUE if you want to include the effect due to beta 


setwd(note$RUNDIR)

source("make_data.R") #Here we also define the restrict functoin and then 
source("get_ranks.R")


get_data_list_2 <- function(cla,cil,V,B=note$B,E=note$E){
  D <- list()
  N <- length(cla) # number of acts
  for(i in 1:N){
    t <- round((cla[[i]]$tl+cla[[i]]$tu)/2) # time of act
    n <- length(cla[[i]]$o) # no. of bishops in the act (for chimp data always 2)
    if(t>=B & n>1 & t<=E){ # exclude those out of time range; we already have it before by using the restrict function
      
      r <- numeric(n) # input the corresponding ranks
      for(j in 1:n){
        a <- (cla[[i]]$o)[j]
        dio <- cil[[a]]$diocese
        r[j] <- rank[a,t-note$B+1]
      }
      
      d <- list(o = cla[[i]]$o,time=round((cla[[i]]$tl+cla[[i]]$tu)/2),rank = r)
      D[[i]] <- d
    }
  }
  return(D)
}


# setting the starting and ending years
B <- note$B;E <- note$E
#####
D <- get_data_list_2(cla=cla,cil=cil,V=V,B=B,E=E) #we dont use the V part so might as well just leave it out. 
D <- D[lengths(D)!=0] # get rid of null lists

############################################################
table(unlist(lapply(D,function(x) x$rank))) # sum of no. of bishops for each rank

# might want to get rid of the ranks with few data??
# <- D[-which(sapply(D,function(x) is.element(15,x$rank)|is.element(16,x$rank)|is.element(14,x$rank)))]

nd <- max(sapply(D,function(x) max(x$rank))) # number of diocese involved; note this now the maximal number of chimps
n <- max(sapply(D,function(x) max(x$o))) # no. of bishops involved; this is now also the number of chimps here; need for denominator

############### log lik function ###
logpp <- function(dp,lambda,beta,B){ 
  ##the log lik contribution of a single list
  o <- dp$o
  n <- length(o)
  t <- dp$time
  r <- dp$rank
  f <- lambda[o,t-B+1] +beta[r]
  ff <- numeric(n)
  for(j in 1:n){
    ff[j]<- f[j]- log(sum(exp(f[j:n]))) #likelihood for one chimp
  }
  return(sum(ff))
}

PL_log_lik <- function(D,lambda,beta,B){
  
  logp <- sapply(D,function(x) logpp(dp=x,lambda=lambda,beta=beta,B=B))
  return(sum(logp)) #take the sum b.c. we look at the log likelihood!
}
############################
# the random walk update functions (giving new states, log lik and the acceptance prob)

theta_log_alpha <- function(oldtheta,oldlambda,oldsigma,tt,N,wtheta){
  ########### tt--time length; N -- A%*%t(A), where A is the centering projection matrix 
  # wtheta- parameter of the uniform proposal
  
  #here we are using a U[0,1] prior
  n <- dim(oldlambda)[1]
  newtheta <- runif(1,min=oldtheta-wtheta,max=oldtheta+wtheta)# proposal
  if(newtheta<=0 | newtheta>=1){logalpha <- log(0)} #reject if out of range
  else{
    # the effect on prior of lambda
    oldlambdap <- oldlambda[1:(n-1),] 
    oldptheta1 <- dmvnorm(t(oldlambdap[,2:tt]-oldtheta*oldlambdap[,1:(tt-1)]),sigma=oldsigma^2*N,log=T) # 2-tt columns
    oldptheta0 <- dmvnorm(oldlambdap[,1],sigma=oldsigma^2/(1-oldtheta^2)*N,log=T) #the first column
    oldptheta <- sum(oldptheta1)+oldptheta0
    
    newptheta1 <- dmvnorm(t(oldlambdap[,2:tt]-newtheta*oldlambdap[,1:(tt-1)]),sigma=oldsigma^2*N,log=T)
    newptheta0 <- dmvnorm(oldlambdap[,1],sigma=oldsigma^2/(1-newtheta^2)*N,log=T)
    newptheta <- sum(newptheta1)+newptheta0
    logalpha <- newptheta-oldptheta
  }
  
  return(list(logalpha=logalpha,newtheta=newtheta))
}

beta_log_alpha <- function(oldbeta,D,i,oldlambda,sigmabeta,oldll,B=B,Np){
  # i indicates which beta is being proposed; sigmabeta-- sd of the normal proposal
  # here we are using a standard multivariate normal prior
  # we also put the constraint that the betas are monotonic decreasing
  nd <- length(oldbeta)
  newbeta <- oldbeta
  newbeta[i]<- rnorm(1,mean=oldbeta[i],sd=sigmabeta)
  newbeta <- newbeta-mean(newbeta)
  oldbetap <- oldbeta[-nd] #if we case the code like this don't we then lawys accecpt the update for the last beta?
  newbetap <- newbeta[-nd]
  if(sum(diff(newbeta)<=0) <(nd-1)){ # reject if the proposed betas are not monotonic decreasing 
    newll <- oldll
    logalpha <- log(0) #translates to new state always rejected 
  }
  else{
  newll <- PL_log_lik(D=D,lambda=oldlambda,beta=newbeta,B=B) # Is that b.c. the likelihood is invariant to shifts in beta and then we dont have to calulcate a new one? 
  #  newll <- oldll #are we really updating the likelihood here? We dont change the likleihood 
  logalpha <- newll-oldll+ dmvnorm(newbetap,sigma=Np,log=T)-dmvnorm(oldbetap,sigma=Np,log=T)
  }
  return(list(newll=newll,logalpha=logalpha,newbeta=newbeta))
}


sigma_log_alpha <- function(oldsigma,wsigma,oldlambda,oldtheta,a,b,tt,N){
  # a is the rate of gamma prior, b is the shape
  # tt-- time length; N-- A%*%t(A), where A is the centering projection matrix
  # wsigma-- parameter of the uniform proposal
  newsigma <- runif(1,min=oldsigma-wsigma,max=oldsigma+wsigma)
  if(newsigma <=0){logalpha <- log(0)} # reject if sigma is less than zero
  else{
    ##note no change in likelihood
    n <- dim(oldlambda)[1]
    ### effect on prior of sigma
    logalpha <- dgamma(newsigma,rate=a,shape=b,log=T)-dgamma(oldsigma,rate=a,shape=b,log=T) ### using gamma(a,b) prior
    ### effect on prior of lambda
    oldlambdap <- oldlambda[1:(n-1),]
    oldp0 <- dmvnorm(oldlambdap[,1],sigma=oldsigma^2/(1-oldtheta^2)*N,log=T)   # the first column
    oldp1 <- dmvnorm(t(oldlambdap[,2:tt]-oldlambdap[,1:(tt-1)]),sigma=oldsigma^2*N,log=T) # 2--tt columns
    oldp <- oldp0+sum(oldp1)
    
    newp0 <- dmvnorm(oldlambdap[,1],sigma=newsigma^2/(1-oldtheta^2)*N,log=T)
    newp1 <- dmvnorm(t(oldlambdap[,2:tt]-oldlambdap[,1:(tt-1)]),sigma=newsigma^2*N,log=T)
    newp <- newp0+sum(newp1)
    
    
    logalpha <- logalpha +newp-oldp
  }
  return(list(newsigma=newsigma,logalpha=logalpha))
}

lambda_log_alpha <- function(oldll,oldlambda,oldtheta,oldsigma,D,oldbeta,sigmalambda,B,i,t,tt,N){
  ## B-- startin year; i,t-- indicate the entry being proposed;
  ##tt-- time length; N--A%*%t(A), where M is the centering projection matrix
  n <- dim(oldlambda)[1]
  oldlambdap <- oldlambda[1:(n-1),]
  ##propose under the constraint that they are mean 0 for each time t (column)
  newlambda <- oldlambda
  newlambda[i,t] <- rnorm(1,oldlambda[i,t],sigmalambda)
  newlambda[,t] <- newlambda[,t]-mean(newlambda) ### center the lambdas
  ################ note the proposal is symmetric
  newlambdap <- newlambda[1:(n-1),]
  ###### update effect on log lik
  ind <- which(sapply(D,function(x) x$time == t+B-1)) # which components in the log lik are affected
 # newll <- oldll
  # Check the change below! 
  if(length(ind)>0){
  Dp <- D[ind]
  newll <- oldll - PL_log_lik(D=Dp,lambda=oldlambda,beta=oldbeta,B=B) + PL_log_lik(D=Dp,lambda=newlambda,beta=oldbeta,B=B)
  } else{
    newll <- oldll
  }
  ########## effect on prior
  logalpha <- newll-oldll
  if(t==1){
    logalpha <- logalpha+dmvnorm(newlambdap[,t],sigma=oldsigma^2/(1-oldtheta^2)*N,log=T)-dmvnorm(oldlambdap[,t],sigma=oldsigma^2/(1-oldtheta^2)*N,log=T)+dmvnorm(newlambdap[,(t+1)]-newlambdap[,t],sigma=oldsigma^2*N,log=T)-dmvnorm(oldlambdap[,(t+1)]-oldlambdap[,t],sigma=oldsigma^2*N,log=T)
  }
  else{
    if(t==tt){
      logalpha <- logalpha+dmvnorm(newlambdap[,t]-newlambdap[,t-1],sigma=oldsigma^2*N,log=T)-dmvnorm(oldlambdap[,t]-oldlambdap[,t-1],sigma=oldsigma^2*N,log=T)
    }
    if(t!=tt){
      logalpha <- logalpha +dmvnorm(newlambdap[,t]-newlambdap[,t-1],sigma=oldsigma^2*N,log=T)-dmvnorm(oldlambdap[,t]-oldlambdap[,t-1],sigma=oldsigma^2*N,log=T) + dmvnorm(newlambdap[,t+1]-newlambdap[,t],sigma=oldsigma^2*N,log=T)-dmvnorm(oldlambdap[,t+1]-oldlambdap[,t],sigma=oldsigma^2*N,log=T)
    }
  }
  return(list(newlambda=newlambda,newll=newll,logalpha=logalpha))
}



###################### the main MCMC function##################
PL_MCMC <- function(D,M,wsigma,wtheta,sigmabeta,sigmalambda,B=1115,E=1145,SS,d,a,b,theta0,sigma0,beta0,lambda0){
  #######parameter explanations#######
  # D--list of dataset; M--number of MCMC steps; wsigma--parameter for the sigma proposal
  # sigmabeta-- parameter for the beta proposal
  # sigmalambda-- parameter for the lambda proposal
  # B-- starting year; E-- ending year; SS--subsampling size; d--progress size
  # a-- rate of the sigma gamma prior; b-- shape of the sigma gamma prior
  # theta0,sigma0, beta0,lambda0 are the initial states
  ############
  tt <- E-B+1 # number of years of interest
  N <- length(D) # number of acts of interest
  n <- max(sapply(D,function(x) max(x$o))) # no. of bishops
  nd <- max(sapply(D,function(x) max(x$rank))) # max number of chimps involved
  ### constructing the centering projection matrix
  A <- diag(n)-matrix(1/n,n,n)
  A <- A[1:(n-1),]
  N <- A%*%t(A)
  C <- diag(nd)-matrix(1/nd,nd,nd)
  C <- C[1:(nd-1),]
  Np <- C%*%t(C)
  
  # building the storage for mcmc samples
  lambdamcmc <- array(dim=c(n,tt,M/SS))
  if(note$DOBETA){
    betamcmc <- matrix(0,nd,M/SS)  
  } else{
    betamcmc <- NULL
  }
  
  thetamcmc <- numeric(M/SS)
  sigmamcmc <- numeric(M/SS)
  oldllmcmc <- numeric(M/SS)
  
  # the initial states
  oldtheta <- theta0
  oldbeta <- beta0 #when note$DOBETA = FALSE then this stays at 0 vector 
  oldsigma <- sigma0
  oldlambda <- lambda0
  # the initial log lik
  oldll <- PL_log_lik(D=D,lambda=oldlambda,beta=oldbeta,B=B)
  # the movement and acceptance counters
  mv <- numeric(4); ac <- numeric(4)
  
  # the main mcmc steps
  for(m in 1:M){
    ### propose a movement in theta
    Xtheta <- theta_log_alpha(oldtheta=oldtheta,oldlambda=oldlambda,oldsigma=oldsigma,tt=tt,N=N,wtheta=wtheta)
    logalpha1 <- Xtheta$logalpha
    newtheta <- Xtheta$newtheta
    mv[1] <- mv[1]+1
    logU1 <- log(runif(1))
    if(logU1<=logalpha1){
      oldtheta <- newtheta
      ac[1]<- ac[1]+1
    }
    
    if(note$DOBETA){
      ### propose a movement in beta. will do a sweep 
      for(i in 1:nd){
        Xbeta <- beta_log_alpha(oldbeta=oldbeta,D=D,i=i,oldlambda=oldlambda,sigmabeta=sigmabeta,oldll=oldll,B=B,Np=Np)
        logalpha2 <- Xbeta$logalpha
        newbeta <- Xbeta$newbeta
        newll <- Xbeta$newll
        mv[2] <- mv[2]+1 #movement counter 
        logU2 <- log(runif(1))
        if(logU2<= logalpha2){
          oldbeta <- newbeta
          oldll <- newll #likelihood doesnt change b.c. invariant 
          ac[2]<- ac[2]+1 #movement counter 
        }
      }
    } else{
      
    }
    

    ### propose a movement in sigma
    Xsigma <- sigma_log_alpha(oldsigma=oldsigma,wsigma=wsigma,oldlambda=oldlambda,oldtheta=oldtheta,a=a,b=b,tt=tt,N=N)
    logalpha3 <- Xsigma$logalpha
    newsigma <- Xsigma$newsigma
    mv[3]<- mv[3]+1
    logU3 <- log(runif(1))
    if(logU3<= logalpha3){
      oldsigma <- newsigma
      ac[3]<- ac[3]+1
    }
    
    ### propose a movement in lambda. Here we do a sweep on the lambdas.
    for(i in 1:n){
      for(t in 1:tt){
        Xlambda <- lambda_log_alpha(oldll=oldll,oldlambda=oldlambda,oldtheta=oldtheta,oldsigma=oldsigma,D=D,oldbeta=oldbeta,sigmalambda=sigmalambda,B=B,i=i,t=t,tt=tt,N=N)
        newlambda <- Xlambda$newlambda
        logalpha4 <- Xlambda$logalpha
        newll <- Xlambda$newll
        mv[4]<- mv[4]+1
        logU4 <- log(runif(1)) 
        if(logU4<= logalpha4){
          oldlambda <- newlambda
          oldll <- newll
          ac[4]<- ac[4]+1
        }
        
      }
}
    
    
    ### subsampling and storing the mcmc samples
    #Below we always have m%%1 which i modulus 0 so the chain never gets executed 
    if(m%%SS==0){ 
      thetamcmc[m/SS]<- oldtheta
      if(note$DOBETA) betamcmc[,m/SS] <- oldbeta 
      sigmamcmc[m/SS]<- oldsigma
      lambdamcmc[,,m/SS]<- oldlambda
      oldllmcmc[m/SS] <- oldll
    }
    ### output the progress
    if(m%%d==0){
      print(paste0("progress: ",m/M*100,"%"))
    }
    if (note$DOWRITE & (m%%note$WRITE.SUBSAMPLE)==0) save(list=ls(all.names = TRUE),file=paste(note$SAVEDIR,'/',note$savefile,sep='')) 
  }
  
  
  return(list(thetamcmc=thetamcmc,betamcmc=betamcmc,sigmamcmc=sigmamcmc,lambdamcmc=lambdamcmc,oldllmcmc=oldllmcmc,ac=ac,mv=mv,acp=ac/mv))
}

###########################################setting the parameters up and do the run#########################################################
tt <- E-B+1

thetahat <- 0.8;sigmahat <- 0.6;betahat <- numeric(nd);lambdahat <- matrix(0,n,tt);
M=1000000;SS=100
if (note$NEWRUN) {
  temp.dir=paste('./',sub('.RData','',note$savefile),'-dir',sep='')
  count=1; while (dir.exists(temp.dir)) {temp.dir=paste('./',sub('.RData','',note$savefile),'-',letters[count],'-dir',sep=''); count=count+1}
  note$SAVEDIR=temp.dir
  dir.create(note$SAVEDIR)
} else {
  note$SAVEDIR=note$LOADDIR
}
X <- PL_MCMC(D=D,M=M,wsigma=0.1,wtheta=0.1,sigmabeta=0.5,sigmalambda=0.5,SS=SS,B=B,E=E,d=10,a=0.5,b=0.5,theta0=thetahat,sigma0=sigmahat,beta0=betahat,lambda0=lambdahat)
if (!note$DOWRITE) my.save(paste(note$SAVEDIR,'/',note$savefile,sep=''),X)
#####################################################################################################



#################################################################
## acceptance rate
(acp <- X$acp)

### loading the mcmc outputs
lambdamcmc <- X$lambdamcmc
betamcmc <- X$betamcmc
sigmamcmc <- X$sigmamcmc
thetamcmc <- X$thetamcmc

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
plot(betamcmc[1,-c(1:1000)],type='l',ylim=c(-3,3)) #5 b.c. we just pick a random one? 
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



########## checking the monotonicity of the betas
C <- matrix(0,(M/SS)*nd,2)
for(i in 1:nd){
  C[((i-1)*(M/SS)+1):(i*(M/SS)),1]<- betamcmc[i,]
  C[((i-1)*(M/SS)+1):(i*(M/SS)),2]<- i
}

boxplot(C[,1]~C[,2],ylab="values",xlab="ranks")
abline(h=0,col='red',lty="dashed")
###################################################
