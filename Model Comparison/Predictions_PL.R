# Load the MCMC outputs & create data for prediction 
library(coda);library(mvtnorm); library(data.table); library(MASS)

# Setup the note 
set.seed(123)
note=list()

#data setup
note$B=1987                  
note$E=2003           #we add an extra year here to have the data set up. If we predict the male data we set note$B=1987 and note$E=2003     
note$gender='male'  #set to male if we want to test the male data
note$selectlists='any'

source("make_data_PL.R") #Here we also define the restrict function and then 
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
B <- note$B;E <- note$E
D_new <- get_data_list_2(cla=cla,cil=cil,V=V,B=B,E=E) #we dont use the V part so might as well just leave it out. 
#D_new <- D_new[lengths(D)!=0] # get rid of null lists



### Female BUC 
path <- "/Users/finnmaass/Downloads/Results_Thesis/PL/Half Data Middle/Female/BUC"
setwd(path)
load(file.path(path,"example-FT-a-dir","example-FT.RData"))
D_new <- Filter(function(x) x$time==2001,D_new) #set x$time==2003 if we want to predict for male data
keep_names <- sapply(D, function(x) x$o) #get the chimpanzees active in fitted period
D_new <- Filter(function(x) all(x$o %in% keep_names), D_new)
sweeps_run <- first(which(sigmamcmc==0))-1
lglkd_fm_buc <- numeric(0)
for(s in 1:sweeps_run){
  # Simulate the new values and extract the old ones
  lambda <- mvrnorm(1,thetamcmc[s]*lambdamcmc[-c(1),21,s],(sigmamcmc[s])^2*N) #with one dimension removed
  lambda <- c(lambda,-sum(lambda)) #
  beta <- betamcmc[,s]
  log.lkd <- PL_log_lik_pred(D_new,lambda,beta)
  lglkd_fm_buc <- c(lglkd_fm_buc,log.lkd)
}
lkd_fm_buc <- exp(lglkd_fm_buc)
log(mean(lkd_fm_buc)) #-15.46901



### Female BC 
path <- "/Users/finnmaass/Downloads/Results_Thesis/PL/Half Data Middle/Female/BC"
setwd(path)
load(file.path(path,"example-FT-a-dir","example-FT.RData"))
D_new <- Filter(function(x) x$time==2001,D_new) #set x$time==2003 if we want to predict for male data
keep_names <- sapply(D, function(x) x$o) #get the chimpanzees active in fitted period
D_new <- Filter(function(x) all(x$o %in% keep_names), D_new)
sweeps_run <- first(which(sigmamcmc==0))-1
lglkd_fm_bc <- numeric(0)
for(s in 1:sweeps_run){
  # Simulate the new values and extract the old ones
  lambda <- mvrnorm(1,thetamcmc[s]*lambdamcmc[-c(1),21,s],(sigmamcmc[s])^2*N) #with one dimension removed
  lambda <- c(lambda,-sum(lambda)) #
  beta <- betamcmc[,s]
  log.lkd <- PL_log_lik_pred(D_new,lambda,beta)
  lglkd_fm_bc <- c(lglkd_fm_bc,log.lkd)
}

lkd_fm_bc <- exp(lglkd_fm_bc)
log(mean(lkd_fm_bc)) #-17.1088



### Female NB 
path <- "/Users/finnmaass/Downloads/Results_Thesis/PL/Half Data Middle/Female/NB"
setwd(path)
load(file.path(path,"example-FT-b-dir","example-FT.RData"))
D_new <- Filter(function(x) x$time==2001,D_new) #set x$time==2003 if we want to predict for male data
keep_names <- sapply(D, function(x) x$o) #get the chimpanzees active in fitted period
D_new <- Filter(function(x) all(x$o %in% keep_names), D_new)
sweeps_run <- first(which(sigmamcmc==0))-1
lglkd_fm_nb <- numeric(0)
for(s in 1:sweeps_run){
  # Simulate the new values and extract the old ones
  lambda <- mvrnorm(1,thetamcmc[s]*lambdamcmc[-c(1),21,s],(sigmamcmc[s])^2*N) #with one dimension removed
  lambda <- c(lambda,-sum(lambda)) #
  beta <- rep(0,nd)
  log.lkd <- PL_log_lik_pred(D_new,lambda,beta)
  lglkd_fm_nb <- c(lglkd_fm_nb,log.lkd)
}
lkd_fm_nb <- exp(lglkd_fm_nb)
log(mean(lkd_fm_nb)) 
mean(lglkd_fm_nb) #-19.57972
# Get and save the latest version! 


### Male BUC 
path <- "/Users/finnmaass/Downloads/Results_Thesis/PL/Half Data Middle/Male/BUC"
setwd(path)
load(file.path(path,"example-FT-a-dir","example-FT.RData"))
D_new <- Filter(function(x) x$time==2003,D_new) #set x$time==2003 if we want to predict for male data
keep_names <- sapply(D, function(x) x$o) #get the chimpanzees active in fitted period
D_new <- Filter(function(x) all(x$o %in% keep_names), D_new)
sweeps_run <- first(which(sigmamcmc==0))-1
lglkd_m_buc <- numeric(0)
for(s in 1:sweeps_run){
  # Simulate the new values and extract the old onesp
  print(s)
  lambda <- mvrnorm(1,thetamcmc[s]*lambdamcmc[-c(1),16,s],(sigmamcmc[s])^2*N) #with one dimension removed
  lambda <- c(lambda,-sum(lambda)) #
  beta <- betamcmc[,s]
  log.lkd <- PL_log_lik_pred(D_new,lambda,beta)
  lglkd_m_buc <- c(lglkd_m_buc,log.lkd)
}
lkd_m_buc <- exp(lglkd_m_buc)
log(mean(lkd_m_buc))#-13.14563


### Male NB 
path <- "/Users/finnmaass/Downloads/Results_Thesis/PL/Half Data Middle/Male/NB"
setwd(path)
load(file.path(path,"example-FT-a-dir","example-FT.RData"))
D_new <- Filter(function(x) x$time==2003,D_new) #set x$time==2003 if we want to predict for male data
keep_names <- sapply(D, function(x) x$o) #get the chimpanzees active in fitted period
D_new <- Filter(function(x) all(x$o %in% keep_names), D_new)
sweeps_run <- first(which(sigmamcmc==0))-1
lglkd_m_nb <- numeric(0)
for(s in 1:sweeps_run){
  # Simulate the new values and extract the old ones
  lambda <- mvrnorm(1,thetamcmc[s]*lambdamcmc[-c(1),16,s],(sigmamcmc[s])^2*N) #with one dimension removed
  lambda <- c(lambda,-sum(lambda)) #
  beta <- rep(0,nd)
  log.lkd <- PL_log_lik_pred(D_new,lambda,beta)
  lglkd_m_nb <- c(lglkd_m_nb,log.lkd)
}
lkd_m_nb <- exp(lglkd_m_nb)
log(mean(lkd_m_nb)) #-13.30152

# Note: it sums the log likelihoods together -> cannot really compare between male and female b.c. the number of 
# observations in the next year is different for them




###### Likelihood functions #####
# Adjusted the function below 
logpp_pred <- function(dp,lambda,beta){ 
  ##the log lik contribution of a single list
  o <- dp$o
  n <- length(o)
  t <- dp$time
  r <- dp$rank
  f <- lambda[o] +beta[r] #changes lambda here sind we only have one dimension
  ff <- numeric(n)
  for(j in 1:n){
    ff[j]<- f[j]- log(sum(exp(f[j:n]))) #likelihood for one chimp
  }
  return(sum(ff))
}

PL_log_lik_pred <- function(D_new,lambda,beta,B){
  
  logp <- sapply(D_new,function(x) logpp_pred(dp=x,lambda=lambda,beta=beta))
  return(sum(logp)) #take the sum b.c. we look at the log likelihood!
}


