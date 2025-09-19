### Steps 
# 1. Get a likelihood function 
# 2. Get the values you need as an input for the likelihood function -> which h do we take? Which p do we take?
# 3. Get the indices/ names of the chimps that you fit
# 4. Filter cla and cil to only keep the chimps of interest, then compute ranks and filter for time
# when you compute the ranks do that till the total end of observations 
# 5. Compute the log likelihood for all observations for the next years and calculate the total 

## ToDo 
# 1. Find out 

# Get the likelihood for one observation 
loglkd2<-function(r=1,mc,la,model,q=1) {
  
  lkpwi <- function(r,mc,o){
    #check that list is longer than 1 
    if (length(o)==1) {return(0)}
    
    #First we need to get the sub DAG for the pairwise order 
    mla<-mc[o,o]
    
    if(mla[1,2]==mla[2,1] & mla[1,2] ==0) {
      fac <- log(1/2)
    } else if (mla[2,1]==1) {
      fac <- log(r/2)
    } else if (mla[1,2]==1) {
      fac <- log(1-r/2)
    } else {
      print("Error in the suborder ")
    }
    
    
  }
  
  lkddown.fac<-function(r,mc,o) {
    
    #evaluate the log-probability for the
    #placement of o[1] - the first person in the order o
    #given the PO with reduction mr
    
    #mr,o, an order of length 1, just one way to place it
    if (length(o)==1) {return(0)}
    
    #the sub-DAG for the order
    mla<-mc[o,o]
    #the sub-DAG with the first element removed
    mlb<-mc[o[-1],o[-1],drop=FALSE]
    
    #first person may have been placed at random
    fac <- r/length(o) #set r to the probability to make an mistake 
    
    #if the first person is in a place that does
    #not violate the proposed PO (given by mc) then
    #they may have been placed using the distribution
    #over linear extensions
    if (sum(mla[,1])==0) {
      fac<-fac+(1-r)*nle(mlb)/nle(mla)
    }
    #return the log-likelihood for this placement
    #plus the log-likelihood for the subsequent placements
    return(log(fac)+lkddown.fac(r,mc,o[-1])) #take the log lkd so that we can add and dont multiply
    
  }
  
  lkdup.fac<-function(r,mc,o) {
    
    #evaluate the log-probability for the
    #placement of o[n] - the last person in the order o
    #given the PO with reduction mr
    
    #mr,o, an order of length 1, just one way to place it
    n<-length(o)
    if (n==1) {return(0)}
    
    #the sub-DAG for the order
    mla<-mc[o,o]
    #the sub-DAG with the last element removed
    mlb<-mc[o[-n],o[-n],drop=FALSE]
    
    #last person may have been placed at random
    fac <- r/length(o)
    
    #if the last person is in a place that does
    #not violate the proposed PO (given by mc) then
    #they may have been placed using the distribution
    #over linear extensions
    if (sum(mla[n,])==0) {
      fac<-fac+(1-r)*nle(mlb)/nle(mla)
    }
    #return the log-likelihood for this placement
    #plus the log-likelihood for the subsequent placements
    return(log(fac)+lkdup.fac(r,mc,o[-n]))
    
  }
  
  lkdnat.fac<-function(r,mc,o) {
    
    #evaluate the log-probability for the
    #placement of o[1] - the first person in the order o
    #given the PO with reduction mr
    
    #mr,o, an order of length 1, just one way to place it
    n<-length(o)
    if (n==1) {return(0)}
    
    #the sub-DAG for the order
    mla<-mc[o,o]
    
    #first person may have been placed at random
    fac <- r/length(o)
    
    #if the last person is in a place that does
    #not violate the proposed PO (given by mc) then
    #they may have been placed at random from the legal placements
    if (sum(mla[,1])==0) {
      fac<-fac+(1-r)/sum(apply(mla,2,sum)==0)
    }
    #return the log-likelihood for this placement
    #plus the log-likelihood for the subsequent placements
    return(log(fac)+lkdnat.fac(r,mc,o[-1]))
    
  }
  
  lkd.mallow<-function(r,mc,o) {
    #evaluate the log-probability for mallows model
    n<-length(o)
    if (n==1) {return(0)}
    
    hamming.dist <- hamming(mc[o,o],o) 
    
    #disagreed elements in any n-length LE; 0(zero disagree), 1,2,...,n (all disagree)
    #derangements
    n.derange=c(1,0)
    for(i in 3:(n+1)){n.derange=c(n.derange,(i-2)*sum(n.derange[i-c(1,2)]))}
    
    #freq of numbers of disagreed elements
    #number of choosing n.derange out of n elements
    f.derange <- c(1,choose(n,c(1:n)))
    
    #normalizing constant
    n.const <- sum(n.derange*f.derange*exp(-r*c(0:n)))
    
    return(log(mean(exp(-r*hamming.dist)/n.const)))
  }
  
  n.order<-length(la)
  llkda<-matrix(0,1,n.order)
  for (k in 1:n.order) {
    ind=match(la[[k]]$o,as.numeric(rownames(mc))) #10/12/19 - handles names that are not packed in time series
    if (model=='lkpairwise') {llkda[1,k]<-lkpwi(r,mc,ind)}
    if (model=='lkddown') {llkda[1,k]<-lkddown.fac(r,mc,ind)}
    if (model=='lkdup') {llkda[1,k]<-lkdup.fac(r,mc,ind)}
    if (model=='lkdnat') {llkda[1,k]<-lkdnat.fac(r,mc,ind)}
    if (model=='bidir') {o=la[[k]]$o; llkda[1,k]<-log(QP.LEProb(mc[ind,ind],o,r,q))}
    if (model=='lkmallow') {llkda[1,k]<-lkd.mallow(r,mc,ind)}
    if (model=='prior') {llkda[1,k]<-0}
  }
  return(llkda)
  
}

# The the sum over the all observations 
log.lkd.ser<-function(h,cla,y2l,years=1:T,p=0,model='lkddown',q=1,cl=NA,f0=NA) {
  tot=vector('list',length(years))
  for (t in years) {
    if (length(y2l[[t]])>0) {
      tot[[t]]=loglkd2(r=p,h[[t]],cla[y2l[[t]]],model,q=q)
    }
  }
  return(tot)
}








df <- data.frame(
  id = 1:5,
  x  = c(0, 2, 1, 5, 3),
  y  = c("a", "b", "c", "d", "e")
)
filter(df, x > 1)





# Extract p value, poset
p <- mean(P[-c(which(is.na(P[,10]))),10]) #find better way than number, also include burn in 
h <- new.h #find a better way -> take poset structure where with >50% posterior probability 

# Load the (unseen) data and filter for the right year 
# Input the years of interest and the gender you are interested in 
note <- list()
note$B=1980                  
note$E=2000  
T <- note$E-note$B+1
if (note$B>note$E) stop('B before E')
note$gender='female'            #'male', 'female
note$selectlists='any'  
note$RUNDIR="~/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1970_1974"
source("get_data.R")

log.lkd.ser(h,cla,)
lapply(cla, function(x) loglkd2(p,h,cla,"lkddown"))

loglkd2(p,h,cla,"lkddown")


year2list<-function(tau,T) {
  #y2l[[t]] gives lists assigned to year t by tau
  y2l=vector("list",T)
  for (t in 1:T) y2l[[t]]=which(tau==t)
  return(y2l)
}