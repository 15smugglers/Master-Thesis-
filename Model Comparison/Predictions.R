### Load libaries 
library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount)
library(data.table)
library(abind)

set.seed(123)

# Female BUC 
# Load the data we need. The function filters for the year to be predicted for those chimps that were modelled before 
output <- get_data("/Users/finnmaass/Downloads/Results_Thesis/PO/Half Data Middle/Female/BUC",1,"female")
lglkd_fm_buc <- numeric(0)
for(j in 1:1000){
  B <- output$B
  Test <- get_partialorder_sweep(j,output$P,output$NB,output$NF,output$Uout,output$active,output$T,output$ranks_pred,output$Z_fitted)
  log.lkd=sum(loglkd2(Test$p,Test$PO,output$cla_pred,"lkddown",output$q))
  lglkd_fm_buc <- c(lglkd_fm_buc,log.lkd)
}
lkd_fm_buc <- exp(lglkd_fm_buc)
log(mean(lkd_fm_buc)) #-9.550674


# Female BC 
output <- get_data("/Users/finnmaass/Downloads/Results_Thesis/PO/Half Data Middle/Female/BC",1,"female")
lglkd_fm_bc <- numeric(0)
for(j in 1:1000){
  B <- output$B
  Test <- get_partialorder_sweep(j,output$P,output$NB,output$NF,output$Uout,output$active,output$T,output$ranks_pred,output$Z_fitted)
  log.lkd=sum(loglkd2(Test$p,Test$PO,output$cla_pred,"lkddown",output$q))
  lglkd_fm_bc <- c(lglkd_fm_bc,log.lkd)
}
lkd_fm_bc <- exp(lglkd_fm_bc)
log(mean(lkd_fm_bc)) #-9.621381


# Female NB
output <- get_data("/Users/finnmaass/Downloads/Results_Thesis/PO/Half Data Middle/Female/NB",1,"female")
lglkd_fm_nb <- numeric(0)
for(j in 1:1000){
  B <- output$B
  Test <- get_partialorder_sweep(j,output$P,output$NB,output$NF,output$Uout,output$active,output$T,output$ranks_pred,output$Z_fitted)
  log.lkd=sum(loglkd2(Test$p,Test$PO,output$cla_pred,"lkddown",output$q))
  lglkd_fm_nb <- c(lglkd_fm_nb,log.lkd)
}

lkd_fm_nb <- exp(lglkd_fm_nb)
log(mean(lkd_fm_nb)) #-10.3268


## Predictions for the males 
# Male Beta unconstrained
output <- get_data("/Users/finnmaass/Downloads/Results_Thesis/PO/Half Data Middle/Male/BUC",1,"male")
lglkd_m_buc <- numeric(0)
for(j in 1:1000){
  B <- output$B
  Test <- get_partialorder_sweep(j,output$P,output$NB,output$NF,output$Uout,output$active,output$T,output$ranks_pred,output$Z_fitted)
  log.lkd=sum(loglkd2(Test$p,Test$PO,output$cla_pred,"lkddown",output$q))
  lglkd_m_buc <- c(lglkd_m_buc,log.lkd)
}

lkd_m_buc <- exp(lglkd_m_buc)
log(mean(lkd_m_buc)) #-23.07651




# Male no beta 
output <- get_data("/Users/finnmaass/Downloads/Results_Thesis/PO/Half Data Middle/Male/NB",1,"male")
lglkd_m_nb <- numeric(0)
for(j in 1:1000){
  B <- output$B
  Test <- get_partialorder_sweep(j,output$P,output$NB,output$NF,output$Uout,output$active,output$T,output$ranks_pred,output$Z_fitted)
  log.lkd=sum(loglkd2(Test$p,Test$PO,output$cla_pred,"lkddown",output$q))
  lglkd_m_nb <- c(lglkd_m_nb,log.lkd)
}

lkd_m_nb <- exp(lglkd_m_nb)
log(mean(lkd_m_nb)) #-23.09198

# When we use that function we get the ranks and the cla/cil list for the year we want to predict
# We also get the Z matrices we have for years we fitted. We will abbind that with value we predicted 
# to use the existing functions for the likelihoods 
get_data <- function(path, nm_yr, gender){
  setwd(path)
  load(file.path(path,"example-FT-b-dir","example-FT.RData")) #for the male data use example-a-FT-dir
  
  source(file="make_data.R")
  source(file="modelfun.R") #whereever the ComputeRanks function is stored in 
  source(file="outputfun.R") #where the log-lkd functions are stored in 
  source(file="dating.R")
  source(file="modelfun.R")
  source(file="pofun.R")
  source(file="dating.R")
  
  # Select the sex to receive the overall data 
  if(gender=="female"){
    cil_pred <- cil_female_chimp
    cla_pred <- cla_female_chimp
  } else {
    cil_pred <- cil_male_chimp
    cla_pred <- cla_male_chimp #problem the index is wrong then -> need to apply restrict function 
  }
  B <- note$B
  E <- note$E
  yr <- note$E+nm_yr
  
  # Rename the fitted cil and cla 
  cil_fit <- cil 
  cla_fit <- cla
  
  keep_names <- sapply(cil_fit, function(x) x$name) #get the chimpanzees active in fitted period
  cla_pred <- Filter(function(x) all(x$names %in% keep_names), cla_pred) #through out all interactions with other chimps
  #cla_pred <- Filter(function(x) all(x$tl >note$E && x$tl <=yr), cla_pred) #unncessary step, have to do for cil instead! 
  cil_pred <- Filter(function(x) all(x$name %in% keep_names), cil_pred) #important: have once name and the other names!! 
  
  finished=FALSE
  print(sprintf("Loaded num lists = %d, num bishops = %d",length(cla_pred),length(cil_pred)))
  print("trimming to [B,E] interval")
  while(!finished) {
    restricted=restrict_pred(La=cla_pred,bil=cil_pred,note$B,yr,boi=1:length(cil_pred),doi=note$do,b_min_list=1,max_list_length=Inf,rule=note$selectlists)
    finished=(identical(cla_pred,restricted$cla_pred) & identical(cil_pred,restricted$cil_pred))
    cla_pred=restricted$cla_pred
    cil_pred=restricted$cil_pred
    print(sprintf("num lists = %d, num bishops = %d",length(cla_pred),length(cil_pred)))
  }
  print(sprintf("Final trimmed num lists = %d, num bishops = %d",length(cla_pred),length(cil_pred)))
  
  
  
  #Next, compute the ranks! 
  ranks_pred <- ComputeRanks(cil_pred,B,yr) #add inputs
  
  cla_pred <- Filter(function(x) all(x$tl >note$E && x$tl <=yr), cla_pred)
  
  
  list(cla_pred=cla_pred, ranks_pred=ranks_pred,
       P=P,T=T,NB=NB,NF=NF,active=active,Uout=Uout,
       Z_fitted=check.Z,B=B)
  
  # Still need to calculate active for the next period 
  
  # Do we also have to filter the ranks we calculcated? 
  # Maybe dont cut all cla here. First fit the ranks, then cut the cla and also the ranks
}





# Function to calculate the predictions 
get_partialorder_sweep <- function(j,P,NB,NF,Uout,active,T,ranks,Z_fitted){
  # Extract the variable of interest from the P object; each row corresponds to j
  theta <- P[j,"theta"]; rho <- P[j,"rho"]; p <- P[j,"p"]; q <- P[j,"q"]; beta <- P[j, grep("^beta", colnames(P))]
  #Z_fitted <- state$Z 
  T <- T
  NB <- NB; NF <- NF
  Ut <- Uout[[j]][,,T] #extract the last U matrix of each sweep!
  active <- !is.na(ranks)  #get active chimps for the next period 
  
  
  U_pred=array(dim=c(NB,NF,1)) #initiale the U matrix. Set to 1 b.c. we only want to predict one year
  Sig=matrix(rho,NF,NF); diag(Sig)=1;
  
  # Calculate the U matrix here and then check if the chimps are active 
  U_pred[,,1] <- theta*Ut+mvrnorm(NB,rep(0,NF),Sig)   #*(1-theta^2))
  for (b in 1:NB) {if (!active[b,T+1]) U_pred[b,,1]<-NA} #filter out the non active chimps in year t+1
  
  # Now compute Z (we only have one year now!)
  Z_pred <- NA*U_pred
  
  for (c in 1:NB) {
    Z_pred[c,,1]=U_pred[c,,1]+beta[ranks[c,T+1]] #Pick the year we proedict here
  }
  
  Z <- abind(Z_fitted,Z_pred, along = 3)
  # Compute the Partial order now 
   PO <- ZtoPO(Z,active,B,1,display.Z=FALSE,PO=NA,years=1:(T+1))  #set T to 1 b.c. we only want one year 
   PO <- PO[[T+1]] #we only want the partial order for the last year
  
   list(PO=PO, Z=Z,ranks=ranks,
        U=U_pred,theta=theta,rho=rho,p=p,q=q,beta=beta,active=active)
  
  # have to recalulcate Z based on the new U', the ranks and beta 
  # Then we can appy the existing function 
  # Question: Easier not to split the data and chop of the likelihood at the end instead? 
  
}

log.lkd.ser<-function(h,cla,y2l,years=1:T,p=0,model='lkddown',q=1,cl=NA,f0=NA) {
  tot=vector('list',length(years))
  for (t in years) {
    if (length(y2l[[t]])>0) {
      tot[[t]]=loglkd2(r=p,h[[t]],cla[y2l[[t]]],model,q=q)
    }
  }
  return(tot)
}

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

restrict_pred<-function(La,bil,B,E,boi,doi,b_min_list=1,max_list_length=Inf,rule="all") #change to cil and cla
{
  
  #Extract the subset of bishops and lists in the interval of interest
  tl=unlist(lapply(La,function(x) x$tl))
  tu=unlist(lapply(La,function(x) x$tu))
  ll=unlist(lapply(La,function(x) x$ll))
  if (rule=='half') {
    fo=sapply(1:length(La), function(i) {(1+min(E,tu[i])-max(B,tl[i]))/(1+tu[i]-tl[i])})
    loi=which(fo>=0.5 & ll<=max_list_length) #XXX was > now >= as fo is rational - may make issues for oldstarts etc as NL may change
  } else {
    if (rule=='any') {
      loi=which(tl<=E & tu>=B & ll<=max_list_length)
    } else {
      if (rule=='all') {
        loi=which(tl>=B & tu<=E & ll<=max_list_length)
      } else {
        stop('makedata: rule in note$selectlists not recognised - see main.R for options')
      }
    }
  }
  #cut the lists - messes up number indexing from list to bishop and vv (solve using "id")
  cla=La[loi]
  bll=sapply(bil[boi],function(x) x$ll)
  boi=boi[bll>=b_min_list] #Bishop must be in b_min_list lists
  cil=bil[boi]
  
  cil.id=unlist(lapply(cil,function(x) x$name.id))
  drop=c()
  for (i in 1:length(cla)) {
    si=which(is.element(cla[[i]]$w,cil.id))
    cla[[i]]$w=cla[[i]]$w[si]
    cla[[i]]$names=cla[[i]]$names[si]
    cla[[i]]$o=match(cla[[i]]$w,cil.id)
    cla[[i]]$ll=length(cla[[i]]$w)
    if (cla[[i]]$ll<2) drop=c(drop,i) #drop lists with only one bishop
  }
  
  if (length(drop)>0) cla=cla[-drop]
  
  cla.id=unlist(lapply(cla,function(x) x$id))
  for (i in 1:length(cil)) {
    si=which(is.element(cil[[i]]$lid,cla.id))
    cil[[i]]$lid=cil[[i]]$lid[si]
    cil[[i]]$ln=match(cil[[i]]$lid,cla.id)
    cil[[i]]$tl=cil[[i]]$tl[si]
    cil[[i]]$tu=cil[[i]]$tu[si]
    cil[[i]]$ac=cil[[i]]$ac[si]
    cil[[i]]$ll=length(cil[[i]]$lid)
    cil[[i]]$di=which(cil[[i]]$diocese==doi) #dimnames(X)[1] should be exactly same as doi
    #not good doi is global
  }
  
  #added 18/04/2019 - list la becomes cla after we restrict to doi/[B,E]
  #Now write in diocese version of witness list cla[[]]$o - this goes in cla[[]]$do
  for (i in 1:length(cla)) {
    y=cla[[i]]$o
    cilo=cil[y]
    cla[[i]]$do=unlist(lapply(cilo,function(x) x$di))
  }
  
  #Would be good to check order in cla[[]]$do is all correct, going right back to la[[]]$w
  #following checks cil & cla(o&do) still match up
  #a=unlist(lapply(cil[unlist(lapply(cla,function(x) x$o))],function(x) x$diocese))
  #b=doi[unlist(lapply(cla,function(x) x$do))]
  #all(a==b)
  
  return(list(cla_pred=cla,cil_pred=cil))
  
}
