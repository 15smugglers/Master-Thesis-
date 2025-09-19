ComputeRanks<-function(cil,B,E) {
  
  NB=length(cil)
  begins=sapply(cil,function(x){x$begin})
  ends=sapply(cil,function(x){x$end})
  T=E-B+1
  rank=matrix(NA,NB,T) #rank[b,t] will be the rank bishop b held in year t (NA if absent)
  
  #go through the years 
  for (t in 1:T) {
    yr=B+t-1
    active=which(begins<=yr & ends>=yr) #cil-index of bishops active in year t
    NBt=length(active) #number active this year
    for (k in 1:NBt) {
      rank[active[k],t]=length(active[begins[active]<begins[active[k]]])+1
    } #rank of bishop b is 1 + the number of active bishops who started earlier 
  } 
  return(rank)
}



B=note$B; E=note$E
rank=ComputeRanks(cil,B,E)

