# Analysis of the true PO and the MCMC PO 
# Problem: it is not about the number of years but about the number of individuals in the partial order
# Not that is not true. B.c. in the output structure we have the individuals in the matrix itself but the number of individuals 
# are not in the [[]][[]] structure. 

n_yrs <- note$E - note$B + 1  #+1 b.c. we also have to count in the year note$E itself 
n_sweep <- length(80:101)
acc <- matrix(rep(0,n_sweep*n_yrs),nrow=n_sweep,ncol=n_yrs)

# Calculate the accuracy in percent 
for(j in 1:n_yrs){
  for(i in 1:n_sweep) acc[i,j]=({1-(sum((O$PO[[i]][[j]] == O$Sstate$h[[j]]))/
      (ncol((O$PO[[i]][[j]] == O$Sstate$h[[j]]))*nrow((O$PO[[i]][[j]] == O$Sstate$h[[j]]))))})
}

for(i in 1:n_yrs){
  print(round(mean(acc[,i]),4))
}
