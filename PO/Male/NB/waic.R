#Get the values we need to build the waic matrix 
B <- note$B
E <- note$E
J_done <- note$MCMC.SWEEPS #problems when the sweeps are not finished -> have to find a way to get J!
J_running <- (length(lkd.for.waic)-1)/N
N <- E-B+1
K <- (length(unlist(lkd.for.waic))-1)/J_running
waic.matrix <- matrix(rep(0,J_running*K),K,J_running)

# Create the matrix needed to calculate the WAIC 
for(j in 1:J_running){
  lower <- 2+N*(j-1); upper <- 1+N*j
  waic.matrix[,j] <- unlist(lkd.for.waic[lower:upper])
}


library(LaplacesDemon)
WAIC(waic.matrix)$WAIC 
WAIC(waic.matrix[,1:200])$WAIC #142.8163
WAIC(waic.matrix[,1:1399])$WAIC
J_running

(length(lkd.for.waic)-1)/N

 #deactivate else you overwrite what is in there! 


-2*WAIC(waic.matrix[,1:850])$lppd +2*WAIC(waic.matrix[,1:850])$pWAIC
WAIC(waic.matrix[,1:450])
