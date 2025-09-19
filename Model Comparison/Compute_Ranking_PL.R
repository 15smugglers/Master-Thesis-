### Get ranking PL model 
# Extract the lambda and beta we need 
### Load the data for the female into the global environment 
lambda <- lambdamcmc[,22,1:5000]
lambda <- apply(lambda,1,mean,na.rm=TRUE)

beta <- betamcmc[,1:5000]
beta <- apply(beta,1,function(x) mean(x[x != 0], na.rm = TRUE))

D_sub <- Filter(function(x) all(x$time>=1990 && x$time <= 1991), D)
unique(unlist(sapply(D_sub,function(x) x$o)))


n <- length(D_sub)
df_winner <- data.frame(Chimp=rep(0,n),
                        Rank=rep(0,n),
                        Lambda_beta=rep(0,n))

for(i in 1:n){
  df_winner$Chimp[i] <- D_sub[[i]]$o[1]
  df_winner$Rank[i] <- D_sub[[i]]$rank[1]
  chimp <- D_sub[[i]]$o[1]; rank <- D_sub[[i]]$rank[1]
  df_winner$Lambda_beta[i] <- lambda[chimp]+beta[rank]
}

df_loser <- data.frame(Chimp=rep(0,n),
                        Rank=rep(0,n),
                        Lambda_beta=rep(0,n))

for(i in 1:n){
  df_loser$Chimp[i] <- D_sub[[i]]$o[2]
  df_loser$Rank[i] <- D_sub[[i]]$rank[2]
  chimp <- D_sub[[i]]$o[2]; rank <- D_sub[[i]]$rank[2]
  df_loser$Lambda_beta[i] <- lambda[chimp]+beta[rank]
}

df_female <- rbind(df_winner,df_loser)

## now we can read the ranking from here! 
## Note that chiimp 28 enters in 1992! Therefore, we put it on last rank (highest seniority rank)
## Chimp 22 does not interact in that time 
## 
df_female %>% arrange(desc(Lambda_beta))



### Load the data for the males into the global environment 
lambda <- lambdamcmc[,10,1:5000]
lambda <- apply(lambda,1,mean,na.rm=TRUE)

beta <- betamcmc[,1:5000]
beta <- apply(beta,1,function(x) mean(x[x != 0], na.rm = TRUE))

D_sub <- Filter(function(x) all(x$time>=1989 && x$time <= 1989), D)
unique(unlist(sapply(D_sub,function(x) x$o)))


n <- length(D_sub)
df_winner <- data.frame(Chimp=rep(0,n),
                        Rank=rep(0,n),
                        Lambda_beta=rep(0,n))

for(i in 1:n){
  df_winner$Chimp[i] <- D_sub[[i]]$o[1]
  df_winner$Rank[i] <- D_sub[[i]]$rank[1]
  chimp <- D_sub[[i]]$o[1]; rank <- D_sub[[i]]$rank[1]
  df_winner$Lambda_beta[i] <- lambda[chimp]+beta[rank]
}

df_loser <- data.frame(Chimp=rep(0,n),
                       Rank=rep(0,n),
                       Lambda_beta=rep(0,n))

for(i in 1:n){
  df_loser$Chimp[i] <- D_sub[[i]]$o[2]
  df_loser$Rank[i] <- D_sub[[i]]$rank[2]
  chimp <- D_sub[[i]]$o[2]; rank <- D_sub[[i]]$rank[2]
  df_loser$Lambda_beta[i] <- lambda[chimp]+beta[rank]
}

df_male <- rbind(df_winner,df_loser)
df_male %>% arrange(desc(Lambda_beta))

