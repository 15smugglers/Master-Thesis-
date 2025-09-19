# Load the synthethic data 
data_pobuc <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_BUC/example-FT-dir",
               "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PO_BUC/example-FT-b-dir")

data_ponb <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB/example-FT-dir",
              "/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB/example-FT-dir")

data_plbuc <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PL_BUC/example-FT_theta-dir",
               "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PL_BUC/example-FT_theta-a-dir")

data_plnb <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PL_NB/example-FT_theta-dir",
               "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PL_NB/example-FT_theta-dir")

#### Define PO functions #####
df <- data.frame(Synthetic_Model=character(),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE) 


get_powaic <- function(path,Model){
  df <- data.frame(Synthetic_Model=character(),
                   Model=character(),
                   Waic=integer(),
                   Sd=integer())
  #ess_lkd <- vector("list",length(path))
  #ess_param <- vector("list",length(path))
  
  for(i in 1:length(path)){
    setwd(path[i])
    load("example-FT.RData")
    
    B <- note$B
    E <- note$E
    J_done <- note$MCMC.SWEEPS #problems when the sweeps are not finished -> have to find a way to get J!
    
    
    N <- E-B+1
    #lkd.for.waic <- lkd.for.waic[-1] #remove the 0 initalization 
    J_running <- (length(lkd.for.waic)-1)/N #get the nummber of current sweeps
    K <- (length(unlist(lkd.for.waic))-1)/J_running #get the number of observations in this time period 
    waic.matrix <- matrix(rep(0,J_running*K),K,J_running)
    
    # Create the matrix needed to calculate the WAIC 
    for(j in 1:J_running){
      lower <- 2+N*(j-1); upper <- 1+N*j
      waic.matrix[,j] <- unlist(lkd.for.waic[lower:upper])
      
    }
    # waic.matrix <- waic.matrix[,seq(from = 1, by = 10, length.out = 1000)] #remove the first 10.000 columns as burn in! 
    
    df[i,1] <- i
    df[i,2] <- Model
    df[i,3] <- loo::waic(t(waic.matrix))$estimates[3,1]
    df[i,4] <- loo::waic(t(waic.matrix))$estimates[3,2]
  }
  df
}


##### Define PL function ####
##### Model with beta
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

# Likelihood with beta 
get_lkdmatrix_b <- function(path){
  setwd(path)
  load("example-FT.RData")
  index <- first(which(sigmamcmc==0))-1
  lkd_matrix <- sapply(1:index, function(k) {
    sapply(D, function(x) logpp(dp = x,
                                lambda = lambdamcmc[,,k],
                                beta   = betamcmc[,k],
                                B      = B))
  })
  lkd_matrix
}

# Likelihood without beta 
get_lkdmatrix_nb <- function(path){
  setwd(path)
  load("example-FT.RData")
  index <- first(which(sigmamcmc==0))-1
  lkd_matrix <- sapply(1:index, function(k) {
    sapply(D, function(x) logpp(dp = x,
                                lambda = lambdamcmc[,,k],
                                beta   = numeric(nd),
                                B      = B))
  })
  lkd_matrix
}

get_waic_path_b <- function(path,Model){
  df <- data.frame(Synthetic_Model=character(),
                   Model=character(),
                   Waic=integer(),
                   Sd=integer())
  
  for(i in 1:length(path)){
    lkd <- get_lkdmatrix_b(path[i])
    df[i,1] <- i
    df[i,2] <- Model
    df[i,3] <- loo::waic(t(lkd))$estimates[3,1]
    df[i,4] <- loo::waic(t(lkd))$estimates[3,2]
  }
  df
}

get_waic_path_nb <- function(path,Model){
  df <- data.frame(Synthetic_Model=character(),
                   Model=character(),
                   Waic=integer(),
                   Sd=integer())
  
  for(i in 1:length(path)){
    lkd <- get_lkdmatrix_nb(path[i])
    df[i,1] <- i
    df[i,2] <- Model
    df[i,3] <- loo::waic(t(lkd))$estimates[3,1]
    df[i,4] <- loo::waic(t(lkd))$estimates[3,2]
  }
  df
}


## Calculate WAIC on the synthethic data 
syn_powaic_fmbuc <- get_powaic(data_pobuc ,"PO BUC Female") 
syn_powaic_fmnb <- get_powaic(data_ponb,"PO NB Female") 
syn_plwaic_fmbuc <- get_waic_path_b(data_plbuc,"PL BUC Female") 
syn_plwaic_fmnb <- get_waic_path_nb(data_plnb,"PL NB Female") 



waic_allmodels <- rbind(syn_powaic_fmbuc,
                        syn_powaic_fmnb,
                        syn_plwaic_fmbuc,
                        syn_plwaic_fmnb)

waic_allmodels_female_plot <- waic_allmodels %>%
  mutate(
    Synthetic_Model=factor(Synthetic_Model,
                           levels=c(1,2),
                           labels=c("PO BUC","PO NB")),
    Family  = ifelse(grepl("^PL", Model), "PL", "PO"),
    Modeltype = case_when(
      grepl("BUC", Model) ~ "BUC",
      grepl("BC",  Model) ~ "BC",
      grepl("NB",  Model) ~ "NB"
    )
  )


waic_female <- ggplot(waic_allmodels_female_plot, aes(x = Synthetic_Model, y = Waic,
                                                      color = Family, shape = Modeltype)) +
  geom_pointrange(aes(ymin = Waic - Sd, ymax = Waic + Sd),
                  position = position_dodge(width = 0.9),
                  size = 0.7) +
  scale_color_manual(values = c("PL" = "#E69F00",   
                                "PO" = "#0072B2")) + 
  scale_shape_manual(values = c("BUC" = 16, 
                                "BC"  = 17,  
                                "NB"  = 15)) + 
  labs(
    title = "WAIC values for PL and PO",
    x = "Synthetic data model",
    y = "WAIC",
    color = "Family",
    shape = "Modeltype"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title   = element_text(size = 16, face = "bold")
  )


ggsave("waic_female_synth.pdf",plot=waic_female,device=cairo_pdf,width=180,height=120,units="mm")

