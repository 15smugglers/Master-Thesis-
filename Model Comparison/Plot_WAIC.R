# 1. Need to create a data frame with the following structure: 
# one column of model, one column for blocks (later subsistute that with the years)
# one column and one column for the sd 
# At the end: have one plot for comparison PL and PO for a given gender and a given beta configuration 
# Have in Appendix Comparision for a given gender and given model for the three different beta constrains 

##### Read in PO data ####
t_1_po <- c("/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1970_1974/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1975_1979/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1980_1984/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1985_1989/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1990_1994/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/1995_1999/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/2000_2004/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/2005_2009/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BUC/2010_2013/example-FT-b-dir")

t_2_po <- c("/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/1978_1982/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/1983_1987/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/1988_1992/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/1993_1997/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/1998_2002/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/2003_2007/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/BUC/2008_2011/example-FT-c-dir")

t_3_po <- c("/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1970_1974/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1975_1979/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1980_1984/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1985_1989/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1990_1994/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/1995_1999/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/2000_2004/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/2005_2009/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/BC/2010_2013/example-FT-c-dir")

t_4_po <- c("/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1970_1974/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1975_1979/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1980_1984/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1985_1989/example-FT-d-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1990_1994/example-FT-d-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/1995_1999/example-FT-c-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/2000_2004/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/2005_2009/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Female/NB/2010_2013/example-FT-c-dir"
)



t_5_po <- c("/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/1978_1982/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/1983_1987/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/1988_1992/example-FT-b-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/1993_1997/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/1998_2002/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/2003_2007/example-FT-a-dir",
         "/Users/finnmaass/Downloads/Results_Thesis/PO/5 Year Block/Male/NB/2008_2011/example-FT-b-dir")


### Synthetic Data runs 
t_7_pobuc <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_BUC/example-FT-dir",
         "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PO_BUC/example-FT-b-dir")

t_8_ponb <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB/example-FT-dir",
         "/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB/example-FT-dir")

t_9_plbuc <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PL_BUC/example-FT_theta-dir",
               "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PL_BUC/example-FT_theta-a-dir")

t_10_plnb <- c("/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PL_NB/example-FT_theta-dir",
               "/Users/finnmaass/Downloads/Synthetic Data/Models on PO NB Data/PL_NB/example-FT_theta-dir")



##### Read in PL data ####
t_1_pl <- c("/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1970_1974/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1975_1979/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1980_1984/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1985_1989/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1990_1994/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/1995_1999/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/2000_2004/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/2005_2009/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BUC/2010_2013/example-FT-dir")

t_2_pl <- c("/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/1978_1982/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/1983_1987/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/1988_1992/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/1993_1997/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/1998_2002/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/2003_2007/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/BUC/2008_2011/example-FT-dir")

t_3_pl <- c("/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1970_1974/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1975_1979/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1980_1984/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1985_1989/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1990_1994/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/1995_1999/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/2000_2004/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/2005_2009/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/BC/2010_2013/example-FT-dir")


t_4_pl <- c("/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1970_1974/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1975_1979/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1980_1984/example-FT-c-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1985_1989/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1990_1994/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/1995_1999/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/2000_2004/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/2005_2009/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Female/NB/2010_2013/example-FT-dir")

t_5_pl <- c("/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/1978_1982/example-FT-a-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/1983_1987/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/1983_1987/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/1993_1997/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/1998_2002/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/2003_2007/example-FT-dir",
            "/Users/finnmaass/Downloads/Results_Thesis/PL/5 Year Block/Male/NB/2008_2011/example-FT-dir")





#### Define PO functions #####
df <- data.frame(Timeblock=character(),
                 File=character(), 
                 User=character(), 
                 stringsAsFactors=FALSE) 


get_powaic <- function(path,Model){
  df <- data.frame(Timeblock=character(),
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
  lkd_matrix <- sapply(1:1000, function(k) {
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
  lkd_matrix <- sapply(1:1000, function(k) {
    sapply(D, function(x) logpp(dp = x,
                                lambda = lambdamcmc[,,k],
                                beta   = numeric(nd),
                                B      = B))
  })
  lkd_matrix
}

'get_waic <- function(lkd_matrix){
  waic <- loo::waic(t(lkd_matrix))[3,1]

}'  

get_waic_path_b <- function(path,Model){
  df <- data.frame(Timeblock=character(),
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
  df <- data.frame(Timeblock=character(),
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


##### Run different models & combine ####
powaic_fmbuc <- get_powaic(t_1_po,"PO BUC Female") 
powaic_mbuc <- get_powaic(t_2_po,"PO BUC Male")
powaic_fmbc <- get_powaic(t_3_po,"PO BC Female") 
powaic_fmnb <- get_powaic(t_4_po,"PO NB Female")
powaic_mnb <- get_powaic(t_5_po,"PO NB Male")

## Checks on synthetic data 
syn_powaic_fmbuc <- get_powaic(t_7_pobuc,"PO BUC Female") 
syn_powaic_fmnb <- get_powaic(t_8_ponb,"PO BUC Female") 
syn_plwaic_fmbuc <- get_waic_path_b(t_9_plbuc,"PL BUC Female") 
syn_plwaic_fmnb <- get_waic_path_b(t_10_plnb,"PL BUC Female") 

plwaic_fmbuc <- get_waic_path_b(t_1_pl,"PL BUC Female") 
plwaic_mbuc <- get_waic_path_b(t_2_pl,"PL BUC Male")
plwaic_fmbc <- get_waic_path_b(t_3_pl,"PL BC Female") 
plwaic_fmnb <- get_waic_path_nb(t_4_pl,"PL NB Female")
plwaic_mnb <- get_waic_path_nb(t_5_pl,"PL NB Male")


waic_fmbuc <- rbind(powaic_fmbuc,plwaic_fmbuc)
waic_mbuc <- rbind(powaic_mbuc,plwaic_mbuc)
waic_fmbc <- rbind(powaic_fmbc,plwaic_fmbc)
waic_fmnb <- rbind(powaic_fmnb,plwaic_fmnb)
waic_mnb <- rbind(powaic_mnb,plwaic_mnb)

waic_allmodels_female <- rbind(powaic_fmbuc,
                        plwaic_fmbuc,
                        powaic_fmbc,
                        plwaic_fmbc,
                        powaic_fmnb,
                        plwaic_fmnb)

waic_allmodels_male <- rbind(powaic_mbuc,
                               plwaic_mbuc,
                               powaic_mnb,
                               plwaic_mnb)


##### Plot different models ####
ggplot(waic_fmbuc,aes(x=Timeblock,y=Waic,group = Model,color=Model,
                      shape = Model))+
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16))+
 labs(
   title = "Waic values BUC Females"
 )+
  geom_pointrange(aes(ymin=Waic-Sd,ymax=Waic+Sd),
                  position = position_dodge(width = 0.5),
                  size=0.7)

ggplot(waic_fmbc,aes(x=Timeblock,y=Waic,group = Model,color=Model))+
  theme_minimal()+
  ggtitle("Waic values BC Females")+
  geom_pointrange(aes(ymin=Waic-Sd,ymax=Waic+Sd),
                  position = position_dodge(width = 0.5))

ggplot(waic_fmnb,aes(x=Timeblock,y=Waic,group = Model,color=Model))+
  theme_minimal()+
  ggtitle("Waic values NB Females")+
  geom_pointrange(aes(ymin=Waic-Sd,ymax=Waic+Sd),
                  position = position_dodge(width = 0.5))

ggplot(waic_mbuc,aes(x=Timeblock,y=Waic,group = Model,color=Model))+
  theme_minimal()+
  ggtitle("Waic values BUC Males")+
  geom_pointrange(aes(ymin=Waic-Sd,ymax=Waic+Sd),
                  position = position_dodge(width = 0.5))

ggplot(waic_mnb,aes(x=Timeblock,y=Waic,group = Model,color=Model))+
  theme_minimal()+
  ggtitle("Waic values NB Males")+
  geom_pointrange(aes(ymin=Waic-Sd,ymax=Waic+Sd),
                  position = position_dodge(width = 0.5))


##### Code to save the plots ####
make_plot <- function(data, title) {
  ggplot(data, aes(x = Timeblock, y = Waic, group = Model, color = Model,
                   shape = Model)) +
    geom_pointrange(
      aes(ymin = Waic - Sd, ymax = Waic + Sd),
      position = position_dodge(width = 0.5),
      size = 0.7
    ) +
    labs(title = title, x = "Timeblock", y = "Waic") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      plot.title  = element_text(size = 20),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
}

# Plots erstellen
plots <- list(
  make_plot(waic_fmbuc, "Waic values BUC Females"),
  make_plot(waic_fmbc,  "Waic values BC Females"),
  make_plot(waic_fmnb,  "Waic values NB Females"),
  make_plot(waic_mbuc,  "Waic values BUC Males"),
  make_plot(waic_mnb,   "Waic values NB Males")
)

# Dateinamen
filenames <- c(
  "waic_fmbuc.pdf",
  "waic_fmbc.pdf",
  "waic_fmnb.pdf",
  "waic_mbuc.pdf",
  "waic_mnb.pdf"
)

# Speichern
for (i in seq_along(plots)) {
  ggsave(filename = filenames[i], plot = plots[[i]], device = "pdf")
}


## Now make a plot for females with all model included
waic_allmodels_female_plot <- waic_allmodels_female %>%
  mutate(
    Family  = ifelse(grepl("^PL", Model), "PL", "PO"),
    Modeltype = case_when(
      grepl("BUC", Model) ~ "BUC",
      grepl("BC",  Model) ~ "BC",
      grepl("NB",  Model) ~ "NB"
    )
  )


waic_female <- ggplot(waic_allmodels_female_plot, aes(x = Timeblock, y = Waic,
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
    x = "Timeblock",
    y = "WAIC",
    color = "Family",
    shape = "Modeltype"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title   = element_text(size = 16, face = "bold")
  )

ggsave("waic_female.pdf",plot=waic_female,device=cairo_pdf,width=180,height=120,units="mm")


## Now make a plot for females with all model included
waic_allmodels_male_plot <- waic_allmodels_male %>%
  mutate(
    Family  = ifelse(grepl("^PL", Model), "PL", "PO"),
    Modeltype = case_when(
      grepl("BUC", Model) ~ "BUC",
      grepl("NB",  Model) ~ "NB"
    )
  )

waic_male <- ggplot(waic_allmodels_male_plot, aes(x = Timeblock, y = Waic,
                            color = Family, shape = Modeltype)) +
  geom_pointrange(aes(ymin = Waic - Sd, ymax = Waic + Sd),
                  position = position_dodge(width = 0.9),
                  size = 0.7) +
  scale_color_manual(values = c("PL" = "#E69F00",   
                                "PO" = "#0072B2")) + 
  scale_shape_manual(values = c("BUC" = 16, 
                                "NB"  = 15)) + 
  labs(
    title = "WAIC values for PL and PO",
    x = "Timeblock",
    y = "WAIC",
    color = "Family",
    shape = "Modeltype"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title   = element_text(size = 16, face = "bold")
  )

ggsave("waic_male.pdf",plot=waic_male,device=cairo_pdf,width=180,height=120,units="mm")
