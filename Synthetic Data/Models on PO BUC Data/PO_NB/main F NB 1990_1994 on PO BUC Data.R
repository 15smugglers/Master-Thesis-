
if (exists('cl') && !is.na(cl)) {stopCluster(cl); gc()}

#rm(list=ls())

library(MASS)
library(mvtnorm)
library(mnem)    #needed for transitive.X - ed 5-4-22
library(igraph) 
library(Rgraphviz)
library(graph) 
library(coda)
library(lecount)
library(data.table)

#key drivers for run - at some point we will write this to a file in a new unique directory for the run
note=list()
note$RUNDIR="~/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB"
note$VERBOSE=FALSE

#data setup
note$B=1990                  
note$E=1994                  
if (note$B>note$E) stop('B before E')

note$doi="Gombe"
#doesnt include "Tusculum"
#note$bishopdatefile="BishopDates-25-7-22b.csv" #"BishopDates-4-11-19.csv"
note$selectlists='any'       #fraction of list time which must overlap target interval 'strict' is all, 'half' is 50% and 'any' is any 
note$maxlistlength=Inf     #what list lengths allowed? NA or Inf is everything useful. Set to say 14 to knock out a few very long lists if speed an issue
note$min.lists.per.bishop=1 #for each bishop i count how many list they appear in <- if they are only in one list then i throw it out 

#synth data setup
note$DOSYNTH=FALSE            #real or synthetic data?
note$srep=1                  #number of repeated copies of cla like scla<-rep(cla,srep) in synth data by setting to 2 it simulates 2 synthetic list per real list 
note$true.p=0.05                #error probability was 0.1 before
note$true.q=0                 #bi-dir prob choose down
note$true.theta=0.9          #if synth then this is true theta
note$true.rho=0.925            #if syth then this is true rho
#note$true.beta=0             #if NA then simulated (and ordered) - set to zero to remove beta
note$sNF=2                   #number of features in synth data (K in notes); true K values was 2 and that gives deeper orders
# have same snF as nF for debugging (should be 6 here)
note$SYNTHTYPE='priorstate' #'poststate' #
#'poststate' means truth is last state in old file, 
#'priorstate' means tau, beta and U are sampled using priors rest are true.p etc
# oldsynth means it uses synthetic data from a past run
note$TRUEINPUTFILE="./example-FT-dir/example-FT.RData" #if poststate.  Saves the output
note$OLDSYNTHINPUTFILE="./testFUN-dir/testFUN.RData" #if oldsynth

#model setup
note$NF=NA #2 before            NA sets it automatically so it sets it to 6 (2 is a lot faster and sacrfice only a little)        #number of features in U, Z so U is NB x NF x T - at least MA/2 gives all possible PO's - set to NA to get floor(MA/2)
note$constrainbeta=FALSE     #if TRUE then beta is contrained to be decreasing
note$model='lkddown'           #'lkpairwise', 'bidir', 'lkddown' or 'lkdup' or 'lkdnat' or 'prior' or 'lkmallow' at the moment - 'prior' gives the joint prior of beta,theta,rho,U 
note$gender='female'            #'male', 'female
note$PhPar=list(model=note$model,
                rfac=c(shape1=1,shape2=1/6,ncp=1), #prior parameters for rho
                p=list(a=1,b=9),      #prior parameters for p(queue jumping), q(bidir), p(mallows)
                q=list(a=1,b=1),      #p is beta prior parameters c(1,9) is default in most runs - called "subjective"
                p.m=list(a=0,b=10))   #p.m is p mallows - uniform(a,b) for mallows penalty param

#MCMC drivers
note$RANDSEED=101
note$NEWRUN=TRUE             #set to FALSE if resarting old run
note$DRAW=FALSE
note$savefile='example-FT.RData'  #write MCMC samples to savefile
note$loadfile=NA             #if restarting set this to old savefile
note$LOADDIR=NA              #if restarting set this to old savedir
note$MCMC.SWEEPS=1000000 
note$SUBSAMPLE.INTERVAL=10   #get note$MCMC.SWEEPS/note$SUBSAMPLE.INTERVAL samples
note$DOWRITE=TRUE            #set to false if you dont want to save along the way - sensible for short runs only.
note$WRITE.SUBSAMPLE=100     #write the state to a file less frequently as this takes time for long runs
note$Usweeps=c(0,2)          #number of U-sweeps in doU (all b's per proposal) and doU2 (one b at a time) for each beta-sweep
note$UALLFRACUPD=c(0,1)      #fraction of two (fast,slow) U-updates to use
note$rho.reps=5              #number of rho-updates per sweep
note$theta.reps=5            #number of theta-updates per sweep
note$rhotheta.reps=5
note$rhotheta.wrap.reps=1
note$rhothetaU.wrap.reps=3
note$p.reps=5
note$q.reps=5
note$comment="Fixed time example (or short time anyway)"

#if the update is switched off (debugging etc) make sure init value set to something sensible
note$STARTSTATE='disordered'    #'disordered','ordered','oldstart', 'true' - 'true' only possible if synth data 
note$STARTFILE="filename.RData"            #if oldstart, give the filename containing the run here - run will start from final state
note$init.beta.fac=1         #if synth & true start this multiplies init beta 
note$init.U.fac=1            #if synth & true start this multiplies init U - set close to zero if testing U mcmc

#init beta is all 0 and init tau is prior sample
note$init.p=ifelse(note$STARTSTATE=='disordered',0.95,0.05)             #0.05 plausible
note$init.q=ifelse(note$STARTSTATE=='disordered',0.5,0.05)              #0.05 plausible
note$init.theta=ifelse(note$STARTSTATE=='disordered',0,0.95)          #init theta - 0.95 plausible
note$init.rho=ifelse(note$STARTSTATE=='disordered',0.1,0.9)             #init rho - 0.9 plausible

note$DOTHETA=TRUE            #do we switch on the theta-update #All the TS updates currently switched off -> need to switch on again; the TS paramter for AR(1); improvement 
note$DOBETA=FALSE             #do we switch on the beta-update
note$DORHO=TRUE              #do we switch on the rho-update
note$DORHOTHETA=TRUE         #combined rho-theta update
note$DOU=TRUE                #do we switch on the U-update
note$DOP=TRUE                #P-update
note$DOQ=(note$model=='bidir')                #Q-update
note$DOTAU=FALSE              #tau-update -> only need it when we have data uncertainity 

#parallel drivers - dont use this - it seems more efficient to use the cores to do multiple independent scalar runs.
#Also in current version it isnt actually any faster - seems to depend delicately on where functions defined.
#was working now v. slow
note$DOPAR=FALSE             #use parallel proc?
if (note$DOPAR) library(parallel)
note$nthread=2               #number of parallel threads - as a rule of thumb, seems not much gain above 4 
#note$optimise.par.blocks=FALSE
note$split.years=1118 #c(1119,1134,1137)

#######################################################

# where are we working?
setwd(note$RUNDIR)

#old work functions including alot of legacy stuff need to be in RUNDIR
source(file="dating.R")
source(file="makedatafun.R") #had that commented out before -> appeareantly not! dont know 
source(file="makesynthdatafun.R")
source(file="makeparametersfun.R")
#source(file="make_data.R")
source(file="pofun.R")

#Functions simulating priors and evalutaing log-prior densities
source(file="modelfun.R")

#Functions for output analysis
source(file="outputfun.R")

#Functions related to the MCMC
source(file="mcmcfun.R",verbose=note$VERBOSE)

#main mcmc function
source(file='mcmcPO.R')

if (note$NEWRUN) {
  temp.dir=paste('./',sub('.RData','',note$savefile),'-dir',sep='')
  count=1; while (dir.exists(temp.dir)) {temp.dir=paste('./',sub('.RData','',note$savefile),'-',letters[count],'-dir',sep=''); count=count+1}
  note$SAVEDIR=temp.dir
  dir.create(note$SAVEDIR)
} else {
  note$SAVEDIR=note$LOADDIR
}

if (note$NEWRUN) {
  this.file <- parent.frame(2)$ofile; #mac version? When $nf what is the function doing 
  file.copy(this.file, paste(note$SAVEDIR,'/',sub('.RData','',note$savefile),'-main.R',sep=''))
} 

#if (FALSE) {
#run MCMC - for longer runs this wont complete so O will never be returned, output via file saving

#note$SAVEDIR <- "/Users/finnmaass/Library/Mobile Documents/com~apple~CloudDocs/Studium/Dissertation MSc/Data/MCMC_Chimp_new_lkd/example-FT-o-dir" #deactivate else you overwrite what is in there! 

O=mcmcPO(note)


## Receive the error that they cant find makedata -> i have to replace this with my own cla and cil and doi file!

#save the output if it wasnt already saved
if (!note$DOWRITE) my.save(paste(note$SAVEDIR,'/',note$savefile,sep=''),O) #saves final result in folder example-FT-k-dir 

output=outputanalysis(out.dir=note$SAVEDIR,
                      out.file=note$savefile,burn=10,
                      yoi=(note$B:note$E)-note$B+1,
                      pdf.file=NA,P.samples=NA,full.analysis=TRUE)

if (note$DOPAR) {stopCluster(cl); gc()}
#}


test_path <- "/Users/finnmaass/Downloads/Synthetic Data/Models on PO BUC Data/PO_NB/example-FT-dir"
save_dir  <- file.path(test_path, "plots")
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

output <- outputanalysis(
  out.dir       = test_path,
  out.file      = note$savefile,
  burn          = 1000,
  yoi           = 1:5,
  pdf.file      = file.path(save_dir, "subset_outputanalysis.pdf"),  # <— hier speichern
  P.samples     = NA,
  full.analysis = TRUE
)
