#### Code from Paper ####
### Load libraries 
library(tidyverse)



################################################################################
# Read data and modify 
################################################################################
# females
female_ago <- read.csv2("female_ago.csv", header = T)
female_presence <- read.csv2("female_presence.csv", header=T, check.names=F, stringsAsFactors=F, sep=";")

# males
male_ago <- read.csv2("male_ago.csv", header=T, stringsAsFactors=F, sep=";")
male_presence <- read.csv2("male_presence.csv", header=T, check.names=F, stringsAsFactors=F, sep=";")

female_ago$Date <- as.Date(female_ago$Date, "%d.%m.%Y")
female_presence$Date <- as.Date(female_presence$Date, "%d.%m.%Y")

male_ago$Date <- as.Date(male_ago$Date, "%d.%m.%Y")
male_presence$Date <- as.Date(male_presence$Date, "%d.%m.%Y")

################################################################################
# Create female list
################################################################################
# cil each list item is a chimp 
n_female <- length(unique(female_ago$Winner))
n_male <- length(unique(male_ago$Winner))


cil_names <- c("name.id","node.name","name","diocese",
               "di","lid","ln","tl","tu","ac","ll","by","till",
               "from","to","begin","end")


cil_female_chimp <- vector("list",length = n_female)

cil_female_chimp <- replicate(
  n_female,
  setNames(vector("list", length(cil_names)), cil_names),
  simplify = FALSE
)

## Get the first and the last interaction for each chimp 
# Females 
female_ago <- female_ago %>% 
  mutate(ID =seq(1:length(female_ago$Date)))

get_indexes_female <- gather(data = female_ago,
                             key = Status, 
                             value = Chimpanzee,
                             Winner, Loser)  %>% 
  arrange(Date) 
female_ago_fl <- data.frame(Chimp = seq(1:44),
                            from_i = as.Date(rep(0,n_female)),
                            to_i = as.Date(rep(0,n_female)))

for(i in 1:n_female){
  female_ago_fl$from_i[i] = get_indexes_female$Date[first(which(get_indexes_female$Chimpanzee==i))] #that is the date of the first interaction
  female_ago_fl$to_i[i] = get_indexes_female$Date[last(which(get_indexes_female$Chimpanzee==i))] #and the date of the last interaction!
}

## Get the first and last presence date for each chimp 
female_pres_fl <- data.frame(Chimp = seq(1:44),
                             from_i = as.Date(rep(0,n_female)),
                             to_i = as.Date(rep(0,n_female)))

for(i in 1:n_female){
  female_pres_fl$from_i[i] = (female_presence$Date[first(which(female_presence[,i+1] ==1))])
  female_pres_fl$to_i[i] = (female_presence$Date[last(which(female_presence[,i+1] ==1))])
}




female_ago_df <- data.frame(Chimp = seq(1:44), Number_wins = rep(0,n_female), 
                            Number_los = rep(0,n_female),
                            Number_interaction = rep(0,n_female))
for(i in 1:n_female){
  female_ago_df$Number_wins[i] = sum(female_ago$Winner==i)
  female_ago_df$Number_los[i] = sum(female_ago$Loser==i)
  female_ago_df$Number_interaction[i] = sum(female_ago$Winner==i) + sum(female_ago$Loser==i)
}


for(i in 1:n_female){
  cil_female_chimp[[i]]$name.id = i
  cil_female_chimp[[i]]$node.name = i #is that correct? 
  cil_female_chimp[[i]]$name = sprintf("Female Chimp %d",i)
  cil_female_chimp[[i]]$diocese = "Gombe"
  cil_female_chimp[[i]]$di = 1
  cil_female_chimp[[i]]$lid = get_indexes_female$ID[get_indexes_female$Chimpanzee==i]   #create a list so that we can map interaction 
  cil_female_chimp[[i]]$ln = get_indexes_female$ID[get_indexes_female$Chimpanzee==i]     #same as above since we dont have two lists here.
  cil_female_chimp[[i]]$tl= year(female_ago$Date[get_indexes_female$ID[get_indexes_female$Chimpanzee==i]])   #would be a vector with the (lower) year of all the interaction 
  cil_female_chimp[[i]]$tu = year(female_ago$Date[get_indexes_female$ID[get_indexes_female$Chimpanzee==i]])  #would be a vector with the (upper) year of all the interaction (same as above since we have no uncertainity)
  cil_female_chimp[[i]]$ac = 6 #irrelevant for our analysis 
  cil_female_chimp[[i]]$ll = length(get_indexes_female$ID[get_indexes_female$Chimpanzee==i] )    #number of lists a chimp is in <- corresponds to the total number of interactions! 
  cil_female_chimp[[i]]$by = year(female_pres_fl$from_i[i])   #first interaction  or presence data instead? 
  cil_female_chimp[[i]]$till = year(female_pres_fl$to_i[i]) ##last interaction 
  cil_female_chimp[[i]]$from = year(female_pres_fl$from_i[i])   #for last four same as above? 
  cil_female_chimp[[i]]$to = year(female_pres_fl$to_i[i]) 
  cil_female_chimp[[i]]$begin = year(female_pres_fl$from_i[i]) 
  cil_female_chimp[[i]]$end = year(female_pres_fl$to_i[i]) 
}


cla_names <- c("w","id","year.1","year.2",
               "ac","allnames","ll","names","o","tl","tu","do")

n_feml_ago <- length(female_ago$Date)

cla_female_chimp <- replicate(
  n_feml_ago,
  setNames(vector("list", length(cla_names)), cla_names),
  simplify = FALSE
)

for(i in 1:n_feml_ago) {
  cla_female_chimp[[i]]$w = c(female_ago$Winner[i],female_ago$Loser[i]) #is that their id but in the other list? 
  cla_female_chimp[[i]]$id = female_ago$ID[i]
  cla_female_chimp[[i]]$year.1 = year(female_ago$Date[i])
  cla_female_chimp[[i]]$year.2 = year(female_ago$Date[i])
  cla_female_chimp[[i]]$ac = 6 #Again the rating for authetnticy
  cla_female_chimp[[i]]$allnames = "Placeholder"
  cla_female_chimp[[i]]$ll = length(c(female_ago$Winner[i],female_ago$Loser[i]) ) #What is that <- list length? 
  cla_female_chimp[[i]]$names = c(sprintf("Female Chimp %d",female_ago$Winner[i]),sprintf("Female Chimp %d",female_ago$Loser[i]))
  cla_female_chimp[[i]]$o = c(female_ago$Winner[i],female_ago$Loser[i])
  cla_female_chimp[[i]]$tl = year(female_ago$Date[i])
  cla_female_chimp[[i]]$tu = year(female_ago$Date[i])
  cla_female_chimp[[i]]$do = c(0,0)    #what is that? 
}


################################################################################
# Create male list
################################################################################
# cil each list item is a chimp 
n_male <- length(unique(male_ago$Winner))


cil_names <- c("name.id","node.name","name","diocese",
               "di","lid","ln","tl","tu","ac","ll","by","till",
               "from","to","begin","end")


cil_male_chimp <- vector("list",length = n_male)

cil_male_chimp <- replicate(
  n_male,
  setNames(vector("list", length(cil_names)), cil_names),
  simplify = FALSE
)

## Get the first and the last interaction for each chimp 
# Males 
male_ago <- male_ago %>% 
  mutate(ID =seq(1:length(male_ago$Date)))

get_indexes_male <- gather(data = male_ago,
                           key = Status, 
                           value = Chimpanzee,
                           Winner, Loser)  %>% 
  arrange(Date) 
male_ago_fl <- data.frame(Chimp = seq(1:n_male),
                          from_i = as.Date(rep(0,n_male)),
                          to_i = as.Date(rep(0,n_male)))

for(i in 1:n_male){
  male_ago_fl$from_i[i] = get_indexes_male$Date[first(which(get_indexes_male$Chimpanzee==i))]
  male_ago_fl$to_i[i] = get_indexes_male$Date[last(which(get_indexes_male$Chimpanzee==i))]
}

## Get the first and last presence date for each chimp 
male_pres_fl <- data.frame(Chimp = seq(1:n_male),
                           from_i = as.Date(rep(0,n_male)),
                           to_i = as.Date(rep(0,n_male)))

for(i in 1:n_male){
  male_pres_fl$from_i[i] = (male_presence$Date[first(which(male_presence[,i+2] ==1))]) #one more column here compared to female! 
  male_pres_fl$to_i[i] = (male_presence$Date[last(which(male_presence[,i+2] ==1))]) #one more column here compared to female!
}




male_ago_df <- data.frame(Chimp = seq(1:n_male), Number_wins = rep(0,n_male), 
                          Number_los = rep(0,n_male),
                          Number_interaction = rep(0,n_male))
for(i in 1:n_male){
  male_ago_df$Number_wins[i] = sum(male_ago$Winner==i)
  male_ago_df$Number_los[i] = sum(male_ago$Loser==i)
  male_ago_df$Number_interaction[i] = sum(male_ago$Winner==i) + sum(male_ago$Loser==i)
}


for(i in 1:n_male){
  cil_male_chimp[[i]]$name.id = i
  cil_male_chimp[[i]]$node.name = i #is that correct? 
  cil_male_chimp[[i]]$name = sprintf("Male Chimp %d",i)
  cil_male_chimp[[i]]$diocese = "Gombe"
  cil_male_chimp[[i]]$di = 1
  cil_male_chimp[[i]]$lid = get_indexes_male$ID[get_indexes_male$Chimpanzee==i]   #create a list so that we can map interaction 
  cil_male_chimp[[i]]$ln = get_indexes_male$ID[get_indexes_male$Chimpanzee==i]     #same as above since we dont have two lists here.
  cil_male_chimp[[i]]$tl= year(male_ago$Date[get_indexes_male$ID[get_indexes_male$Chimpanzee==i]])   #would be a vector with the (lower) year of all the interaction 
  cil_male_chimp[[i]]$tu = year(male_ago$Date[get_indexes_male$ID[get_indexes_male$Chimpanzee==i]])  #would be a vector with the (upper) year of all the interaction (same as above since we have no uncertainity)
  cil_male_chimp[[i]]$ac = 6 #irrelevant for our analysis 
  cil_male_chimp[[i]]$ll = length(get_indexes_male$ID[get_indexes_male$Chimpanzee==i] )    #number of lists a chimp is in <- corresponds to the total number of interactions! 
  cil_male_chimp[[i]]$by = year(male_pres_fl$from_i[i])   #first interaction  or presence data instead? 
  cil_male_chimp[[i]]$till = year(male_pres_fl$to_i[i]) ##last interaction 
  cil_male_chimp[[i]]$from = year(male_pres_fl$from_i[i])   #for last four same as above? 
  cil_male_chimp[[i]]$to = year(male_pres_fl$to_i[i]) 
  cil_male_chimp[[i]]$begin = year(male_pres_fl$from_i[i]) 
  cil_male_chimp[[i]]$end = year(male_pres_fl$to_i[i]) 
}


cla_names <- c("w","id","year.1","year.2",
               "ac","allnames","ll","names","o","tl","tu","do")

n_mal_ago <- length(male_ago$Date)

cla_male_chimp <- replicate(
  n_mal_ago,
  setNames(vector("list", length(cla_names)), cla_names),
  simplify = FALSE
)

for(i in 1:n_mal_ago) {
  cla_male_chimp[[i]]$w = c(male_ago$Winner[i],male_ago$Loser[i]) #is that their id but in the other list? 
  cla_male_chimp[[i]]$id = male_ago$ID[i]
  cla_male_chimp[[i]]$year.1 = year(male_ago$Date[i])
  cla_male_chimp[[i]]$year.2 = year(male_ago$Date[i])
  cla_male_chimp[[i]]$ac = 6 #Again the rating for authetnticy
  cla_male_chimp[[i]]$allnames = "Placeholder"
  cla_male_chimp[[i]]$ll = length(c(male_ago$Winner[i],male_ago$Loser[i]) ) #What is that <- list length? 
  cla_male_chimp[[i]]$names = c(sprintf("Male Chimp %d",male_ago$Winner[i]),sprintf("Male Chimp %d",male_ago$Loser[i]))
  cla_male_chimp[[i]]$o = c(male_ago$Winner[i],male_ago$Loser[i])
  cla_male_chimp[[i]]$tl = year(male_ago$Date[i])
  cla_male_chimp[[i]]$tu = year(male_ago$Date[i])
  cla_male_chimp[[i]]$do = c(0,0)    #what is that? 
}

## Define the restrict function 
restrict<-function(La,bil,B,E,boi,doi,b_min_list=1,max_list_length=Inf,rule="all") #change to cil and cla
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
  
  return(list(cla=cla,cil=cil))
  
}

  if(note$gender=='male') {
    cla=cla_male_chimp; cil=cil_male_chimp; doi=note$doi
  } else if(note$gender=='female'){
    cla=cla_female_chimp; cil=cil_female_chimp; doi=note$doi 
  } else{
    print('Check spelling')
}




finished=FALSE
while(!finished) {
  restricted=restrict(La=cla,bil=cil,note$B,note$E,boi=1:length(cil),doi=note$doi,b_min_list=1,max_list_length=Inf,rule=note$selectlists)
  finished=(identical(cla,restricted$cla) & identical(cil,restricted$cil))
  cla=restricted$cla
  cil=restricted$cil
  print(sprintf("num lists = %d, num bishops = %d",length(cla),length(cil)))
}
print(sprintf("Final trimmed num lists = %d, num bishops = %d",length(cla),length(cil)))

'## Remove libraries not needed to avoid conflicts/masking of other functions 
detach("package:dplyr",unload=TRUE)
detach("package:forcats",unload=TRUE)
detach("package:ggplot2",unload=TRUE)
detach("package:lubridate",unload=TRUE)
detach("package:purrr",unload=TRUE)
detach("package:readr",unload=TRUE)
detach("package:stringr",unload=TRUE)
detach("package:tibble",unload=TRUE)
detach("package:tidyr",unload=TRUE)
detach("package:tidyverse",unload=TRUE)'

