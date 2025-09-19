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

## Remove libraries not needed to avoid conflicts/masking of other functions 
'detach("package:dplyr",unload=TRUE)
detach("package:forcats",unload=TRUE)
detach("package:ggplot2",unload=TRUE)
detach("package:lubridate",unload=TRUE)
detach("package:purrr",unload=TRUE)
detach("package:readr",unload=TRUE)
detach("package:stringr",unload=TRUE)
detach("package:tibble",unload=TRUE)
detach("package:tidyr",unload=TRUE)
detach("package:tidyverse",unload=TRUE)
'
