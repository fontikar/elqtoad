
#######################################################################################
## Manuscript: "El questro toad cognition data" 
## Authors: Martin Whiting, Fonti Kar
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.16
######################################################################################

#Set the wd in ubuntu

setwd("/home/ubuntu/gitrepo/elqtoad/")

#load library you need

library(MCMCglmm)

#Read in data
toadrev <- read.csv("data/processed/toad_rev_final.csv", stringsAsFactors = F)

#Change variable types from integers to factors

toadrev$toad.id <- as.factor(toadrev$toad.id)
toadrev$sex <- as.factor(toadrev$sex)
toadrev$choice <- as.factor(toadrev$choice)
toadrev$learnt <- as.factor(toadrev$learnt)

#Setting priors

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Setting seeds for 3 independent chains

chains <- c(runif(3, 0, 100))

#Running probability of making correct choice with interaction

rev_probcor.1 <- list()
  for(i in 1:3){
  set.seed(chains[i])
    rev_probcor.1[[i]] <- MCMCglmm(choice ~ sex*trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=toadrev, verbose= T)
  }

saveRDS(rev_probcor.1, file="output/toad_rev_probcormod.1")

#Running probability of making correct choice with interaction

rev_probcor.wo <- list()
  for(i in 1:3){
  set.seed(chains[i])
    rev_probcor.wo[[i]] <- MCMCglmm(choice ~ sex+trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=toadrev, verbose= T)
  }

saveRDS(rev_probcor.wo, file="output/toad_rev_probcormod.wo")

#Setting priors for latency model

prior.test1<- list(R = list(V =1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running log latency model with interaction

toad_lat.rev <- list()
  for(i in 1:3){
  set.seed(chains[i])
    toad_lat.rev[[i]] <- MCMCglmm(log.latency ~ sex*trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =2000000, thin = 1000, prior=prior.test1, burnin = 10000, data=toadrev, verbose = T)
  }

saveRDS(toad_lat.rev, file="output/toad_rev_lat.1")

#Running log latency model without interaction

toad_lat.rev.wo <- list()
  for(i in 1:3){
  set.seed(chains[i])
    toad_lat.rev.wo[[i]] <- MCMCglmm(log.latency ~ sex+trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =2000000, thin = 1000, prior=prior.test1, burnin = 10000, data=toadrev, verbose = T)
  }

saveRDS(toad_lat.rev.wo , file="output/toad_rev_lat.wo")