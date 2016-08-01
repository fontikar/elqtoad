
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
toadassoc <- read.csv("data/processed/toad_assoc_final.csv", stringsAsFactors = F)

#Change variable types from integers to factors

toadassoc$toad.id <- as.factor(toadassoc$toad.id)
toadassoc$sex <- as.factor(toadassoc$sex)
toadassoc$choice <- as.factor(toadassoc$choice)
toadassoc$learnt <- as.factor(toadassoc$learnt)

#Setting priors for probability models

prior.1<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Setting seeds for 3 independent chains

chains <- c(runif(3, 0, 100))

#Running 3 chains for probability of making correct choice with interaction #prior 1

toad_probcor.1 <- list()
  for(i in 1:3){
  set.seed(chains[i])
    toad_probcor.1[[i]] <- MCMCglmm(choice ~ sex*trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 110000, thin = 100, prior=prior.1, burnin = 10000, data=toadassoc, verbose= T)
  }

saveRDS(toad_probcor.1, file="output/toad_probcormod.1")

#Running probability of making correct choice without interaction #prior 1

toad_probcor.wo.1 <- list()
  for(i in 1:3){
  set.seed(chains[i])
  toad_probcor.wo.1[[i]] <- MCMCglmm(choice ~ sex+trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 110000, thin = 100, prior=prior.1, burnin = 10000, data=toadassoc, verbose= T)
  }

saveRDS(toad_probcor.wo.1, file="output/toad_probcormod.wo.1")

#Setting priors for latency model

prior.test1<- list(R = list(V =1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running log latency model with interaction

toad_lat.1 <- list()
for(i in 1:3){
  set.seed(chains[i])
  toad_lat.1[[i]]<- MCMCglmm(log.latency ~ sex*trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt = 110000, thin = 100, prior=prior.1, burnin = 10000,, data=toadassoc, verbose = T)
 }

saveRDS(toad_lat.1, file="output/toad_lat.1")

#Running log latency model without interaction

toad_lat.wo <- list()
  for(i in 1:3){
  set.seed(chains[i])
    toad_lat.wo[[i]] <- MCMCglmm(log.latency ~ sex+trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =110000, thin = 100, prior=prior.1, burnin = 10000, data=toadassoc, verbose = T)
  }

saveRDS(toad_lat.wo , file="output/toad_lat.wo")
