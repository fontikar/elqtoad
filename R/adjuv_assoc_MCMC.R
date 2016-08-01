
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
adjuv.assoc <- read.csv('data/processed/combined_assoc_final.csv')

#Change variable types 
adjuv.assoc$toad.id <- as.factor(adjuv.assoc$toad.id)
adjuv.assoc$age <- as.factor(adjuv.assoc$age)
adjuv.assoc$choice <- as.factor(adjuv.assoc$choice)
adjuv.assoc$learnt <- as.factor(adjuv.assoc$learnt)

#Setting priors for probability models

prior.1<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Setting seeds for 3 independent chains

chains <- c(runif(3, 0, 100))

#Running probability of making correct choice with interaction

adjuv_probcor.1 <- list()
 for(i in 1:3){
  set.seed(chains[i])
   adjuv_probcor.1[[i]] <- MCMCglmm(choice ~ age*trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 110000, thin = 100, prior=prior.1, burnin = 10000, data=adjuv.assoc, verbose= T)
 }

saveRDS(adjuv_probcor.1, file="output/adjuv_assoc_probcormod.1")

#Running probability of making correct choice without interaction

adjuv_probcor.wo <- list()
 for(i in 1:3){
  set.seed(chains[i])
   adjuv_probcor.wo[[i]] <- MCMCglmm(choice ~ age+trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 110000, thin = 100, prior=prior.1, burnin = 10000, data=adjuv.assoc, verbose= T)
 }

saveRDS(adjuv_probcor.wo, file="output/adjuv_assoc_probcormod.wo")

#Setting priors for latency model

prior.test1 <- list(R = list(V =1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running log latency model with interaction

adjuv_lat.1 <- list()
  for(i in 1:3){
  set.seed(chains[i])
    adjuv_lat.1[[i]] <- MCMCglmm(log.latency ~ age*trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =110000, thin = 100, prior=prior.test1, burnin = 10000, data=adjuv.assoc, verbose = T)
  }

saveRDS(adjuv_lat.1, file="output/adjuv_assoc_lat.1")

#Running log latency model without interaction

adjuv_lat.wo <- list()
  for(i in 1:3){
  set.seed(chains[i])
    adjuv_lat.wo[[i]] <- MCMCglmm(log.latency ~ age+trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =110000, thin = 100, prior=prior.test1, burnin = 10000, data=adjuv.assoc, verbose = T)
  }

saveRDS(adjuv_lat.wo, file="output/adjuv_assoc_lat.wo")

