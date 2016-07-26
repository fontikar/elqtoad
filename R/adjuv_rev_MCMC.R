
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
adjuv.rev <- read.csv('data/processed/combined_rev_final.csv')

#Change variable types 
adjuv.rev$toad.id <- as.factor(adjuv.rev$toad.id)
adjuv.rev$age <- as.factor(adjuv.rev$age)
adjuv.rev$choice <- as.factor(adjuv.rev$choice)
adjuv.rev$learnt <- as.factor(adjuv.rev$learnt)

#Setting priors for probability models

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running probability of making correct choice with interaction

adjuv_rev_probcor.1<-MCMCglmm(choice ~ sex*trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=adjuv.rev, verbose= T)

saveRDS(adjuv_rev_probcor.1, file="output/adjuv_rev_probcormod.1")

#Running probability of making correct choice without interaction

adjuv_rev_probcor.wo <-MCMCglmm(choice ~ sex+trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=adjuv.rev, verbose= T)

saveRDS(adjuv_rev_probcor.wo, file="output/adjuv_rev_probcormod.wo")

#Setting priors for latency model

prior.test1<- list(R = list(V =1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running log latency model with interaction

adjuv_rev_lat.1 <- MCMCglmm(log.latency ~ sex*trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =2000000, thin = 1000, prior=prior.test1, burnin = 10000, data=adjuv.rev, verbose = T)

saveRDS(adjuv_rev_lat.1, file="output/adjuv_rev_lat.1")

#Running log latency model without interaction

adjuv_rev_lat.wo <- MCMCglmm(log.latency ~ sex+trial, random = ~us(1+trial):toad.id, family = "gaussian", nitt =2000000, thin = 1000, prior=prior.test1, burnin = 10000, data=adjuv.rev, verbose = T)

saveRDS(adjuv_rev_lat.wo, file="output/adjuv_rev_lat.wo")
