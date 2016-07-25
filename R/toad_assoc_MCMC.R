
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

#Setting priors

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

#Running probability of making correct choice with interaction

toad_probcor.1<-MCMCglmm(choice ~ sex*trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=toadassoc, verbose= T)

saveRDS(toad_probcor.1, file="output/toad_probcormod.1")

#Running probability of making correct choice with interaction

toad_probcor.wo<-MCMCglmm(choice ~ sex+trial, random = ~us(1+trial):toad.id, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=toadassoc, verbose= T)

saveRDS(toad_probcor.wo, file="output/toad_probcormod.wo")