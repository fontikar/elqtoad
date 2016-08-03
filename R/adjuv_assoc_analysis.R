
#######################################################################################
## Manuscript: "El questro toad cognition data" 
## Authors: Martin Whiting, Fonti Kar
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.16
## Notes: Comparing between adults and juveniles
######################################################################################

#Setting working directory

setwd("/Users/fontikar/Dropbox/Toad cognition - El Questro Fonti/")

#Clear R memory

rm(list=ls())

#Load libraries
library(MASS)
library(plyr)
library(lme4)
library(MCMCglmm)

#Read in adult data
adultassoc <- read.csv("data/processed/toad_assoc_final.csv", stringsAsFactors = F)
str(adultassoc) #227 obs

#Age variable
adultassoc$age <- "1"

#Read in juvenile data
juvassoc <- read.csv("data/processed/juv_assoc_final.csv", stringsAsFactors = F)
str(juvassoc) #198 obs

#Age variable in juvs
juvassoc$age <- 0

#Creating log.latency variable
hist(juvassoc$latency)
hist(log(juvassoc$latency))

juvassoc$log.latency <- log(juvassoc$latency)
shapiro.test(juvassoc$log.latency)

#Names of variables in each dataframe
names(adultassoc)
names(juvassoc)

#Column bind the two dataframes
adjuv.assoc <- rbind(adultassoc, juvassoc)
str(adjuv.assoc)

#Change variable types 
adjuv.assoc$toad.id <- as.factor(adjuv.assoc$toad.id)
adjuv.assoc$age <- as.factor(adjuv.assoc$age)
adjuv.assoc$choice <- as.factor(adjuv.assoc$choice)
adjuv.assoc$learnt <- as.factor(adjuv.assoc$learnt)

#Having a look at the data
head(adjuv.assoc) ; tail(adjuv.assoc)

write.csv(adjuv.assoc, file = 'data/processed/combined_assoc_final.csv', row.names =  F)

#######################################################################################
## ANALYSIS 1 - Trials taken to learn the association task
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object "data_learnt" ; 425 obs, 10 var, 32 individuals (2 toads died before learning)
######################################################################################

#Read in data
adjuv.assoc <- read.csv('data/processed/combined_assoc_final.csv')
str(adjuv.assoc)
length(unique(adjuv.assoc$toad.id))

#Descriptive stats
length(unique(adjuv.assoc[adjuv.assoc$age == 1,1]))
length(unique(adjuv.assoc[adjuv.assoc$age == 0,1]))

#Subset indivudals that have learnt the task
data_learnt <- adjuv.assoc[adjuv.assoc$learnt == 1,]

data_learnt$toad.id <- as.factor(data_learnt$toad.id)
length(unique(data_learnt$toad.id)) == 32 #32 toads learnt

#Descriptive stats
length(unique(data_learnt[data_learnt$age == 1,1]))
length(unique(data_learnt[data_learnt$age == 0,1]))

#Create dataframe using data_learnt, split by toad.id and age and add up lt variable for each individual. Length of lt variable = number of trials taken to learn 

trialdat <-ddply(.data=data_learnt, .(toad.id, age), summarise, trials_to_learn=sum((lt)))
trialdat
trialdat$age <- as.factor(trialdat$age)
str(trialdat)
  
#Fit a GLM with negative binomial errors

adj.fit.1 <- glm.nb(trials_to_learn ~ age, data= trialdat)
summary(adj.fit.1 ) #slight differences

round(summary(adj.fit.1)$coef,2)

#Set up prediction grid

newdata.1 = data.frame(age = factor(c(1,0)))
str(newdata.1)

#This code tells the model to predict from our model what is the number of trials taken to learn for males and females 
pred.1 <- predict(adj.fit.1, type="response", newdata=newdata.1, se.fit = T)

#Adding predictions and se.fit to my newdata.1 dataframe 
newdata.1$pred <- pred.1$fit
newdata.1$se.fit <- pred.1$se.fit

#Creating upper and lower standard error limit
newdata.1$se.u <- pred.1$fit + pred.1$se.fit
newdata.1$se.l <- pred.1$fit - pred.1$se.fit

#Plotting these predictions and sving as pdf

pdf("output/fig/Association_agediff.pdf", 7.5, 9)

barplot(newdata.1$pred <- pred.1$fit, ylim = c(0,20), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5, axisnames = F) 
box()

#Axes labels

title(ylab = list("Mean number of trials taken to learn", cex=1.5), line = 3)
mtext("Adults", at= 1, side = 1, line = 1.2 ,cex = 1.5)
mtext("Juveniles", at=2.5, side = 1,line =1.2, cex= 1.5)

#Error bars

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = newdata.1$pred, x1 = up.x, y1 = newdata.1$se.u, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = newdata.1$pred, x1 =low.x, y1 = newdata.1$se.l, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=17.5, x1 = 2.6, y1 =17.5, lwd= 2)
text(x=1.8, y=18, labels="P = 0.0683", cex=1.5, font=1)

dev.off()

#######################################################################################
## ANALYSIS 1b - Number of errors
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Read in data
adjuv.assoc <- read.csv('data/processed/combined_assoc_final.csv')
str(adjuv.assoc)

incordat <- ddply(.data=adjuv.assoc, .(toad.id, age), summarise, incorrect_choice=sum((choice==0)), total=length(choice))

incordat$age <- as.factor(incordat$age)
str(incordat)

incorfit<-glm.nb(incorrect_choice~age, data=incordat)
summary(incorfit)

round(summary(incorfit)$coef,2)

#######################################################################################
## ANALYSIS 2a - 3 chain MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

MCMC.chains <- function(path){
  imp <- readRDS(path)
  #Import chains
  VCV.list <- mcmc.list(lapply(imp, function(x) x$VCV))
  Sol.list <- mcmc.list(lapply(imp, function(x) x$Sol))
  
  #Combine chains
  VCV.mode <- MCMCglmm::posterior.mode(as.mcmc(plyr::ldply(VCV.list)))
  VCV.HPD <- coda::HPDinterval(as.mcmc(plyr::ldply(VCV.list)))
  
  Sol.mode <- MCMCglmm::posterior.mode(as.mcmc(plyr::ldply(Sol.list)))
  Sol.HPD <- coda::HPDinterval(as.mcmc(plyr::ldply(Sol.list)))
  
  return(list(solVCVlist = list(VCV = VCV.list, Sol = Sol.list ), solVCVchain = list(VCV = cbind(VCV.mode, VCV.HPD), Sol = cbind( Sol.mode, Sol.HPD))))
}

##########################################################################

adjuv_assoc_lat.1 <- MCMC.chains("output/adjuv_assoc_lat.1")

names(adjuv_assoc_lat.1)

plot(adjuv_assoc_lat.1$solVCVlist$Sol[[1]])
plot(adjuv_assoc_lat.1$solVCVlist$Sol[[2]])
plot(adjuv_assoc_lat.1$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[3]])

heidel.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[1]])
heidel.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[2]])
heidel.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[3]])

geweke.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[1]])
geweke.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[2]])
geweke.diag(adjuv_assoc_lat.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_assoc_lat.1$solVCVlist$Sol[[1]], adjuv_assoc_lat.1$solVCVlist$Sol[[2]], adjuv_assoc_lat.1$solVCVlist$Sol[[3]]))

adjuv_assoc_lat.1$solVCVchain$Sol

##########################################################################
#Table 1 in manuscript

Table1_Assoc <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(Table1_Assoc)[1:3] <- "Probcor"
colnames(Table1_Assoc)[4:6] <- "Latency"

rownames(Table1_Assoc) <- c("Intercept", "Age (Adult)", "Trial", "Age:Trial")

 Table1_Assoc[4:6] <- round(adjuv_assoc_lat.1$solVCVchain$Sol,2)

##########################################################################
#Make predictions from adjuv_assoc_lat.1

adjuv_assoc_lat.1 <- MCMC.chains("output/adjuv_assoc_lat.1")
adjuv_assoc_lat.1$solVCVchain$Sol

newdata = data.frame(age = rep(c(1,0), each=35),
                   trial = seq(1,35))

str(newdata)


X <-model.matrix(~age*trial, data=newdata)

beta <- adjuv_assoc_lat.1$solVCVchain$Sol[,1]

######################################################

newdata$pred_latent <- X %*% beta #Predictions on log scale
newdata$pred_orig  <- exp(newdata$pred_latent) #back-transforming because response is LOG latency
round(newdata$pred_orig,2) == round(exp(newdata$pred_latent),2) #Double checking

######################################################

covbeta <- var(ldply(adjuv_assoc_lat.1$solVCVlist$Sol))

newdata$se    <- sqrt(diag(X %*% covbeta %*% t(X)))
newdata$up  <- exp(newdata$pred_latent + 1.96 *newdata$se) 
newdata$low  <- exp(newdata$pred_latent - 1.96 *newdata$se) 
newdata

adultdata <- newdata[newdata$age == "1",]
juvdata <- newdata[newdata$age == "0",]

plot(adultdata$pred_orig~adultdata$trial, ylim =c(40, 160), xlim =c(1,35),pch=19, bg= "Black", cex.axis=1.5, cex.lab=1.5, ylab= "Latency in making correct choice (s)",  xlab= "Trial Number", las=1, type = 'n')

#Adult

lines(spline(adultdata$pred_orig~adultdata$trial), lwd=2.5)

#CIs
lines(spline(adultdata$up~adultdata$trial), lwd=1.2)
lines(spline(adultdata$low~adultdata$trial), lwd=1.2)

#Juvenile

lines(spline(juvdata$pred_orig~juvdata$trial), lwd=2, lty=2.5)

#CIs
lines(spline(juvdata$up~juvdata$trial),lwd=2, lty=3)
lines(spline(juvdata$low~juvdata$trial),lwd=2, lty=3)

##########################################################################

adjuv_assoc_lat.wo <- MCMC.chains("output/adjuv_assoc_lat.wo") #We don't need this model as interaction was significant in previous model

names(adjuv_assoc_lat.wo)

plot(adjuv_assoc_lat.wo$solVCVlist$Sol[[1]])
plot(adjuv_assoc_lat.wo$solVCVlist$Sol[[2]])
plot(adjuv_assoc_lat.wo$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[3]])

heidel.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[1]])
heidel.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[2]])
heidel.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[3]])

geweke.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[1]])
geweke.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[2]])
geweke.diag(adjuv_assoc_lat.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_assoc_lat.wo$solVCVlist$Sol[[1]], adjuv_assoc_lat.wo$solVCVlist$Sol[[2]], adjuv_assoc_lat.wo$solVCVlist$Sol[[3]]))

adjuv_assoc_lat.wo$solVCVchain$Sol

##########################################################################

adjuv_assoc_probcormod.1 <- MCMC.chains("output/adjuv_assoc_probcormod.1")

names(adjuv_assoc_probcormod.1)

plot(adjuv_assoc_probcormod.1$solVCVlist$Sol[[1]])
plot(adjuv_assoc_probcormod.1$solVCVlist$Sol[[2]])
plot(adjuv_assoc_probcormod.1$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[3]])

heidel.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[1]])
heidel.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[2]])
heidel.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[3]])

geweke.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[1]])
geweke.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[2]])
geweke.diag(adjuv_assoc_probcormod.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_assoc_probcormod.1$solVCVlist$Sol[[1]], adjuv_assoc_probcormod.1$solVCVlist$Sol[[2]], adjuv_assoc_probcormod.1$solVCVlist$Sol[[3]]))

adjuv_assoc_probcormod.1$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

Table1_Assoc[4,1:3] <- round(adjuv_assoc_probcormod.1$solVCVchain$Sol,2)[4,]

##########################################################################

adjuv_assoc_probcormod.wo <- MCMC.chains("output/adjuv_assoc_probcormod.wo")

names(adjuv_assoc_probcormod.wo)

plot(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[1]])
plot(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[2]])
plot(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[3]])

heidel.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[1]])
heidel.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[2]])
heidel.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[3]])

geweke.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[1]])
geweke.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[2]])
geweke.diag(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_assoc_probcormod.wo$solVCVlist$Sol[[1]], adjuv_assoc_probcormod.wo$solVCVlist$Sol[[2]], adjuv_assoc_probcormod.wo$solVCVlist$Sol[[3]]))

adjuv_assoc_probcormod.wo$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

Table1_Assoc[1:3,1:3] <- round(adjuv_assoc_probcormod.wo$solVCVchain$Sol,2)

write.csv(Table1_Assoc, file = 'output/table/Table1_association.csv')

#######################################################################################
## ANALYSIS 2b - 1 chain MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Load the model output of probability of making the correct choice with interaction between age and trial
adjuv_assoc_probcormod.1 <- readRDS("output/adjuv_assoc_probcormod.1")

#Trace plot - model diagnostics
plot(adjuv_assoc_probcormod.1)
heidel.diag(adjuv_assoc_probcormod.1$Sol)
geweke.diag(adjuv_assoc_probcormod.1$Sol)

#Summary of parameters
summary(adjuv_assoc_probcormod.1)

#Posterior modes of fixed effects
posterior.mode(adjuv_assoc_probcormod.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_assoc_probcormod.1$Sol) #No differences between age on each trial

#Load the model output of probability of making the correct choice withOUT interaction between age and trial
adjuv_assoc_probcormod.wo <- readRDS("output/adjuv_assoc_probcormod.wo")

#Trace plot - model diagnostics
plot(adjuv_assoc_probcormod.wo)
heidel.diag(adjuv_assoc_probcormod.wo$Sol)
geweke.diag(adjuv_assoc_probcormod.wo$Sol)

#Summary of parameters
summary(adjuv_assoc_probcormod.wo)

#Posterior modes of fixed effects
posterior.mode(adjuv_assoc_probcormod.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_assoc_probcormod.wo$Sol) #No differences between age and trial on each trial

#Load the model output of latency to reach correct arm with interaction between age and trial

adjuv_assoc_lat.1 <- readRDS("output/adjuv_assoc_lat.1")

#Trace plot - model diagnostics
plot(adjuv_assoc_lat.1)
heidel.diag(adjuv_assoc_lat.1)
geweke.diag(adjuv_assoc_lat.1)

#Summary of parameters
summary(adjuv_assoc_lat.1)

#Posterior modes of fixed effects
posterior.mode(adjuv_assoc_lat.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_assoc_lat.1$Sol) #There are differences betwee ages on each trial

#Model predictions for latency model with interaction

adjuv_newdata=data.frame(age = rep(c(1,0), each=35),
                          trial = c(1:35))
                         
X <-model.matrix(~age*trial, data=adjuv_newdata)
beta <-colMeans(adjuv_assoc_lat.1$Sol)

adjuv_newdata$pred_latent <- X %*% beta
adjuv_newdata$pred_orig <- exp(adjuv_newdata$pred_latent)

covbeta <- var(adjuv_assoc_lat.1$Sol)

adjuv_newdata$se_latent <- sqrt(diag(X %*% covbeta %*% t(X)))

adjuv_newdata$upCI_orig <- exp(adjuv_newdata$pred_latent) + 1.96*exp(adjuv_newdata$se_latent)

adjuv_newdata$lowCI_orig  <- exp(adjuv_newdata$pred_latent) - 1.96*exp(adjuv_newdata$se_latent)

####Plotting this

par(mfrow=c(1,1))

adultdat <- adjuv_newdata[adjuv_newdata$age == 1,]
juvdat <- adjuv_newdata[adjuv_newdata$age == 0,]

min(adjuv_newdata$pred_orig)
max(adjuv_newdata$pred_orig)

plot(adultdat$pred_orig~adultdat$trial, type = "n", ylim = c(45, 150), ylab="Latency to reach correct arm", xlab="Trial number")

lines(adultdat$pred_orig~adultdat$trial, lwd=3)
lines(adultdat$upCI_orig~adultdat$trial, lwd=1.5)
lines(adultdat$lowCI_orig~adultdat$trial, lwd=1.5)

lines(juvdat$pred_orig~juvdat$trial, lty=2, lwd=3)
lines(juvdat$upCI_orig~juvdat$trial, lty=3, lwd=2)
lines(juvdat$lowCI_orig~juvdat$trial, lty=3, lwd=2)


