
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
adultrev <- read.csv("data/processed/toad_rev_final.csv", stringsAsFactors = F)

str(adultrev) #322 obs

#Age variable
adultrev$age <- "1"

#Read in juvenile data
juvrev <- read.csv("data/processed/juv_rev_final.csv", stringsAsFactors = F)
str(juvrev) #201 obs

#Age variable in juvs
juvrev$age <- 0

#Creating log.latency variable
hist(juvrev$latency)
hist(log(juvrev$latency))

juvrev$log.latency <- log(juvrev$latency)
shapiro.test(juvrev$log.latency)

#Names of variables in each dataframe
names(adultrev)
names(juvrev)

#Column bind the two dataframes
adjuv.rev <- rbind(adultrev, juvrev)
str(adjuv.rev)

#Change variable types 
adjuv.rev$toad.id <- as.factor(adjuv.rev$toad.id)
adjuv.rev$age <- as.factor(adjuv.rev$age)
adjuv.rev$choice <- as.factor(adjuv.rev$choice)
adjuv.rev$learnt <- as.factor(adjuv.rev$learnt)

#Having a look at the data
head(adjuv.rev) ; tail(adjuv.rev)

write.csv(adjuv.rev, file = 'data/processed/combined_rev_final.csv', row.names =  F)

#######################################################################################
## ANALYSIS 1 - Trials taken to learn the reversal task
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object "data_learnt" ; 523 obs, 10 var, 32 individuals, 2 didn't learn

######################################################################################

#Read in data
adjuv.rev <- read.csv('data/processed/combined_rev_final.csv')
str(adjuv.rev) 

#Descriptive stats

length(unique(adjuv.rev[adjuv.rev$age == "1",1]))
length(unique(adjuv.rev[adjuv.rev$age == "0",1]))

#Subset indivudals that have learnt the task
rev_learnt <- adjuv.rev[adjuv.rev$learnt == 1,]
rev_learnt$toad.id <- as.factor(rev_learnt$toad.id)
length(unique(rev_learnt$toad.id)) == 30 #30 toads learnt

str(rev_learnt)

#Descriptive stats

length(unique(rev_learnt[rev_learnt$age == "1",1]))
length(unique(rev_learnt[rev_learnt$age == "0",1]))

#Create dataframe using data_learnt, split by toad.id and age and add up lt variable for each individual. Length of lt variable = number of trials taken to learn 

trialdat <-ddply(.data=rev_learnt, .(toad.id, age), summarise, trials_to_learn=sum((lt)))
trialdat
trialdat$age <- as.factor(trialdat$age)
str(trialdat)

#Fit a GLM with negative binomial errors

adj.fit.rev <- glm.nb(trials_to_learn ~ age, data= trialdat)
summary(adj.fit.rev) #No differences between age

round(summary(adj.fit.rev)$coeff,2)

#Set up prediction grid

newdata.2 = data.frame(age = factor(c(1,0)))
str(newdata.2)

#This code tells the model to predict from our model what is the number of trials taken to learn for males and females 
pred.1 <- predict(adj.fit.rev, type="response", newdata=newdata.2, se.fit = T)

#Adding predictions and se.fit to my newdata.1 dataframe 
newdata.2$pred <- pred.1$fit
newdata.2$se.fit <- pred.1$se.fit

#Creating upper and lower standard error limit
newdata.2$se.u <- pred.1$fit + pred.1$se.fit
newdata.2$se.l <- pred.1$fit - pred.1$se.fit

#Plotting these predictions and sving as pdf

pdf("output/fig/Reversal_agediff.pdf", 7.5, 9)

barplot(newdata.2$pred <- pred.1$fit, ylim = c(0,22), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5, axisnames = F) 
box()

#Axes labels

title(ylab = list("Mean number of trials taken to learn", cex=1.5), line = 3)
mtext("Adults", at= 1, side = 1, line = 1.2 ,cex = 1.5)
mtext("Juveniles", at=2.5, side = 1,line =1.2, cex= 1.5)

#Error bars

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = newdata.2$pred, x1 = up.x, y1 = newdata.2$se.u, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = newdata.2$pred, x1 =low.x, y1 = newdata.2$se.l, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=18.7, x1 = 2.6, y1 =18.7, lwd= 2)
text(x=1.8, y=19.7, labels="n.s.", cex=1.5, font=1)

dev.off()

#######################################################################################
## ANALYSIS 1b - Number of errors
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Read in data
adjuv.rev <- read.csv('data/processed/combined_rev_final.csv')
str(adjuv.rev)

incordat <- ddply(.data=adjuv.rev, .(toad.id, age), summarise, incorrect_choice=sum((choice==0)), total=length(choice))

incordat$age <- as.factor(incordat$age)
str(incordat)

incorfit<-glm.nb(incorrect_choice~age, data=incordat)
summary(incorfit)

round(summary(incorfit)$coef,2)

#######################################################################################
## ANALYSIS 2a - 3 chains MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object "toadassoc" ; 227 obs, 11 var.
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

adjuv_rev_lat.1 <- MCMC.chains("output/adjuv_rev_lat.1")

names(adjuv_rev_lat.1)

plot(adjuv_rev_lat.1$solVCVlist$Sol[[1]])
plot(adjuv_rev_lat.1$solVCVlist$Sol[[2]])
plot(adjuv_rev_lat.1$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_rev_lat.1$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_rev_lat.1$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_rev_lat.1$solVCVlist$Sol[[3]])

heidel.diag(adjuv_rev_lat.1$solVCVlist$Sol[[1]])
heidel.diag(adjuv_rev_lat.1$solVCVlist$Sol[[2]])
heidel.diag(adjuv_rev_lat.1$solVCVlist$Sol[[3]])

geweke.diag(adjuv_rev_lat.1$solVCVlist$Sol[[1]])
geweke.diag(adjuv_rev_lat.1$solVCVlist$Sol[[2]])
geweke.diag(adjuv_rev_lat.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_rev_lat.1$solVCVlist$Sol[[1]], adjuv_rev_lat.1$solVCVlist$Sol[[2]], adjuv_rev_lat.1$solVCVlist$Sol[[3]]))

adjuv_rev_lat.1$solVCVchain$Sol

##########################################################################
#Table 1 in manuscript

Table1_Reversal <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(Table1_Reversal)[1:3] <- "Probcor"
colnames(Table1_Reversal)[4:6] <- "Latency"

rownames(Table1_Reversal) <- c("Intercept", "Age (Adult)", "Trial", "Age:Trial")

Table1_Reversal[4,4:6] <- round(adjuv_rev_lat.1$solVCVchain$Sol,2)[4,]

######################################################################################

adjuv_rev_lat.wo <- MCMC.chains("output/adjuv_rev_lat.wo")

names(adjuv_rev_lat.wo)

plot(adjuv_rev_lat.wo$solVCVlist$Sol[[1]])
plot(adjuv_rev_lat.wo$solVCVlist$Sol[[2]])
plot(adjuv_rev_lat.wo$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[3]])

heidel.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[1]])
heidel.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[2]])
heidel.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[3]])

geweke.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[1]])
geweke.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[2]])
geweke.diag(adjuv_rev_lat.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_rev_lat.wo$solVCVlist$Sol[[1]], adjuv_rev_lat.wo$solVCVlist$Sol[[2]], adjuv_rev_lat.wo$solVCVlist$Sol[[3]]))

adjuv_rev_lat.wo$solVCVchain$Sol

#Make predictions from adjuv_rev_lat.wo

adjuv_rev_lat.wo <- MCMC.chains("output/adjuv_rev_lat.wo")

adjuv_rev_lat.wo$solVCVchain$Sol

newdata = data.frame(age = rep(c(1,0), each=37),
                     trial = seq(1,37))

str(newdata)


X <-model.matrix(~age+trial, data=newdata)

beta <- adjuv_rev_lat.wo$solVCVchain$Sol[,1]

######################################################

newdata$pred_latent <- X %*% beta #Predictions on log scale
newdata$pred_orig  <- exp(newdata$pred_latent) #back-transforming because response is LOG latency
round(newdata$pred_orig,2) == round(exp(newdata$pred_latent),2) #Double checking

######################################################

covbeta <- var(ldply(adjuv_rev_lat.wo$solVCVlist$Sol))

newdata$se    <- sqrt(diag(X %*% covbeta %*% t(X)))
newdata$up  <- exp(newdata$pred_latent + 1.96 *newdata$se) 
newdata$low  <- exp(newdata$pred_latent - 1.96 *newdata$se) 
newdata

adultdata <- newdata[newdata$age == "1",]
juvdata <- newdata[newdata$age == "0",]

plot(adultdata$pred_orig~adultdata$trial, ylim = c(0, 200), xlim =c(1,37),pch=19, bg= "Black", cex.axis=1.5, cex.lab=1.5, ylab= "Latency in making correct choice (s)",  xlab= "Trial Number", las=1, type = 'n')

#Adult

lines(spline(adultdata$pred_orig~adultdata$trial), lwd=2.5)

#CIs
lines(spline(adultdata$up~adultdata$trial), lwd=1.2)
lines(spline(adultdata$low~adultdata$trial), lwd=1.2)

#Juveniles
lines(spline(juvdata$pred_orig~juvdata$trial), lwd=2, lty=2.5)

#CIs
lines(spline(juvdata$up~juvdata$trial),lwd=2, lty=3)
lines(spline(juvdata$low~juvdata$trial),lwd=2, lty=3)

##########################################################################
#Table 1 in manuscript

Table1_Reversal[1:3,4:6] <- round(adjuv_rev_lat.wo$solVCVchain$Sol,2)

##########################################################################

adjuv_rev_probcormod.1 <- MCMC.chains("output/adjuv_rev_probcormod.1")

names(adjuv_rev_probcormod.1)

plot(adjuv_rev_probcormod.1$solVCVlist$Sol[[1]])
plot(adjuv_rev_probcormod.1$solVCVlist$Sol[[2]])
plot(adjuv_rev_probcormod.1$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[3]])

heidel.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[1]])
heidel.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[2]])
heidel.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[3]])

geweke.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[1]])
geweke.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[2]])
geweke.diag(adjuv_rev_probcormod.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_rev_probcormod.1$solVCVlist$Sol[[1]], adjuv_rev_probcormod.1$solVCVlist$Sol[[2]], adjuv_rev_probcormod.1$solVCVlist$Sol[[3]]))

adjuv_rev_probcormod.1$solVCVchain$Sol

##########################################################################
#Table 1 in manuscript

Table1_Reversal[4,1:3] <- round(adjuv_rev_probcormod.1$solVCVchain$Sol,2)[4,]

######################################################################################

adjuv_rev_probcormod.wo <- MCMC.chains("output/adjuv_rev_probcormod.wo")

names(adjuv_rev_probcormod.wo)

plot(adjuv_rev_probcormod.wo$solVCVlist$Sol[[1]])
plot(adjuv_rev_probcormod.wo$solVCVlist$Sol[[2]])
plot(adjuv_rev_probcormod.wo$solVCVlist$Sol[[3]])

autocorr.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[1]])
autocorr.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[2]])
autocorr.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[3]])

heidel.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[1]])
heidel.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[2]])
heidel.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[3]])

geweke.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[1]])
geweke.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[2]])
geweke.diag(adjuv_rev_probcormod.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(adjuv_rev_probcormod.wo$solVCVlist$Sol[[1]], adjuv_rev_probcormod.wo$solVCVlist$Sol[[2]], adjuv_rev_probcormod.wo$solVCVlist$Sol[[3]]))

adjuv_rev_probcormod.wo$solVCVchain$Sol 

##########################################################################
#Table 1 in manuscript

Table1_Reversal[1:3,1:3] <- round(adjuv_rev_probcormod.wo$solVCVchain$Sol,2)

write.csv(Table1_Reversal, file = "output/table/Table1_reversal.csv")

#######################################################################################
## ANALYSIS 2b - 1 chains MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Load the model output of probability of making the correct choice with interaction between age and trial
adjuv_rev_probcormod.1 <- readRDS("output/adjuv_rev_probcormod.1")

#Trace plot - model diagnostics
plot(adjuv_rev_probcormod.1)
heidel.diag()
geweke.diag()

#Summary of parameters
summary(adjuv_rev_probcormod.1)

#Posterior modes of fixed effects
posterior.mode(adjuv_rev_probcormod.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_rev_probcormod.1$Sol) #No differences between age on each trial

#Load the model output of probability of making the correct choice withOUT interaction between age and trial
adjuv_rev_probcormod.wo <- readRDS("output/adjuv_rev_probcormod.wo")

#Trace plot - model diagnostics
plot(adjuv_rev_probcormod.wo)
heidel.diag()
geweke.diag()

#Summary of parameters
summary(adjuv_rev_probcormod.wo)

#Posterior modes of fixed effects
posterior.mode(adjuv_rev_probcormod.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_rev_probcormod.wo$Sol) #No differences between age and trial on each trial

#Load the model output of latency to reach correct arm with interaction between age and trial

adjuv_rev_lat.1 <- readRDS("output/adjuv_rev_lat.1")

#Trace plot - model diagnostics
plot(adjuv_rev_lat.1)
heidel.diag()
geweke.diag()

#Summary of parameters
summary(adjuv_rev_lat.1)

#Posterior modes of fixed effects
posterior.mode(adjuv_rev_lat.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_rev_lat.1$Sol) #No effect of interaction

#Load the model output of latency to reach correct arm without interaction between age and trial

adjuv_rev_lat.wo <- readRDS("output/adjuv_rev_lat.wo")

#Trace plot - model diagnostics
plot(adjuv_rev_lat.wo)
heidel.diag(adjuv_rev_lat.wo)
geweke.diag(adjuv_rev_lat.wo)

#Summary of parameters
summary(adjuv_rev_lat.wo)

#Posterior modes of fixed effects
posterior.mode(adjuv_rev_lat.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(adjuv_rev_lat.wo$Sol) #No interaction effect but there are differences between age

#Model predictions for latency without the interaction

adjuv_rev_newdata=data.frame(age = rep(c(1,0), each=37),
                         trial = c(1:37))

X <-model.matrix(~age+trial, data=adjuv_rev_newdata)

beta <-colMeans(adjuv_rev_lat.wo$Sol)

adjuv_rev_newdata$pred_latent <- X %*% beta
adjuv_rev_newdata$pred_orig <- exp(adjuv_rev_newdata$pred_latent)

covbeta <- var(adjuv_rev_lat.wo$Sol)

adjuv_rev_newdata$se_latent <- sqrt(diag(X %*% covbeta %*% t(X)))
adjuv_rev_newdata$se_orig <- exp(adjuv_rev_newdata$se_latent)


adjuv_rev_newdata$upCI_orig <- adjuv_rev_newdata$pred_orig + 1.96*adjuv_rev_newdata$se_orig

adjuv_rev_newdata$lowCI_orig  <- adjuv_rev_newdata$pred_orig - 1.96*adjuv_rev_newdata$se_orig

####Plotting this

par(mfrow=c(1,1))

adult_rev_dat <- adjuv_rev_newdata[adjuv_rev_newdata$age == 1,]
juv_rev_dat <- adjuv_rev_newdata[adjuv_rev_newdata$age == 0,]

min(adjuv_rev_newdata$pred_orig)
max(adjuv_rev_newdata$pred_orig)

plot(adult_rev_dat$pred_orig~adult_rev_dat$trial, type = "n", ylim = c(45, 180), ylab="Latency to reach correct arm", xlab="Trial number")

lines(adult_rev_dat$pred_orig~adult_rev_dat$trial, lwd=3)
lines(adult_rev_dat$upCI_orig~adult_rev_dat$trial, lwd=1.5)
lines(adult_rev_dat$lowCI_orig~adult_rev_dat$trial, lwd=1.5)

lines(juv_rev_dat$pred_orig~juv_rev_dat$trial, lty=2, lwd=3)
lines(juv_rev_dat$upCI_orig~juv_rev_dat$trial, lty=3, lwd=2)
lines(juv_rev_dat$lowCI_orig~juv_rev_dat$trial, lty=3, lwd=2)





