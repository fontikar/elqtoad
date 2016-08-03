
#######################################################################################
## Manuscript: "El questro toad cognition data" 
## Authors: Martin Whiting, Fonti Kar
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.16
## Notes: 
##        
######################################################################################

#Setting working directory

setwd("/Users/fontikar/Dropbox/Toad cognition - El Questro Fonti/")

#Clear R memory

rm(list=ls())

#Load libraries

library(plyr)
library(lme4)
library(MCMCglmm)

#Read in data
toadassoc <- read.csv("data/processed/toad_assoc_final.csv", stringsAsFactors = F)
str(toadassoc) 

#Change variable types from integers to factors

toadassoc$toad.id <- as.factor(toadassoc$toad.id)
toadassoc$sex <- as.factor(toadassoc$sex)
toadassoc$choice <- as.factor(toadassoc$choice)
toadassoc$learnt <- as.factor(toadassoc$learnt)

#Have a look at the data
head(toadassoc) ; tail(toadassoc)

#Number of females and males
length(unique(toadassoc[toadassoc$sex == "0", 1])) #10 Females
length(unique(toadassoc[toadassoc$sex == "1", 1])) #10 Males

#Reward position summary
table(toadassoc$reward.position) #L 109, R 118

#Histogram of latency
hist(toadassoc$latency) #Very right skew, need to transform
hist(log(toadassoc$latency)) #Looks way better

#Create new variable log latency
toadassoc$log.latency <- log(toadassoc$latency)

#Test null hypothesis that data is normally distributed or not
shapiro.test(toadassoc$log.latency) #Reject null, not normal 

write.csv(toadassoc, file="data/processed/toad_assoc_final2.csv", row.names = F)

#######################################################################################
## ANALYSIS 1 - Trials taken to learn the association task
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object "toadassoc" ; 227 obs, 11 var.
######################################################################################

str(toadassoc[toadassoc$learnt == 1,]) #All toads learnt

#Create dataframe using toadassoc, split by toad.id and sex and add up lt variable for each individual. Length of lt variable = number of trials taken to learn 
trialdat <-ddply(.data=toadassoc, .(toad.id, sex), summarise, trials_to_learn=sum((lt)))

trialdat

str(trialdat)

#Fit a GLM with negative binomial errors

toad.fit.1 <- glm.nb(trials_to_learn ~ sex, data= trialdat)
summary(toad.fit.1) #No differences in trials taken to learn 

round(summary(toad.fit.1)$coef,2)

#Set up prediction grid

newdata.1 = data.frame(sex = factor(c(1,0)))
str(newdata.1)

#This code tells the model to predict from our model what is the number of trials taken to learn for males and females 
pred.1 <- predict(toad.fit.1, type="response", newdata=newdata.1, se.fit = T)

#Adding predictions and se.fit to my newdata.1 dataframe 
newdata.1$pred <- pred.1$fit
newdata.1$se.fit <- pred.1$se.fit

#Creating upper and lower standard error limit
newdata.1$se.u <- pred.1$fit + pred.1$se.fit
newdata.1$se.l <- pred.1$fit - pred.1$se.fit

#Plotting these predictions and sving as pdf

pdf("output/fig/Association_sexdiff.pdf", 7.5, 9)

barplot(newdata.1$pred <- pred.1$fit, ylim = c(0,16.5), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5, axisnames = F) 
box()

#Axes labels

title(ylab = list("Mean number of trials taken to learn", cex=1.5), line = 3)
mtext("Male", at= 1, side = 1, line = 1.2 ,cex = 1.5)
mtext("Female", at=2.5, side = 1,line =1.2, cex= 1.5)

#Error bars

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = newdata.1$pred, x1 = up.x, y1 = newdata.1$se.u, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = newdata.1$pred, x1 =low.x, y1 = newdata.1$se.l, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=15, x1 = 2.6, y1 =15, lwd= 2)
text(x=1.8, y=15.5, labels="n.s.", cex=1.5, font=1)

dev.off()

#######################################################################################
## ANALYSIS 1b - Number of errors
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Read in data
toadassoc<- read.csv("data/processed/toad_assoc_final.csv")
str(toadassoc)

incordat <- ddply(.data=toadassoc, .(toad.id, sex), summarise, incorrect_choice=sum((choice==0)), total=length(choice))

incordat$sex <- as.factor(incordat$sex)
str(incordat)

incorfit<-glm.nb(incorrect_choice~sex, data=incordat)
summary(incorfit)

round(summary(incorfit)$coef,2)


#######################################################################################
## ANALYSIS 2b - 3 chain MCMC analysis, probability learning the association task, latency to reach correct arm
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

######################################################################################

toad_assoc_lat.1 <- MCMC.chains("output/toad_assoc_lat.1")

names(toad_assoc_lat.1)

plot(toad_assoc_lat.1$solVCVlist$Sol[[1]])
plot(toad_assoc_lat.1$solVCVlist$Sol[[2]])
plot(toad_assoc_lat.1$solVCVlist$Sol[[3]])

autocorr.diag(toad_assoc_lat.1$solVCVlist$Sol[[1]])
autocorr.diag(toad_assoc_lat.1$solVCVlist$Sol[[2]])
autocorr.diag(toad_assoc_lat.1$solVCVlist$Sol[[3]])

heidel.diag(toad_assoc_lat.1$solVCVlist$Sol[[1]])
heidel.diag(toad_assoc_lat.1$solVCVlist$Sol[[2]])
heidel.diag(toad_assoc_lat.1$solVCVlist$Sol[[3]])

geweke.diag(toad_assoc_lat.1$solVCVlist$Sol[[1]])
geweke.diag(toad_assoc_lat.1$solVCVlist$Sol[[2]])
geweke.diag(toad_assoc_lat.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_assoc_lat.1$solVCVlist$Sol[[1]], toad_assoc_lat.1$solVCVlist$Sol[[2]], toad_assoc_lat.1$solVCVlist$Sol[[3]]))

toad_assoc_lat.1$solVCVchain$Sol

##########################################################################
#Table S1 in ESM

TableS1_Assoc <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(TableS1_Assoc)[1:3] <- "Probcor"
colnames(TableS1_Assoc)[4:6] <- "Latency"

rownames(TableS1_Assoc) <- c("Intercept", "Sex (Male)", "Trial", "Age:Trial")

TableS1_Assoc[4,4:6] <- round(toad_assoc_lat.1$solVCVchain$Sol,2)[4,]

######################################################################################

toad_assoc_lat.wo <- MCMC.chains("output/toad_assoc_lat.wo")

names(toad_assoc_lat.wo)

plot(toad_assoc_lat.wo$solVCVlist$Sol[[1]])
plot(toad_assoc_lat.wo$solVCVlist$Sol[[2]])
plot(toad_assoc_lat.wo$solVCVlist$Sol[[3]])

autocorr.diag(toad_assoc_lat.wo$solVCVlist$Sol[[1]])
autocorr.diag(toad_assoc_lat.wo$solVCVlist$Sol[[2]])
autocorr.diag(toad_assoc_lat.wo$solVCVlist$Sol[[3]])

heidel.diag(toad_assoc_lat.wo$solVCVlist$Sol[[1]])
heidel.diag(toad_assoc_lat.wo$solVCVlist$Sol[[2]])
heidel.diag(toad_assoc_lat.wo$solVCVlist$Sol[[3]])

geweke.diag(toad_assoc_lat.wo$solVCVlist$Sol[[1]])
geweke.diag(toad_assoc_lat.wo$solVCVlist$Sol[[2]])
geweke.diag(toad_assoc_lat.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_assoc_lat.wo$solVCVlist$Sol[[1]], toad_assoc_lat.wo$solVCVlist$Sol[[2]], toad_assoc_lat.wo$solVCVlist$Sol[[3]]))

toad_assoc_lat.wo$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Assoc[1:3,4:6] <- round(toad_assoc_lat.wo$solVCVchain$Sol,2)

######################################################################################

toad_assoc_probcormod.1 <- MCMC.chains("output/toad_assoc_probcormod.1")

names(toad_assoc_probcormod.1)

plot(toad_assoc_probcormod.1$solVCVlist$Sol[[1]])
plot(toad_assoc_probcormod.1$solVCVlist$Sol[[2]])
plot(toad_assoc_probcormod.1$solVCVlist$Sol[[3]])

autocorr.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[1]])
autocorr.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[2]])
autocorr.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[3]])

heidel.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[1]])
heidel.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[2]])
heidel.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[3]])

geweke.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[1]])
geweke.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[2]])
geweke.diag(toad_assoc_probcormod.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_assoc_probcormod.1$solVCVlist$Sol[[1]], toad_assoc_probcormod.1$solVCVlist$Sol[[2]], toad_assoc_probcormod.1$solVCVlist$Sol[[3]]))

toad_assoc_probcormod.1$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Assoc[4,1:3] <- round(toad_assoc_probcormod.1$solVCVchain$Sol,2)[4,]

######################################################################################

toad_assoc_probcormod.wo.1 <- MCMC.chains("output/toad_assoc_probcormod.wo.1")

names(toad_assoc_probcormod.wo.1)

plot(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[1]])
plot(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[2]])
plot(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[3]])

autocorr.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[1]])
autocorr.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[2]])
autocorr.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[3]])

heidel.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[1]])
heidel.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[2]])
heidel.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[3]])

geweke.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[1]])
geweke.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[2]])
geweke.diag(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_assoc_probcormod.wo.1$solVCVlist$Sol[[1]], toad_assoc_probcormod.wo.1$solVCVlist$Sol[[2]], toad_assoc_probcormod.wo.1$solVCVlist$Sol[[3]]))

toad_assoc_probcormod.wo.1$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Assoc[1:3,1:3] <- round(toad_assoc_probcormod.wo.1$solVCVchain$Sol,2)

write.csv(TableS1_Assoc, file = 'output/table/TableS1_association.csv')

#######################################################################################
## ANALYSIS 2b - 1 chain MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Load the model output of probability of making the correct choice with interaction between sex and trial
toad.probcor.1 <- readRDS("output/toad_assoc_probcormod.1")

#Trace plot - model diagnostics
plot(toad.probcor.1)

heidel.diag(toad.probcor.1$Sol)
geweke.diag(toad.probcor.1$Sol)

heidel.diag(toad.probcor.1$VCV)
geweke.diag(toad.probcor.1$VCV)

#Summary of parameters
summary(toad.probcor.1)

#Posterior modes of fixed effects
posterior.mode(toad.probcor.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad.probcor.1$Sol) #No differences between sex on each trial

#Load the model output of probability of making the correct choice withOUT interaction between sex and trial

toad.probcor.wo <- readRDS("output/toad_assoc_probcormod.wo")

#Trace plot - model diagnostics
plot(toad.probcor.wo)

heidel.diag(toad.probcor.wo$Sol)
geweke.diag(toad.probcor.wo$Sol)

heidel.diag(toad.probcor.wo$VCV)
geweke.diag(toad.probcor.wo$VCV)

#Summary of parameters
summary(toad.probcor.wo)

#Posterior modes of fixed effects
posterior.mode(toad.probcor.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad.probcor.wo$Sol) #No differences between sex on each trial

#Load the model output of latency to reach correct arm with interaction between sex and trial

toad.lat.1 <- readRDS("output/toad_assoc_lat.1")

#Trace plot - model diagnostics
plot(toad.lat.1) 

heidel.diag(toad.lat.1$Sol)
geweke.diag(toad.lat.1$Sol)

heidel.diag(toad.lat.1$VCV)
geweke.diag(toad.lat.1$VCV)

#Summary of parameters
summary(toad.lat.1)

#Posterior modes of fixed effects
posterior.mode(toad.lat.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad.lat.1$Sol) #No differences between sex on each trial

#Load the model output of latency to reach correct arm withOUT interaction between sex and trial

toad.lat.wo <- readRDS("output/toad_assoc_lat.wo")

#Trace plot - model diagnostics
plot(toad.lat.wo)

heidel.diag(toad.lat.wo$Sol)
geweke.diag(toad.lat.wo$Sol)

heidel.diag(toad.lat.wo$VCV)
geweke.diag(toad.lat.wo$VCV)

#Summary of parameters
summary(toad.lat.wo)

#Posterior modes of fixed effects
posterior.mode(toad.lat.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad.lat.wo$Sol) #No differences between sex on each trial
