
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

#Read in data
toadrev<- read.csv("data/processed/toad_rev_final.csv", stringsAsFactors = F)
str(toadrev) 

#Change variable types from integers to factors

toadrev$toad.id <- as.factor(toadrev$toad.id)
toadrev$sex <- as.factor(toadrev$sex)
toadrev$choice <- as.factor(toadrev$choice)
toadrev$learnt <- as.factor(toadrev$learnt)

#Have a look at the data
head(toadrev) ; tail(toadrev)

#Number of females and males
length(unique(toadrev[toadrev$sex == "0", 1])) #10 Females
length(unique(toadrev[toadrev$sex == "1", 1])) #10 Males

#Reward position summary
table(toadrev$reward.position) #L 155, R 167

#Histogram of latency
hist(toadrev$latency) #Very right skew, need to transform
hist(log(toadrev$latency)) #Looks way better

#Create new variable log latency
toadrev$log.latency <- log(toadrev$latency)

#Test null hypothesis that data is normally distributed or not
shapiro.test(toadrev$log.latency) #Reject null, not normal 

write.csv(toadrev, file = "data/processed/toad_rev_final2.csv", row.names = F)

#######################################################################################
## ANALYSIS 1 - Trials taken to learn the reviation task
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object "toadrev" ; 322 obs, 11 var.
######################################################################################

str(toadrev[toadrev$learnt == 1,]) #All toads learnt

#Create dataframe using toadrev, split by toad.id and sex and add up lt variable for each individual. Length of lt variable = number of trials taken to learn 
trialdat.rev <-ddply(.data=toadrev, .(toad.id, sex), summarise, trials_to_learn=sum((lt)))

trialdat.rev

str(trialdat.rev)

#Fit a GLM with negative binomial errors

toad.rev.1 <- glm.nb(trials_to_learn ~ sex, data= trialdat.rev)
summary(toad.rev.1) #No differences in trials taken to learn 

round(summary(toad.rev.1)$coef,2)
#Set up prediction grid

newdata.1.rev = data.frame(sex = factor(c(1,0)))
str(newdata.1.rev)

#This code tells the model to predict from our model what is the number of trials taken to learn for males and females 
pred.1.rev <- predict(toad.rev.1, type="response", newdata=newdata.1.rev, se.fit = T)

#Adding predictions and se.fit to my newdata.1 dataframe 
newdata.1.rev$pred <- pred.1.rev$fit
newdata.1.rev$se.fit <- pred.1.rev$se.fit

#Creating upper and lower standard error limit
newdata.1.rev$se.u <- pred.1.rev$fit + pred.1.rev$se.fit
newdata.1.rev$se.l <- pred.1.rev$fit - pred.1.rev$se.fit

#Plotting these predictions saving as a pdf

pdf("output/fig/Reversal_sexdiff.pdf", 7.5, 9)

barplot(newdata.1.rev$pred, ylim = c(0,21), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5, axisnames = F) 
box()

#Axes labels

title(ylab = list("Mean number of trials taken to learn", cex=1.5), line = 3)
mtext("Male", at= 1, side = 1, line = 1.2 ,cex = 1.5)
mtext("Female", at=2.5, side = 1,line =1.2, cex= 1.5)

#Error bars

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = newdata.1.rev$pred, x1 = up.x, y1 = newdata.1.rev$se.u, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = newdata.1.rev$pred, x1 =low.x, y1 = newdata.1.rev$se.l, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=19, x1 = 2.6, y1 =19, lwd= 2)
text(x=1.8, y=19.5, labels="n.s.", cex=1.5, font=1)

dev.off()

#######################################################################################
## ANALYSIS 1b - Number of errors
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Read in data
toadrev<- read.csv("data/processed/toad_rev_final.csv")
str(toadrev)

incordat <- ddply(.data=toadrev, .(toad.id, sex), summarise, incorrect_choice=sum((choice==0)), total=length(choice))

incordat$sex <- as.factor(incordat$sex)
str(incordat)

incorfit<-glm.nb(incorrect_choice~sex, data=incordat)
summary(incorfit)
round(summary(incorfit)$coeff,2)
#######################################################################################
## ANALYSIS 2 - 3 chain MCMC analysis, probability learning the association task, latency to reach correct arm
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

###############################################################################

toad_rev_lat.1 <- MCMC.chains("output/toad_rev_lat.1")

names(toad_rev_lat.1)

plot(toad_rev_lat.1$solVCVlist$Sol[[1]])
plot(toad_rev_lat.1$solVCVlist$Sol[[2]])
plot(toad_rev_lat.1$solVCVlist$Sol[[3]])

autocorr.diag(toad_rev_lat.1$solVCVlist$Sol[[1]])
autocorr.diag(toad_rev_lat.1$solVCVlist$Sol[[2]])
autocorr.diag(toad_rev_lat.1$solVCVlist$Sol[[3]])

heidel.diag(toad_rev_lat.1$solVCVlist$Sol[[1]])
heidel.diag(toad_rev_lat.1$solVCVlist$Sol[[2]])
heidel.diag(toad_rev_lat.1$solVCVlist$Sol[[3]])

geweke.diag(toad_rev_lat.1$solVCVlist$Sol[[1]])
geweke.diag(toad_rev_lat.1$solVCVlist$Sol[[2]])
geweke.diag(toad_rev_lat.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_rev_lat.1$solVCVlist$Sol[[1]], toad_rev_lat.1$solVCVlist$Sol[[2]], toad_rev_lat.1$solVCVlist$Sol[[3]]))

toad_rev_lat.1$solVCVchain$Sol

##########################################################################
#Table S1 in ESM

TableS1_Reversal <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(TableS1_Reversal)[1:3] <- "Probcor"
colnames(TableS1_Reversal)[4:6] <- "Latency"

rownames(TableS1_Reversal) <- c("Intercept", "Sex (Male)", "Trial", "Age:Trial")

TableS1_Reversal[4,4:6] <- round(toad_rev_lat.1$solVCVchain$Sol,2)[4,]

###############################################################################

toad_rev_lat.wo <- MCMC.chains("output/toad_rev_lat.wo")

names(toad_rev_lat.wo)

plot(toad_rev_lat.wo$solVCVlist$Sol[[1]])
plot(toad_rev_lat.wo$solVCVlist$Sol[[2]])
plot(toad_rev_lat.wo$solVCVlist$Sol[[3]])

autocorr.diag(toad_rev_lat.wo$solVCVlist$Sol[[1]])
autocorr.diag(toad_rev_lat.wo$solVCVlist$Sol[[2]])
autocorr.diag(toad_rev_lat.wo$solVCVlist$Sol[[3]])

heidel.diag(toad_rev_lat.wo$solVCVlist$Sol[[1]])
heidel.diag(toad_rev_lat.wo$solVCVlist$Sol[[2]])
heidel.diag(toad_rev_lat.wo$solVCVlist$Sol[[3]])

geweke.diag(toad_rev_lat.wo$solVCVlist$Sol[[1]])
geweke.diag(toad_rev_lat.wo$solVCVlist$Sol[[2]])
geweke.diag(toad_rev_lat.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_rev_lat.wo$solVCVlist$Sol[[1]], toad_rev_lat.wo$solVCVlist$Sol[[2]], toad_rev_lat.wo$solVCVlist$Sol[[3]]))

toad_rev_lat.wo$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Reversal[1:3,4:6] <- round(toad_rev_lat.wo$solVCVchain$Sol,2)

###############################################################################

toad_rev_probcormod.1 <- MCMC.chains("output/toad_rev_probcormod.1")

names(toad_rev_probcormod.1)

plot(toad_rev_probcormod.1$solVCVlist$Sol[[1]])
plot(toad_rev_probcormod.1$solVCVlist$Sol[[2]])
plot(toad_rev_probcormod.1$solVCVlist$Sol[[3]])

autocorr.diag(toad_rev_probcormod.1$solVCVlist$Sol[[1]])
autocorr.diag(toad_rev_probcormod.1$solVCVlist$Sol[[2]])
autocorr.diag(toad_rev_probcormod.1$solVCVlist$Sol[[3]])

heidel.diag(toad_rev_probcormod.1$solVCVlist$Sol[[1]])
heidel.diag(toad_rev_probcormod.1$solVCVlist$Sol[[2]])
heidel.diag(toad_rev_probcormod.1$solVCVlist$Sol[[3]])

geweke.diag(toad_rev_probcormod.1$solVCVlist$Sol[[1]])
geweke.diag(toad_rev_probcormod.1$solVCVlist$Sol[[2]])
geweke.diag(toad_rev_probcormod.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_rev_probcormod.1$solVCVlist$Sol[[1]], toad_rev_probcormod.1$solVCVlist$Sol[[2]], toad_rev_probcormod.1$solVCVlist$Sol[[3]]))

toad_rev_probcormod.1$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Reversal[4,1:3] <- round(toad_rev_probcormod.1$solVCVchain$Sol,2)[4,]

###############################################################################

toad_rev_probcormod.wo <- MCMC.chains("output/toad_rev_probcormod.wo")

names(toad_rev_probcormod.wo)

plot(toad_rev_probcormod.wo$solVCVlist$Sol[[1]])
plot(toad_rev_probcormod.wo$solVCVlist$Sol[[2]])
plot(toad_rev_probcormod.wo$solVCVlist$Sol[[3]])

autocorr.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[1]])
autocorr.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[2]])
autocorr.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[3]])

heidel.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[1]])
heidel.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[2]])
heidel.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[3]])

geweke.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[1]])
geweke.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[2]])
geweke.diag(toad_rev_probcormod.wo$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(toad_rev_probcormod.wo$solVCVlist$Sol[[1]], toad_rev_probcormod.wo$solVCVlist$Sol[[2]], toad_rev_probcormod.wo$solVCVlist$Sol[[3]]))

toad_rev_probcormod.wo$solVCVchain$Sol

##########################################################################
#Table1 in manuscript

TableS1_Reversal[1:3,1:3] <- round(toad_rev_probcormod.wo$solVCVchain$Sol,2)

write.csv(TableS1_Reversal, file = 'output/table/TableS1_reversal.csv')

#######################################################################################
## ANALYSIS 2b - 1 chain MCMC analysis, probability learning the association task, latency to reach correct arm
## Analysis Author: Fonti Kar fonti.kar@gmail.com
## Analysis Start Date: 26.07.2016
## Data: Object model output in output folder
######################################################################################

#Load the model output of probability of making the correct choice with interaction between age and trial
toad_rev_probcormod.1<- readRDS("output/toad_rev_probcormod.1")

#Trace plot - model diagnostics
plot(toad_rev_probcormod.1)

heidel.diag(toad_rev_probcormod.1$Sol)
geweke.diag(toad_rev_probcormod.1$Sol)

heidel.diag(toad_rev_probcormod.1$VCV)
geweke.diag(toad_rev_probcormod.1$VCV)

#Summary of parameters
summary(toad_rev_probcormod.1)

#Posterior modes of fixed effects
posterior.mode(toad_rev_probcormod.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad_rev_probcormod.1$Sol) #No differences between age on each trial

#Load the model output of probability of making the correct choice withOUT interaction between age and trial
toad_rev_probcormod.wo <- readRDS("output/toad_rev_probcormod.wo")

#Trace plot - model diagnostics
plot(toad_rev_probcormod.wo)

heidel.diag(toad_rev_probcormod.wo$Sol)
geweke.diag(toad_rev_probcormod.wo$Sol)

heidel.diag(toad_rev_probcormod.wo$VCV)
geweke.diag(toad_rev_probcormod.wo$VCV)

#Summary of parameters
summary(toad_rev_probcormod.wo)

#Posterior modes of fixed effects
posterior.mode(toad_rev_probcormod.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad_rev_probcormod.wo$Sol) #No differences between age and trial on each trial

#Load the model output of latency to reach correct arm with interaction between age and trial

toad_rev_lat.1 <- readRDS("output/toad_rev_lat.1")

#Trace plot - model diagnostics
plot(toad_rev_lat.1)

heidel.diag(toad_rev_lat.1$Sol)
geweke.diag(toad_rev_lat.1$Sol)

heidel.diag(toad_rev_lat.1$VCV)
geweke.diag(toad_rev_lat.1$VCV)

#Summary of parameters
summary(toad_rev_lat.1)

#Posterior modes of fixed effects
posterior.mode(toad_rev_lat.1$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad_rev_lat.1$Sol) #No effect of interaction

#Load the model output of latency to reach correct arm without interaction between age and trial

toad_rev_lat.wo <- readRDS("output/toad_rev_lat.wo")

#Trace plot - model diagnostics
plot(toad_rev_lat.wo)

heidel.diag(toad_rev_lat.wo$Sol)
geweke.diag(toad_rev_lat.wo$Sol)

heidel.diag(toad_rev_lat.wo$VCV)
geweke.diag(toad_rev_lat.wo$VCV)

#Summary of parameters
summary(toad_rev_lat.wo)

#Posterior modes of fixed effects
posterior.mode(toad_rev_lat.wo$Sol)

#HPD intervals of fixed effects (similar to confidence intervals)
HPDinterval(toad_rev_lat.wo$Sol) #No interaction effect but there are differences between age






