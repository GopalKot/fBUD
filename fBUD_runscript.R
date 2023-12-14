source("fBUD_functions.R")

#Packages needed - may need to install with install.packages("~")
library(ramify)
library(mvtnorm)
library(Matrix)
library(MASS)

#Set up parameters
numsteps=50 #Trial size
k=2 #Number of arms
d=2 #Degree of interactions
true_beta=c(-2,0,1,-0.3) #Scenario parameters beta
beta_length=length(true_beta) 
prior=10*eye(length(true_beta)) #Prior 
h=20 #Temperature parameter h
IS_size=1000 #number of importance samples
IM="tracevar" #Information measure.  Can toggle between 'tracevar', 'entropy', 'variance' and 'teff'
paramset=10 #Scenario number.  
outcome_delay=0 #Whether outcomes are delayed or not

trial_result=simulate_trial(numsteps,k,d,true_beta,prior,h,IS_size,IM,paramset,outcome_delay)

#The trial result object is a list.  [[1]] is the information during the trial.  [[2]] is a further list with data (z,y,x, a matrix of possible actions)
#[[3]] is the final posterior fit (with MAP beta and hessian)

#This part recommends an arm at the end of the trial
init_beta=rmvnorm(1,rep(0,beta_length),diag(beta_length))
normaldraw=rmvnorm(IS_size,mean=rep(0,beta_length))
posterior=fit_gaussian(init_beta,trialdata=trial_result[[2]],prior) #Calculate the posterior estimate of beta
importancesample=importance_sample(posterior,trial_result[[2]],normaldraw,prior)

#Use the importance sample to choose an arm/treatment combination and calculate Bayesian power,type 1 error
armresult=chooseoverallarm(importancesample,possible_z=trial_result[[2]][[4]],true_beta,k,d,paramset,threshold=0.5,threshold2=0.9) #note threshold 2 is 0.9

#The results are as follows
armchosen=armresult[[1]]
true_positive=armresult[[2]]
false_positive=armresult[[3]]
