
source("fBUD_functions.R") #Load up the functions.  May need to add path to ensure the functions are in environment

#Packages needed - may need to install with install.packages("~")
library(ramify)
library(mvtnorm)
library(Matrix)
library(MASS)

#These libraries are used for parallelization
library(parallel)
library(foreach)

#These libraries are used as one or two functions from them are used in the analysis/plotting code.  May be able to get away without
library(reshape2)
library(ggplot2)
library(latex2exp)
library(matrixStats)
library(forcats)
library(abind)
library(dplyr)

######################################################### Simulate BAL, BAR and fBUD Designs ####################################

numsims=5 #Number of simulations S
numsteps=25 #Length of each trial T
scenario_number=9 #Scenario
scenario=get_scenario(scenario_number) #Get scenario details
k=scenario[[1]] #Number of actions K
d=scenario[[2]] #Degree of interactions to use 
true_beta=scenario[[3]] #Parameter vector beta
beta_length=length(true_beta) 
prior=10*eye(length(true_beta)) #Prior for the Bayesian model
IM="tracevar" #The information measure.  Options: "tracevar", "entropy", "variance", "teff"
hvals=c(0,-1,20) #h=0 gives us a blaanced design.  h=-1 gives BAR.  h=20 gives fBUD with a temperature parameter h of 20
titles=c("BAL","BAR","fBUD-I1") #Design names that correspond to the hvals vectures
IS_size=1000 #Size of importance samples w_i and W_i 
outcome_delay=0 #Change to 1 for outcome delay setting


foldername = "/homes1/gkotecha/paper1" #CHANGE THIS to a local folder
cl <- parallel::makeCluster(max(1,detectCores(logical = TRUE)-3),outfile=paste0(foldername,'/outputlogfile.txt')) #For parallelization


#Loop through the hvals vector and simulate the trials
trialresults=list()
for (i in 1:length(hvals)){
trialresults[[i]]=multiple_simulations(numsims,numsteps,k,d,true_beta,prior,h,IS_size,IM,draw_from_prior,scenario_number,outcome_delay)}




################################################# Analyze Trials and Create Plots ########################################################

saving_params=list(k,d,numsims,numsteps,IS_size,true_beta,prior,hvals,scenario_number) #For saving
trialresult=trialresults
result=list(saving_params,trialresults) #Can save this variable and reload locally

trialresult_processed=process_parallel_trial_result(trialresult,saving_params) #This processes a 'result' structure into a more manageable one

plot1=lineplot_parallel(trialresult_processed,hvals,titles) #Plot info over time
plot1
plot2=specific_histogram(trialresult_processed,hvals,titles,tvalue=10) #Plots info distribution at a certain t (change tvalue)
plot2
action_counts_full=get_all_action_counts_fast(result,trialresult_processed,hvals) #returns average action proportions for each simulation, for each design
plot3=action_counts_plot(action_counts_full[[2]],result[[1]],titles) #Plot action proportions
plot3
plot4=EO_lineplot_parallel(trialresult_processed,hvals,titles) #Plot number of successes over time
plot4

powersimresult=calcpower_sample(result,numsteps,scenario_number,threshold1=c(0.5),numsims,infomeasure_index=1,threshold2=0.9)  #Calculates the arm recommended at the end of the trial, power and T1E.
final_arm_chosen=drop(powersimresult)[1,] #Arm recommended at the end of the trial
final_arm_chosen[final_arm_chosen==0]=1 
efficacies=get_probs(true_beta,k,d)

#Hard code the efficacies 
apfelprobs=c(0.41,0.45,0.48,0.35,0.44,0.44,0.56,0.57,0.53,0.57,0.7,0.68,0.72,0.59,0.62,0.67,0.5,0.56,0.6,0.66,0.63,0.65,0.7,0.65,0.6,0.61,0.76,0.77,0.69,0.74,0.78,0.72,0.58,0.54,0.56,0.69,0.69,0.64,0.66,0.61,0.77,0.65,0.74,0.76,0.77,0.63,0.67,0.78,0.63,0.66,0.58,0.65,0.79,0.78,0.82,0.78,0.77,0.67,0.75,0.69,0.83,0.81,0.8,0.83)
if(paramset==11){efficacies[,1]=apfelprobs}
if(paramset==12){efficacies[,1]=c(0.1164,0.1338,0.1284,0.1404)}
numresponders=final_arm_chosen
for (i in 1:2^k){
  currentaction=i
  currentprob=efficacies[i,1]
  numresponders[numresponders==currentaction]=currentprob
}
#Calculate the expected number of responders after the trial
numresponders=numresponders*1000
output=mean(numresponders)

#Calculate the number of times the best arm was chosen
numtimes_bestarmchosen=calc_numtimes_bestchosen(final_arm_chosen,scenario_number)


########################################## Asymptotic Analysis #######################################
rholength=2^k
rho=numeric(rholength)+1/(rholength)

i=0
rholength=2^k
rho=numeric(rholength)+1/(rholength)
num_iter=60
stepsize=0.03  # THIS NEEDS TWEAKING.  Is different for each scenario/h value.  May also need to adjust the number of iterations (num_iter)

#Fixed point iteration to solve the system of equations
while (i<num_iter){
  print(i)
  rho2=rhofn(k,true_beta,rho,d,20)
  print(round(rho,3))
  rho=(rho2*stepsize)+(rho*(1-stepsize))
  i=i+1
}

#Calculate probability of allocation to each arm
stepsize=5
acomb <- function(...) abind(..., along=3)
triallengthlist=seq(from = 1, to = result[[1]][[4]], by = stepsize)
rlmat_transposed=t(result[[2]][[1]]) #transpose as parallelization happens over columns

functionlist=c("dmvnorm","importance_sample","log_posterior","plogis","get_interactions","action_combinations","nearPD",'ginv','calcEG','tr')
EG_sims=
  foreach(rlmat_transposed_column=rlmat_transposed,.export=functionlist,.combine='acomb')%:%  #rlmat_transposed_column=t(rlmat) specifically iterates through columns to prevent memory overload
  foreach(triallength=triallengthlist, .combine='rbind') %dopar%{
    output=calcEG(rlmat_transposed_column,triallength,normaldraw,prior)
  }
probs=apply(EG_sims,c(1,3),normalize) #normalize across actions
probmeans=apply(probs,c(1,2),mean)
probvars=apply(probs,c(1,2),var)

#Calculate cumulative allocation to each arm
action_counts_full=get_all_action_counts_per_timepoint_fast(result,trialresult_processed,h_index=2) #Change the h_index to toggle between BAL, BAR and BUD etc. 
cumulative_action_counts=action_counts_full[[3]]
cumulative_action_counts_plot=plot_cumulative_action_counts(cumulative_action_counts) #Plot cumulative allocations to each arm
cumulative_action_counts_plot

