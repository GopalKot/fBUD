################################# Functions for trial simulation #########################

#' Function: get_probs: converts beta values to probability estimates
#' @param beta parameters for the model
#' @param k number of possible binary actions
#' @param d degree of interaction to consider
#' @return probability estimates for each action and the corresponding treatment combinations
#' @example get_probs(c(-1,1,0,3))
get_probs=function(beta,k,d){
  actions=get_interactions(action_combinations(k),d)
  gamma=actions%*%beta
  probs=1/(1+exp(-gamma))
  return(cbind(probs,actions))
}

#' Function: action_combinations.  Auxiliary Function to calculate possible treatment combinations
#' @param n The number of dimensions required
#' @return An array of all possible combinations
#' @example action_combinations(3)
action_combinations=function(n){
  possible_combinations=expand.grid(replicate(n, c(0,1), simplify=FALSE))
  bias=matrix(1,dim(possible_combinations)[1],1)
  return(as.matrix(cbind(bias,possible_combinations)))
}


## This function returns an outcome by drawing from a specific trial: the trial of Apfel et al NEJM
drawoutcome_apfel=function(action){
  action=as.vector(action)
  if (identical(action,c(1,0,0,0,0,0,0))){prob=0.59
  } else if (identical(action,c(1,0,0,0,0,0,1))){prob=0.55
  } else if (identical(action,c(1,0,0,0,0,1,0))){prob=0.52
  } else if (identical(action,c(1,0,0,0,0,1,1))){prob=0.65  #This is action index 49 in possible_actions
  
  } else if (identical(action,c(1,0,0,0,1,0,0))){prob=0.56
  } else if (identical(action,c(1,0,0,0,1,0,1))){prob=0.56
  } else if (identical(action,c(1,0,0,0,1,1,0))){prob=0.44
  } else if (identical(action,c(1,0,0,0,1,1,1))){prob=0.43
  
  } else if (identical(action,c(1,0,0,1,0,0,0))){prob=0.47
  } else if (identical(action,c(1,0,0,1,0,0,1))){prob=0.43
  } else if (identical(action,c(1,0,0,1,0,1,0))){prob=0.30
  } else if (identical(action,c(1,0,0,1,0,1,1))){prob=0.32
  
  } else if (identical(action,c(1,0,0,1,1,0,0))){prob=0.28
  } else if (identical(action,c(1,0,0,1,1,0,1))){prob=0.41
  } else if (identical(action,c(1,0,0,1,1,1,0))){prob=0.38
  } else if (identical(action,c(1,0,0,1,1,1,1))){prob=0.33
  
  } else if (identical(action,c(1,0,1,0,0,0,0))){prob=0.50
  } else if (identical(action,c(1,0,1,0,0,0,1))){prob=0.44
  } else if (identical(action,c(1,0,1,0,0,1,0))){prob=0.40
  } else if (identical(action,c(1,0,1,0,0,1,1))){prob=0.34
  
  } else if (identical(action,c(1,0,1,0,1,0,0))){prob=0.37
  } else if (identical(action,c(1,0,1,0,1,0,1))){prob=0.35
  } else if (identical(action,c(1,0,1,0,1,1,0))){prob=0.30
  } else if (identical(action,c(1,0,1,0,1,1,1))){prob=0.35
  
  } else if (identical(action,c(1,0,1,1,0,0,0))){prob=0.40
  } else if (identical(action,c(1,0,1,1,0,0,1))){prob=0.39
  } else if (identical(action,c(1,0,1,1,0,1,0))){prob=0.24
  } else if (identical(action,c(1,0,1,1,0,1,1))){prob=0.23
  
  } else if (identical(action,c(1,0,1,1,1,0,0))){prob=0.31
  } else if (identical(action,c(1,0,1,1,1,0,1))){prob=0.26
  } else if (identical(action,c(1,0,1,1,1,1,0))){prob=0.22
  } else if (identical(action,c(1,0,1,1,1,1,1))){prob=0.28
  
  } else if (identical(action,c(1,1,0,0,0,0,0))){prob=0.42
  } else if (identical(action,c(1,1,0,0,0,0,1))){prob=0.46
  } else if (identical(action,c(1,1,0,0,0,1,0))){prob=0.44
  } else if (identical(action,c(1,1,0,0,0,1,1))){prob=0.31
  
  } else if (identical(action,c(1,1,0,0,1,0,0))){prob=0.31
  } else if (identical(action,c(1,1,0,0,1,0,1))){prob=0.36
  } else if (identical(action,c(1,1,0,0,1,1,0))){prob=0.34
  } else if (identical(action,c(1,1,0,0,1,1,1))){prob=0.39
  
  } else if (identical(action,c(1,1,0,1,0,0,0))){prob=0.23
  } else if (identical(action,c(1,1,0,1,0,0,1))){prob=0.35
  } else if (identical(action,c(1,1,0,1,0,1,0))){prob=0.26
  } else if (identical(action,c(1,1,0,1,0,1,1))){prob=0.24
  
  } else if (identical(action,c(1,1,0,1,1,0,0))){prob=0.23
  } else if (identical(action,c(1,1,0,1,1,0,1))){prob=0.37
  } else if (identical(action,c(1,1,0,1,1,1,0))){prob=0.33
  } else if (identical(action,c(1,1,0,1,1,1,1))){prob=0.22
  
  } else if (identical(action,c(1,1,1,0,0,0,0))){prob=0.37
  } else if (identical(action,c(1,1,1,0,0,0,1))){prob=0.34
  } else if (identical(action,c(1,1,1,0,0,1,0))){prob=0.42
  } else if (identical(action,c(1,1,1,0,0,1,1))){prob=0.35
  
  } else if (identical(action,c(1,1,1,0,1,0,0))){prob=0.21
  } else if (identical(action,c(1,1,1,0,1,0,1))){prob=0.22
  } else if (identical(action,c(1,1,1,0,1,1,0))){prob=0.18
  } else if (identical(action,c(1,1,1,0,1,1,1))){prob=0.22
  
  } else if (identical(action,c(1,1,1,1,0,0,0))){prob=0.23
  } else if (identical(action,c(1,1,1,1,0,0,1))){prob=0.33
  } else if (identical(action,c(1,1,1,1,0,1,0))){prob=0.25
  } else if (identical(action,c(1,1,1,1,0,1,1))){prob=0.31
  
  } else if (identical(action,c(1,1,1,1,1,0,0))){prob=0.17
  } else if (identical(action,c(1,1,1,1,1,0,1))){prob=0.19
  } else if (identical(action,c(1,1,1,1,1,1,0))){prob=0.20
  } else if (identical(action,c(1,1,1,1,1,1,1))){prob=0.17
  
  } else {prob=0.32 
  print("Not found")}
  negprob=1-prob #WE CODE NEGATIVE PROBABILITIES AS THE AIM OF THE TRIAL IS TO REDUCE NAUSEA/VOMITING
  return(rbinom(1,1,negprob))
}

## This function returns an outcome by drawing from a specific trial: NCT02266277 Cutrona 2018
drawoutcome_cutrona=function(action){
  action=as.vector(action)
  if (identical(action,c(1,1,1,1))){prob=702/5000
  } else if (identical(action,c(1,1,0,0))){prob=669/5000
  } else if (identical(action,c(1,0,1,0))){prob=642/5000
  } else {prob=582/5000}
  return(rbinom(1,1,prob))
}

## This function returns an outcome by drawing from a specific trial: Piper et. al.
drawoutcome_piper=function(action,paramset){
  params=get_scenario(paramset)
  true_beta=params[[3]]
  k=params[[1]]
  d=params[[2]]
  probstable=get_probs(true_beta,k,d)
  probs=probstable[,1]
  actions=probstable[,2:(2+k)]
  actionfound=0
  for (i in 1:nrow(actions)){
    if (identical(as.vector(action),actions[i,])){
      prob=probs[i]
      actionfound=1
    }
  }
  if(actionfound==0){print("Action not found")}
  return(rbinom(1,1,prob))
}

#This function returns parameters (number of factors k, number of interactions d and parameter vector beta) for each scenario.
get_scenario=function(paramset){
  # Combinations of 4 treatments
  if  (paramset==1){
    k=4
    d=2
    true_beta=c(1,.2,0.1,0.2,.4,-0.5,-.9,-0.9,-1.5,-1.5,.6) # One combination noticeably better than the rest
  }  else if (paramset==2){
    k=4
    d=2
    true_beta=c(0.4,.3,0.5,-3,-0.5,-0.5,4.5,-1.5,-1,-0.9,-1) # One combination noticeably better than the rest
  }  else if (paramset==3){
    k=4
    d=2
    true_beta=c(0.4,.3,0.5,-.3,-0.5,-0.5,-.05,-1.5,-1,0.09,3)  # Two or more combinations noticeably better than the rest
  }  else if (paramset==4){
    k=4
    d=2
    true_beta=c(0.4,.3,1,1,-0.5,-2.5,-1.5,-1.5,-1,-0.9,-1) # Two or more combinations noticeably better than the rest
   }  else if (paramset==5){
    k=4
    d=2
    true_beta=c(-0.3,-0.6,-0.3,-0.4,-0.35,-0.01,-0.1,0,0,-0.1,0.1) #Control arm best
    
    #Combinations of 3 treatments
  }  else if (paramset==6){
    k=3
    d=2
    true_beta=c(-1.70, 0.05, 0, -0.10, -0.25, -0.85, 1.2) #One arm noticeably better
  }  else if (paramset==7){
    k=3
    d=2
    true_beta=c(2, -0.4, -0.35,-0.45,-0.20,-0.20, -0.75) #Control arm best
    
    #Combinations of 2 treatments
  }  else if (paramset==8){
    k=2
    d=2
    true_beta=c(0,0,0.9,-1.1) # One arm noticeably better
  }  else if (paramset==9){
    k=2
    d=2
    true_beta=c(-2,0,1,-.3) #Control arm best
  }  else if (paramset==10){
    k=2
    d=2
    true_beta=c(0,0,0,0) # All zeros.  Also the null for paramset 8 and itself (paramset 10)
    
# Scenarios from real trials
  }  else if (paramset==11){ #prev 49
    k=6
    d=2
    true_beta=numeric(22) # Apfel NEJM
  }  else if (paramset==12){ #prev 20
    k=2
    d=2
    true_beta=c(1,1,1,1) # Cutrona Flu vaccine study - params are placeholders as we bootstrap data

  }  else if (paramset==13){ #prev 55
    k=6
    d=2
    true_beta=c(0.43,0.04,0,0.04,0.15,-0.03,0.01,0.05,0.18,0.15,0.23,-0.07,0.05,0.12,-0.08,0,-0.03,0,0.06,-0.06,-0.04,-0.06) #Piper study 

# Null scenarios  
  }  else if (paramset==21){
    k=4
    d=2
    true_beta=c(1,0,0,0,0,0,0,0,0,0,0) #Null scenario for paramset 1
  }  else if (paramset==22){
    k=4
    d=2
    true_beta=c(0.4,0,0,0,0,0,0,0,0,0,0)  #Null scenario for paramsets 2-4
  }  else if (paramset==25){
    k=4
    d=2
    true_beta=c(-0.3,0,0,0,0,0,0,0,0,0,0) #Null scenario for paramset 5
  }  else if (paramset==26){
    k=3
    d=2
    true_beta=c(-1.7,0,0,0,0,0,0) #Null scenario for paramset 6
  }  else if (paramset==27){
    k=3
    d=2
    true_beta=c(2,0,0,0,0,0,0) #Null scenario for paramset 7
  }  else if (paramset==29){
    k=2
    d=2
    true_beta=c(-2,0,0,0) 
  }  else if (paramset==31){ #Nullsims for Apfel NEJM (14)
    k=6
    d=1
    true_beta=c(-0.3639654,0,0,0,0,0,0) 
  }  else if (paramset==32){ #Nullsims for Flu vaccine study Cutrona et al (13)
    k=2
    d=1
    true_beta=c(-2.026972,0,0) 
  }  else if (paramset==33){ #Nullsims for Piper et al smoking (15)
    k=6
    d=1
    true_beta=c(0.43,0,0,0,0,0,0)
  } 
  return(list(k,d,true_beta))  
}

#' Function: get_interactions: converts an x vector to a vector with all possible interactions of x
#' @param a vector x
#' @param d degree of interaction effects to consider
#' @return an expanded vector full_x
#' @example get_interactions(c(0,1,1))  
#' @example get(interactions(t(array(c(1,1,2,3,1,4,5,6,1,7,8,9),dim=c(4,3)))))
get_interactions=function(x,d){
  #Remove the first column and make a vertical one to append to
  x2<-x[,-1,drop=FALSE]
  firstcol=matrix(1,ncol=dim(x2)[1])
  
  for (i in 1:d){
    #Calculate the ith other interaction
    column=apply(x2,1,combn,i,function(y) prod(y))
    #Append it to the matrix
    firstcol=rbind(firstcol,column)
  }
  result=t(firstcol)
  return(result)
}

#' Function: log_posterior.  This function calculates a value proportional to the posterior of beta given the trialdata, and returns its log
#' For use in optimization
#' @param beta_param A vector of covariates of dimension k
#' @param trialdata A list with the trialdata in rows.  1: Historical actions x.  2: Historical outcomes y.  3: x expanded with interactions.  4: possible x
#' @param prior The covariance matrix
#' @return A single posterior value
#' @examples log_posterior(c(1,3,-1),list(t(array(c(1,0,0,1,1,0,1,0,1),dim=c(3,3))),array(c(1,1,0),dim=c(3,1)),t(array(c(1,0,0,1,1,0,1,0,1),dim=c(3,3)))),diag(3)*(1^2))=-9.281374
log_posterior=function(beta_param,trialdata,prior){
  m=dim(as.array(beta_param))[1]
  logprior=-(m*0.5)*log(2*pi)-(0.5)*log(det(prior))-0.5*diag(t(beta_param)%*%solve(prior)%*%beta_param)
  
  ##Return correct prior
  #If no data return prior only
  if(is.null(trialdata[[3]])){return(logprior)}
  #Else calculate posterior
  else{
    x=trialdata[[3]]
    y=trialdata[[2]]
    log_g_x=plogis(x%*%beta_param,log=TRUE)
    log_one_minus_g_x=log(1-plogis(x%*%beta_param))
    
    log_g_x[is.infinite(log_g_x) & log_g_x < 0]=-1e100
    log_one_minus_g_x[is.infinite(log_one_minus_g_x) & log_one_minus_g_x < 0]=-1e100
    
    #Calculate log likelihood
    loglike=(colSums(t(y)%*%log_g_x+(t(1-y)%*%log_one_minus_g_x)))
    result=loglike+logprior
    return(result)
  }
}

#' Function: fit_gaussian.  Fits a Gaussian distribution to the data using various methods
#' @param init_beta A vector of optimization starting values for beta (length k)
#' @param trialdata A list with the trialdata in rows.  1: Historical actions x.  2: Historical outcomes y.  3: x expanded with interactions.  4: possible x
#' @param prior The covariance matrix of the prior distribution
#' @param step
#' @return posterior A posterior object with a $par attribute (the optimal values) and $hessian attribute 
fit_gaussian=function(init_beta,trialdata,prior){
  posterior=optim(init_beta,log_posterior,gr=NULL,trialdata=trialdata, prior=prior,hessian = TRUE,control=list(fnscale=-1),method="BFGS")
  return(posterior)
}

#' Function: generates unnormalized importance samples in log space log_w_i and corresponding probabilities g_x
#' @param trialdata A list with the trialdata in rows.  1: Historical actions x.  2: Historical outcomes y.  3: x expanded with interactions.  4: possible x
#' @param prior The covariance matrix of the prior distribution
#' @param posterior provides the parameters of the proposal Gaussian distribution.  Output of fit_gaussian.  A list with 'par' and 'hessian' attributes for the mean and hessian
#' @param normaldraw draw from a standard gaussian
#' @return g draw of g(x,\beta)
#' @return log_w_i corresponding importance weights (their logarithm)
#' @examples
importance_sample=function(posterior,trialdata,normaldraw,prior){
  
  var_estimate=as.matrix(nearPD(ginv(-posterior$hessian))$mat)
  
  b_i=t(replicate(dim(normaldraw)[1],posterior$par,simplify=TRUE))+normaldraw%*%chol(var_estimate)
  
  log_p_bi=log_posterior(t(b_i),trialdata,prior) #density under posterior
  log_q_bi=dmvnorm(b_i,posterior$par,var_estimate,log=TRUE) #density under q:proposal
  
  #calculate importance weights
  log_w_i=log_p_bi-log_q_bi
  
  #Calculate success probability g
  possible_x=trialdata[[4]]
  g=plogis(possible_x%*%t(b_i))
  return(list(g,log_w_i,b_i,var_estimate))
}

#' Function: info_tracevar.  Returns the negative trace of the variance
info_tracevar=function(importancesample){
  w_i=exp(importancesample[[2]]-max(importancesample[[2]]))
  w_i=(w_i)/(sum(w_i))
  b_i=importancesample[[3]]
  E_b=t(w_i%*%b_i)
  E_bb=t(replicate(dim(b_i)[2],w_i)*b_i)%*%b_i
  covar=E_bb-E_b%*%t(E_b)
  return(-(tr(covar)))
}

#' Function: info_ent.  Calculates an information measure - the entropy of the best treatment or the probability each treatment is the best
#' @param g_x An importance sample of the success probabilities g(x,\beta).  Each column is a sample
#' @param w Vector of weights
#' @return The calculated information
#' @examples
info_ent=function(g_x,w){
  maxvals=apply(g_x,2,max) #Find max of each column
  replicated=matrix(rep(maxvals,dim(g_x)[1]), ncol=length(maxvals), byrow=T) #make a matrix of these maxima
  indicator=replicated==g_x #Then calculate the indicator function
  lhs=indicator%*%w #First term in entropy
  rhs=log(lhs) #Second term in entropy
  E         = rhs * lhs
  E[lhs==0] = 0
  return(sum(E))
}

#' Function: info_var.  Calculates an information measure - the variance of the best treatment
#' @param g_x An importance sample of the success probabilities g(x,\beta).  Each column is a sample
#' @param w Vector of weights
#' @return The calculated information
#' @examples
info_var=function(g_x,w){
  Pi_Star=apply(g_x,2,max)
  E_Pi_Star=sum(Pi_Star*w)
  E_Pi_Star_Sq=sum((Pi_Star^2)*w)
  neg_var=(E_Pi_Star^2)-E_Pi_Star_Sq 
  return(neg_var)
}

#' Function: info_tef.  Calculates an information measure - the variance of the largest treatment effect
#' @param g_x An importance sample of the success probabilities g(x,\beta).  Each column is a sample
#' @param w Vector of weights
#' @return The calculated information
#' @examples
info_teff=function(g_x,w){
  Teff_Star=apply(g_x,2,max)-g_x[1,]
  E_Teff_Star=sum(Teff_Star*w)
  E_Teff_Star_Sq=sum((Teff_Star^2)*w)
  neg_var=(E_Teff_Star^2)-E_Teff_Star_Sq 
  return(neg_var)
}

#' Function: BAR_probs.  Calculates an information measure - the entropy of the best treatment or the probability each treatment is the best
#' @param importancesample An importance sample of the success probabilities g(x,\beta).  Each column is a sample
#' @return The BAR probabilities
#' @examples
BAR_probs=function(importancesample){
  g_x=importancesample[[1]]
  w=exp(importancesample[[2]]-max(importancesample[[2]]))
  w=(w)/(sum(w))
  maxvals=apply(g_x,2,max) #Find max of each column
  replicated=matrix(rep(maxvals,dim(g_x)[1]), ncol=length(maxvals), byrow=T) #make a matrix of these maxima
  indicator=replicated==g_x #Then calculate the indicator function
  prob_xstar=indicator%*%w #First term in entropy
  return(prob_xstar)
}

#' Function: calculateinformation.  Calculates the information using importance sampling
#' @param g_x An importance sample of the success probabilities g(x,\beta).  Each column is a sample
#' @param log_w_i the logarithms of the weights corresponding to the g_x
#' @param IM The information measure to use: either entropy or variance
#' @return The actual information
#' @examples
calculateinformation=function(IM, importancesample){
  w_i=exp(importancesample[[2]]-max(importancesample[[2]]))
  w_i=(w_i)/(sum(w_i))
  if(IM=="entropy"){information=info_ent(importancesample[[1]],w_i)} 
  else if(IM=="variance") {information=info_var(importancesample[[1]],w_i)}
  else if(IM=="teff") {information=info_teff(importancesample[[1]],w_i)}
  else if(IM=="five") {information=info_five(importancesample[[1]],w_i)}
  else {information=info_tracevar(importancesample)}
  return(information)
}

#' Function: expected_information.  Calculates the expected information using importance sampling
#' @param g_x An importance sample of the success probabilities g(x,\beta)
#' @param log_w_i the logarithms of the weights corresponding to the g_x
#' @param IM The information measure to use: either entropy or variance
#' @param possible_x a matrix of unique elements of $\mathcal(X)$
#' @return The expected information for each action-outcome pair
#' @examples
expected_information=function(IM,importancesample,possible_x){
  g_x=importancesample[[1]]
  #Calculate weights
  w_i=exp(importancesample[[2]]-max(importancesample[[2]]))
  w_i=(w_i)/(sum(w_i))

  possible_y=c(0,1)
  I_results=matrix(0,length(possible_y),dim(possible_x)[1])
  p_results=matrix(0,length(possible_y),dim(possible_x)[1])
  
  # Calculate I(Sigma_t+1) by looping over action-outcome pairs
  for (j in 1:length(possible_y)){
    y=possible_y[j]
    for (k in 1:dim(possible_x)[1]){
      #Calculate weights w_ixy
      if(y==1){ log_g_x_k = log(g_x[k,])
      # Importance sampling for posterior predictive probabilities
      p.y       = sum(g_x[k,] * w_i) 
      }else{    log_g_x_k = log(1-g_x[k,])
      # Importance sampling for posterior predictive probabilities
      p.y       = sum((1-g_x[k,]) * w_i) 
      }
      log_wn    = importancesample[[2]]+log_g_x_k
      #Calculate information
      I_results[j,k]=calculateinformation(IM = IM, importancesample=list(g_x,log_wn,importancesample[[3]],importancesample[[4]]))
      p_results[j,k]=p.y
    }
  }
  expectedinformation=colSums(p_results*I_results)
  return(expectedinformation)
}

#' Function: release_trialdata - returns the trialdata with a lagged delay of 10 patients, so that outcomes are delayed
release_trialdata=function(trialdata,i){
  if(i<10){length=0}
  else {length=i-10}
  if (length==0){return(list(NULL,NULL,NULL,trialdata[[4]],trialdata[[5]],trialdata[[6]]))}
  else {trialdata_trimmed=list(trialdata[[1]][1:length,],trialdata[[2]][1:length,],trialdata[[3]][1:length,],trialdata[[4]],trialdata[[5]],trialdata[[6]])}
  return(trialdata_trimmed)
}

#' Function: Simulate trial: Simulates a trial with the BUD algorithm
#' @param numsteps The total T to run the trial for. 
#' @param true_beta A vector with the true values of the beta parameters
#' @param k the number of actions
#' @param d the degree of interaction effects that should be considered: between 1 (main effects) and the size of true_beta
#' @param prior A covariance matrix
#' @param h The power to which G (the expected information gain) should be raised
#' @param IS_size The number of monte carlo simulations
#' @param IM The information measure to use: either entropy or variance
#' @return The actions taken and the information values discovered
#' @examples

simulate_trial=function(numsteps,k,d,true_beta,prior,h,IS_size,IM,paramset,outcome_delay){
  param_length=length(true_beta)

  init_beta=rmvnorm(1,rep(0,param_length),diag(param_length))
  normaldraw=rmvnorm(IS_size,mean=rep(0,param_length)) #Create a standard gaussian draw
  trialdata=list()
  possible_z=action_combinations(k)
  
  trialdata[[4]]=get_interactions(possible_z,d) #Possible actions
  trialdata[[5]]=list()
  trialdata[[6]]=list()
  information_values=matrix(0,numsteps,1)
  
  if(h<0){Design="BAR"
  h=1}else{Design="BUD"}
  for (i in 1:numsteps){
    if (outcome_delay==1){
      #Process trialdata_input so that the outcome does not appear immediately
      trialdata_input=release_trialdata(trialdata,i)
    } else {trialdata_input=trialdata}
    #Approximate the posterior as a gaussian
    posterior=fit_gaussian(init_beta,trialdata_input,prior)
    #Carry out importance sampling
    importancesample=importance_sample(posterior,trialdata_input,normaldraw,prior)
    #Calculate and store the information
    Inf_current=calculateinformation(IM,importancesample)
    information_values[i,]=Inf_current
    if (Design=="BUD"){
      #Calculate E_G, the expected gain in information
      E_Inf_next=expected_information(IM = IM, importancesample = importancesample, possible_x = trialdata[[4]])
      E_G=E_Inf_next-Inf_current
      #Take an action with prob proportional to E_G^h then draw y
    }  else if (Design=="BAR") {
      E_G=BAR_probs(importancesample=importancesample)
    } 
    actionchosen= sample(x=1:length(E_G), size=1, prob =E_G^h)   
    new_z=possible_z[actionchosen,]
    new_x=get_interactions(matrix(new_z,nrow=1),d)
    if(paramset==11){new_y=drawoutcome_apfel(new_x)
    }else if(paramset==12){new_y=drawoutcome_cutrona(new_z)
    }else if(paramset==13){
      new_y=drawoutcome_piper(new_z,paramset)
    }else                 {new_y=rbinom(1,1,(exp(new_x%*%true_beta))/(1+exp(new_x%*%true_beta)))}
    #Store values
    trialdata[[1]]=rbind(trialdata[[1]],new_z)
    trialdata[[2]]=rbind(trialdata[[2]],new_y)
    trialdata[[3]]=rbind(trialdata[[3]],new_x)
  }

  #Set all to zero to start with
  armchosen=0
  true_positive=0
  false_positive=0
  return(list(information_values,trialdata,posterior,E_G,armchosen,true_positive,false_positive))
}


#' Function: Multiple_Simulations: Simulates multiple trials in a parallel fashion
#' @param numsims The number of simulations to run
#' @param numsteps A vector of covariates of dimension k
#' @param true_beta A vector with the true values of the beta parameters
#' @param k the number of actions
#' @param d the degree of interaction effects that should be considered: between 1 (main effects) and the size of true_beta
#' @param prior A covariance matrix
#' @param hvec The power to which G (the expected information gain) should be raised
#' @param IS_size The number of monte carlo simulations
#' @param IM The information measure to use: either entropy or variance
#' @return The actions taken and the information values discovered
#' @examples Can be run in parallel by changing %dopar% to %do%
multiple_simulations=function(numsims,numsteps,k,d,true_beta,prior,h,IS_size,IM,draw_from_prior,paramset,outcome_delay){
  functionlist=c('simulate_trial','rmvnorm','action_combinations','get_interactions','calculateinformation','expected_information','log_posterior','drawoutcome_apfel','drawoutcome_cutrona',
                 'nearPD','ginv','drawoutcome_piper','get_paramset','info_tracevar','tr','get_threshold',
                 'dmvnorm','info_ent','info_var','fit_gaussian','optimx','importance_sample','info_teff',
                 'BAR_probs','get_probs','choosebestarm','getprobless','chooseoverallarm','release_trialdata')
  Parallelised=foreach(i = 1:numsims, .combine=rbind,.export=functionlist) %dopar% {
    print(true_beta)
    print("started trial")
    trial_resultpar=simulate_trial(numsteps,k,d,true_beta,prior,h,IS_size,IM,paramset,outcome_delay)
  }
}

########################## Functions for analysis after the trial #######################################################
#' Function:process_parallel_trial_result This processes a parallelized trial result for plotting
#' @param trialresult an output from multiple_simulations with trial simulation results
#' @param savedparams an output from run_cluster with the trial simulation parameters
#' @return A list with 7 elements: information values, x values, y values, posteriors,final probabilities, 
#' intermediate posteriors throughout the trial and a full set of data processed into the intermediate chunks
#' @example
process_parallel_trial_result=function(trialresult,savedparams){
  hvals=length(trialresult)
  numsims=dim(trialresult[[1]])[1]
  numsteps=length(trialresult[[1]][[1]])
  betalength=dim(trialresult[[1]][1,2][[1]][[1]])[2]
  numactions=length(trialresult[[1]][1,4][[1]])
  collectionvalues=c(10,20,30,40,50,75,100,150,200,250,300,350,400,450,500)
  actual_collectionvalues=collectionvalues[collectionvalues<numsteps] #Use total trial length to trim vector
  
  y_values=array(0,dim=c(numsims,numsteps,hvals))
  x_values=array(0,dim=c(numsims,numsteps,betalength,hvals))
  extended_x_values=array(0,dim=c(numsims,numsteps,dim(trialresult[[1]][1,2][[1]][[3]])[2],hvals))
  information_values=array(0,dim=c(numsims,numsteps,hvals))
  posteriors <- matrix(list(), nrow=numsims,ncol=hvals)
  final_probs=array(0,dim=c(numsims,numactions,hvals))
  numposteriors=length(trialresult[[1]][1,][[2]][[5]])
  all_posteriors=array(list(),dim=c(numsims,hvals,numposteriors)) #Final dimension uses the collectionvalues vector 
  actions_chosen=array(0,dim=c(numsims,hvals))
  true_positives=array(0,dim=c(numsims,hvals))
  false_positives=array(0,dim=c(numsims,hvals))
  
  for (j in 1:hvals){
    for (i in 1:numsims){
      information_values[i,,j]=t(trialresult[[j]][i,1][[1]])
      x_values[i,,,j]=trialresult[[j]][i,2][[1]][[1]]
      y_values[i,,j]=trialresult[[j]][i,2][[1]][[2]]
      extended_x_values[i,,,j]=trialresult[[j]][i,2][[1]][[3]]
      posteriors[i,j]=trialresult[[j]][i,3]
      final_probs[i,,j]=trialresult[[j]][i,4][[1]]
      actions_chosen[i,j]=trialresult[[j]][i,5][[1]]
      true_positives[i,j]=trialresult[[j]][i,6][[1]]
      false_positives[i,j]=trialresult[[j]][i,7][[1]]
    }
  }
  
  k=savedparams[[1]]
  d=savedparams[[2]]
  true_beta=savedparams[[6]]
  get_probs_output=get_probs(true_beta,k,d)
  potential_actions=get_probs_output[,2:dim(get_probs_output)[2]]
  datamatrix=array(list(),dim=c(numsims,hvals,length(actual_collectionvalues)))
  return(list(information_values,x_values,y_values,posteriors,final_probs,all_posteriors,datamatrix,actions_chosen,true_positives,false_positives))
}

#Function that picks the best arm from a selection
#' @param chosen_set is a vector length num_actions that decides what to do.
#' @param delta is an array of importance sampled treatment effects
#' @param w_i is an array of corresponding weights from the importance sample
#' @param threshold2 is 0.9
choosebestarm=function(chosen_set,delta,w_i,threshold2){
  k=0
  lower=0
  upper=1
  while(k<100){
    middle=(lower+upper)/2
    probfound=max(1-getprobless(middle,delta,w_i))
    if(probfound<threshold2){upper=middle}else{lower=middle}
    k=k+1
  }
  armchosen=which.max(1-getprobless(middle,delta,w_i))
  difference=middle
  return(list(armchosen,difference))
}

#Function that picks the best arm for the trial
#' @param chosen_set is a vector length num_actions that decides what to do.
#' @param delta is an array of importance sampled treatment effects
#' @param w_i is an array of corresponding weights from the importance sample
chooseoverallarm=function(importancesample,possible_z,true_beta,k,d,paramset,threshold,threshold2){
  w_i=exp(importancesample[[2]]-max(importancesample[[2]])) # Get importance weights
  w_i=(w_i)/(sum(w_i))
  g_x=importancesample[[1]]
  delta=g_x-t(replicate(dim(possible_z)[1],g_x[1,])) # Determine how much better than controls
  problessthanzero=getprobless(0,delta,w_i)  
  chosen_set=(1-problessthanzero)>threshold # Chosen set is where probability of being better than control is 0.9
  if(sum(chosen_set[2:length(chosen_set)])<1){ #If chosen_set is empty
    armchosen=0
    true_positive=0
    false_positive=0
  } else {
    armchosen=choosebestarm(chosen_set,delta,w_i,threshold2)[[1]] # Choose the best arm of these 
    if (paramset==11){  #Apfel
      if (armchosen==49){
        true_positive=0
        false_positive=1
      } else if (armchosen==0){
        true_positive=0
        false_positive=1
      } else if (armchosen>0){
        true_positive=1
        false_positive=0
      }
    }
    else if(paramset==12) {  #Cutrona
      if (armchosen>0) {
        true_positive=1
        false_positive=0  #Was the chosen arm better than control?
      } else {
        true_positive=0
        false_positive=1
      }
    } else {  #The rest
      probvals=get_probs(true_beta,k,d)
      true_positive=1*(probvals[,1][armchosen]-probvals[,1][1]>0)  #Was the chosen arm better than control?
      false_positive=1*(probvals[,1][armchosen]-probvals[,1][1]<=0) #Was the chosen arm worse than or equal to control?  
    }
  }
  return(list(armchosen,true_positive,false_positive))
}


#This function uses a trial result and calculates arm recommended at the end of the trial
# as well as the Bayesian power/type 1 error that we use.
#The step size argument determines the length of trial to use if it is less than the full trial
#The use_num_sims argument determines how many simulations to use if you just want to sample (e.g. for debugging)
#The use_trial_size allows for analyzing the first t patients
#Can change threshold1 (a vector of gamma thresholds) and threshold2
#infomeasure_index changes the index of the design to analyze.  In the example, 1 is BAL, 2 is BAR and 3 is fBUD
calcpower_sample=function(result,use_trial_size,scenario,threshold1,use_num_sims,infomeasure_index,threshold2){
  true_beta=result[[1]][[6]]
  IS_size=result[[1]][[5]]
  k=result[[1]][[1]]
  d=result[[1]][[2]]
  
  param_length=length(true_beta)
  init_beta=rmvnorm(1,rep(0,param_length),diag(param_length))
  normaldraw=rmvnorm(IS_size,mean=rep(0,param_length))
  
  prior=result[[1]][[7]] 
  numsims=result[[1]][[3]]
  numdesigns=length(result[[2]])
  possible_z=action_combinations(k)
  responseprobs=get_probs(true_beta,k,d)[,1]
  truepositives=array(dim=c(numdesigns,numsims))
  falsepositives=array(dim=c(numdesigns,numsims))
  armschosen=array(dim=c(numdesigns,numsims))
  numactions=length(responseprobs)
  importancesamples=list()
  functionlist=c("dmvnorm","importance_sample","fit_gaussian","getprobless","choosebestarm","get_probs","log_posterior","plogis","get_interactions","action_combinations","nearPD",'ginv',"choosebestarm","chooseoverallarm")
  overallresults=array(dim=c(3,numsims,numdesigns))
  
  acomb <- function(...) abind(..., along=3)
  rlmat=result[[2]][[infomeasure_index]] 
  rlmat_transposed=t(rlmat)
  rlmat_transposed=rlmat_transposed[,1:use_num_sims]
  powers_par=foreach(rlmat_transposed_column=rlmat_transposed,.export=functionlist,.combine='acomb')%do%{  #rlmat_transposed_column=t(rlmat) specifically iterates through columns to prevent memory overload
    trial_result=rlmat_transposed_column # rlmat_transposed[,1] #when debugging
    trial_result[[2]][[1]]=trial_result[[2]][[1]][1:use_trial_size,]
    trial_result[[2]][[2]]=trial_result[[2]][[2]][1:use_trial_size]
    trial_result[[2]][[3]]=trial_result[[2]][[3]][1:use_trial_size,]
    fakeposterior=list()
    fakeposterior$hessian=-solve(prior)
    fakeposterior$par=numeric(length(true_beta))
    posteriortouse=fit_gaussian(init_beta,trialdata=trial_result[[2]],prior)
    importancesample=importance_sample(posteriortouse,trial_result[[2]],normaldraw,prior)
    par_result=array(dim=c(length(threshold1),3))
    for (tix in 1:length(threshold1)){
      thresholdval=threshold1[tix]
      armresult=chooseoverallarm(importancesample,possible_z,true_beta,k,d,scenario,threshold=thresholdval,threshold2)
      par_result[tix,1]=armresult[[1]]
      par_result[tix,2]=armresult[[2]]
      par_result[tix,3]=armresult[[3]]
    }
    return(par_result)
  }
  return(powers_par)
}

#For a given difference matrix and threshold probability level, this function calculates the probability using importance sampling
#that the importance is less than the threshold.  
getprobless=function(threshold_prob,delta,w_i){
  indicator2=(delta<threshold_prob)*1
  probgreater=indicator2%*%w_i
  return(probgreater)
}

#This function returns response probabilities from a paramset object
get_probs_from_scenario=function(scenario){
  params=get_scenario(scenario)
  return(get_probs(params[[3]],params[[1]],params[[2]]))
}

#' Function:  calculates action counts - mean and var
#' @param result an output from simulate_trial
#' @return A table with action counts
#' @example
get_all_action_counts_fast=function(result,trialresult_processed,hvalues){
  k=result[[1]][[1]]
  d=result[[1]][[2]]
  numsims=dim(result[[2]][[1]])[1]
  num_steps=result[[1]][[4]]
  true_beta=result[[1]][[6]]
  num_actions=2^k
  collect_action_means=array(dim=c(num_actions,length(hvalues)))
  collect_action_sds=array(dim=c(num_actions,length(hvalues)))
  collect_counts=array(dim=c(length(hvalues),num_actions,numsims))
  num_terms=log2(num_actions)+1
  for(hvalue in 1:length(hvalues)){
    trialdata=trialresult_processed[[2]][,,,hvalue] 
    action_counts=matrix(0,nrow=num_actions,ncol=dim(trialdata)[1])
    possible_actions=result[[2]][[1]][1,2][[1]][[4]]
    print(hvalue)
    for (i in 1:dim(possible_actions)[1]){
      currentrow=possible_actions[i,1:num_terms]
      replicated=matrix( rep( currentrow , dim(trialdata)[2] ),ncol=length(currentrow),byrow=TRUE)
      for (j in 1:dim(trialdata)[1]){
        count=sum(rowProds(trialdata[j,,]==replicated))
        action_counts[i,j]=count
      }
    }
    collect_action_means[,hvalue]=round(rowMeans(action_counts/num_steps),3) 
    collect_action_sds[,hvalue]=round(rowSds(action_counts/num_steps),3)
    collect_counts[hvalue,,]=action_counts/num_steps
  }
  probtable=cbind(round(get_probs(true_beta,k,d)[,1],2),get_probs(true_beta,k,d)[,2:(num_terms+1)])
  return(list(cbind(possible_actions,collect_action_means,collect_action_sds),collect_counts,collect_action_means,collect_action_sds))
}


##################################### Functions for generating plots ##########################################

#' Function:lineplot_parallel
#' @param trialresult_processed an output from process_parallel_trial_result that processed the output from multiple_simulations
#' @return A line plot with information over time
#' @example
lineplot_parallel=function(trialresult_processed,hvals,titles){
  df_trialresult=data.frame(colMeans(trialresult_processed[[1]]))
  colnames(df_trialresult)=titles
  df_trialresult$ID <- 1:nrow(df_trialresult)
  df_melt=melt(df_trialresult,id.vars = "ID")
  colnames(df_melt)[2]="h"
  q_var=ggplot(df_melt,aes(x=ID,y=value,col=h))+geom_line(size=0.7)+
    labs (x="t",y="Information") 
  return(q_var)
}


#Function takes a specific time point and plots a histogram of the information values at that time
specific_histogram=function(trialresult_processed,hvals,titles,tvalue){
  df_trialresult=data.frame(trialresult_processed[[1]][,tvalue,])
  colnames(df_trialresult)=as.character(hvals)
  colnames(df_trialresult)=titles
  df_trialresult$ID <- 1:nrow(df_trialresult)
  df_melt=melt(df_trialresult,id.vars = "ID")
  colnames(df_melt)[2]="Design"
  n_plot=ggplot(df_melt,aes(x=Design,y=value,fill=Design))+geom_violin()+
    xlab(TeX("Design"))+
    ylab(TeX("Distribution of Information"))
  return(n_plot)
}

#This function plots the number of times each action was taken, by design. 
action_counts_plot=function(action_counts_full,savedparams,titles){
  true_beta=savedparams[[6]]
  k=savedparams[[1]]
  d=savedparams[[2]]
  true_beta=savedparams[[6]]
  #action_counts_full=cbind(get_probs(true_beta,k,d)[,1],action_counts_full)
  df1=melt(action_counts_full[1,,])
  colnames(df1)=c("action","sim","count")
  df1$Design=titles[1]
  df2=melt(action_counts_full[2,,])
  colnames(df2)=c("action","sim","count")
  df2$Design=titles[2]
  df3=melt(action_counts_full[3,,])
  colnames(df3)=c("action","sim","count")
  df3$Design=titles[3]
  melted=rbind(df1,df2,df3)
  efficacies=get_probs(true_beta,k,d)[,1]
  actions_descriptive=paste0(round(efficacies,2)," (",1:length(efficacies),")")
  melted$fefficacies=lvls_revalue(as.factor(melted$action),as.character(round(efficacies,2)))
  melted$factor_actions_descriptive=lvls_revalue(as.factor(melted$action),as.character(actions_descriptive))
  melted$efficacies=as.numeric(as.character(melted$fefficacies))
  melted$reordered=fct_reorder(as.factor(melted$action),melted$efficacies)
  melted$revalued=lvls_revalue(melted$reordered,as.character(round(efficacies,2)))
  actionsplot=ggplot(melted, aes(x= fct_reorder(as.factor(factor_actions_descriptive),efficacies),y=count,fill=Design)) +
    geom_boxplot(outlier.shape=NA)+
    xlab(TeX("Arm specific success probability (arm number)"))+
    ylab("Allocation proportion")
  return(actionsplot)
}

#' Function:Plots the number of successes during the trial itself.
#' @param trialresult_processed an output from process_parallel_trial_result that processed the output from multiple_simulations
EO_lineplot_parallel=function(trialresult_processed,hvals,designnames){
  df_trialresult=data.frame(colMeans(trialresult_processed[[3]])) #non-cumulative
  colnames(df_trialresult)=as.character(hvals)
  colnames(df_trialresult)=designnames
  df_trialresult$ID <- 1:nrow(df_trialresult)
  df_melt=melt(df_trialresult,id.vars = "ID")
  colnames(df_melt)[2]="Design"
  q_var=ggplot(df_melt,aes(x=ID,y=value,col=Design))+geom_smooth(method="auto", fullrange=FALSE)+
    labs (x="Number of previously enrolled patients",y="Probability of positive response")
  return(q_var)
}

#This function plots the cumulative counts of actions taken over the course of a trial
plot_cumulative_action_counts=function(cumulative_action_counts){
melted=melt(cumulative_action_counts)
melted$Combination=as.factor(melted$Var2)
p<- ggplot(melted, aes(x=Var1, y=value)) + xlab("T")+
  #geom_line(aes(color=Combination))+
  geom_smooth(aes(color=Combination),method="auto", fullrange=FALSE, level=0.95)+
  ylab("Cumulative Allocation Proportion")
return(p)}

#Function takes a vector of treatment combinations chosen at the end of trials.  
# It also takes in the scenario or paramset
# Returns the number of times the best arm was chosen.  
calc_numtimes_bestchosen=function(final_arm_chosen,scenario){
  
  #For the hardcoded scenarios, hardcode the best arm.  For the other scenarios, can calculate using max.
  if (scenario==13){return(sum(final_arm_chosen==32))}
  if (scenario==11){return(sum(final_arm_chosen==61)+sum(final_arm_chosen==64))}
  if (scenario==12){return(sum(final_arm_chosen==4))} 
  else {return(sum(get_probs_from_scenario(scenario)[,1][final_arm_chosen]==max(get_probs_from_scenario(scenario)[,1])))
  }
}



####################### Functions for asymptotic analysis #######################

#Logistic function
logistic=function(x){ return(1/(1+exp(-x))) }

#Response probability of logistic model
G=function(a,beta_0){
  return(logistic(t(a)%*%(beta_0)))
}

#Gamma equation from asymptotic approximation
Gamma=function(k,beta_0,rho,d){
  atilde_all=get_interactions(cbind(1,expand.grid(replicate(k, c(0,1), simplify=FALSE))),d)  #matrix of all the atilde
  sumoutputs=array(dim=c(dim(atilde_all),dim(atilde_all)[2])) #Collect things to sum. atilde by atilde by numactions (k)
  for (atildeindex in 1:dim(sumoutputs)[1]){ 
    atilde=t(as.matrix(atilde_all[atildeindex,])) #pick the atilde we consider
    varterm=G(t(atilde),beta_0)*(1-G(t(atilde),beta_0))
    sumoutputs[atildeindex,,]=rho[atildeindex]*as.numeric(varterm)*t(atilde)%*%(atilde) #calculate
  }
  return(apply(sumoutputs,c(2,3),sum)) #Sum vertically
}

#Rho equation from asymptotic approximation
rhofn=function(k,beta_0,rho,d,h){
  atilde_all=get_interactions(cbind(1,expand.grid(replicate(k, c(0,1), simplify=FALSE))),d)  #matrix of all the atilde 
  
  outputs=numeric(length(rho))
  for (ajindex in 1:dim(atilde_all)[1]){
    a=t(atilde_all[ajindex,])
    Gbrackets=G(t(a),beta_0)*(1-G(t(a),beta_0))
    Gamma_Mat=Gamma(k,beta_0,rho,d)
    GammaBracket=(a%*%solve(Gamma_Mat%*%Gamma_Mat)%*%t(a))
    numerator=(Gbrackets*GammaBracket)^h
    outputs[ajindex]=numerator
  }
  denominator=sum(outputs)
  return(outputs/denominator)
}


#This function takes a specific trial result and finds the expected gain in information/probability of
#allocation of a specific arm.  It does this for a subset of the data.  
calcEG=function(singletrialresult,triallength,normaldraw,prior){
  singletrialresult[[2]][[1]]=singletrialresult[[2]][[1]][1:triallength,]
  singletrialresult[[2]][[2]]=as.vector(singletrialresult[[2]][[2]][1:triallength,])
  singletrialresult[[2]][[3]]=singletrialresult[[2]][[3]][1:triallength,]
  importancesample=importance_sample(singletrialresult[[3]],singletrialresult[[2]],normaldraw,prior)  #posterior, trialdata,normaldraw,prior
  Inf_current=calculateinformation(IM="tracevar",importancesample)
  E_Inf_next=expected_information(IM = "tracevar", importancesample = importancesample, possible_x = singletrialresult[[2]][[4]])
  E_G=E_Inf_next-Inf_current
  return(E_G)
}

#Normalizes a vector
normalize=function(x){
  return(x/sum(x))
}

#' Function:  calculates action counts - mean and var for each timepoint
#' @param result an output from simulate_trial
#' @return A table with action counts
get_all_action_counts_per_timepoint_fast=function(result,trialresult_processed,h_index){
  k=result[[1]][[1]]
  d=result[[1]][[2]]
  numsims=dim(result[[2]][[1]])[1]
  num_steps=result[[1]][[4]]
  true_beta=result[[1]][[6]]
  num_actions=2^k
  num_terms=log2(num_actions)+1
  trialdata=trialresult_processed[[2]][,,,h_index] 
  action_counts=array(dim=c(num_actions,dim(trialdata)[2],numsims))
  possible_actions=result[[2]][[1]][1,2][[1]][[4]]
  for (i in 1:dim(possible_actions)[1]){
    currentrow=possible_actions[i,1:num_terms]
    replicated=matrix( rep( currentrow , dim(trialdata)[2] ),ncol=length(currentrow),byrow=TRUE)
    for (j in 1:dim(trialdata)[1]){
      count=rowProds(trialdata[j,,]==replicated)
      action_counts[i,,j]=count
    }
  }
  action_counts_mean=apply(action_counts,c(1,2),mean)
  action_counts_cummean=apply(action_counts_mean,1,cummean)
  probtable=cbind(round(get_probs(true_beta,k,d)[,1],2),get_probs(true_beta,k,d)[,2:(num_terms+1)])
  return(list(action_counts,action_counts_mean,action_counts_cummean))
}

