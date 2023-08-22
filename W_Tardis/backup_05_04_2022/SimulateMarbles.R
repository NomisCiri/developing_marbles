simulateMarbleModel<-function(dataList,rho,kappa,alpha_add,psi_Risk,psi){
  #unpack data!
  N       = dataList$N## number of subjects in each group.
  T       = dataList$T
  #Seq     = Sequence_Length,
  Tsubj   = dataList$Tsubj
  # numPars = numPars,
  safe_payoff    = dataList$safe_payoff
  risky_payoff = dataList$risky_payoff
  p_gamble_est= dataList$p_gamble_est
  Sucess  = dataList$Sucess
  Fail    = dataList$Fail
  condition  = dataList$condition # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = dataList$p_gamble
  gambleList=dataList$gambleList
  NGambles=length(gambleList)
  risk1Unc0=dataList$risk1Unc0
  #first define the betacdf function here too
  generalized_beta_cdf<-function(x,alpha,beta,begin,end){
    z = x/end;#rescale the input value
    prob = pbeta(z, alpha, beta);# just look up in the beta cumulative distribution function. 
    if (prob>0.99){
      prob=0.99;
    }else if(prob<0.01){
      prob=0.01;
    }
    return(prob);
  }
  
  y_pred=array(0,dim=c(N,T))
  alpha_shape=array(0,dim=c(N,NGambles))
  beta_shape=array(0,dim=c(N,NGambles))
  for (i in 1:N) {
    for(gambles in 1:NGambles){
      alpha_shape[i,gambles]=gambleList[gambles]*kappa
      beta_shape[i,gambles]=(1-gambleList[gambles])*kappa
    }#end Gambleloop
    #check trials.
    for (t in 1:Tsubj[i]) {
      # internal variables that i can throw away afterwards 
      newAlpha=alpha_shape[i,p_gamble[i,t]]
      newBeta=beta_shape[i,p_gamble[i,t]]
      if(risk1Unc0[i, t]==0){
        #first compute utilities here. 
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #SocialInfoBlock: Overwrite the shape parameters
        if (condition[i, t] == 1) {  #safe-safe
          newBeta=beta_shape[i,p_gamble[i,t]]*psi_Risk#beta to the power of Social Info
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=alpha_shape[i,p_gamble[i,t]]*psi_Risk#alpha to the power of Social Info
        }
      }else if(risk1Unc0[i, t]==1){ #is it an uncertain trial?
        newAlpha=Sucess[i,t]^alpha_add
        newBeta=Fail[i,t]^alpha_add
        p_gamble_est[i,t]=rbeta(1,newAlpha,newBeta)
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #socialinfo Block: Overwrite the shape parameters
        if (condition[i, t] == 1) {  # safe-safe
          newBeta=newBeta*psi# beta to the power of Social Info
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=newAlpha*psi#alpha to the power of Social Info
        }
      }#end Uncertain
      y_pred[i, t] = as.numeric(rbernoulli(1,1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky)))# each choice is predicted as a bernoulli distributed random variable.		} //endTrail
    } #endTrial
  }#endSub
  return(list(y_pred,p_gamble_est))
}#end PostPred


simulateMarbleModel2<-function(dataList,rho,kappa,alpha_add_risk,alpha_add_safe,psi_Risk,psi){
  #unpack data!
  N       = dataList$N## number of subjects in each group.
  T       = dataList$T
  #Seq     = Sequence_Length,
  Tsubj   = dataList$Tsubj
  # numPars = numPars,
  safe_payoff    = dataList$safe_payoff
  risky_payoff = dataList$risky_payoff
  p_gamble_est= dataList$p_gamble_est
  Sucess  = dataList$Sucess
  Fail    = dataList$Fail
  condition  = dataList$condition # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = dataList$p_gamble
  gambleList=dataList$gambleList
  NGambles=length(gambleList)
  risk1Unc0=dataList$risk1Unc0
  #first define the betacdf function here too
  generalized_beta_cdf<-function(x,alpha,beta,begin,end){
    z = x/end;#rescale the input value
    prob = pbeta(z, alpha, beta);# just look up in the beta cumulative distribution function. 
    if (prob>0.99){
      prob=0.99;
    }else if(prob<0.01){
      prob=0.01;
    }
    return(prob);
  }
  
  y_pred=array(0,dim=c(N,T))
  alpha_shape=array(0,dim=c(N,NGambles))
  beta_shape=array(0,dim=c(N,NGambles))
  
  for (i in 1:N) {
    for(gambles in 1:NGambles){
      alpha_shape[i,gambles]=gambleList[gambles]*kappa
      beta_shape[i,gambles]=(1-gambleList[gambles])*kappa
    }#end Gambleloop
    #check trials.
    for (t in 1:Tsubj[i]) {
      # internal variables that i can throw away afterwards 
      newAlpha=alpha_shape[i,p_gamble[i,t]]
      newBeta=beta_shape[i,p_gamble[i,t]]
      if(risk1Unc0[i, t]==0){
        #first compute utilities here. 
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #SocialInfoBlock: Overwrite the shape parameters
        if (condition[i, t] == 1) {  #safe-safe
          newBeta=beta_shape[i,p_gamble[i,t]]*psi_Risk#beta to the power of Social Info
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=alpha_shape[i,p_gamble[i,t]]*psi_Risk#alpha to the power of Social Info
        }
      }else if(risk1Unc0[i, t]==1){ #is it an uncertain trial?
        newAlpha=Sucess[i,t]^alpha_add_risk
        newBeta=Fail[i,t]^alpha_add_safe
        p_gamble_est[i,t]=rbeta(1,newAlpha,newBeta)
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #socialinfo Block: Overwrite the shape parameters
        if (condition[i, t] == 1) {  # safe-safe
          newBeta=newBeta*psi# beta to the power of Social Info
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=newAlpha*psi#alpha to the power of Social Info
        }
      }#end Uncertain
      y_pred[i, t] = as.numeric(rbernoulli(1,1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky)))# each choice is predicted as a bernoulli distributed random variable.		} //endTrail
    } #endTrial
  }#endSub
  return(list(y_pred,p_gamble_est))
}#end PostPred



simulateMarbleModelFinal<-function(dataList,rho,kappa,alpha_add,psi_Risk_risk,psi_Risk_safe,psi_safe,psi_risk,tau){
  #unpack data!
  N       = dataList$N## number of subjects in each group.
  T       = dataList$T
  #Seq     = Sequence_Length,
  Tsubj   = dataList$Tsubj
  # numPars = numPars,
  safe_payoff    = dataList$safe_payoff
  risky_payoff = dataList$risky_payoff
  p_gamble_est= dataList$p_gamble_est
  Sucess  = dataList$Sucess
  Fail    = dataList$Fail
  condition  = dataList$condition # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = dataList$p_gamble
  gambleList=dataList$gambleList
  NGambles=length(gambleList)
  risk1Unc0=dataList$risk1Unc0
  #first define the betacdf function here too
  generalized_beta_cdf<-function(x,alpha,beta,end,tau){
    z = x/end#rescale the input value
    # print(paste0("z: ",z))
    # print(paste0("x: ",x))
    # print(paste0("end: ",end))
    # print(paste0("alpha: ",alpha))
    # print(paste0("beta: ",beta))
    #z=1/(1+exp(-((x-end)*tau)))#safe-risk
    #print(z)
    prob = pbeta(z, alpha, beta);# just look up in the beta cumulative distribution function. 
    if (prob>0.99){
      prob=0.99
    }else if(prob<0.01){
      prob=0.01;
    }
    return(prob)
  }
  
  y_pred=array(0,dim=c(N,T))
  alpha_shape=array(0,dim=c(N,NGambles))
  beta_shape=array(0,dim=c(N,NGambles))
  
  for (i in 1:N) {
    for(gambles in 1:NGambles){
      alpha_shape[i,gambles]=gambleList[gambles]*kappa# should actually be more kappas but this cancels out over all iterations
      beta_shape[i,gambles]=(1-gambleList[gambles])*kappa
    }#end Gambleloop
    #check trials.
    for (t in 1:Tsubj[i]) {

      # internal variables that i can throw away afterwards 
      newAlpha=alpha_shape[i,p_gamble[i,t]]
      newBeta=beta_shape[i,p_gamble[i,t]]
      # browser()
      if(risk1Unc0[i, t]==0){
        #first compute utilities here. 
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #SocialInfoBlock: Overwrite the shape parameters
        if (condition[i, t] == 1) {  #safe-safe
          #print(newBeta)
          newBeta=beta_shape[i,p_gamble[i,t]]*psi_Risk_safe#beta to the power of Social Info
          #print("hm")
          #print(newBeta)
          #print(psi_Risk_safe)
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=alpha_shape[i,p_gamble[i,t]]*psi_Risk_risk#alpha to the power of Social Info
        }
      }else if(risk1Unc0[i, t]==1){ #is it an uncertain trial?
        newAlpha=Sucess[i,t]^alpha_add
        newBeta=Fail[i,t]^alpha_add
        p_gamble_est[i,t]=rbeta(1,newAlpha,newBeta)
        U_safe  = safe_payoff[i, t]^rho
        U_risky = risky_payoff[i, t]^rho
        #socialinfo Block: Overwrite the shape parameters
        if (condition[i, t] == 1) {  # safe-safe
          newBeta=newBeta*psi_safe# beta to the power of Social Info
          
        }else if (condition[i, t] == 3) {  # risky-risky
          newAlpha=newAlpha*psi_risk#alpha to the power of Social Info
        }
      }#end Uncertain
      #browser()
      #print(y_pred[i, t])
      #if(t==73){
      # browser()
      #}
      # print(t)
      #  print(newBeta)
      #  print(newAlpha)
      
      #print(as.numeric(rbernoulli(1,1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky))))
      #print(y_pred)
      #browser()
      y_pred[i, t] = as.numeric(rbernoulli(1,1-generalized_beta_cdf(U_safe, newAlpha,newBeta,U_risky,tau)))# each choice is predicted as a bernoulli distributed random variable.		} //endTrail
      # print(paste0("ypred:",y_pred[i, t]))
      # print(paste0("prob:",1-generalized_beta_cdf(U_safe,newAlpha,newBeta,U_risky,tau)))
      # print("")
      # print(paste0("U_safe:",U_safe))
      # print(paste0("U_risky:",U_risky))
      # print(paste0("newAlpha:",newAlpha))
      # print(paste0("newBeta:",newBeta))
      # print(paste0("SI:",condition[i, t]))
      # print("")
      # print("")
      } #endTrial
    print(paste0("sub",i))
  }#endSub
  return(list(y_pred,p_gamble_est))
}#end PostPred



