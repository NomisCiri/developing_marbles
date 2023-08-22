### for simulations

generalized_beta_cdf<-function(x,alpha,beta,begin,end){
  z = x/end#rescale the input value
  prob = pbeta(z, alpha, beta)#just look up in the beta cumulative distribution function. 
  return(prob)
}

rho<-seq(0,2,length.out = dataList$N)%>%sample()
UncertaintyMulti<-seq(0,5,length.out = dataList$N)%>%sample()
alpha_add<-seq(0,2,length.out = dataList$N)%>%sample()

UncertaintyMulti<-rep(1,dataList$N)
alpha_add<-rep(1,dataList$N)

simulateModel<-function(dataList,rho,UncertaintyMulti,alpha_add){
  ## unpack data
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
  p_gamble = dataList$p_gambleIndex
  gambleList=dataList$gambleList
  NGambles=length(gambleList)
  choice = dataList$choice
  risk1Unc0=dataList$risk1Unc0
  choice2=choice
  
  for(i in 1:N){
    for(t in 1:Tsubj[i]) {
      if(risk1Unc0[i, t]==1){#is it an uncertain trial?
        newAlpha=Sucess[i,t]^alpha_add[i]
        newBeta=Fail[i,t]^alpha_add[i]
      #  p_gamble_est[i,t] ~ beta(newAlpha,newBeta)estimate the compression parameters. 
        #beta_rng(alpha_total,beta_total)
        U_safe  = safe_payoff[i, t]^rho[i]
        U_risky = risky_payoff[i, t]^rho[i]
        EU_risky= p_gamble_est[i,t]*U_risky
        #socialinfo Block: Overwrite the shape parameters
        if (condition[i, t] == 1) {#safe-safe
          newBeta=newBeta*UncertaintyMulti[i];# beta to the power of Social Info
          newAlpha=newAlpha
        }else if (condition[i, t] == 3) {# risky-risky
          newBeta=newBeta
          newAlpha=newAlpha*UncertaintyMulti[i];#alpha to the power of Social Info
        }
        #print(as.numeric(rbernoulli(n=1,p=generalized_beta_cdf((EU_risky-EU_safe), newAlpha,newBeta,0.00001,U_risky))))
        choice[i, t]=as.numeric(rbernoulli(1,p=(1-generalized_beta_cdf((U_safe), newAlpha,newBeta,0,U_risky))))#each choice is predicted as a bernoulli distributed random variable.
        choice2[i,t]=rbernoulli(1,(1/1+exp(-(EU_risky-U_safe)*1)))
      }#end Uncertain
    } #endTrail
  }#endSub
  return(list(choice,choice2))
}


