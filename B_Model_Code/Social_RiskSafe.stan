data {
  int<lower=1> N;// Number of Subjects
  int<lower=1> T;// Trials
  int <lower=1> n_gambles;
  
  int<lower=1, upper=T> t_subj[N];//number of trials for each subject per Group
  real<lower=0,upper=1>gambles_list[n_gambles];//to estimate gamble level priors
  
  int<lower=0, upper=1>choice[N, T];//actual decision
  
  int<lower=1, upper=2>risk1Unc2[N,T]; // was decision made under risk or uncertainty
  int<lower=0, upper=1>social_info_risky[N,T]; // dummy variable, 1 if advice was risky
  int<lower=0, upper=1>social_info_safe[N,T]; // dummy variable, 1 if advice was safe
  int<lower=0, upper=1>social_trial[N,T]; // dummy variable, 1 if advice was safe
  int<lower=1, upper=2>condition[N,T]; // dummy variable, 1 if advice was safe
  
  real risky_payoff[N, T];
  //safe payoff is always 5
  
  int<lower=0, upper=n_gambles> gamble[N, T];//index of actual gamble
  real<lower=0, upper=1> p_gamble_est[N, T];//probability; either estimated or described 
}

transformed data {
  int soc=2;// how many social conditions
  int unc=2;// how many uncertainty conditions
  
  //social conditions * uncertainty conditions + gambles times uncertainty conditions +
  // model parameters inverse temperature and reward sensitvity
  int n_params=soc*unc + unc + 2;// number of parameters 
}

parameters {
  // Define parameters based on their correlation and means
  vector[n_params] mu_pars;            // hyperprior means
  vector[n_params] sig;                // hyperprior variances
  cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix<lower=0>[n_params,N] scale; // prior scaling for each parameter
  // group predictors
}

transformed parameters {
  // individual level pars on probit scale
  matrix[N, n_params] params_phi;
  //kappas
  vector[N] kappas[unc];
  //psis
  vector[N] psis[soc,unc];
  vector[N] rho;
  vector[N] tau;  
  //model internal
  // from alpha & beta, we derive proabilites
  vector[N] alpha[T];
  vector[N] beta[T];
  vector[N] probs[T];
  
  vector[N] ev_diffs[T];
  vector[N] U_risk[T];
  vector[N] U_safe;
  // transform individual displacements with var-cov matrix and transpose
  params_phi = (diag_pre_multiply(sig, l_omega) * scale)';
  // for all participants, transform back on modelscale
  rho=Phi_approx(mu_pars[1] + params_phi[,1]) * 2; 
  tau=Phi_approx(mu_pars[2] + params_phi[,2]) * 5; 
  //social parameters
  psis[1,1]=Phi_approx(mu_pars[3] + params_phi[,3]) * 5; 
  psis[2,1]=Phi_approx(mu_pars[4] + params_phi[,4]) * 5; 
  psis[1,2]=Phi_approx(mu_pars[5] + params_phi[,5]) * 5; 
  psis[2,2]=Phi_approx(mu_pars[6] + params_phi[,6]) * 5; 
  //uncertainty_parameter
  kappas[1]=0.00001+Phi_approx(mu_pars[7] + params_phi[,7]) * 5; 
  kappas[2]=0.00001+Phi_approx(mu_pars[8] + params_phi[,8]) * 5; 
  //model internal parameters
  
  for (ppt in 1:N){
    //safe payoff is always 5
    U_safe[ppt]=pow(5,rho[ppt]);
    
    for (trial in 1:t_subj[ppt]){
      //calculate beta distribution parameters from rate (kappa) 
      beta[ppt][trial]=(1-p_gamble_est[ppt,trial])*kappas[ppt][risk1Unc2[ppt,trial]];
      alpha[ppt][trial]=probs[ppt,trial]*kappas[ppt][risk1Unc2[ppt,trial]];
      //dummycode conditions
      //update belief based on social info
      // condition is always 1 for nonsocial trials.
      alpha[ppt][trial]+=psis[ppt][risk1Unc2[ppt,trial],condition[ppt,trial]]*social_info_risky[ppt,trial]*social_trial[ppt,trial];
      beta[ppt][trial]+=psis[ppt][risk1Unc2[ppt,trial],condition[ppt,trial]]*social_info_safe[ppt,trial]*social_trial[ppt,trial];
      // get updated mean
      probs[ppt][trial]=alpha[ppt][trial]/(alpha[ppt][trial]+beta[ppt][trial]);
      //riskypayoff
      U_risk[ppt][trial]=probs[ppt][trial]*pow(risky_payoff[ppt,trial],rho[ppt]);
    }//end trials
    ev_diffs[ppt]=(U_risk[ppt]-U_safe[ppt])*tau[ppt];
  }//end ppts
}//end transfomred params




model {
  //hyperpriors
  mu_pars~ std_normal();            // hyperprior means
  sig~gamma(1, 10);                 // hyperprior variances
  to_vector(scale) ~ std_normal();// hyperprior ppt level
  l_omega~lkj_corr_cholesky(6);   // prior correlation of parameters
  //
  for (ppt in 1:N){
    choice[ppt,] ~ bernoulli_logit(ev_diffs[ppt]);
  }
}//endModel








// 
// 
// 
// 
// 
// 
// 
// 
// generated quantities {
  //   // For log likelihood calculation
  //   real log_lik[N,T];
  //   // For posterior predictive check
  //   real y_pred[N,T];
  //   real gamble_Pred[N,NGambles];
  //   // Set all posterior predictions to 0 (avoids NULL values)
  //   for (i in 1:N) {
    //     for (t in 1:T) {
      //       y_pred[i,t] = -1;
      //       log_lik[i,t]=0;
      //     }
      //   }
      //   
      //   for (i in 1:N) {
        //     //check trials.
        //     for (t in 1:Tsubj[i]) {
          //       // internal variables that i can throw away afterwards 
          //       real U_safe;
          //       real U_risky;
          //       real newAlpha=alpha_shape[i,p_gamble[i,t]];
          //       real newBeta=beta_shape[i,p_gamble[i,t]];
          //       real EU_risky;
          //       
          //       if(risk1Unc0[i, t]==0){
            //         //first compute utilities here. 
            //         U_safe  = pow(safe_payoff[i, t], rho[i]);
            //         U_risky = pow(risky_payoff[i, t], rho[i]);
            //         //SocialInfoBlock: Overwrite the shape parameters
            //         if (condition[i, t] == 1) {  // safe-safe
            //         newBeta=newBeta+psi_Risk_safe[i];// beta to the power of Social Info
            //         }if (condition[i, t] == 3) {  // risky-risky
            //         newAlpha=newAlpha+psi_Risk_risk[i];//alpha to the power of Social Info
            //         }
            //       }//end Risk
            //       if(risk1Unc0[i, t]==1){// is it an uncertain trial?
            //       real socialAlphaUnc;
            //       real socialBetaunc;
            //       newAlpha=pow(Sucess[i,t],alpha_add[i]);
            //       newBeta=pow(Fail[i,t],alpha_add[i]);
            //       //p_gamble_est[i,t] ~ beta(newAlpha,newBeta);// estimate the compression parameters.
            //       gamble_Pred[i,p_gamble[i,t]]=beta_rng(newAlpha,newBeta);
            //       //beta_rng(alpha_total,beta_total)
            //       U_safe  = pow(safe_payoff[i, t], rho[i]);
            //       U_risky = pow(risky_payoff[i, t], rho[i]);
            //       //socialinfo Block: Overwrite the shape parameters
            //       if (condition[i, t] == 1) {  // safe-safe
            //       newBeta=newBeta+psi_safe[i];// beta to the power of Social Info
            //       }if (condition[i, t] == 3) {  // risky-risky
            //       newAlpha=newAlpha+psi_risk[i];//alpha to the power of Social Info
            //       }
            //       }//end Uncertain
            //       log_lik[i,t]= bernoulli_lpmf(choice[i,t]|1-generalized_beta_cdf(U_safe, newAlpha,newBeta,U_risky));
            //       y_pred[i, t] = bernoulli_rng(1-generalized_beta_cdf(U_safe, newAlpha,newBeta,U_risky));// each choice is predicted as a bernoulli distributed random variable.		} //endTrail
            //     } //endSub
            //   }//end PostPred
            // }
            