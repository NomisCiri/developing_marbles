// social influence under Risk and Uncertainty on risky and safe options

data {
  int<lower=1> N;// Number of Subjects
  int<lower=1> T;// Trials
  int <lower=1> n_gambles;
  
  int<lower=1, upper=T> t_subj[N];//number of trials for each subject per Group
  real<lower=0,upper=1>gambles_list[n_gambles];//to estimate gamble level priors
  
  int<lower=0, upper=1>choice[N, T];//actual decision
  
  int<lower=0, upper=2>risk1_Unc2[N,T]; // was decision made under risk or uncertainty
  int<lower=0, upper=1>social_info_risky[N,T]; // dummy variable, 1 if advice was risky
  int<lower=0, upper=1>social_info_safe[N,T]; // dummy variable, 1 if advice was safe
  int<lower=0, upper=1>social_trial[N,T]; // dummy variable, 1 if advice was safe
  int<lower=0, upper=2>social_info_risk2_safe1[N,T]; // dummy variable, 1 if advice was safe
  
  real risky_payoff[N, T];
  //safe payoff is always 5
  
  //int<lower=0, upper=n_gambles> gamble_idx[N, T];//index of actual gamble
  real<lower=0, upper=1> p_gamble_est[N, T];//probability; either estimated or described 
}

transformed data {
  int soc=2;// how many social social_info_risk2_safe1s
  int unc=2;// how many uncertainty social_info_risk2_safe1s
  //social social_info_risk2_safe1s * uncertainty social_info_risk2_safe1s + gambles times uncertainty social_info_risk2_safe1s +
  // model parameters inverse temperature and reward sensitvity
  int n_params=soc*unc + unc + 4;// number of parameters + 4 kappas
}

parameters {
  // Define parameters based on their correlation and means
  vector[n_params] mu_pars;            // hyperprior means
  vector<lower=0>[n_params] sig;                // hyperprior variances
  cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix[n_params,N] scale; // prior scaling for each parameter
  // group predictors
}

transformed parameters {
  // individual level pars on probit scale
  matrix[N, n_params] params_phi;
  //kappas (sample size param of beta)
  vector<lower=1>[N] kappas[unc,2];// makes a vector of size 2 where each element contains a vector of size N
  //psis (social influence)
  vector<lower=0>[N] psis[unc];
  //stochastic utility parameters risk attittue and temperature
  vector<lower=0,upper=2>[N] rho;
  vector<lower=0>[N] tau;  
  //model internal variables
  // makes N entries that each contain a vector of size T (trials)
  vector<lower=0>[T] alpha[N];
  vector<lower=0>[T] beta[N];
  vector<lower=0, upper=1>[T] probs[N];
  
  vector[T] ev_diffs[N];
  vector[T] U_risk[N];
  vector[T] U_safe;
  // what follows this is the same as noncentered parametrzation, but taken from 
  // the cholesky factors of the prameter correlation matrix. 
  // variance of parameter, scaled with a value sampled from a standard normal 
  // on the individual level
  params_phi = (diag_pre_multiply(sig, l_omega) * scale)';
  // for all participants, transform back on modelscale
  rho=Phi_approx(mu_pars[1] + params_phi[,1])*2; 
  tau=exp(mu_pars[2] + params_phi[,2]);
  //social parameters
  psis[1]=1+Phi_approx(mu_pars[3] + params_phi[,3])*5;
  psis[2]=1+Phi_approx(mu_pars[4] + params_phi[,4])*5; 
  //uncertainty_parameter must be at least 1, maximum 20
  kappas[1,1]=1+Phi_approx(mu_pars[5] + params_phi[,5])*20; 
  kappas[1,2]=1+Phi_approx(mu_pars[6] + params_phi[,6])*20;
  kappas[2,1]=1+Phi_approx(mu_pars[7] + params_phi[,7])*20;
  kappas[2,2]=1+Phi_approx(mu_pars[8] + params_phi[,8])*20;
  //kappas[2]=1+Phi_approx(mu_pars[8] + params_phi[,8])*20; 
  
  for (ppt in 1:N){
    //fill with dummy values because some trials are missing
    for (t in 1:T){
      alpha[ppt][t]=1;
      beta[ppt][t]=1;
      probs[ppt][t]=0.5;
      ev_diffs[ppt][t]=-1;
      U_risk[ppt][t]=-1;
    }
    // Bayesian updating utility model
    U_safe[ppt]=pow(5,rho[ppt]);    //safe payoff is always 5
    for (trial in 1:t_subj[ppt]){
      //calculate beta distribution parameters from rate (kappa) and estimated gamble probabilty
      beta[ppt][trial]=(1-p_gamble_est[ppt,trial])*kappas[risk1_Unc2[ppt,trial],1][ppt];
      alpha[ppt][trial]=p_gamble_est[ppt,trial]*kappas[risk1_Unc2[ppt,trial],2][ppt];
      // update belief based on social info
      // in the reward sensitvity model, add to alpha shape parameter
      alpha[ppt][trial]+=psis[risk1_Unc2[ppt,trial]][ppt]*social_trial[ppt,trial];
      // get updated mean
      probs[ppt][trial]=alpha[ppt][trial]/(alpha[ppt][trial]+beta[ppt][trial]);
      //riskypayoff
      U_risk[ppt][trial]=probs[ppt][trial]*pow(risky_payoff[ppt,trial],rho[ppt]);
      ev_diffs[ppt][trial]=(U_risk[ppt][trial]-U_safe[ppt])/tau[ppt];
    }//end trials
  }//end ppts
}//end transfomred params




model {
  //hyperprior means
  mu_pars[1]~ normal(0,0.1);   //rho
  mu_pars[2]~ std_normal();  // social
  mu_pars[3:4]~ std_normal();  // social
  mu_pars[5:8]~ std_normal();  // kappa
  // hyperprior variances
  sig[1]~ gamma(1,10);   
  sig[2]~ gamma(1,1);  
  sig[3:4]~ gamma(1,10); 
  sig[5:8]~ gamma(1, 10); 
  // hyperprior ppt level
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  l_omega~lkj_corr_cholesky(1);   
  //predict choices
  for (ppt in 1:N){
    //p_gamble_est[ppt,]~beta(alpha[ppt],beta[ppt]);//to properly estimate the rate
    choice[ppt,1:t_subj[ppt]] ~ bernoulli_logit(ev_diffs[ppt][1:t_subj[ppt]]);
  }
}//endModel


generated quantities {
  //For log likelihood calculation
  real log_lik[N,T];
  // For posterior predictive check
  real y_pred[N,T];
  //Set all posterior predictions to 0 (avoids NULL values)
  for (ppt in 1:N) {
    for (t in 1:T) {
      y_pred[ppt,t] = -1;
      log_lik[ppt,t]=0;
    }
    // loglik etc
    for (t_s in 1:t_subj[ppt]){
      y_pred[ppt,t_s]=bernoulli_logit_rng(ev_diffs[ppt][t_s]);
      log_lik[ppt,t_s]=bernoulli_logit_lpmf(choice[ppt,t_s] | ev_diffs[ppt][t_s]);
    }
  }
}

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
            //         if (social_info_risk2_safe1[i, t] == 1) {  // safe-safe
            //         newBeta=newBeta+psi_Risk_safe[i];// beta to the power of Social Info
            //         }if (social_info_risk2_safe1[i, t] == 3) {  // risky-risky
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
            //       if (social_info_risk2_safe1[i, t] == 1) {  // safe-safe
            //       newBeta=newBeta+psi_safe[i];// beta to the power of Social Info
            //       }if (social_info_risk2_safe1[i, t] == 3) {  // risky-risky
            //       newAlpha=newAlpha+psi_risk[i];//alpha to the power of Social Info
            //       }
            //       }//end Uncertain
            //       log_lik[i,t]= bernoulli_lpmf(choice[i,t]|1-generalized_beta_cdf(U_safe, newAlpha,newBeta,U_risky));
            //       y_pred[i, t] = bernoulli_rng(1-generalized_beta_cdf(U_safe, newAlpha,newBeta,U_risky));// each choice is predicted as a bernoulli distributed random variable.		} //endTrail
            //     } //endSub
            //   }//end PostPred
            // }
            
            