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
  int n_params=soc*unc + unc + 2;// number of parameters 
}

parameters {
  // Define parameters based on their correlation and means
  vector[n_params] mu_pars;            // hyperprior means
  vector<lower=0>[n_params] sig;                // hyperprior variances
  //cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix<lower=0>[n_params,N] scale; // prior scaling for each parameter
  // group predictors
}

transformed parameters {
  // individual level pars on probit scale
  matrix[N, n_params] params_phi;
  //kappas
  vector<lower=0>[N] kappas[unc];// makes a vector of size 2 where each element contains a vector of size N
  //psis
  vector[N] psis[soc,unc];// makes a matrix where each element contains a vector of size N
  vector<lower=0,upper=2>[N] rho;
  vector<lower=0>[N] tau;  
  //model internal
  // from alpha & beta, we derive proabilites
  vector[T] alpha[N];// makes N alphas that each contain a vector of size T
  vector[T] beta[N];
  vector[T] probs[N];
  
  vector[T] ev_diffs[N];
  vector[T] U_risk[N];
  vector[T] U_safe;
  // this is the same as noncentered parametrzation. 
  // variance of parameter, scaled with a value sampled from a standard normal 
  // on the individual level
  //params_phi = (diag_pre_multiply(sig, l_omega) * scale)';
// params_phi = (diag_pre_multiply(sig * scale)';
  //print(params_phi)
  // for all participants, transform back on modelscale
  rho=Phi_approx(mu_pars[1] + sig[1]*scale[1,]')*2; 
  tau=exp(mu_pars[2] + sig[2]*scale[2,]'); 
  //social parameters
  psis[1,1]=mu_pars[3] + sig[3]*scale[3,]';
  psis[2,1]=mu_pars[4] + sig[4]*scale[4,]'; 
  psis[1,2]=mu_pars[5] + sig[5]*scale[5,]'; 
  psis[2,2]=mu_pars[6] + sig[6]*scale[6,]'; 
   //print(psis[1,1])
  //uncertainty_parameter
  kappas[1]=exp(mu_pars[7] + sig[7]*scale[7,])'; 
  kappas[2]=exp(mu_pars[8] + sig[8]*scale[8,])'; 
  //model internal parameters
  for (ppt in 1:N){
    //safe payoff is always 5
    U_safe[ppt]=pow(5,rho[ppt]);
    //print(U_safe[ppt])
    for (trial in 1:t_subj[ppt]){
      //calculate beta distribution parameters from rate (kappa) 
      beta[ppt][trial]=(1-p_gamble_est[ppt,trial])*kappas[risk1_Unc2[ppt,trial]][ppt];
      alpha[ppt][trial]=p_gamble_est[ppt,trial]*kappas[risk1_Unc2[ppt,trial]][ppt];
      //update belief based on social info
      // social_info_risk2_safe1 is always 1 for nonsocial trials.
      // add to shape parameters based on dummycoded trial information
      alpha[ppt][trial]+=psis[risk1_Unc2[ppt,trial],social_info_risk2_safe1[ppt,trial]][ppt]*social_info_risky[ppt,trial]*social_trial[ppt,trial];
      beta[ppt][trial]+=psis[risk1_Unc2[ppt,trial],social_info_risk2_safe1[ppt,trial]][ppt]*social_info_safe[ppt,trial]*social_trial[ppt,trial];
      // get updated mean
      probs[ppt][trial]=alpha[ppt][trial]/(alpha[ppt][trial]+beta[ppt][trial]);
      //riskypayoff
      U_risk[ppt][trial]=probs[ppt][trial]*pow(risky_payoff[ppt,trial],rho[ppt]);
      ev_diffs[ppt][trial]=(U_risk[ppt][trial]-U_safe[ppt])/tau[ppt];
    }//end trials
  }//end ppts
}//end transfomred params




model {
  //hyperpriors
  mu_pars[1]~ normal(1,1);   
  mu_pars[2]~ normal(1,1);  
  mu_pars[3:6]~ normal(0,5); 
  mu_pars[7:8]~ gamma(1, 1); 
  
  sig~gamma(1, 1);                 // hyperprior variances
  to_vector(scale) ~ std_normal();//  ppt level
  //print(l_omega)
  //l_omega~lkj_corr_cholesky(2);   // prior correlation of parameters
  //
  for (ppt in 1:N){
    //print(Phi_approx(ev_diffs[ppt][1:t_subj[ppt]]))
    choice[ppt,1:t_subj[ppt]] ~ bernoulli_logit(ev_diffs[ppt][1:t_subj[ppt]]);
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

