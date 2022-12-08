data {
	int<lower=1> N;// Number of Subjects
	int<lower=1> T;// Trials
	//int<lower=1> Seq; // Sequence size
  
	int<lower=1, upper=T> Tsubj[N];//number of trials for each subject per Group
	//int<lower=1,upper=3>	Groups[N];
  
	int<lower=0, upper=1>choice[N, T];
	int<lower=0, upper=1>risk1Unc0[N,T]; 
	int<lower=0, upper=3>condition[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  
	int<lower=0, upper=99>Sucess[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
	int<lower=0, upper=99>Fail[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
	//int<lower=0, upper=3> group[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  
	real safe_payoff[N, T];
	real risky_payoff[N, T];
	
	//real<lower=0, upper=1> ambigLevel[N, T];
	real<lower=0, upper=1> p_gamble[N, T];
	real<lower=0, upper=1> p_gamble_est[N, T];
  
}

transformed data {
}

parameters {
	//Group mus.
	real<lower=0, upper=2> mu_rho ;
	real<lower=0> mu_tau ;
	// Beta Updating Parameters
	real<lower=0, upper=5> mu_update_add;
	// Social Parameters
	real mu_ocu_Risk;
	real mu_ocu_Uncertainty;
	//group Sigmas
	real<lower=0>sigma_rho;
	real<lower=0>sigma_tau;
	real<lower=0>sigma_ocu_Risk;
	real<lower=0>sigma_ocu_Uncertainty;
	real<lower=0>sigma_update_add;

	//individual.
	real rho_p[N];
	real tau_p[N];
	real ocu_Risk_p[N];
	real ocu_Uncertainty_p[N];
	real update_add_p[N];

	real<lower=0, upper=1> eta[N,T];//PROBABILITY THAT I GET OUT OF THE BETA UPDATE
}

transformed parameters {
	real<lower=0, upper=2> rho[N];
	real<lower=0, upper=5> update_add[N];
	real<lower=0> tau[N];
	real  ocu_Risk[N];
	real  ocu_Uncertainty[N];
	//A Normal(μ,σ) distribution, like other distributions in the location–scale distribution family, can be reparameterized to 
	//be sampled from a unit normal distribution that is multiplied by the scale parameter σ and then shifted with the location parameter μ. Formally,
	//  ξ∼Normal(μξ,σξ)
	//is mathematically equivalent to
	//  ξ′∼Normal(0,1) ........ Which is defined in the nmodel
	//   ξ∼Normal(μξ+ξ′·σξ). which is defined over here.
	for (i in 1:N) {// subs
		// Social Utility Model
		rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; // i dont quite understand this part. but it makes the correct estimates. Good i would s
		ocu_Risk[i] = mu_ocu_Risk + sigma_ocu_Risk * ocu_Risk_p[i];
		ocu_Uncertainty[i] = mu_ocu_Uncertainty + sigma_ocu_Uncertainty * ocu_Uncertainty_p[i];
		// updating Model
		update_add[i]= Phi_approx(mu_update_add + sigma_update_add * update_add_p[i])*5;
		// choice model
		tau[i] = exp(mu_tau + sigma_tau * tau_p[i]);
	}//endsubs
}//end transfomred params

model {
	// peer_ocu
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 

	
	mu_rho  ~ normal(0,1);
	mu_tau  ~ normal(0,1);
	
	mu_ocu_Risk  ~ normal(0,1);
	mu_ocu_Uncertainty  ~ normal(0,1);
	
	mu_update_add ~ normal(0,1);

	sigma_rho ~ normal(0, 0.2);
	sigma_tau ~ normal(0, 0.2);
	
	sigma_ocu_Risk ~ cauchy(0, 1);
	sigma_ocu_Uncertainty ~ cauchy(0, 1);
	
	sigma_update_add ~ normal(0,0.2);
	// individual parameters w/ Matt trick
	// I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
	for (i in 1:N) {
		rho_p[i] ~ normal(0, 1.0);
		tau_p[i] ~ normal(0, 1.0);
		ocu_Risk_p[i] ~ normal(0, 1.0);
		ocu_Uncertainty_p[i] ~ normal(0, 1.0);
		update_add_p[i] ~ normal(0,1.0);
		
		for (t in 1:Tsubj[i]) {
			real U_safe;
			real U_risky;
			// is it a risk trial?
			if(risk1Unc0[i, t]==0){
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = p_gamble[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_risky = U_risky + ocu_Risk[i];        
				}
				 if (condition[i, t] == 3) {  // risky-risky
						U_risky = U_risky + ocu_Risk[i];
					}
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Risk
			
		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
			// 	//LEARNING MODEL.
				eta[i,t] ~ beta(pow(Sucess[i,t],update_add[i]),pow(Fail[i,t],update_add[i])); // we know that the winning probability is beta distributed with the obtained shape parameters. 
				//beta_rng(update_total,beta_total)
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = eta[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_risky = U_risky + ocu_Uncertainty[i];        
				}
				 if (condition[i, t] == 3) {  // risky-risky
						U_risky = U_risky + ocu_Uncertainty[i];
					}
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Uncertain
			} //endTrail
		} //endSub
}

generated quantities {
  real mu_rhoG;  
	real mu_tauG;  
	
	real mu_ocu_RiskG;  
	real mu_ocu_UncertaintyG;  
	real mu_update_addG; 

  // For log likelihood calculation
  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_rhoG = Phi_approx(mu_rho) * 2;
  mu_tauG = exp(mu_tau);
	mu_ocu_RiskG  = mu_ocu_Risk;
	mu_ocu_UncertaintyG  = mu_ocu_Uncertainty;
	mu_update_addG = Phi_approx(mu_update_add)*5; 
	
	  { // local section, this saves time and space
		for (i in 1:N) {
    log_lik[i] = 0.0;
		for (t in 1:Tsubj[i]) {
			real U_safe;
			real U_risky;
			real pGamble;
			// is it a risk trial?
			if(risk1Unc0[i, t]==0){
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = p_gamble[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_risky = U_risky + ocu_Risk[i];        
				}
				 if (condition[i, t] == 3) {  // risky-risky
						U_risky = U_risky + ocu_Risk[i];
				}
					log_lik[i] += bernoulli_logit_lpmf(choice[i, t] | tau[i] * (U_risky - U_safe));
					y_pred[i, t] = bernoulli_rng(inv_logit(tau[i] * (U_risky - U_safe)));
				}//end Risk
			
		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
			// 	//LEARNING MODEL.
				//pGamble ~ beta(pow(Sucess[i,t],update_add[i]),pow(Fail[i,t],update_add[i])); // we know that the winning probability is beta distributed with the obtained shape parameters. 
				//beta_rng(update_total,beta_total)
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = eta[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_risky = U_risky + ocu_Uncertainty[i];        
				}
				if (condition[i, t] == 3) {  // risky-risky
					U_risky = U_risky + ocu_Uncertainty[i];
				}
					log_lik[i] += bernoulli_logit_lpmf(choice[i, t] | tau[i] * (U_risky - U_safe));
					y_pred[i, t] = bernoulli_rng(inv_logit(tau[i] * (U_risky - U_safe)));
				}//end Uncertain
			} //endTrail
		}//endSub
	}//end
}
	
	

