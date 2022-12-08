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
	real<lower=0, upper=5> mu_alpha_add;
	//real<lower=0, upper=5> mu_beta_add;
	
	// Social Parameters
	real mu_ocu_safe;
	//real mu_ocu_safe_Uncertainty;
	
	real mu_ocu_risk;
	real mu_UncertaintyMulti;
	//roup Sigmas
	real<lower=0>sigma_rho;
	real<lower=0>sigma_tau;
	
	real<lower=0>sigma_ocu_safe;
	//real<lower=0>sigma_ocu_safe_Uncertainty;
	
	real<lower=0>sigma_ocu_risk;
	real<lower=0>sigma_UncertaintyMulti;
	
	real<lower=0>sigma_alpha_add;
	//real<lower=0>sigma_beta_add;

	//individual.
	real rho_p[N];
	real tau_p[N];
	
	real ocu_safe_p[N];
	//real ocu_safe_Uncertainty_p[N];
	real ocu_risk_p[N];
	real UncertaintyMulti_p[N];
	
	real alpha_add_p[N];
	//real beta_add_p[N];
	  
}

transformed parameters {
	real<lower=0, upper=2> rho[N];
	
	real<lower=0, upper=5> alpha_add[N];
//	real<lower=0, upper=3> beta_add[N];
	
	real<lower=0> tau[N];
	
	real  ocu_risk[N];
	//real  ocu_risk_Uncertainty[N];
	real  ocu_safe[N];
	real  UncertaintyMulti[N];
	
	//A Normal(μ,σ) distribution, like other distributions in the location–scale distribution family, can be reparameterized to 
	//be sampled from a unit normal distribution that is multiplied by the scale parameter σ and then shifted with the location parameter μ. Formally,
	//  ξ∼Normal(μξ,σξ)
	//is mathematically equivalent to
	//  ξ′∼Normal(0,1) ........ Which is defined in the nmodel
	//   ξ∼Normal(μξ+ξ′·σξ). which is defined over here.

	for (i in 1:N) {// subs
		// Social Utility Model
		rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; // i dont quite understand this part. but it makes the correct estimates. Good i would s
		ocu_safe[i] = mu_ocu_safe + sigma_ocu_safe * ocu_safe_p[i];
		//ocu_safe_Uncertainty[i] = mu_ocu_safe_Uncertainty + sigma_ocu_safe_Uncertainty * ocu_safe_Uncertainty_p[i];
		
		ocu_risk[i] = mu_ocu_risk + sigma_ocu_risk * ocu_risk_p[i];
		UncertaintyMulti[i] = mu_UncertaintyMulti + sigma_UncertaintyMulti * UncertaintyMulti_p[i];
	  
		// updating Model
		alpha_add[i]= Phi_approx(mu_alpha_add + sigma_alpha_add * alpha_add_p[i])*5;
		//beta_add[i]=  Phi_approx(mu_beta_add + sigma_beta_add * beta_add_p[i])*3;
	 
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
	
	mu_ocu_safe  ~ normal(0,1);
	//mu_ocu_safe_Uncertainty  ~ normal(0,1);
	mu_ocu_risk  ~ normal(0,1);
	mu_UncertaintyMulti  ~ normal(0,1);
	
	mu_alpha_add ~ normal(1,1);
	//mu_beta_add ~ normal(0,1);
		
	sigma_rho ~ normal(0, 0.2);
	sigma_tau ~ normal(0, 0.2);
	
	sigma_ocu_safe ~ cauchy(0, 1);
	//sigma_ocu_safe_Uncertainty ~ cauchy(0, 1);
	
	sigma_ocu_risk ~ cauchy(0, 1);
	sigma_UncertaintyMulti  ~ cauchy(0, 1);
	
	sigma_alpha_add ~ normal(0,0.2);
	//sigma_beta_add ~ normal(0,0.2);
	// individual parameters w/ Matt trick


	// I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
	for (i in 1:N) {
		rho_p[i] ~ normal(0, 1.0);
		tau_p[i] ~ normal(0, 1.0);
		
		ocu_safe_p[i] ~ normal(0, 1.0);
		//ocu_safe_Uncertainty_p[i] ~ normal(0, 1.0);
		
		ocu_risk_p[i] ~ normal(0, 1.0);
		UncertaintyMulti_p[i] ~ normal(0, 1.0);
	
		alpha_add_p[i] ~ normal(0,1.0);
		//beta_add_p[i] ~ normal(0,1.0);
	
		for (t in 1:Tsubj[i]) {
			real U_safe;
			real U_risky;
			
			// is it a risk trial?
			if(risk1Unc0[i, t]==0){
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = p_gamble[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_safe = U_safe + ocu_safe[i];        
				}
				 if (condition[i, t] == 3) {  // risky-risky
						U_risky = U_risky + ocu_risk[i];
					}
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Risk
			
		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
			 
			// 	//LEARNING MODEL.
			// 	for (s in 1:Seq) {
			// 		alpha_total=alpha_total+ Sucess[i,t,s]*alpha_add[i];// thats the weighting multiplier for gains
			// 		beta_total=beta_total+ Fail[i,t,s]*beta_add[i];// thats the weighting multiplier for losses
			// 	}
			
				p_gamble_est[i,t] ~ beta((Sucess[i,t]*alpha_add[i]),(Fail[i,t]*alpha_add[i])); // we know that the winning probability is beta distributed with the obtained shape parameters. 
				//beta_rng(alpha_total,beta_total)
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = p_gamble_est[i,t] * pow(risky_payoff[i, t], rho[i]);
				
				if (condition[i, t] == 1) {  // safe-safe
					U_safe = U_safe + UncertaintyMulti[i]*ocu_safe[i];        
				}
				 if (condition[i, t] == 3) {  // risky-risky
						U_risky = U_risky + UncertaintyMulti[i]*ocu_risk[i];
					}
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Uncertain
		

			} //endTrail
		} //endSub
} 

