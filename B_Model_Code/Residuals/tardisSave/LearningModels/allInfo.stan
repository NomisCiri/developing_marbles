data {
	int<lower=1> N;// Number of Subjects
	int<lower=1> T;// Trials
	int <lower=1> NGambles;

	int<lower=1, upper=T> Tsubj[N];//number of trials for each subject per Group
  real<lower=0,upper=1>gambleList[NGambles];
  
	int<lower=0, upper=1>choice[N, T];
	int<lower=0, upper=1>risk1Unc0[N,T]; 
	int<lower=0, upper=3>condition[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  
	int<lower=0, upper=99>Sucess[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
	int<lower=0, upper=99>Fail[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
	
	real safe_payoff[N, T];
	real risky_payoff[N, T];
	
	//real<lower=0, upper=1> ambigLevel[N, T];
	int<lower=0, upper=NGambles> p_gamble[N, T];
	real<lower=0, upper=1> p_gamble_est[N, T];
}

transformed data {
  real<lower=0> alpha_c[N,NGambles];//each trail a new one!
	real<lower=0> beta_c[N,NGambles];
	for (i in 1:N){
	for (t in 1:NGambles){
		  //if(risk1Unc0[i, t]==0){
		  alpha_c[i,t] =(gambleList[p_gamble[i,t]]*10)+1;//maybe try Jeffreys Prior? why does it feel like phacking...
      beta_c[i,t] = (10-gambleList[p_gamble[i,t]]*10)+1;
		}
	}
}

parameters {
	//Group mus.
	real<lower=0, upper=2> mu_rho ;
	real<lower=0> mu_tau ;
	real<lower=0, upper=10> mu_alpha_add;
	real<lower=0, upper=10> mu_alpha_add_Risk;

	//real<lower=0, upper=5> mu_beta_add;
	// Social Parameters
	real<lower=0> mu_UncertaintyMulti;
	real<lower=0> mu_UncertaintyMulti_Risk;
	//roup Sigmas
	real<lower=0>sigma_rho;
	real<lower=0>sigma_tau;
	//real<lower=0>sigma_ocu_safe_Uncertainty;
	real<lower=0>sigma_UncertaintyMulti;
	real<lower=0>sigma_UncertaintyMulti_Risk;
	real<lower=0>sigma_alpha_add;
	real<lower=0>sigma_alpha_add_Risk;

	//real<lower=0>sigma_beta_add;
	//individual.
	real rho_p[N];
	real tau_p[N];
		//real ocu_safe_Uncertainty_p[N];
	real UncertaintyMulti_p[N];
	real UncertaintyMulti_Risk_p[N];
	real alpha_add_p[N];
	real alpha_add_Risk_p[N];

	real<lower=0, upper=1> p_gambleU[N, T];
	//real<lower=0,upper=0.2>Unc_Beta[N,NGambles];// TODO: Set Boundary between 0 and 0.25
}

transformed parameters {
	real<lower=0, upper=2> rho[N];
	real<lower=0> alpha_add[N];
	real<lower=0> alpha_add_Risk[N];

//	real<lower=0, upper=3> beta_add[N];
	real<lower=0> tau[N];
	real <lower=0, upper=5> UncertaintyMulti[N];
	real <lower=0,upper=5> UncertaintyMulti_Risk[N];

	for (i in 1:N) {// subs
		// Social Utility Model
		rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; 
		UncertaintyMulti[i] = Phi_approx(mu_UncertaintyMulti + sigma_UncertaintyMulti * UncertaintyMulti_p[i])*5;
		UncertaintyMulti_Risk[i] = Phi_approx(mu_UncertaintyMulti_Risk + sigma_UncertaintyMulti_Risk * UncertaintyMulti_Risk_p[i])*5;
		// updating Model
		alpha_add[i]= mu_alpha_add + sigma_alpha_add * alpha_add_p[i];
		alpha_add_Risk[i]= mu_alpha_add_Risk+ sigma_alpha_add_Risk * alpha_add_Risk_p[i];

		tau[i] = exp(mu_tau + sigma_tau * tau_p[i]);
		
	}//endsubs
}//end transfomred params

model {
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	mu_rho  ~ normal(0,1);
	mu_tau  ~ normal(0,1);
	mu_UncertaintyMulti  ~ normal(0,1);
	mu_UncertaintyMulti_Risk  ~ normal(0,1);
	mu_alpha_add ~ normal(0,1);
	mu_alpha_add_Risk ~ normal(0,0.2);
	
	sigma_rho ~ normal(0, 0.2);
	sigma_tau ~ normal(0, 0.2);
	sigma_UncertaintyMulti  ~ cauchy(0, 1);
	sigma_alpha_add ~ normal(0,0.2);
	sigma_alpha_add_Risk ~ normal(0,0.2);

	sigma_UncertaintyMulti_Risk  ~ cauchy(0, 1);
	//sigma_beta_add ~ normal(0,0.2);
	// individual parameters w/ Matt trick
	// I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
	for (i in 1:N) {
		rho_p[i] ~ normal(0, 1.0);
		tau_p[i] ~ normal(0, 1.0);
		UncertaintyMulti_p[i] ~ normal(0, 1.0);
		UncertaintyMulti_Risk_p[i] ~ normal(0, 1.0);
		
		alpha_add_p[i] ~ normal(0,1.0);
		alpha_add_Risk_p[i]~ normal(0, 1);// why is this so hard?? maybe its the wrong approach after all... :(
    //check trials.
		for (t in 1:Tsubj[i]) {
			real U_safe;
			real U_risky;
			real alpha;
			real beta;

			if(risk1Unc0[i, t]==0){
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				alpha=alpha_c[i,p_gamble[i,t]]*alpha_add_Risk[i];
				beta=beta_c[i,p_gamble[i,t]]*alpha_add_Risk[i];
				
			  gambleList[p_gamble[i,t]]~beta(alpha,beta);
				U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
				if (condition[i, t] == 1) {  // safe-safe
				  p_gambleU[i,t]~beta(alpha,beta*UncertaintyMulti_Risk[i]);
					U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
				}
				 if (condition[i, t] == 3) {  // risky-risky
				    p_gambleU[i,t]~beta(alpha*UncertaintyMulti_Risk[i],beta);
						U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
				 }
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Risk
				
		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
				p_gamble_est[i,t] ~ beta((Sucess[i,t]+1*alpha_add[i]),(Fail[i,t]+1*alpha_add[i])); 
				//beta_rng(alpha_total,beta_total)
				U_safe  = pow(safe_payoff[i, t], rho[i]);
				U_risky = p_gamble_est[i,t] * pow(risky_payoff[i, t], rho[i]);
				if (condition[i, t] == 1) {  // safe-safe
					p_gambleU[i,t]~ beta((Sucess[i,t]+1*alpha_add[i]),(Fail[i,t]*alpha_add[i]+1*UncertaintyMulti[i])); 
				  U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
				}
				 if (condition[i, t] == 3) {  // risky-risky
				 	p_gambleU[i,t] ~ beta((Sucess[i,t]+1*alpha_add[i]*UncertaintyMulti[i]),(Fail[i,t]+1*alpha_add[i])); 
				  U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
					}
					choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
				}//end Uncertain
			} //endTrail
		} //endSub
} 

