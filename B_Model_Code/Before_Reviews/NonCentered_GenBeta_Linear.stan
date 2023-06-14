functions {  // declare and define user-defined function before calling it
  real generalized_beta_lpdf(real x, real alpha, real beta,real begin, real end) { // probability density function of beta prime distribution (aka beta distribution of the second kind); has to end with "_log" for functions that implement a probability distribution and that permit access to increment_log_prob function; alpha=0; beta=2 (reduces to beta distribution when beta=1)
    real logf1;
    real logf2;
    real logdens;
    logf1=1/(exp(lbeta(alpha,beta))*pow((end-begin),(alpha+beta-1)));
    logf2=pow((x-begin),(alpha-1))*pow((end-x),(beta-1));
    logdens=log(logf1*logf2);
    if (x>end){// this cant be!
    print(log(0.00001))
      return log(0.00001);// just return sth really bad. lets see if that works.
    }else{
      return(logdens);
    }
}

  // Cumulative Distribution function. Returns the cumulative Probability of the value of the generalized beta.
  real generalized_beta_cdf(real x, real alpha, real beta,real begin, real end){
    real y;
    real z;
    real prob;
  	z = x/end;//rescale the input value
  	prob = beta_cdf(z, alpha, beta);// just look up in the beta cumulative distribution function. 
  	if (prob==1){
  	  prob=0.99;
  	}if(prob==0){
  	  prob=0.01;
  	}
    return(prob);
  }
  
  real generalized_beta_rng(real alpha, real beta,real begin, real end){
    real y;
    real z;
  	z = beta_rng(alpha,beta);// just draw a random value from the beta.
  	y = z*end;//rescale
  	return(y);//does that still work? i suppose yes
  }
}



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
	
	int<lower=0, upper=NGambles> p_gamble[N, T];
	real<lower=0, upper=1> p_gamble_est[N, T];
}

transformed data {
}

parameters {
	//Group mus.
	real<lower=0, upper=2> mu_rho ;
	// Beta Updating Parameters
	real<lower=0, upper=5> mu_alpha_add;
	// Social Parameters
	//real<lower=0> mu_UncertaintyMulti;
	//real<lower=0> mu_UncertaintyMulti_Risk;
	//roup Sigmas
	real<lower=0>sigma_rho;
	//real<lower=0>sigma_UncertaintyMulti;
	//real<lower=0>sigma_UncertaintyMulti_Risk;
	real<lower=0>sigma_alpha_add;
	//individual.
	real rho_p[N];
	real alpha_add_p[N];
	// Social Parameters
	//real UncertaintyMulti_p[N];
	//real UncertaintyMulti_Risk_p[N];
	//
	real<lower=1> alpha_shape[N,NGambles];
	real<lower=1> beta_shape[N,NGambles];
}

transformed parameters {
	real<lower=0, upper=2> rho[N];
	real<lower=0, upper=4> alpha_add[N];
//	real<lower=0, upper=3> beta_add[N];
	//real <lower=0,upper=10> UncertaintyMulti[N];
	//real <lower=0,upper=10> UncertaintyMulti_Risk[N];
	for (i in 1:N) {// subs
		// Social Utility Model
		rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; // i dont quite understand this part. but it makes the correct estimates. Good i would s
		//UncertaintyMulti[i] = Phi_approx(mu_UncertaintyMulti + sigma_UncertaintyMulti * UncertaintyMulti_p[i])*10;
		//UncertaintyMulti_Risk[i] = Phi_approx(mu_UncertaintyMulti_Risk + sigma_UncertaintyMulti_Risk * UncertaintyMulti_Risk_p[i])*10;
		// updating Model
		alpha_add[i]= Phi_approx(mu_alpha_add + sigma_alpha_add * alpha_add_p[i])*4;
		//estimate the alpha & Beta Parameter on the trail level. 
	}//endsubs
}//end transfomred params

model {
	// peer_ocu
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	mu_rho  ~ normal(1,1);
	//mu_UncertaintyMulti  ~ normal(1,1);
	mu_alpha_add ~ normal(1,1);
  //mu_UncertaintyMulti_Risk  ~ normal(1,0.5);
  
	//mu_beta_add ~ normal(0,1);
	sigma_rho ~ cauchy(0, 1);
	//sigma_UncertaintyMulti  ~ cauchy(0, 1);
	sigma_alpha_add ~ cauchy(0,1);
	//sigma_UncertaintyMulti_Risk  ~ cauchy(0, 1);
	//sigma_beta_add ~ normal(0,0.2);
	// individual parameters w/ Matt trick
	// I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
	for (i in 1:N) {
		rho_p[i] ~ normal(0, 1.0);
		//UncertaintyMulti_p[i] ~ normal(0, 1.0);
		//UncertaintyMulti_Risk_p[i] ~ normal(0, 1.0);
		alpha_add_p[i] ~ normal(0,1.0);
		// for estiamting the variance parameter.
		for (gambles in 1:NGambles){//gambleUncertainty Loop
		  alpha_shape[i,gambles]~gamma(2,2);//lets see.
      beta_shape[i,gambles]~gamma(2,2);//more flexible here. Make hyperprior for diffbles.
      gambleList[gambles]~beta(alpha_shape[i,gambles],beta_shape[i,gambles]);
    }
    //check trials.
		for (t in 1:Tsubj[i]) {
		  // internal variables that i can throw away afterwards 
			real U_safe;
			real U_risky;
			real newAlpha=alpha_shape[i,p_gamble[i,t]];
			real newBeta=beta_shape[i,p_gamble[i,t]];
			real EU_risky;
			if(risk1Unc0[i, t]==0){
			  //first compute utilities here. 
			  U_safe  = pow(safe_payoff[i, t], rho[i]);
			  U_risky = pow(risky_payoff[i, t], rho[i]);
			  EU_risky= gambleList[p_gamble[i,t]]*U_risky;
				//SocialInfoBlock: Overwrite the shape parameters
				if (condition[i, t] == 1) {  // safe-safe
				  newAlpha=alpha_shape[i,p_gamble[i,t]];//alpha Stays the same
  				//newBeta=beta_shape[i,p_gamble[i,t]]*UncertaintyMulti_Risk[i];// beta to the power of Social Info
  				newBeta=beta_shape[i,p_gamble[i,t]];
				}if (condition[i, t] == 3) {  // risky-risky
  				newAlpha=alpha_shape[i,p_gamble[i,t]];//*UncertaintyMulti_Risk[i];//alpha to the power of Social Info
  				newBeta=beta_shape[i,p_gamble[i,t]];// beta stays the same
				 }
				 	target+= generalized_beta_lpdf(EU_risky| newAlpha,newBeta,0.00001,U_risky);
					choice[i, t] ~ bernoulli(generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky));
				}//end Risk
  		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
        real socialAlphaUnc;
    		real socialBetaunc;
  		  newAlpha=Sucess[i,t]*alpha_add[i];
  		  newBeta=Fail[i,t]*alpha_add[i];
  			p_gamble_est[i,t] ~ beta(newAlpha,newBeta);// estimate the compression parameters. 
  			//beta_rng(alpha_total,beta_total)
  			U_safe  = pow(safe_payoff[i, t], rho[i]);
  			U_risky = pow(risky_payoff[i, t], rho[i]);
  			EU_risky= p_gamble_est[i,t]*U_risky;
  			//socialinfo Block: Overwrite the shape parameters
  			if (condition[i, t] == 1) {  // safe-safe
    			newBeta=newBeta;//*UncertaintyMulti[i];// beta to the power of Social Info
    			newAlpha=newAlpha;
  			}if (condition[i, t] == 3) {  // risky-risky
  			  newBeta=newBeta;
    			newAlpha=newAlpha;//*UncertaintyMulti[i];//alpha to the power of Social Info
  			}
  			target+= generalized_beta_lpdf(EU_risky| newAlpha,newBeta,0.00001,U_risky);
  			choice[i, t] ~ bernoulli(generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky));// each choice is predicted as a bernoulli distributed random variable.
  		}//end Uncertain
  		//print(generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky))
		} //endTrail
	} //endSub
} 

// 
// 
// generated quantities {
//   // For log likelihood calculation
//   real log_lik[N,T];
// 
//   // For posterior predictive check
//   real y_pred[N,T];
// 
//   // Set all posterior predictions to 0 (avoids NULL values)
//     for (i in 1:N) {
//       for (t in 1:T) {
//         y_pred[i,t] = -1;
//       }
//     }
//   
//   { // local section, this saves time and space
//   	for (i in 1:N) {
//     //check trials.
// 		for (t in 1:Tsubj[i]) {
// 			real U_safe;
// 			real U_risky;
// 			if(risk1Unc0[i, t]==0){
// 				U_safe  = pow(safe_payoff[i, t], rho[i]);
// 				U_risky = gambleList[p_gamble[i,t]] * pow(risky_payoff[i, t], rho[i]);
// 				if (condition[i, t] == 1) {  // safe-safe
// 					U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
// 				}
// 				 if (condition[i, t] == 3) {  // risky-risky
// 					U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
// 				 }
// 				}//end Risk
// 				
// 		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
// 				//beta_rng(alpha_total,beta_total)
// 				U_safe  = pow(safe_payoff[i, t], rho[i]);
// 				U_risky = p_gamble_est[i,t] * pow(risky_payoff[i, t], rho[i]);
// 				if (condition[i, t] == 1) {  // safe-safe
// 				  U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
// 				}
// 				 if (condition[i, t] == 3) {  // risky-risky
// 				  U_risky = p_gambleU[i,t] * pow(risky_payoff[i, t], rho[i]);
// 					}
// 				}//end Uncertain
// 				  log_lik[i,t] =  bernoulli_logit_lpmf(choice[i, t] | (tau[i]) * (U_risky - U_safe));// i defined it before. i dont want it 
//           y_pred[i, t] = bernoulli_rng(inv_logit((tau[i]) * (U_risky - U_safe)));
// 			} //endTrail
// 		} //endSub
//   }//end local
// }
