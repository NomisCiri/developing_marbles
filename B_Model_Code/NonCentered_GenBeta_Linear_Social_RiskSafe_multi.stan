functions {  // declare and define user-defined function before calling it
  real generalized_beta_lpdf(real x, real alpha, real beta,real begin, real end) { // probability density function of beta prime distribution (aka beta distribution of the second kind); has to end with "_log" for functions that implement a probability distribution and that permit access to increment_log_prob function; alpha=0; beta=2 (reduces to beta distribution when beta=1)
    real logf1;
    real logf2;
    real logdens;
    logf1=1/(exp(lbeta(alpha,beta))*pow((end-begin),(alpha+beta-1)));
    logf2=pow((x-begin),(alpha-1))*pow((end-x),(beta-1));
    
    if (logf1==0){// this cant be!
        logf1=0.001;
    }
    if(logf2==0){
      logf2=0.001;
    }
    logdens=log(logf1*logf2);
    if (x>end){// this cant be!
    //print(log(0.00001))
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
  	if (prob>0.99){
  	  prob=0.99;
  	}if(prob<0.01){
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
	real mu_rho ;
	// Beta Updating Parameters
	real mu_alpha_add_risk;
	real mu_alpha_add_safe;

	// Social Parameters
	//roup Sigmas
	real<lower=0>sigma_rho;
	real<lower=0>sigma_alpha_add_risk;
	real<lower=0>sigma_alpha_add_safe;

	//individual.
	real rho_p[N];
	real alpha_add_risk_p[N];
	real alpha_add_safe_p[N];

	// Social Parameters
	real<lower=0> UncertaintyMulti_safe[N];
	real<lower=0> UncertaintyMulti_Risk_safe[N];

	real<lower=0> UncertaintyMulti_risk[N];
	real<lower=0> UncertaintyMulti_Risk_risk[N];
	//
	real<lower=0> kappa[N,NGambles];
	//real<lower=1,upper=20> beta_shape[N,NGambles];
	//real<lower=0,upper=1> trembl;
}

transformed parameters {
	real<lower=0, upper=2> rho[N];
	real<lower=0, upper=2> alpha_add_risk[N];
	real<lower=0, upper=2> alpha_add_safe[N];

	real<lower=0>alpha_shape[N,NGambles];
	real<lower=0>beta_shape[N,NGambles];
//	real<lower=0, upper=3> beta_add[N];

	for (i in 1:N) {// subs
		// Social Utility Model
		rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; 
		// updating Model
		alpha_add_risk[i]= Phi_approx(mu_alpha_add_risk + sigma_alpha_add_risk * alpha_add_risk_p[i])*2;
		alpha_add_safe[i]= Phi_approx(mu_alpha_add_safe + sigma_alpha_add_safe * alpha_add_safe_p[i])*2;
		// from Wikipedia: α = μ*kappa, β = (1 − μ)*kappa
		//get back the shape parameters. 
		for(gambles in 1:NGambles){
		  alpha_shape[i,gambles]=gambleList[gambles]*(kappa[i,gambles]);
		  beta_shape[i,gambles]=(1-gambleList[gambles])*(kappa[i,gambles]);
		}//end Gambleloop
	}//endsubs
}//end transfomred params

model {
	// peer_ocu
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	//hyper parameters... hyperpriors for all parameteres.
	// i could in principle set different hyperpriors for each 
	  mu_rho  ~ normal(1,1);
  mu_alpha_add_risk ~ normal(1,1);
  mu_alpha_add_safe ~ normal(1,1);

  sigma_rho ~ cauchy(1, 1);
  sigma_alpha_add_risk ~ cauchy(1,1);
  sigma_alpha_add_safe ~ cauchy(1,1);
	// individual parameters w/ Matt trick
	// I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
	for (i in 1:N) {
  	  #UncertaintyMulti[i]
  		rho_p[i] ~ normal(0, 1.0);
  		alpha_add_risk_p[i] ~ normal(0,1.0);
  		alpha_add_safe_p[i] ~ normal(0,1.0);


      //centred parametization works for these parameters. 
  		UncertaintyMulti_risk[i] ~ normal(2, 1);
  		UncertaintyMulti_Risk_risk[i] ~ normal(1, 1);
  		UncertaintyMulti_safe[i] ~ normal(2, 1);
  		UncertaintyMulti_Risk_safe[i] ~ normal(1, 1);
  		// for estiamting the variance parameter.
  		for (gambles in 1:NGambles){//gambleUncertainty Loop
  		  //alpha_shape[i,gambles]~gamma(2,2);//lets see.
        kappa[i,gambles]~gamma(10,1);//more flexible here. Make hyperprior for diffbles.
        gambleList[gambles]~beta_proportion(gambleList[gambles],kappa[i,gambles]);
      }
      //check trials.
  		for (t in 1:Tsubj[i]) {
  		  // internal variables that i can throw away afterwards 
  			real U_safe;
  			real U_risky;
  			real estimateAlpha;
  			real estimateBeta;
  			real newAlpha=alpha_shape[i,p_gamble[i,t]];
  			real newBeta=beta_shape[i,p_gamble[i,t]];
  			if(risk1Unc0[i, t]==0){
  			  //first compute utilities here. 
  			  U_safe  = pow(safe_payoff[i, t], rho[i]);
  			  U_risky = pow(risky_payoff[i, t], rho[i]);
  				//SocialInfoBlock: Overwrite the shape parameters
  				if (condition[i, t] == 1) {  // safe-safe
    				newBeta=beta_shape[i,p_gamble[i,t]]*UncertaintyMulti_Risk_safe[i];// beta to the power of Social Info
  				}if (condition[i, t] == 3) {  // risky-risky
    				newAlpha=alpha_shape[i,p_gamble[i,t]]*UncertaintyMulti_Risk_risk[i];//alpha to the power of Social Info				 }
  				  }//end Social
  			 }//end  	Risk
    		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
    		  estimateAlpha=pow(Sucess[i,t],alpha_add_risk[i]);
    		  estimateBeta=pow(Fail[i,t],alpha_add_safe[i]);
    			p_gamble_est[i,t] ~ beta(estimateAlpha,estimateBeta);// estimate the compression parameters. 
    			//beta_rng(alpha_total,beta_total)
    			U_safe  = pow(safe_payoff[i, t], rho[i]);
    			U_risky = pow(risky_payoff[i, t], rho[i]);
    			//socialinfo Block: Overwrite the shape parameters
    			if (condition[i, t] == 1) {  // safe-safe
    				newBeta=estimateBeta*UncertaintyMulti_safe[i];// beta to the power of Social Info
    			}if (condition[i, t] == 3) {  // risky-risky
    				newAlpha=estimateAlpha*UncertaintyMulti_risk[i];//alpha to the power of Social Info				 }
    			}
    			// a bernoulli distributed random variable.
    		}//end Uncertain
    			choice[i, t] ~ bernoulli(1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky));
  	} //endTrail
  }//endSubs
}//endModel



generated quantities {
  // For log likelihood calculation
  real log_lik[N];
  // For posterior predictive check
  real y_pred[N,T];
  real gamble_Pred[N,NGambles];
  // Set all posterior predictions to 0 (avoids NULL values)
    for (i in 1:N) {
      log_lik[i]=0;
      for (t in 1:T) {
        y_pred[i,t] = -1;
      }
    }

	for (i in 1:N) {
    //check trials.
		for (t in 1:Tsubj[i]) {
		  // internal variables that i can throw away afterwards 
			real U_safe;
			real U_risky;
			real estimateAlpha;
  		real estimateBeta;
			real newAlpha=alpha_shape[i,p_gamble[i,t]];
			real newBeta=beta_shape[i,p_gamble[i,t]];
			real EU_risky;
			
			if(risk1Unc0[i, t]==0){
			  //first compute utilities here. 
			  U_safe  = pow(safe_payoff[i, t], rho[i]);
			  U_risky = pow(risky_payoff[i, t], rho[i]);
				//SocialInfoBlock: Overwrite the shape parameters
				if (condition[i, t] == 1) {  // safe-safe
  				newBeta=beta_shape[i,p_gamble[i,t]]*UncertaintyMulti_Risk_safe[i];// beta to the power of Social Info
				}if (condition[i, t] == 3) {  // risky-risky
  				newAlpha=alpha_shape[i,p_gamble[i,t]]*UncertaintyMulti_Risk_risk[i];//alpha to the power of Social Info
				 }
				}//end Risk
  		 if(risk1Unc0[i, t]==1){// is it an uncertain trial?
        real socialAlphaUnc;
    		real socialBetaunc;
   		  estimateAlpha=pow(Sucess[i,t],alpha_add_safe[i]);
    		estimateBeta=pow(Fail[i,t],alpha_add_risk[i]);
  			// p_gamble_est[i,t] ~ beta(newAlpha,newBeta);// estimate the compression parameters.
  			gamble_Pred[i,p_gamble[i,t]]=beta_rng(estimateAlpha,estimateBeta);
  			//beta_rng(alpha_total,beta_total)
  			U_safe  = pow(safe_payoff[i, t], rho[i]);
  			U_risky = pow(risky_payoff[i, t], rho[i]);
  			//socialinfo Block: Overwrite the shape parameters
  			if (condition[i, t] == 1) {  // safe-safe
    			newBeta=estimateBeta*UncertaintyMulti_safe[i];// beta to the power of Social Info
    			newAlpha=estimateAlpha;
  			}if (condition[i, t] == 3) {  // risky-risky
    			newAlpha=estimateAlpha*UncertaintyMulti_risk[i];//alpha to the power of Social Info
    			newBeta=estimateBeta;
  			}
  		}//end Uncertain
	      log_lik[i]+= bernoulli_lpmf(choice[i,t]|1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky));
		    y_pred[i, t] = bernoulli_rng(1-generalized_beta_cdf(U_safe, newAlpha,newBeta,0.00001,U_risky));// each choice is predicted as a bernoulli distributed random variable.		} //endTrail
	} //endSub
}//end PostPred
}
