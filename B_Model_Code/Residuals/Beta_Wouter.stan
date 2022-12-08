functions {  // declare and define user-defined function before calling it
  real generalized_beta_lpdf(real x, real alpha, real beta,real begin, real end) { // probability density function of beta prime distribution (aka beta distribution of the second kind); has to end with "_log" for functions that implement a probability distribution and that permit access to increment_log_prob function; alpha=0; beta=2 (reduces to beta distribution when beta=1)
    real logf1;
    real logf2;
    real logdens;
    logf1=1/(exp(lbeta(alpha,beta))*pow((end-begin),(alpha+beta-1)));
    logf2=pow((x-begin),(alpha-1))*pow((end-x),(beta-1));
    logdens=log(logf1*logf2);
    return(logdens);
}

  // Cumulative Distribution function. Returns the cumulative Probability of the value of the generalized beta.
  real generalized_beta_cdf(real x, real alpha, real beta,real begin, real end){
    real y;
    real z;
    real prob;
  	z = x/end;//rescale the input value
  	prob = beta_cdf(z, alpha, beta);// just look up in the beta cumulative distribution function. 
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
  int nData;
  real<lower=0> x[nData];// Number of Subjects
}


parameters {
  real<lower=2> alpha;
  real<lower=2> beta;
  real<lower=0> end;
  //real<lower=0> begin;
}

model {
  // peer_ocu
	alpha  ~ gamma(2,2);//prior
	beta ~ gamma(2, 2);
	
	end~cauchy(10,1);
	//begin~cauchy(0.1,20);
	
	for (i in 1:nData){
	  	target +=  generalized_beta_lpdf(x[i]|alpha,beta, 0.00001,end);
	}// each single datapoint follows the same stuff
} 

generated quantities {
  // For log likelihood calculation
  real log_lik[nData];
  // For posterior predictive check
  real y_pred[nData];
  // Set all posterior predictions to 0 (avoids NULL values)
    for (i in 1:nData) {
        y_pred[i] = beta_rng(alpha,beta);
    }
}
