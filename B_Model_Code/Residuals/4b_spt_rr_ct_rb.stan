functions {
  
  real pwf(real p, real gam, real pn) {
    
    real wp;
    
    if (p > 0) { wp = exp(-pow(-log(p), gam)); } else { wp = pn; }
    
    return wp;
  }
  
}

data {
  
  // n - no. of data points; N - no. of subjects
  int<lower = 0> n;
  int<lower = 0> N;
  
  // vax-brand effects
  matrix[8, 4] vax_x;
  int<lower = 1> vax[n];
  
  // subject ind vector
  int<lower = 1> sub[n];
  
  // vax-pars data
  matrix[n, 3] pse;
  matrix[n, 3] pbe;
  
  matrix[n, 3] se;
  matrix[n, 3] be;
  int<lower = 0, upper = 1> co[n];
  
  // se groups
  int se_g[8, 3];
  
}

parameters {
  
  // spt parameters
  real<lower = 0> alpha;
  real<lower = 0> beta;
  real<lower = 0, upper = 1> gam;
  real<lower = 0, upper = 1> p_se_e[3];
  real<lower = 0, upper = 1> p_be_e;
  real<lower = 0> phi;
  
  // individual response bias
  real rb_mu;
  real<lower = 0> rb_sigma;
  real rb[N];
  
  // vax-brand effects
  vector[4] vax_b;
  
}


transformed parameters {
  
  // spt sv cpmutpation
  matrix[n, 3] w_pse;
  matrix[n, 3] v_se;
  matrix[n, 3] w_pbe;
  matrix[n, 3] v_be;
  real sv[n];
  
  // container for the linear predictor
  real lin_pred[n];
  
  // for each data point with SOME search
  for(i in 1:n) {
    
    for(j in 1:3) {
      
      // side effects
      v_se[i,j] = -pow( fabs(se[i,j]), alpha );
      w_pse[i,j] = pwf(pse[i,j], gam, p_se_e[se_g[ vax[i], j ]]);
      
      // benefits
      v_be[i,j] = pow( be[i,j], alpha );
      w_pbe[i,j] = pwf(pbe[i,j], gam, p_be_e);
      
    }
    
    // subjective vax value
    sv[i] = beta * dot_product(v_se[i,], w_pse[i,]) + dot_product(v_be[i,], w_pbe[i,]);
    
    // linear predictor
    lin_pred[i] = dot_product(vax_b, vax_x[vax[i],]) + rb[sub[i]] + phi *  sv[i];
    
  }
  
}

model {
  
  // spt parameters
  alpha ~ lognormal(.5, .5);
  beta ~ lognormal(.5, .5);
  // alpha ~ uniform(0, 5);
  // beta ~ uniform(0, 5);
  gam ~ beta(1, 1);
  p_se_e ~ beta(1, 1);
  p_be_e ~ beta(1, 1);
  phi ~ lognormal(-2, 1);
  // phi ~ uniform(0, 1);
  
  // ind-lvl response bias
  rb_mu ~ normal(0, 1);
  rb_sigma ~ gamma(1, 1);
  rb ~ normal(rb_mu, rb_sigma);
  
  // vax pars
  vax_b ~ normal(0, 1);
  
  // likelihood
  co ~ bernoulli_logit( lin_pred );
  
}

// logliks for loo
generated quantities {
  
  real log_lik[n];
  
  for(i in 1:n) {
    
    log_lik[i] = bernoulli_logit_lpmf( co[i] | lin_pred[i] );
    
  }
  
}
