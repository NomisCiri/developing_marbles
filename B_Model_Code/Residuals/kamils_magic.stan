functions {
  
  real pwf(real p, real gam) {
    
    real wp;
    
    wp = exp(-pow(-log(p), gam));
    
    // if (p > 0) { wp = exp(-pow(-log(p), gam)); } else { wp = .5; }
    
    return wp;
  }
  
}


data {
  
  // n - no. of data points; N - no. of subjects
  int<lower = 0> n;
  int<lower = 0> N;
  
  // vax-brand effects
  matrix[8, 7] vax_x;
  int<lower = 1> vax[n];
  
  // subject ind vector
  int<lower = 1> sub[n];
  
  // vax-pars data
  matrix[n, 3] pse;
  matrix[n, 3] pbe;
  
  matrix[n, 3] se;
  matrix[n, 3] be;
  int<lower = 0, upper = 1> co[n];
  
}

parameters {
  
  // SPT pop-lvl parameters
  real<lower = 0> alpha;
  real beta_phi; // this should be analyzed on 0-1 range
  real gam_phi; // this should be analyzed on 0-1 range
  real<lower = 0> phi;
  
  // RB pop-lvl
  real rb;
  
  // individual LA and RB z-scale displacements
  matrix[3, N] id_z;
  
  // ind-dist variances
  vector[3] id_sig;
  
  // rb-la corr matrix
  cholesky_factor_corr[3] L_omega;
  
  // vax-brand effects
  vector[7] vax_b;
  
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
  
  // individual level pars on probit scale
  matrix[N, 3] id_phi;
  
  // id-lvl model parameters on model scale
  vector[N] irb;
  vector[N] ibeta;
  vector[N] igam;
  
  // transform individual displacements with var-cov matrix
  id_phi = (diag_pre_multiply(id_sig, L_omega) * id_z)';

  // id-lvl parameters on the model scale
  irb = rb + id_phi[,1];
  ibeta = Phi( beta_phi + id_phi[,2] );
  igam = Phi( gam_phi + id_phi[,3] );

  // for each data point
  for(i in 1:n) {
    
    // for each outcome branch
    for(j in 1:3) {
      
      // side effects
      v_se[i,j] = -pow( fabs(se[i,j]), alpha);
      w_pse[i,j] = pwf( pse[i,j], igam[sub[i]] );

      
      // benefits
      v_be[i,j] = pow(be[i,j], alpha);
      w_pbe[i,j] = pwf( pbe[i,j], igam[sub[i]] );

    }
    
    // subjective vax value
    sv[i] = ibeta[sub[i]] * dot_product(v_se[i,], w_pse[i,]) + (1 - ibeta[sub[i]]) * dot_product(v_be[i,], w_pbe[i,]);
    
    // linear predictor
    lin_pred[i] = dot_product(vax_b, vax_x[vax[i],]) + irb[sub[i]] + phi *  sv[i];
    
  }
  
}

model {
  
  // OH parameters
  alpha ~ lognormal(.5, .5);
  beta_phi ~ std_normal();
  gam_phi ~ std_normal();
  // alpha ~ uniform(0, 5);
  // beta ~ uniform(0, 5);
  phi ~ lognormal(-2, 1);
  // phi ~ uniform(0, 1);
  
  // ind-lvl response bias
  rb ~ std_normal();
  
  // ind-lvl z-scale displacemenets
  to_vector(id_z) ~ std_normal();
  
  id_sig[1] ~ gamma(1, 1); // id var of RB
  id_sig[2] ~ gamma(1, 10); // id var of LA
  id_sig[3] ~ gamma(1, 10); // id var of PS
  
  L_omega ~ lkj_corr_cholesky(6); // prior for the corr matrix

  // vax pars
  vax_b ~ normal(0, 1);
  
  // likelihood
  co ~ bernoulli_logit( lin_pred );
  
}

// 
generated quantities {
  
  // LA to monitor
  real <lower = 0, upper = 1> beta;
  real <lower = 0> beta_sigma;
  
  // PS to monitor
  real <lower = 0, upper = 1> gam;
  real <lower = 0> gam_sigma;
  
  // id-var rb
  real <lower = 0> rb_sigma;
  
  // correlations
  matrix[3,3] omega;
  real rb_la_rho;
  real rb_ps_rho;
  real la_ps_rho;

  
  //logliks for loo
  real log_lik[n];
  
  // parameters to monitor
  beta  = Phi(beta_phi);
  beta_sigma = id_sig[2];
  
  gam = Phi(gam_phi);
  gam_sigma = id_sig[3];
  
  rb_sigma = id_sig[1];

  // correlation
  omega = L_omega * L_omega';
  rb_la_rho = omega[1,2];
  rb_ps_rho = omega[1,3];
  la_ps_rho = omega[2,3];


// logliks
for(i in 1:n) {
  
  log_lik[i] = bernoulli_logit_lpmf( co[i] | lin_pred[i] );
  
}

}
