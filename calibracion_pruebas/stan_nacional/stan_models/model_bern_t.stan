functions {
real student_t_trunc_rng(real nu, real mu, real sigma, real lb, real ub) {
    real x_sim;
    real rejection;
    rejection = -1;
    while(rejection < 0){
      x_sim = student_t_rng(nu, mu, sigma);
      if(x_sim > lb && x_sim < ub){
        rejection = 1;
      }
    }
    return x_sim;
  }
}

data {
  int N; // number of stations
  int n_strata; 
  int n_covariates;
  vector<lower=0, upper=750>[N] y; // observed vote counts
  vector<lower=0, upper=750>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates] x;
  
  int N_f; // total number of stations
  int n_strata_f; 
  int n_covariates_f;
  vector<lower=0, upper=750>[N_f] y_f; // observed vote counts
  vector[N_f] in_sample;
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;
  
}

parameters {
  real beta_0;
  real beta_0_p;
  vector[n_covariates] beta;
  vector[n_covariates] beta_p;
  vector<lower=0>[n_strata] sigma_st;
  vector<lower=0>[n_strata] sigma_st_p;
  real<lower=0> sigma;
  vector[n_strata] beta_st_raw;
  vector[n_strata] beta_st_p_raw;
  vector<lower=0>[n_strata] nu;
}

transformed parameters {
   vector[N] theta; 
   vector[N] theta_p;
   vector[N] mu; 
   vector<lower=0, upper=1>[N] p; 
   vector[n_strata] beta_st;
   vector[n_strata] beta_st_p;
   vector<lower=0>[N] sigma_obs;
   vector[N] pred;

  beta_st = beta_0 + beta_st_raw .* sigma_st;
  beta_st_p = beta_0_p + beta_st_p_raw .* sigma_st_p;
  pred = x * beta;
  
  for(i in 1:N){
    theta[i] = inv_logit(beta_st[stratum[i]] + pred[i]);
    theta_p[i] = beta_st_p[stratum[i]];  // + pred_p[i];
  }
  mu = n .* theta;
  p = inv_logit(theta_p);

  for(i in 1:N) {
    sigma_obs[i] = sqrt(n[i]) * sigma * sqrt(theta[i]*(1-theta[i]));
  }
}

model {
  for(i in 1:N){
    if(y[i] == 0) 
      target += bernoulli_lpmf(0 | p[i]);
    else 
      target += bernoulli_lpmf(1 | p[i]) + 
          student_t_lpdf(y[i] | nu[stratum[i]], mu[i], sigma_obs[i]);
      target += -log_diff_exp(student_t_lcdf(750 | nu[stratum[i]], mu[i], sigma_obs[i]),
                             student_t_lcdf(   0 | nu[stratum[i]], mu[i], sigma_obs[i]));

  }
 
  beta_0 ~ normal(0, 2);
  beta_0_p ~ normal(0, 2);
  beta ~ normal(0 , 1);
  beta_p ~ normal(0 , 1);
  beta_st_raw   ~ normal(0, 1);
  beta_st_p_raw ~ normal(0, 1);
  sigma ~ normal(0, 2);
  sigma_st ~ normal(0, 2);
  sigma_st_p ~ normal(0, 2);
  nu ~ normal(0, 20);
}

generated quantities {
  vector[N_f] y_sim;
  vector<lower=0>[N_f] sigma_obs_f;
  vector[N_f] mu_f;
  vector[N_f] theta_f;

  
   for(i in 1:N_f){
    theta_f[i] = inv_logit(beta_st[stratum_f[i]] + dot_product(x_f[i, ], beta));
    sigma_obs_f[i] = sqrt(n_f[i]) * sigma *sqrt(theta_f[i]*(1-theta_f[i]));
    mu_f[i] =  n_f[i] * theta_f[i];
  }
  
  for(i in 1:N_f){
    if(in_sample[i] == 1){
      y_sim[i] = y_f[i];
      mu_f[i] = 0;
      theta_f[i] = 0;
    } else {
      real b_sim = bernoulli_rng(inv_logit(beta_st_p[stratum_f[i]] ));
      if(b_sim == 0){
        y_sim[i] = 0;
      } else {
        y_sim[i] = student_t_trunc_rng(nu[stratum_f[i]], mu_f[i], sigma_obs_f[i], 0, 750);
      }
    }
  }
}
