
data {
  int N; // number of stations
  int k; // number of parties
  int n_strata; 
  int n_covariates;
  int<lower=0> y[N,k]; // observed vote counts
  vector<lower=0, upper=750>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates] x[k];
  
  int N_f; // total number of stations
  int n_strata_f; 
  int n_covariates_f;
  vector<lower=0, upper=750>[N_f, k] y_f; // observed vote counts
  vector[N_f] in_sample;
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f[k];
  
}

parameters {
  real beta_0[k];
  real beta_0_p[k];
  vector[n_covariates] beta[k];
  vector[n_covariates] beta_p[k];
  vector<lower=0>[n_strata] beta_bn;
  vector<lower=0>[n_strata] sigma_st;
  vector<lower=0>[n_strata] sigma_st_p;
  real<lower=0> sigma;
  vector[n_strata] beta_st_raw;
  vector[n_strata] beta_st_p_raw;
  vector<lower=0>[n_strata] nu;
}

transformed parameters {
   vector[N] theta; 
   vector<lower=0>[N] alpha_bn;

   vector[N] theta_p;
   vector<lower=0, upper=1>[N] p; 
   vector[n_strata] beta_st;
   vector[n_strata] beta_st_p;
   vector[N] pred;

    
    beta_st = beta_0 + beta_st_raw .* sigma_st;
    beta_st_p = beta_0_p + beta_st_p_raw .* sigma_st_p;
    pred = x * beta;
  
  for(i in 1:N){
    theta[i] = inv_logit(beta_st[stratum[i]] + pred[i]);
    theta_p[i] = beta_st_p[stratum[i]];  // + pred_p[i];
  }
  
  alpha_bn = n .* theta;
  p = inv_logit(theta_p);


}

model {
  for(i in 1:N){
    if(y[i] == 0) 
      target += log_sum_exp(bernoulli_lpmf(0 | p[i]),
        bernoulli_lpmf(0 | p[i]) + neg_binomial_lpmf(y[i] | alpha_bn[i]*beta_bn[stratum[i]], beta_bn[stratum[i]]));
    else 
      target += bernoulli_lpmf(1 | p[i]) + 
          neg_binomial_lpmf(y[i] | alpha_bn[i]*beta_bn[stratum[i]], beta_bn[stratum[i]]);
      

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
  beta_bn ~ normal(0, 1);
}


generated quantities {
  vector[N_f] y_sim;
  vector[N_f] alpha_bn_f;
  vector[N_f] theta_f;

  
   for(i in 1:N_f){
    theta_f[i] = inv_logit(beta_st[stratum_f[i]] + dot_product(x_f[i, ], beta));
    alpha_bn_f[i] =  n_f[i] * theta_f[i];
  }
  
  for(i in 1:N_f){
    if(in_sample[i] == 1){
      y_sim[i] = y_f[i];
    } else {
      real b_sim = bernoulli_rng(inv_logit(beta_st_p[stratum_f[i]]));
      if(b_sim == 0){
        y_sim[i] = 0;
      } else {
        y_sim[i] = neg_binomial_rng(alpha_bn_f[i] * beta_bn[stratum_f[i]], beta_bn[stratum_f[i]]);
      }
    }
  }
}