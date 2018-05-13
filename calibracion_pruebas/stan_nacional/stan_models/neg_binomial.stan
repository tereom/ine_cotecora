

data {
  int N; // number of stations
  int n_strata; 
  int n_covariates;
  int<lower=0> y[N]; // observed vote counts
  vector<lower=0>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates] x;
  
  int N_f; // total number of stations
  int n_strata_f; 
  int n_covariates_f;
  vector<lower=0>[N_f] y_f; // observed vote counts
  vector[N_f] in_sample;
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;
  
}

parameters {
  real beta_0;
  vector[n_covariates] beta;
  vector<lower=0>[n_strata] beta_bn;
  vector<lower=0>[n_strata] sigma_st;
  vector[n_strata] beta_st_raw;
}

transformed parameters {
   vector<lower=0, upper=1>[N] theta; 
   vector<lower=0>[N] alpha_bn;
   vector[n_strata] beta_st;
   vector[N] pred;

   beta_st = beta_0 + beta_st_raw .* sigma_st;
   pred = x * beta;
   theta = inv_logit(beta_st[stratum] + pred);
   alpha_bn = n .* theta;

}

model {

  beta_0 ~ normal(0, 3);
  beta ~ normal(0 , 1);
  beta_st_raw   ~ normal(0, 3);
  sigma_st ~ normal(0, 2);
  beta_bn ~ normal(0, 5);

  y ~ neg_binomial( alpha_bn .* beta_bn[stratum], beta_bn[stratum]);

}

generated quantities {
  vector[N_f] y_sim;
  vector[N_f] alpha_bn_f;
  vector<lower=0, upper=1>[N_f] theta_f;
  vector[N_f] pred_f;

  pred_f = x_f * beta;
  theta_f = inv_logit(beta_st[stratum_f] + pred_f);
  alpha_bn_f =  n_f .* theta_f;
 
  
  for(i in 1:N_f){
        y_sim[i] = neg_binomial_rng(alpha_bn_f[i] * beta_bn[stratum_f[i]], beta_bn[stratum_f[i]]);
  }
    
  
}