

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
  real beta_0_p;
  //vector[n_covariates] beta;
  vector[n_covariates] beta_p;
  vector<lower=0>[n_strata] beta_bn;
  vector<lower=0>[n_strata] sigma_st;
  vector<lower=0>[n_strata] sigma_st_p;
  real<lower=0> sigma;
  vector[n_strata] beta_st_raw;
  vector[n_strata] beta_st_p_raw;
}

transformed parameters {
   vector[N] theta; 
   vector<lower=0>[N] alpha_bn;

   vector[N] theta_p;
   vector<lower=0, upper=1>[N] p; 
   vector[n_strata] beta_st;
   vector[n_strata] beta_st_p;
   //vector[N] pred;
   real test_neg[N];

    
    beta_st = beta_0 + beta_st_raw .* sigma_st;
    beta_st_p = beta_0_p + beta_st_p_raw .* sigma_st_p;
    //pred = x * beta;

  //for(i in 1:N){
    theta = inv_logit(beta_st[stratum]);
    theta_p = beta_st_p[stratum];  // + pred_p[i];
  //}
  
    alpha_bn = n .* theta;
    p = inv_logit(theta_p);
    for(i in 1:N){
    test_neg[i] = neg_binomial_lpmf(y[i] | alpha_bn[i] .* beta_bn[stratum[i]], beta_bn[stratum[i]]);
    }

}

model {
  
  for(i in 1:N){
    if(y[i] == 0) 
      target += log_sum_exp(bernoulli_lpmf(0 | p[i]),
        bernoulli_lpmf(1 | p[i]) + test_neg[i]);
    else 
      target += bernoulli_lpmf(1 | p[i]) + test_neg[i];
      

  }
 
  beta_0 ~ normal(0, 3);
  beta_0_p ~ normal(0, 3);
  //beta ~ normal(0 , 1);
  beta_p ~ normal(0 , 1);
  beta_st_raw   ~ normal(0, 3);
  beta_st_p_raw ~ normal(0, 3);
  sigma ~ normal(0, 10);
  sigma_st ~ normal(0, 2);
  sigma_st_p ~ normal(0, 2);
  beta_bn ~ normal(0, 5);
}


