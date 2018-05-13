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
  int k; // number of parties
  matrix<lower=0, upper=750>[N, k] y; // observed vote counts
  vector<lower=0, upper=750>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates] x;
  
  int N_f; // total number of stations
  int n_strata_f; 
  int n_covariates_f;
  matrix<lower=0, upper=750>[N_f, k] y_f; // observed vote counts
  vector[N_f] in_sample;
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;
  
}

parameters {
  vector[k] beta_0;
  vector[k] beta_0_p;
  matrix[n_covariates, k] beta;
  matrix[n_covariates, k] beta_p;
  matrix<lower=0>[n_strata, k] sigma_st;
  matrix<lower=0>[n_strata, k] sigma_st_p;
  matrix<lower=0>[n_strata, k] sigma;
  matrix[n_strata, k] beta_st_raw;
  matrix[n_strata, k] beta_st_p_raw;
  matrix<lower=0>[n_strata, k] nu;
}

transformed parameters {
   matrix[N, k] theta; 
   matrix[N ,k] theta_p;
   matrix[N,k] mu; 
   matrix<lower=0, upper=1>[N,k] p; 
   matrix[n_strata,k] beta_st;
   matrix[n_strata, k] beta_st_p;
   matrix<lower=0>[N,k] sigma_obs;
   matrix[N,k] pred;

  beta_st =   beta_st_raw .* sigma_st;
  beta_st_p = to_matrix(beta_0_p, n_covariates,k) + beta_st_p_raw .* sigma_st_p;
  pred = x * beta;
  
  for(i in 1:N){
    for(j in 1:k){
    theta[i,j] = beta_0[j] + beta_st[stratum[i],j] + pred[i,j];
    theta_p[i,j] = beta_0_p[j] + beta_st_p[stratum[i],j];  // + pred_p[i];
  }
  }
  mu = to_matrix(n, N, k) .* inv_logit(theta);
  p = inv_logit(theta_p);

  for(i in 1:N) {
    for(j in 1:k){
    sigma_obs[i,j] = sqrt(n[i]) * sigma[stratum[i],j];
  }
  }
}

model {
  for(i in 1:N){
  for(j in 1:k){
    if(y[i,j] == 0) 
      target += bernoulli_lpmf(0 | p[i,j]);
    else 
      target += bernoulli_lpmf(1 | p[i,j]) + 
          student_t_lpdf(y[i,j] | nu[stratum[i],j], mu[i,j], sigma_obs[i,j]);
     target += -log_diff_exp(student_t_lcdf(750| nu[stratum[i],j], mu[i,j], sigma_obs[i,j]),
                             student_t_lcdf(0  | nu[stratum[i],j], mu[i,j], sigma_obs[i,j]));
}
  }
  
  beta_0 ~ normal(0, 5);
  beta_0_p ~ normal(0, 5);
 for(j in 1:k) {

  to_vector(beta[,j]) ~ normal(0 , 1);
  to_vector(beta_p[,j]) ~ normal(0 , 1);
  to_vector(beta_st_raw[,j])   ~ normal(0, 1);
  to_vector(beta_st_p_raw[,j]) ~ normal(0, 1);
  to_vector(sigma[,j]) ~ normal(0, 1);
  to_vector(sigma_st[,j]) ~ normal(0, 2);
  to_vector(sigma_st_p[,j]) ~ normal(0, 2);
  to_vector(nu[,j]) ~ gamma(2, 0.1);
 }
}
