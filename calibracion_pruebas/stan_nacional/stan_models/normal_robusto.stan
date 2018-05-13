
data {
  int H; 
  int n_covariates;
  vector<lower=0>[H] y; // observed vote counts
  matrix[H, n_covariates] x;
  int N_h[H];
  int n_h[H];
  int estado[H];
  int total_votos[H];
  int m;
}

parameters {
  vector[m] beta_estado_raw;
  real mu_estado;
  vector[n_covariates] beta;
  real<lower=0> sigma[m];
  real<lower=0> sigma_estado;
}

transformed parameters {
   vector<lower=0,upper=1>[H] theta;
   vector[H] pred;
   vector[m] beta_estado;

   pred = x * beta;
   beta_estado = mu_estado + beta_estado_raw * sigma_estado ;
   theta = inv_logit(beta_estado[estado] + pred);

}

model {
  
  y ~ normal(750 * to_vector(n_h) .* theta, 750*sqrt(to_vector(n_h)) .* to_vector(sigma[estado]));
  
  beta ~ normal(0, 1);
  beta_estado_raw ~ normal(0, 1);
  sigma ~ normal(0, 5);
  sigma_estado ~ normal(0, 1);
  mu_estado ~ normal(0, 5);
}

generated quantities {
  real total;
  real num;
  real denom;
  num = 0;
  for(i in 1:H){
    num += 750*normal_rng(N_h[i] * theta[i], sqrt(N_h[i]) * sigma[estado[i]]);
  }
  denom = 0;
  for(i in 1:H){
    denom += total_votos[i] * N_h[i] / n_h[i];
  }
  total = num / denom;
}
