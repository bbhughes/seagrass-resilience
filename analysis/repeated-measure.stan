data {
  int N;               // number of observations
  int K;               // number of predictors
  vector[N] y;
  int<lower=0> n_tank;
  matrix[n_tank, K] X;      // model predictor matrix
  int<lower=1, upper=n_tank> tank[N];
}
parameters {
  vector[n_tank] tank_mu;
  real<lower=0> tank_sigma[n_tank];
  vector[K] beta;      // vector of predictors
  real alpha;          // intercept
  real<lower=0> sigma; // residual sd
} 
model { 
  tank_mu ~ student_t(3, 0, 10);
  tank_sigma ~ student_t(3, 0, 1);
  sigma ~ student_t(3, 0, 1);  // prior
  alpha ~ student_t(3, 0, 10);  // prior
  beta ~ student_t(3, 0, 1);   // prior
  
  for (i in 1:N) {
    y[i] ~ normal(tank_mu[tank[i]], tank_sigma[tank[i]]);
  }
  
  for (j in 1:n_tank) {
   tank_mu[j] ~ normal(alpha + X * beta, sigma);
  }
}
