data {
  int<lower = 0> n;
  vector[n] Y;
  vector[n] X;
}
parameters {
  real beta0;
  real beta1;
  real<lower = 0> sigma;
}
model {
  Y ~ normal(beta0 + beta1 * X, sigma);
  beta0 ~ normal(-2000, 1000);
  beta1 ~ normal(100, 40);
  sigma ~ exponential(0.0008);
}
