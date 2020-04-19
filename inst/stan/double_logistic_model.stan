data {
  // observations
  int<lower=0> N;       // no. samples
  real<lower=0> t[N];   // time
  real<lower=1> sg[N];  // specific gravity (points)

  // Knowns
  real<lower=1> og;     // original gravity (points)

  // Prior parameters
  real<lower=1> fg_ant; // anticipated final gravity (points)
  real<lower=0> fg_sd;   // std. dev for fg.

  // generated quantities parameters
  int<lower=0> days;    // no. days to measure
}

parameters {
  real<upper=0> b[2];
  ordered[2] M;
  real<lower=0> nu[2];
  real<lower=0,upper=1> theta;
  real<lower=1> fg;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=1> mg;
  real f1[N];
  real f2[N];
  real f[N];

  mg = theta * fg + (1 - theta) * og;

  for(n in 1:N){
    f1[n] = mg + (og-mg) / (1 + exp(-b[1] * (t[n] - M[1])))^(1/nu[1]);
    f2[n] = (mg - fg) - (mg-fg) / (1 + exp(-b[2] * (t[n] - M[2])))^(1/nu[2]);
    f[n]  = f1[n] - f2[n];
  }
}

model {
  // priors
  fg ~ normal(fg_ant, fg_sd);
  b ~ normal(0, 2);
  M ~ exponential(1);
  nu ~ exponential(1);
  theta ~ beta(20,5);
  sigma ~ normal(1, 0.05);

  // likelihood
  sg ~ normal(f, sigma);
}

generated quantities{
  real sg_fit1[1 + 48 * days];
  real sg_fit2[1 + 48 * days];
  real sg_fit[1 + 48 *days];

  for(s in 1:(1 + 48*days)){
    sg_fit1[s] = mg + (og-mg) / (1 + exp(-b[1] * ((s-1)/48.0 - M[1])))^(1/nu[1]);
    sg_fit2[s] = (mg - fg) - (mg-fg) / (1 + exp(-b[2] * ((s-1)/48.0 - M[2])))^(1/nu[2]);
    sg_fit[s] = normal_rng(0,sigma) + sg_fit1[s] - sg_fit2[s];
  }
}
