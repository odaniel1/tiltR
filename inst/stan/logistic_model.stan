data {
  // observations
  int<lower=0> N;       // no. samples
  real<lower=0> t[N];   // time
  real<lower=1> sg[N];  // specific gravity (points)

  // calibration points
  int<lower=0> N_cal;       // no. samples
  real<lower=0> t_cal[N_cal];   // time
  real<lower=1> sg_cal[N_cal];  // specific gravity (points)

  // Knowns
  real<lower=1> og;     // original gravity (points)

  // Prior parameters
  real<lower=1> fg_ant; // anticipated final gravity (points)
  real<lower=0> fg_sd;  // std. dev for fg.

  // generated quantities parameters
  int<lower=0> days;    // no. days to measure
}

parameters {
  real<upper=0> b;
  real M;
  real<lower=1> fg;
  real<lower=0> nu;
  real<lower=0> sigma;
  real<lower=0> sigma_cal;
}

transformed parameters {
  real f[N];
  real f_cal[N_cal];

  for(n in 1:N)
    f[n] = fg + (og-fg) / (1 + exp(-b * (t[n] - M)))^(1/nu);

  for(n in 1:N_cal)
    f_cal[n] = fg + (og-fg) / (1 + exp(-b * (t_cal[n] - M)))^(1/nu);
}

model {
  // priors
  fg ~ normal(fg_ant, fg_sd);
  b ~ normal(0, 2);
  M ~ exponential(1);
  nu ~ exponential(1);
  sigma ~ normal(1, 0.05);
  sigma_cal ~ normal(1, 0.2);

  // likelihood
  sg ~ normal(f, sigma);
  sg_cal ~ normal(f_cal, sigma_cal);
}

generated quantities{
  real sg_fit[1 + 48 *days];

  for(s in 1:(1 + 48*days))
    sg_fit[s] = normal_rng(0,sigma) + fg + (og-fg) / (1 + exp(-b * ((s-1)/48.0 - M)))^(1/nu);
}
