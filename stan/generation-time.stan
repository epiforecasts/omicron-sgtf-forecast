functions {
#include functions/R_gamma_approx.stan
}

data {
  int t;
  vector[t] voc_r;
  vector[t] nvoc_r;
  real gt_mean_mean;
  real gt_mean_sd;
  real gt_sd_mean;
  real gt_sd_sd;
}

parameters {
  real<lower = 0> gt_mean;
  real<lower = 0> gt_sd;
  real<lower = 0> voc_gt_mean_mod;
  real<lower = 0> voc_gt_sd_mod;
  real<lower = 0> ta;
  real<lower = 0> sigma;
}

transformed parameters {
   real<lower = 0> voc_gt_mean;
   real<lower = 0> voc_gt_sd;
   vector[t] approx_voc_r;

   voc_gt_mean = gt_mean * voc_gt_mean_mod;
   voc_gt_sd = gt_sd * voc_gt_sd_mod;
   
  {
    vector[t] nvoc_R;
    nvoc_R = growth_to_R(nvoc_r, gt_mean, gt_sd);
    approx_voc_r = R_to_growth(ta * nvoc_R, voc_gt_mean, voc_gt_sd);
  }
}

model {
  gt_mean ~ normal(gt_mean_mean, gt_mean_sd);
  gt_sd ~ normal(gt_sd_mean, gt_sd_sd) T[0,];
  voc_gt_mean_mod ~ lognormal(0, 1);
  voc_gt_sd_mod ~ lognormal(0, 1);

  ta ~ lognormal(0, 1);

  sigma ~ normal(0, 0.1) T[0,];
  voc_r ~ normal(approx_voc_r, sigma);
}

generated quantities {
  vector[t] pp_voc_r;
  for (i in 1:t) {
    pp_voc_r[i] = normal_rng(approx_voc_r[i], sigma);
  }
  
}
