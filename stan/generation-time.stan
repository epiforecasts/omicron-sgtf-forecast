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
  real gt_mean;
  real<lower = 0> gt_sd;
  real voc_gt_mean_mod;
  real voc_gt_sd_mod;
  real ta;
  real<lower = 0> sigma;
}

transformed parameters {
   real voc_gt_mean;
   real voc_gt_sd;
   vector[t] voc_R;
   vector[t] nvoc_R;
   vector[t] approx_nvoc_R;

   voc_gt_mean = gt_mean * voc_gt_mean_mod;
   voc_gt_sd = gt_sd * voc_gt_sd_mod;

   gt_pmf = 
   voc_R = growth_to_R(voc_r, voc_gt_mean, voc_gt_sd);
   nvoc_R = growth_to_R(nvoc_r, gt_mean, gt_sd);

   approx_nvoc_R = ta * voc_R;
}

model {
  gt_mean ~ normal(gt_mean_mean, gt_mean_sd);
  gt_sd ~ normal(gt_sd_mean, gt_sd_sd);

  voc_gt_mean_mod ~ lognormal(0, 1);
  voc_gt_sd_mod ~ lognormal(0, 1);

  ta ~ lognormal(0, 1);

  sigma ~ normal(0, 0.01);
  nvoc_R ~ normal(approx_nvoc_R, sigma);
}

generated quantities {
}
