functions {
#include functions/R_gamma_approx.stan
}

data {
  int t;
  vector[t] nvoc_r_mean;
  vector[t] nvoc_r_sd;
  vector[t] voc_r;
  vector[t] voc_sd2;
  real gt_mean_mean;
  real gt_mean_sd;
  real gt_sd_mean;
  real gt_sd_sd;
  int l;
  int loc[t];
  int gt_diff;
  int debug;
}

parameters {
  real<lower = 0> gt_mean;
  real<lower = 0> gt_sd;
  vector[t] nvoc_r;
  real<lower = 0, upper = 2> m_gt[gt_diff];
  real<lower = 0, upper = 2> m_gt_sd[gt_diff];
  real ta;
  real<lower = 0> ta_sd;
  vector<offset = ta, multiplier = ta_sd>[l] local_ta;
  real<lower = 0> sigma;
}

transformed parameters {
   real<lower = 0> voc_gt_mean;
   real<lower = 0> voc_gt_sd;
   real<lower = 0> k;
   real<lower = 0> k_v;
   vector[t] e_voc_r;
   vector[t] combined_sigma;
   k = sd_to_k(gt_sd, gt_mean);
   if (gt_diff) {
    voc_gt_mean = gt_mean * m_gt[1];
    voc_gt_sd = gt_sd * m_gt_sd[1];
   }else{
    voc_gt_mean = gt_mean;
    voc_gt_sd = gt_sd;
   }
   k_v = sd_to_k(voc_gt_sd, voc_gt_mean);
  {
    vector[t] rel_ta;
    for (i in 1:t) {
      rel_ta[i] = local_ta[loc[i]];
    }
    if (gt_diff) {
      e_voc_r = r_to_r_diff_gt(nvoc_r, gt_mean, k, voc_gt_mean, k_v,
                                 exp(rel_ta));
    }else{
      e_voc_r = r_to_r(nvoc_r, gt_mean, k, exp(rel_ta));
    }
  }
  combined_sigma = sqrt(square(sigma) + voc_sd2);
  if (debug) {
    int j = 0;
    for (i in 1:t) {
        j += is_nan(e_voc_r[i]) ? 1 : 0;
      }
      if (j) {
        print("Issue with iteration");
        print(e_voc_r);
        print(ta);
        print(gt_mean);
        print(gt_sd);
        print(k);
        print(voc_gt_mean);
        print(voc_gt_sd);
        print(k_v);
      }
  }
}

model {
  gt_mean ~ normal(gt_mean_mean, gt_mean_sd) T[0,];
  gt_sd ~ normal(gt_sd_mean, gt_sd_sd) T[0,];

  nvoc_r ~ normal(nvoc_r_mean, nvoc_r_sd);
  if (gt_diff) {
    m_gt[1] ~ lognormal(0, 0.25);
    m_gt_sd[1] ~ lognormal(0, 0.1);
  }

  ta ~ normal(0, 1);
  ta_sd ~ normal(0, 0.01) T[0,];
  local_ta ~ normal(ta, ta_sd);

  sigma ~ normal(0, 0.01) T[0,];
  voc_r ~ normal(e_voc_r, combined_sigma);
}

generated quantities {
  vector[t] pp_voc_r;
  vector[t] log_lik;
  for (i in 1:t) {
    pp_voc_r[i] = normal_rng(e_voc_r[i], combined_sigma[i]);
    log_lik[i] = normal_lpdf(voc_r[i] | e_voc_r[i], combined_sigma[i]);
  }
}
