vector growth_to_R(vector r, real gt_mean, real gt_sd) {
  int t = num_elements(r);
  vector[t] R = rep_vector(1e-8, t);
  if (gt_sd > 1e-2) {
    vector[t] precomp;
    real k = pow(gt_sd / gt_mean, 2);
    if (is_nan(k)) {
      reject("growth_to_R: k must be real; found k = ", k);
    }
    if (is_inf(k)) {
      reject("growth_to_R: k must be real; found k = ", k);
    }
    if (k < 1e-3) {
      reject("growth_to_R: k must be greater than 1e-3; found k = ", k);
    }
    precomp = r * k * gt_mean;
    precomp = pow(1 + precomp, 1 / k);
    for (s in 1:t) {
      R[s] += precomp[s]; 
      if (is_nan(R[s])) {
        reject("growth_to_R: R[s] must be real; found R[s] = ", R[s]);
      }
      if (is_inf(R[s])) {
        reject("growth_to_R: R[s] must be real; found R[s] = ", R[s]);
      }
    }
  } else {
    // limit as gt_sd -> 0
    R = exp(gt_mean * r);
  }
  return(R);
}

vector R_to_growth(vector R, real gt_mean, real gt_sd) {
  int t = num_elements(R);
  vector[t] r = rep_vector(1e-8, t);
  if (gt_sd > 1e-2) {
    vector[t] precomp;
    real k = pow(gt_sd / gt_mean, 2);
    if (is_nan(k)) {
      reject("R_to_growth: k must be real; found k = ", k);
    }
    if (is_inf(k)) {
      reject("R_to_growth: k must be real; found k = ", k);
    }
    if (k < 1e-3) {
      reject("R_to_growth: k must be greater than 1e-3; found k = ", k);
    }
    precomp = (pow(R, k) - 1) / (k * gt_mean);
    for (s in 1:t) {
      r[s] += precomp[s];
      if (is_nan(r[s])) {
        reject("R_to_growth: r[s] must be real; found r[s] = ", r[s]);
      }
      if (is_inf(r[s])) {
        reject("R_to_growth: r[s] must be real; found r[s] = ", r[s]);
      }
    }
  } else {
    // limit as gt_sd -> 0
    r = log(R) / gt_mean;
  }
  return(r);
}

vector r_to_r(vector r, real G, real k, vector alpha) {
  int t = num_elements(r);
  vector[t] r_n;
  vector[t] palpha = pow(alpha, k);
  r_n = palpha .* r + (palpha - 1)/(k* G);
  return(r_n);
}

vector r_to_r_diff_gt(vector r, real G, real k, real G_v, real k_v,
                      vector alpha) {
  int t = num_elements(r);
  vector[t] r_n;
  r_n = pow(alpha, k_v) .* pow(1 + r * k * G, k_v / k) - 1;
  r_n = r_n / (k_v * G_v);
  return(r_n);
}

real sd_to_k(real G_sd, real G) {
  real k;
  k = exp(log(G_sd) - log(G));
  k = pow(k, 2);
  return(k);
}
real k_to_sd(real k, real G) {
  real G_sd;
  G_sd = exp(log(sqrt(k)) + log(G));
  return(G_sd);
}
