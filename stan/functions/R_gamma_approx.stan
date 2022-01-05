vector growth_to_R(vector r, real gt_mean, real gt_sd) {
  int t = num_elements(r);
  vector[t] R = rep_vector(1e-8, t);
  if (gt_sd > 1e-2) {
    vector[t] precomp;
    real k = pow(gt_sd / gt_mean, 2);
    precomp = r * k * gt_mean;
    precomp = pow(1 + precomp, 1 / k);
    for (s in 1:t) {
      R[s] += precomp[s]; 
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
    precomp = (pow(R, k) - 1) / (k * gt_mean);
    for (s in 1:t) {
      r[s] += precomp[s];
    }
  } else {
    // limit as gt_sd -> 0
    r = log(R) / gt_mean;
  }
  return(r);
}