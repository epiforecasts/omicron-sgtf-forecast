vector growth_to_R(vector r, real gt_mean, real gt_sd) {
  int t = num_elements(r);
  vector[t] R = rep_vector(1e-8, t);
  if (gt_sd > 0) {
    real k = pow(gt_sd / gt_mean, 2);
    for (s in 1:t) {
      R[s] += pow(1 + r[s] * (k * gt_mean), 1 / k); 
    }
  } else {
    // limit as gt_sd -> 0
    for (s in 1:t) {
      R[s] += exp(gt_mean * r[s]);
    }
  }
  return(R);
}

vector R_to_growth(vector R, real gt_mean, real gt_sd) {
  int t = num_elements(R);
  vector[t] r = rep_vector(1e-8, t);
  if (gt_sd > 0) {
    real k = pow(gt_sd / gt_mean, 2);
    for (s in 1:t) {
      r[s] += (pow(R[s], k) - 1) / (k * gt_mean);
    }
  } else {
    // limit as gt_sd -> 0
    for (s in 1:t) {
      r[s] += log(R[s]) / gt_mean;
    }
  }
  return(r);
}